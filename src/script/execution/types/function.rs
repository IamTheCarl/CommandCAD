use std::{fmt::Debug, rc::Rc};

use fortuples::fortuples;

use crate::script::{
    execution::{run_named_block, ControlFlow, ExecutionContext, ExecutionResult},
    parsing::{Expression, NamedBlock, VariableType},
    LogMessage, RuntimeLog, Span,
};

use super::{NamedObject, Object, Value};

#[derive(Clone)]
pub struct UserFunction<'a, S: Span> {
    pub block: &'a NamedBlock<S>,
    pub return_type: &'a VariableType<S>,
}

impl<'a, S: Span> Object<'a, S> for UserFunction<'a, S> {
    fn matches_type(&self, _ty: &VariableType<S>) -> bool {
        false
    }

    fn call(
        &self,
        context: &mut ExecutionContext<'a, S>,
        span: &S,
        arguments: Vec<Value<'a, S>>,
        spans: &[Expression<S>],
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        context.new_isolated_scope(|context| {
            run_named_block(context, self.block, arguments, spans, span)
                .map_err(|_| ControlFlow::Failure)
        })
    }
}

impl<'a, S: Span> Debug for UserFunction<'a, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("UserFunction").finish()
    }
}

impl<'a, S: Span> PartialEq for UserFunction<'a, S> {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl<'a, S: Span> NamedObject for UserFunction<'a, S> {
    fn static_type_name() -> &'static str {
        "Function"
    }
}

pub trait BuiltinFunctionPointer<'a, S: Span + 'a>:
    Fn(
    &mut RuntimeLog<S>,
    &S,
    Vec<Value<'a, S>>,
    &[Expression<S>],
) -> ExecutionResult<'a, S, Value<'a, S>>
{
}

impl<'a, S, F> BuiltinFunctionPointer<'a, S> for F
where
    S: Span + 'a,
    F: Fn(
        &mut RuntimeLog<S>,
        &S,
        Vec<Value<'a, S>>,
        &[Expression<S>],
    ) -> ExecutionResult<'a, S, Value<'a, S>>,
{
}

#[derive(Clone)]
pub struct BuiltinFunction<'a, S: Span>(Rc<dyn BuiltinFunctionPointer<'a, S>>);

impl<'a, S: Span> Object<'a, S> for BuiltinFunction<'a, S> {
    fn matches_type(&self, _ty: &VariableType<S>) -> bool {
        false
    }

    fn call(
        &self,
        context: &mut ExecutionContext<'a, S>,
        span: &S,
        arguments: Vec<Value<'a, S>>,
        expressions: &[Expression<S>],
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        self.0(&mut context.log, span, arguments, expressions)
    }
}

impl<S: Span> Debug for BuiltinFunction<'_, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BuiltinFunction").finish()
    }
}

impl<S: Span> PartialEq for BuiltinFunction<'_, S> {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl<S: Span> NamedObject for BuiltinFunction<'_, S> {
    fn static_type_name() -> &'static str {
        "BuiltinFunction"
    }
}

pub trait UnpackArguments<'a, S: Span, Tuple> {
    fn unpack_arguments(
        log: &mut RuntimeLog<S>,
        span: &S,
        arguments: Vec<Value<'a, S>>,
        expressions: &[Expression<S>],
    ) -> ExecutionResult<'a, S, Tuple>;
}

#[rustfmt::skip]
fortuples! {
    impl<'a, S> UnpackArguments<'a, S, #Tuple> for #Tuple
    where
	S: Span + 'a,
        #(#Member: NamedObject),*
	#(Value<'a, S>: TryInto<#Member>),*
    {
	fn unpack_arguments(
            log: &mut RuntimeLog<S>,
            _span: &S,
            mut arguments: Vec<Value<'a, S>>,
            expressions: &[Expression<S>],
	) -> ExecutionResult<'a, S, #Tuple> {
	    arguments.reverse();
	    let mut _expression_iter = expressions.iter();
	    
	    #(let casey::lower!(#Member) = {
		if let Some(value) = arguments.pop() {
		    value.downcast(log, _expression_iter.next().unwrap().get_span())?
		} else {
		    log.push(LogMessage::MissingArguments(_span.clone()));
		    return Err(ControlFlow::Failure);
		}
	    };)*

	    for expression in _expression_iter {
		log.push(LogMessage::TooManyArguments(expression.get_span().clone()));
	    }
	    
            Ok((#(casey::lower!(#Member)),*))
	}
    }
}

pub trait AutoCall<'a, S, T>
where
    S: Span,
{
    type Unpacker: UnpackArguments<'a, S, T>;

    fn auto_call(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        arguments: Vec<Value<'a, S>>,
        expressions: &[Expression<S>],
    ) -> ExecutionResult<'a, S, Value<'a, S>>;
}

#[rustfmt::skip]
fortuples! {
    impl<'a, S, F> AutoCall<'a, S, #Tuple> for F
    where
	S: Span + 'a,
	#(#Member: NamedObject),*
        #(Value<'a, S>: TryInto<#Member>),*
	F: Fn(&mut RuntimeLog<S>, &S, #(#Member),*) -> ExecutionResult<'a, S, Value<'a, S>>,
    {
	type Unpacker = #Tuple;

	fn auto_call(
            &self,
            log: &mut RuntimeLog<S>,
            span: &S,
            arguments: Vec<Value<'a, S>>,
            expressions: &[Expression<S>],
	) -> ExecutionResult<'a, S, Value<'a, S>> {
            let (#(casey::lower!(#Member)),*) = Self::Unpacker::unpack_arguments(log, span, arguments, expressions)?;

            (self)(log, span, #(casey::lower!(#Member)),*)
	}
    }
}

pub trait IntoBuiltinFunction<'a, S: Span, T>: AutoCall<'a, S, T> {
    fn into_builtin_function(self) -> BuiltinFunction<'a, S>;
}

#[rustfmt::skip]
 fortuples! {
     impl<'a, S, F> IntoBuiltinFunction<'a, S, #Tuple> for F
     where
 	S: Span +'a,
 	F: Fn(&mut RuntimeLog<S>, &S, #(#Member),*) -> ExecutionResult<'a, S, Value<'a, S>> + 'static,
 	#(#Member: NamedObject),*
         #(Value<'a, S>: TryInto<#Member>),*
     {
 	fn into_builtin_function(self) -> BuiltinFunction<'a, S> {
 	    BuiltinFunction(Rc::new(move |log: &mut RuntimeLog<S>, span: &S, arguments: Vec<Value<'a, S>>, expressions: &[Expression<S>]| -> ExecutionResult<'a, S, Value<'a, S>> {
 		self.auto_call(log, span, arguments, expressions)
 	    }))
 	}
     }
 }
