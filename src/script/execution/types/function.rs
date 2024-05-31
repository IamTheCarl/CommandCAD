/*
 * Copyright 2024 James Carl
 * AGPL-3.0-only or AGPL-3.0-or-later
 *
 * This file is part of Command Cad.
 *
 * Command CAD is free software: you can redistribute it and/or modify it under the terms of
 * the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License along with this
 * program. If not, see <https://www.gnu.org/licenses/>.
 */

use std::rc::Rc;

use fortuples::fortuples;

use crate::script::{
    execution::{run_callable_block, types::NoneType, ExecutionContext, Failure},
    logging::RuntimeLog,
    parsing::{Expression, Function, VariableType},
    Span,
};

use super::{NamedObject, Object, OperatorResult, Value};

#[derive(Clone)]
pub struct UserFunction<S: Span> {
    pub source: Rc<Function<S>>,
}

impl<S: Span> std::fmt::Debug for UserFunction<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("UserFunction")
            .field("address", &Rc::as_ptr(&self.source))
            .finish()
    }
}

impl<S: Span> From<&'_ Rc<Function<S>>> for UserFunction<S> {
    fn from(value: &'_ Rc<Function<S>>) -> Self {
        Self {
            source: Rc::clone(value),
        }
    }
}

impl<S: Span> UserFunction<S> {
    pub fn new(source: Function<S>) -> Self {
        Self {
            source: Rc::new(source),
        }
    }
}

impl<S: Span> Object<S> for UserFunction<S> {
    fn matches_type(
        &self,
        _ty: &VariableType<S>,
        _log: &mut dyn RuntimeLog<S>,
        _variable_name_span: &S,
    ) -> OperatorResult<S, bool> {
        Ok(false)
    }

    fn call(
        &self,
        context: &mut ExecutionContext<S>,
        span: &S,
        arguments: Vec<Value<S>>,
        spans: &[Expression<S>],
    ) -> OperatorResult<S, Value<S>> {
        context.new_isolated_scope(|context| {
            run_callable_block(
                context,
                &self.source.named_block.callable,
                arguments,
                spans,
                span,
            )
        })
    }
}

impl<S: Span> PartialEq for UserFunction<S> {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl<S: Span> NamedObject for UserFunction<S> {
    fn static_type_name() -> &'static str {
        "Function"
    }
}

pub type BuiltinFunction<S> = dyn Fn(
    &mut ExecutionContext<S>,
    &S,
    Vec<Value<S>>,
    &[Expression<S>],
) -> OperatorResult<S, Value<S>>;

pub struct BuiltinFunctionRef<S: Span>(Rc<BuiltinFunction<S>>);

impl<S: Span> std::ops::Deref for BuiltinFunctionRef<S> {
    type Target = BuiltinFunction<S>;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl<S: Span> std::cmp::Eq for BuiltinFunctionRef<S> {}
impl<S: Span> std::cmp::PartialEq for BuiltinFunctionRef<S> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::addr_eq(Rc::as_ptr(&self.0), Rc::as_ptr(&other.0))
    }
}

impl<S: Span> Clone for BuiltinFunctionRef<S> {
    fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }
}

impl<S: Span> std::fmt::Debug for BuiltinFunctionRef<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut debug_struct = f.debug_struct("BuiltinFunction");
        debug_struct.field("address", &Rc::as_ptr(&self.0));
        debug_struct.finish()
    }
}

impl<S: Span> From<Box<BuiltinFunction<S>>> for BuiltinFunctionRef<S> {
    fn from(value: Box<BuiltinFunction<S>>) -> Self {
        Self(value.into())
    }
}

impl<S: Span> Object<S> for BuiltinFunctionRef<S> {
    fn matches_type(
        &self,
        _ty: &VariableType<S>,
        _log: &mut dyn RuntimeLog<S>,
        _variable_name_span: &S,
    ) -> OperatorResult<S, bool> {
        Ok(false)
    }

    fn call(
        &self,
        context: &mut ExecutionContext<S>,
        span: &S,
        arguments: Vec<Value<S>>,
        expressions: &[Expression<S>],
    ) -> OperatorResult<S, Value<S>> {
        (*self)(context, span, arguments, expressions)
    }
}

impl<S: Span> NamedObject for BuiltinFunctionRef<S> {
    fn static_type_name() -> &'static str {
        "BuiltinFunction"
    }
}

pub trait UnpackArguments<S: Span, Tuple> {
    fn unpack_arguments(
        span: &S,
        arguments: Vec<Value<S>>,
        expressions: &[Expression<S>],
    ) -> OperatorResult<S, Tuple>;

    fn unpack_arguments_optional(
        _span: &S,
        arguments: Vec<Value<S>>,
        expressions: &[Expression<S>],
    ) -> OperatorResult<S, Tuple>;
}

#[rustfmt::skip]
fortuples! {
    impl<S> UnpackArguments<S, #Tuple> for #Tuple
    where
	S: Span,
        #(#Member: NamedObject),*
	#(Value<S>: TryInto<#Member>),*
    {
	fn unpack_arguments(
            _span: &S,
            mut arguments: Vec<Value<S>>,
            expressions: &[Expression<S>],
	) -> OperatorResult<S, #Tuple> {
	    arguments.reverse();
	    let mut expression_iter = expressions.iter();
	    
	    #(let casey::lower!(#Member) = {
		if let Some(value) = arguments.pop() {
		    value.downcast(expression_iter.next().unwrap().get_span()).map_err(|f| f.make_from_function_call())?
		} else {
		    return Err(Failure::MissingArguments(_span.clone()).make_from_function_call());
		}
	    };)*

	    if let Some(extra_expression) = expression_iter.next() {
		Err(Failure::TooManyArguments(extra_expression.get_span().clone()).make_from_function_call())
	    } else {
		Ok((#(casey::lower!(#Member)),*))
	    }
	}
	
	fn unpack_arguments_optional(
            _span: &S,
            mut arguments: Vec<Value<S>>,
            expressions: &[Expression<S>],
	) -> OperatorResult<S, #Tuple> {
	    arguments.reverse();
	    let mut expression_iter = expressions.iter();
	    
	    #(let casey::lower!(#Member) = {
		if let Some(value) = arguments.pop() {
		    value.downcast(expression_iter.next().unwrap().get_span()).map_err(|f| f.make_from_function_call())?
		} else {
		    Value::<S>::from(NoneType).downcast(_span)?
		}
	    };)*

	    if let Some(extra_expression) = expression_iter.next() {
		Err(Failure::TooManyArguments(extra_expression.get_span().clone()).make_from_function_call())
	    } else {
		Ok((#(casey::lower!(#Member)),*))
	    }
	}
    }
}

pub trait AutoCall<S, T>
where
    S: Span,
{
    type Unpacker: UnpackArguments<S, T>;

    fn auto_call(
        self,
        context: &mut ExecutionContext<S>,
        span: &S,
        arguments: Vec<Value<S>>,
        expressions: &[Expression<S>],
    ) -> OperatorResult<S, Value<S>>;

    fn auto_call_optional(
        self,
        context: &mut ExecutionContext<S>,
        span: &S,
        arguments: Vec<Value<S>>,
        expressions: &[Expression<S>],
    ) -> OperatorResult<S, Value<S>>;
}

#[rustfmt::skip]
fortuples! {
    impl<S, F> AutoCall<S, #Tuple> for F
    where
	S: Span,
	#(#Member: NamedObject),*
        #(Value<S>: TryInto<#Member>),*
	F: FnOnce(&mut ExecutionContext<S>, &S, #(#Member),*) -> OperatorResult<S, Value<S>>,
    {
	type Unpacker = #Tuple;

	fn auto_call(
            self,
            context: &mut ExecutionContext<S>,
            span: &S,
            arguments: Vec<Value<S>>,
            expressions: &[Expression<S>],
	) -> OperatorResult<S, Value<S>> {
            let (#(casey::lower!(#Member)),*) = Self::Unpacker::unpack_arguments(span, arguments, expressions)?;

            (self)(context, span, #(casey::lower!(#Member)),*)
	}
	
	fn auto_call_optional(
            self,
            context: &mut ExecutionContext<S>,
            span: &S,
            arguments: Vec<Value<S>>,
            expressions: &[Expression<S>],
	) -> OperatorResult<S, Value<S>> {
            let (#(casey::lower!(#Member)),*) = Self::Unpacker::unpack_arguments_optional(span, arguments, expressions)?;

            (self)(context, span, #(casey::lower!(#Member)),*)
	}
    }
}

pub trait IntoBuiltinFunction<S: Span, T>: AutoCall<S, T> {
    fn into_builtin_function(self) -> BuiltinFunctionRef<S>;
    fn into_builtin_function_optional(self) -> BuiltinFunctionRef<S>;
}

#[rustfmt::skip]
 fortuples! {
     impl<S, F> IntoBuiltinFunction<S, #Tuple> for F
     where
 	S: Span,
 	F: Fn(&mut ExecutionContext<S>, &S, #(#Member),*) -> OperatorResult<S, Value<S>> + Clone + 'static,
 	#(#Member: NamedObject),*
         #(Value<S>: TryInto<#Member>),*
     {
 	 fn into_builtin_function(self) -> BuiltinFunctionRef<S> {
	     let function: Box<BuiltinFunction<S>> = Box::new(move |context: &mut ExecutionContext<S>, span: &S, arguments: Vec<Value<S>>, expressions: &[Expression<S>]| -> OperatorResult<S, Value<S>> {
 		 self.clone().auto_call(context, span, arguments, expressions)
 	     });
	     
	     BuiltinFunctionRef::from(function)
 	 }
	 
 	 fn into_builtin_function_optional(self) -> BuiltinFunctionRef<S> {
	     let function: Box<BuiltinFunction<S>> = Box::new(move |context: &mut ExecutionContext<S>, span: &S, arguments: Vec<Value<S>>, expressions: &[Expression<S>]| -> OperatorResult<S, Value<S>> {
 		 self.clone().auto_call_optional(context, span, arguments, expressions)
 	     });
	     
	     BuiltinFunctionRef::from(function)
 	 }
     }
 }
