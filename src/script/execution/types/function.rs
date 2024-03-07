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

use std::{fmt::Debug, rc::Rc};

use fortuples::fortuples;

use crate::script::{
    execution::{run_callable_block, ExecutionContext, Failure},
    parsing::{Expression, FunctionSignature, NamedBlock, VariableType},
    Span,
};

use super::{NamedObject, Object, OperatorResult, Value};

#[derive(Clone)]
pub struct UserFunction<'a, S: Span> {
    pub block: &'a NamedBlock<S>,
    pub signature: Rc<FunctionSignature<S>>,
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
    ) -> OperatorResult<S, Value<'a, S>> {
        context.new_isolated_scope(|context| {
            run_callable_block(context, &self.block.callable, arguments, spans, span)
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
    &mut ExecutionContext<'a, S>,
    &S,
    Vec<Value<'a, S>>,
    &[Expression<S>],
) -> OperatorResult<S, Value<'a, S>>
{
}

impl<'a, S, F> BuiltinFunctionPointer<'a, S> for F
where
    S: Span + 'a,
    F: Fn(
        &mut ExecutionContext<'a, S>,
        &S,
        Vec<Value<'a, S>>,
        &[Expression<S>],
    ) -> OperatorResult<S, Value<'a, S>>,
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
    ) -> OperatorResult<S, Value<'a, S>> {
        self.0(context, span, arguments, expressions)
    }
}

impl<S: Span> Debug for BuiltinFunction<'_, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Function").finish()
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
        span: &S,
        arguments: Vec<Value<'a, S>>,
        expressions: &[Expression<S>],
    ) -> OperatorResult<S, Tuple>;
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
            _span: &S,
            mut arguments: Vec<Value<'a, S>>,
            expressions: &[Expression<S>],
	) -> OperatorResult<S, #Tuple> {
	    arguments.reverse();
	    let mut expression_iter = expressions.iter();
	    
	    #(let casey::lower!(#Member) = {
		if let Some(value) = arguments.pop() {
		    value.downcast(expression_iter.next().unwrap().get_span())?
		} else {
		    return Err(Failure::MissingArguments(_span.clone()));
		}
	    };)*

	    if let Some(extra_expression) = expression_iter.next() {
		Err(Failure::TooManyArguments(extra_expression.get_span().clone()))
	    } else {
		Ok((#(casey::lower!(#Member)),*))
	    }
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
        context: &mut ExecutionContext<'a, S>,
        span: &S,
        arguments: Vec<Value<'a, S>>,
        expressions: &[Expression<S>],
    ) -> OperatorResult<S, Value<'a, S>>;
}

#[rustfmt::skip]
fortuples! {
    impl<'a, S, F> AutoCall<'a, S, #Tuple> for F
    where
	S: Span + 'a,
	#(#Member: NamedObject),*
        #(Value<'a, S>: TryInto<#Member>),*
	F: Fn(&mut ExecutionContext<'a, S>, &S, #(#Member),*) -> OperatorResult<S, Value<'a, S>>,
    {
	type Unpacker = #Tuple;

	fn auto_call(
            &self,
            context: &mut ExecutionContext<'a, S>,
            span: &S,
            arguments: Vec<Value<'a, S>>,
            expressions: &[Expression<S>],
	) -> OperatorResult<S, Value<'a, S>> {
            let (#(casey::lower!(#Member)),*) = Self::Unpacker::unpack_arguments(span, arguments, expressions)?;

            (self)(context, span, #(casey::lower!(#Member)),*)
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
 	F: Fn(&mut ExecutionContext<'a, S>, &S, #(#Member),*) -> OperatorResult<S, Value<'a, S>> + 'static,
 	#(#Member: NamedObject),*
         #(Value<'a, S>: TryInto<#Member>),*
     {
 	fn into_builtin_function(self) -> BuiltinFunction<'a, S> {
 	    BuiltinFunction(Rc::new(move |context: &mut ExecutionContext<'a, S>, span: &S, arguments: Vec<Value<'a, S>>, expressions: &[Expression<S>]| -> OperatorResult<S, Value<'a, S>> {
 		self.auto_call(context, span, arguments, expressions)
 	    }))
 	}
     }
 }
