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

use std::{fmt::Display, sync::Arc};

use crate::{
    compile::{AstNode, ClosureDefinition, Expression, SourceReference},
    execute_expression,
    execution::{
        errors::{ExpressionResult, Raise},
        logging::{RuntimeLog, StackScope},
        stack::{ScopeType, Stack},
        values::{Dictionary, DowncastError, Value},
    },
};

use super::{Object, StaticTypeName, StructDefinition, ValueType};

/// Signature of a closure, used for type comparison.
#[derive(Debug, Eq, PartialEq)]
pub struct Signature {
    pub argument_type: StructDefinition,
    pub return_type: ValueType,
}

impl Display for Signature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> {}", self.argument_type, self.return_type)
    }
}

/// Closures are immutable, meaning that all copies can reference the same data.
/// This is that common data.
#[derive(Debug, Eq, PartialEq)]
struct UserClosureInternals {
    signature: Arc<Signature>,
    expression: Arc<AstNode<Expression>>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct UserClosure {
    data: Arc<UserClosureInternals>,
}

impl UserClosure {
    pub fn from_ast(
        log: &mut dyn RuntimeLog,
        stack_trace: &mut Vec<SourceReference>,
        stack: &mut Stack,
        source: &AstNode<Box<ClosureDefinition>>,
    ) -> ExpressionResult<Self> {
        let argument_type = stack_trace.stack_scope(
            source.node.argument_type.reference.clone(),
            |stack_trace| {
                let argument_type =
                    execute_expression(log, stack_trace, stack, &source.node.argument_type)?
                        .downcast::<ValueType>(stack_trace)?;

                if let ValueType::Dictionary(argument_type) = argument_type {
                    Ok(argument_type)
                } else {
                    Err(DowncastError {
                        expected: StructDefinition::static_type_name().into(),
                        got: argument_type.name(),
                    }
                    .to_error(stack_trace.as_slice()))
                }
            },
        )?;

        let return_type =
            stack_trace.stack_scope(source.node.return_type.reference.clone(), |stack_trace| {
                execute_expression(log, stack_trace, stack, &source.node.return_type)?
                    .downcast::<ValueType>(stack_trace)
            })?;

        let signature = Arc::new(Signature {
            argument_type,
            return_type,
        });

        let expression = source.node.expression.clone();

        Ok(Self {
            data: Arc::new(UserClosureInternals {
                signature,
                expression,
            }),
        })
    }
}

impl Object for UserClosure {
    fn get_type(&self) -> ValueType {
        ValueType::Closure(self.data.signature.clone())
    }

    fn call(
        &self,
        log: &mut dyn RuntimeLog,
        stack_trace: &mut Vec<SourceReference>,
        stack: &mut Stack,
        argument: Dictionary,
    ) -> ExpressionResult<Value> {
        self.data
            .signature
            .argument_type
            .check_other_qualifies(argument.struct_def())
            .map_err(|error| error.to_error(stack_trace.iter()))?;

        stack.scope(stack_trace, ScopeType::Isolated, |stack, stack_trace| {
            for (name, value) in argument.iter() {
                stack.insert_value(name, value.clone());
            }

            let result = execute_expression(log, stack_trace, stack, &self.data.expression)?;

            self.data
                .signature
                .return_type
                .check_other_qualifies(&result.get_type())
                .map_err(|error| error.to_error(stack_trace.iter()))?;

            Ok(result)
        })?
    }
}

impl StaticTypeName for UserClosure {
    fn static_type_name() -> &'static str {
        "Closure"
    }
}

pub type BuiltinFunctionPointer = fn(
    &mut dyn RuntimeLog,
    &mut Vec<SourceReference>,
    &mut Stack,
    Dictionary,
) -> ExpressionResult<Value>;

#[derive(Debug, Eq, Clone)]
pub struct BuiltinFunction {
    name: &'static str,
    function: BuiltinFunctionPointer,
    signature: Arc<Signature>,
}

impl PartialEq for BuiltinFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.signature == other.signature
    }
}

impl BuiltinFunction {
    pub fn new(
        name: &'static str,
        signature: Arc<Signature>,
        function: BuiltinFunctionPointer,
    ) -> BuiltinFunction {
        BuiltinFunction {
            name,
            function,
            signature,
        }
    }
}

impl Object for BuiltinFunction {
    fn get_type(&self) -> ValueType {
        ValueType::Closure(self.signature.clone())
    }

    fn call(
        &self,
        log: &mut dyn RuntimeLog,
        stack_trace: &mut Vec<SourceReference>,
        stack: &mut Stack,
        argument: Dictionary,
    ) -> ExpressionResult<Value> {
        (self.function)(log, stack_trace, stack, argument)
    }
}

impl StaticTypeName for BuiltinFunction {
    fn static_type_name() -> &'static str {
        "Builtin Function"
    }
}
// pub trait UnpackArguments<S: Span, Tuple> {
//     fn unpack_arguments(
//         span: &S,
//         arguments: Vec<Value<S>>,
//         expressions: &[Expression<S>],
//     ) -> OperatorResult<S, Tuple>;
//
//     fn unpack_arguments_optional(
//         _span: &S,
//         arguments: Vec<Value<S>>,
//         expressions: &[Expression<S>],
//     ) -> OperatorResult<S, Tuple>;
// }
//
// #[rustfmt::skip]
// fortuples! {
//     impl<S> UnpackArguments<S, #Tuple> for #Tuple
//     where
// 	S: Span,
//         #(#Member: NamedObject),*
// 	#(Value<S>: TryInto<#Member>),*
//     {
// 	fn unpack_arguments(
//             _span: &S,
//             mut arguments: Vec<Value<S>>,
//             expressions: &[Expression<S>],
// 	) -> OperatorResult<S, #Tuple> {
// 	    arguments.reverse();
// 	    let mut expression_iter = expressions.iter();
//
// 	    #(let casey::lower!(#Member) = {
// 		if let Some(value) = arguments.pop() {
// 		    value.downcast(expression_iter.next().unwrap().get_span()).map_err(|f| f.make_from_function_call())?
// 		} else {
// 		    return Err(Failure::MissingArguments(_span.clone()).make_from_function_call());
// 		}
// 	    };)*
//
// 	    if let Some(extra_expression) = expression_iter.next() {
// 		Err(Failure::TooManyArguments(extra_expression.get_span().clone()).make_from_function_call())
// 	    } else {
// 		Ok((#(casey::lower!(#Member)),*))
// 	    }
// 	}
//
// 	fn unpack_arguments_optional(
//             _span: &S,
//             mut arguments: Vec<Value<S>>,
//             expressions: &[Expression<S>],
// 	) -> OperatorResult<S, #Tuple> {
// 	    arguments.reverse();
// 	    let mut expression_iter = expressions.iter();
//
// 	    #(let casey::lower!(#Member) = {
// 		if let Some(value) = arguments.pop() {
// 		    value.downcast(expression_iter.next().unwrap().get_span()).map_err(|f| f.make_from_function_call())?
// 		} else {
// 		    Value::<S>::from(NoneType).downcast(_span)?
// 		}
// 	    };)*
//
// 	    if let Some(extra_expression) = expression_iter.next() {
// 		Err(Failure::TooManyArguments(extra_expression.get_span().clone()).make_from_function_call())
// 	    } else {
// 		Ok((#(casey::lower!(#Member)),*))
// 	    }
// 	}
//     }
// }
//
// pub trait AutoCall<S, T>
// where
//     S: Span,
// {
//     type Unpacker: UnpackArguments<S, T>;
//
//     fn auto_call(
//         self,
//         context: &mut ExecutionContext<S>,
//         span: &S,
//         arguments: Vec<Value<S>>,
//         expressions: &[Expression<S>],
//     ) -> OperatorResult<S, Value<S>>;
//
//     fn auto_call_optional(
//         self,
//         context: &mut ExecutionContext<S>,
//         span: &S,
//         arguments: Vec<Value<S>>,
//         expressions: &[Expression<S>],
//     ) -> OperatorResult<S, Value<S>>;
// }
//
// #[rustfmt::skip]
// fortuples! {
//     impl<S, F> AutoCall<S, #Tuple> for F
//     where
// 	S: Span,
// 	#(#Member: NamedObject),*
//         #(Value<S>: TryInto<#Member>),*
// 	F: FnOnce(&mut ExecutionContext<S>, &S, #(#Member),*) -> OperatorResult<S, Value<S>>,
//     {
// 	type Unpacker = #Tuple;
//
// 	fn auto_call(
//             self,
//             context: &mut ExecutionContext<S>,
//             span: &S,
//             arguments: Vec<Value<S>>,
//             expressions: &[Expression<S>],
// 	) -> OperatorResult<S, Value<S>> {
//             let (#(casey::lower!(#Member)),*) = Self::Unpacker::unpack_arguments(span, arguments, expressions)?;
//
//             (self)(context, span, #(casey::lower!(#Member)),*)
// 	}
//
// 	fn auto_call_optional(
//             self,
//             context: &mut ExecutionContext<S>,
//             span: &S,
//             arguments: Vec<Value<S>>,
//             expressions: &[Expression<S>],
// 	) -> OperatorResult<S, Value<S>> {
//             let (#(casey::lower!(#Member)),*) = Self::Unpacker::unpack_arguments_optional(span, arguments, expressions)?;
//
//             (self)(context, span, #(casey::lower!(#Member)),*)
// 	}
//     }
// }
//
// pub trait IntoBuiltinFunction<S: Span, T>: AutoCall<S, T> {
//     fn into_builtin_function(self) -> BuiltinFunctionRef<S>;
//     fn into_builtin_function_optional(self) -> BuiltinFunctionRef<S>;
// }
//
// #[rustfmt::skip]
//  fortuples! {
//      impl<S, F> IntoBuiltinFunction<S, #Tuple> for F
//      where
//  	S: Span,
//  	F: Fn(&mut ExecutionContext<S>, &S, #(#Member),*) -> OperatorResult<S, Value<S>> + Clone + 'static,
//  	#(#Member: NamedObject),*
//          #(Value<S>: TryInto<#Member>),*
//      {
//  	 fn into_builtin_function(self) -> BuiltinFunctionRef<S> {
// 	     let function: Box<BuiltinFunction<S>> = Box::new(move |context: &mut ExecutionContext<S>, span: &S, arguments: Vec<Value<S>>, expressions: &[Expression<S>]| -> OperatorResult<S, Value<S>> {
//  		 self.clone().auto_call(context, span, arguments, expressions)
//  	     });
//
// 	     BuiltinFunctionRef::from(function)
//  	 }
//
//  	 fn into_builtin_function_optional(self) -> BuiltinFunctionRef<S> {
// 	     let function: Box<BuiltinFunction<S>> = Box::new(move |context: &mut ExecutionContext<S>, span: &S, arguments: Vec<Value<S>>, expressions: &[Expression<S>]| -> OperatorResult<S, Value<S>> {
//  		 self.clone().auto_call_optional(context, span, arguments, expressions)
//  	     });
//
// 	     BuiltinFunctionRef::from(function)
//  	 }
//      }
//  }

#[cfg(test)]
mod test {
    use super::*;
    use crate::execution::{test_run, values, values::UnsignedInteger};
    use hashable_map::HashableMap;
    use pretty_assertions::assert_eq;
    use std::collections::HashMap;

    #[test]
    fn define_closure() {
        let product = test_run("() -> std.types.None std.consts.None").unwrap();

        let expression = product.as_userclosure().unwrap().data.expression.clone();

        assert_eq!(
            product,
            UserClosure {
                data: Arc::new(UserClosureInternals {
                    signature: Arc::new(Signature {
                        argument_type: StructDefinition {
                            members: HashableMap::new().into(),
                            variadic: false,
                        },
                        return_type: ValueType::TypeNone,
                    }),
                    expression
                })
            }
            .into()
        );
    }

    #[test]
    fn call_closure() {
        let product = test_run(
            "let my_function = (a: std.types.UInt) -> std.types.UInt: a + 2u; in my_function(a = 3u)",
        )
        .unwrap();
        assert_eq!(product, values::UnsignedInteger::from(5).into());
    }

    #[test]
    fn call_closure_bad_args() {
        test_run(
            "let my_function = (a: std.types.UInt) -> std.types.UInt: a + 2u; in my_function(a = 3i)",
        )
        .unwrap_err();
    }

    #[test]
    fn call_closure_bad_result() {
        test_run(
            "let my_function = (a: std.types.UInt) -> std.types.UInt: \"test\"; in my_function(a = 3u)",
        )
        .unwrap_err();
    }

    #[test]
    fn builtin_function() {
        let test_function = BuiltinFunction::new(
            "test_function",
            Arc::new(Signature {
                argument_type: StructDefinition::from(HashMap::new()),
                return_type: ValueType::UnsignedInteger,
            }),
            |_log: &mut dyn RuntimeLog,
             _stack_trace: &mut Vec<SourceReference>,
             _stack: &mut Stack,
             _argument: Dictionary| Ok(UnsignedInteger::from(846).into()),
        );

        use crate::execution::standard_environment::build_prelude;

        let root = crate::compile::full_compile("test_function()");
        let prelude = build_prelude();
        let mut stack = Stack::new(prelude);
        stack.insert_value("test_function", test_function.into());

        let product =
            execute_expression(&mut Vec::new(), &mut Vec::new(), &mut stack, &root).unwrap();

        assert_eq!(product, values::UnsignedInteger::from(846).into());
    }
}
