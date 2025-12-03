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

        let argument = self.data.signature.argument_type.fill_defaults(argument);

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

pub trait Callable {
    fn call(
        &self,
        runtime: &mut dyn RuntimeLog,
        stack_trace: &mut Vec<SourceReference>,
        stack: &mut Stack,
        argument: Dictionary,
    ) -> ExpressionResult<Value>;

    fn name(&self) -> &str;

    fn signature(&self) -> &Arc<Signature>;
}

macro_rules! build_member_from_sig {
    ($name:ident: $ty:ident) => {
        (
            String::from(stringify!($name)),
            crate::execution::values::value_type::StructMember {
                ty: ValueType::$ty,
                default: None,
            },
        )
    };
    ($name:ident: $ty:ident = $default:expr) => {
        (
            String::from(stringify!($name)),
            crate::execution::values::value_type::StructMember {
                ty: ValueType::$ty,
                default: Some($default),
            },
        )
    };
}

macro_rules! build_argument_signature_list {
    ($($arg:ident: $ty:ident $(= $default:expr)?),*) => {{
        let list: [(String, crate::execution::values::value_type::StructMember); _] = [$(build_member_from_sig!($arg: $ty $(= $default)?),)*];
        list
    }};
}

macro_rules! build_function {
    ($name:ident ($log:ident: &mut dyn RuntimeLog, $stack_trace:ident: &mut Vec<SourceReference>, $stack:ident: &mut Stack $(, $($arg:ident: $ty:ident $(= $default:expr)?),+)?) $code:block) => {
        build_function!($name ($log: &mut dyn RuntimeLog, $stack_trace: &mut Vec<SourceReference>, $stack: &mut Stack $(, $($arg: $ty $(= $default)?),*)?) -> ValueType::TypeNone $code)
    };
    ($name:ident ($log:ident: &mut dyn RuntimeLog, $stack_trace:ident: &mut Vec<SourceReference>, $stack:ident: &mut Stack $(, $($arg:ident: $ty:ident $(= $default:expr)?),+)?) -> $return_type:path $code:block) => {{
        struct BuiltFunction<F: Fn(&mut dyn RuntimeLog, &mut Vec<SourceReference>, &mut Stack $(, $($ty),*)?) -> ExpressionResult<Value>> {
            function: F,
            signature: Arc<crate::execution::values::closure::Signature>,
        }

        impl<F: Fn(&mut dyn RuntimeLog, &mut Vec<SourceReference>, &mut Stack $(, $($ty),*)?) -> ExpressionResult<Value>> Callable for BuiltFunction<F> {
            fn call(
                &self,
                runtime: &mut dyn RuntimeLog,
                stack_trace: &mut Vec<SourceReference>,
                stack: &mut Stack,
                argument: Dictionary,
            ) -> ExpressionResult<Value> {
                self.signature
                    .argument_type
                    .check_other_qualifies(argument.struct_def())
                    .map_err(|error| error.to_error(stack_trace.iter()))?;

                // Argument is potentially unused if we take no arguments.
                let mut _argument = self.signature.argument_type.fill_defaults(argument);

                let _data = Arc::make_mut(&mut _argument.data);
                $($(let $arg: $ty = _data.members.remove(stringify!($arg))
                        .expect("Argument was not present after argument check.").downcast(stack_trace)?;)*)?

                    (self.function)(runtime, stack_trace, stack $(, $($arg),*)?)
            }

            fn name(&self) -> &str {
                stringify!($name)
            }

            fn signature(&self) -> &Arc<Signature> {
                &self.signature
            }
        }

        let members = std::sync::Arc::new(hashable_map::HashableMap::from(std::collections::HashMap::from(build_argument_signature_list!($($($arg: $ty $(= $default)?),*)?))));

        crate::execution::values::closure::BuiltinFunction {
            callable: Arc::new(BuiltFunction {
            function: move |$log: &mut dyn RuntimeLog, $stack_trace: &mut Vec<SourceReference>, $stack: &mut Stack $(, $($arg: $ty),*)?| -> ExpressionResult<Value> { $code },
            signature: std::sync::Arc::new(Signature {
                argument_type: StructDefinition {
                    members,
                    variadic: false,
                },
                return_type: $return_type
            }),
        })
        }
    }};
}

#[derive(Clone)]
pub struct BuiltinFunction {
    callable: Arc<dyn Callable>,
}

impl std::fmt::Debug for BuiltinFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BuiltinFunction")
            .field("name", &self.callable.name())
            .field("signature", self.callable.signature())
            .finish()
    }
}

impl BuiltinFunction {
    pub fn new(function: Arc<dyn Callable>) -> BuiltinFunction {
        BuiltinFunction { callable: function }
    }
}

impl Eq for BuiltinFunction {}

impl PartialEq for BuiltinFunction {
    fn eq(&self, other: &Self) -> bool {
        self.callable.signature() == other.callable.signature()
            && self.callable.name() == other.callable.name()
    }
}

impl Object for BuiltinFunction {
    fn get_type(&self) -> ValueType {
        ValueType::Closure(self.callable.signature().clone())
    }

    fn call(
        &self,
        log: &mut dyn RuntimeLog,
        stack_trace: &mut Vec<SourceReference>,
        stack: &mut Stack,
        argument: Dictionary,
    ) -> ExpressionResult<Value> {
        self.callable.call(log, stack_trace, stack, argument)
    }
}

impl StaticTypeName for BuiltinFunction {
    fn static_type_name() -> &'static str {
        "Builtin Function"
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::execution::{
        test_run,
        values::{self, StructMember, UnsignedInteger, ValueNone},
    };
    use hashable_map::HashableMap;
    use pretty_assertions::assert_eq;

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
    fn call_closure_default_value() {
        let product = test_run(
            "let my_function = (a: std.types.UInt = 3u) -> std.types.UInt: a + 2u; in my_function()",
        )
        .unwrap();
        assert_eq!(product, values::UnsignedInteger::from(5).into());
    }

    #[test]
    fn build_argument_signature() {
        assert_eq!(build_argument_signature_list!(), []);
        assert_eq!(
            build_argument_signature_list!(value: SignedInteger),
            [(
                String::from("value"),
                StructMember {
                    ty: ValueType::SignedInteger,
                    default: None
                }
            )]
        );
        assert_eq!(
            build_argument_signature_list!(value: UnsignedInteger),
            [(
                String::from("value"),
                StructMember {
                    ty: ValueType::UnsignedInteger,
                    default: None
                }
            )]
        );
        assert_eq!(
            build_argument_signature_list!(value: UnsignedInteger = UnsignedInteger::from(23).into()),
            [(
                String::from("value"),
                StructMember {
                    ty: ValueType::UnsignedInteger,
                    default: Some(Value::UnsignedInteger(23.into()))
                }
            )]
        );

        assert_eq!(
            build_argument_signature_list!(value: UnsignedInteger, value1: SignedInteger),
            [
                (
                    String::from("value"),
                    StructMember {
                        ty: ValueType::UnsignedInteger,
                        default: None
                    }
                ),
                (
                    String::from("value1"),
                    StructMember {
                        ty: ValueType::SignedInteger,
                        default: None
                    }
                )
            ]
        );

        assert_eq!(
            build_argument_signature_list!(value: UnsignedInteger = UnsignedInteger::from(32).into(), value1: SignedInteger),
            [
                (
                    String::from("value"),
                    StructMember {
                        ty: ValueType::UnsignedInteger,
                        default: Some(UnsignedInteger::from(32).into())
                    }
                ),
                (
                    String::from("value1"),
                    StructMember {
                        ty: ValueType::SignedInteger,
                        default: None
                    }
                )
            ]
        );
    }

    #[test]
    fn builtin_function_no_args_no_result() {
        let test_function = build_function!(
            test_function(_log: &mut dyn RuntimeLog, _stack_trace: &mut Vec<SourceReference>, _stack: &mut Stack) {
                Ok(ValueNone.into())
            }
        );

        use crate::execution::standard_environment::build_prelude;

        let root = crate::compile::full_compile("test_function()");
        let prelude = build_prelude();
        let mut stack = Stack::new(prelude);
        stack.insert_value("test_function", test_function.into());

        let product =
            execute_expression(&mut Vec::new(), &mut Vec::new(), &mut stack, &root).unwrap();

        assert_eq!(product, values::ValueNone.into());
    }

    #[test]
    fn builtin_function_no_args() {
        let test_function = build_function!(
            test_function(_log: &mut dyn RuntimeLog, _stack_trace: &mut Vec<SourceReference>, _stack: &mut Stack) -> ValueType::UnsignedInteger {
                Ok(values::UnsignedInteger::from(846).into())
            }
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

    #[test]
    fn builtin_function_with_args() {
        let test_function = build_function!(
            test_function(_log: &mut dyn RuntimeLog, _stack_trace: &mut Vec<SourceReference>, _stack: &mut Stack, a: UnsignedInteger, b: UnsignedInteger) -> ValueType::UnsignedInteger {
                Ok(values::UnsignedInteger::from(a.0 + b.0).into())
            }
        );

        use crate::execution::standard_environment::build_prelude;

        let root = crate::compile::full_compile("test_function(a = 1u, b = 2u)");
        let prelude = build_prelude();
        let mut stack = Stack::new(prelude);
        stack.insert_value("test_function", test_function.into());

        let product =
            execute_expression(&mut Vec::new(), &mut Vec::new(), &mut stack, &root).unwrap();

        assert_eq!(product, values::UnsignedInteger::from(3).into());
    }

    #[test]
    fn builtin_function_with_default_value() {
        let test_function = build_function!(
            test_function(_log: &mut dyn RuntimeLog, _stack_trace: &mut Vec<SourceReference>, _stack: &mut Stack, a: UnsignedInteger, b: UnsignedInteger = UnsignedInteger::from(2).into()) -> ValueType::UnsignedInteger {
                Ok(values::UnsignedInteger::from(a.0 + b.0).into())
            }
        );

        use crate::execution::standard_environment::build_prelude;

        let root = crate::compile::full_compile("test_function(a = 1u)");
        let prelude = build_prelude();
        let mut stack = Stack::new(prelude);
        stack.insert_value("test_function", test_function.into());

        let product =
            execute_expression(&mut Vec::new(), &mut Vec::new(), &mut stack, &root).unwrap();

        assert_eq!(product, values::UnsignedInteger::from(3).into());
    }

    #[test]
    fn builtin_function_captured_value() {
        let b = 2;

        let test_function = build_function!(
            test_function(_log: &mut dyn RuntimeLog, _stack_trace: &mut Vec<SourceReference>, _stack: &mut Stack, a: UnsignedInteger) -> ValueType::UnsignedInteger {
                Ok(values::UnsignedInteger::from(a.0 + b).into())
            }
        );

        use crate::execution::standard_environment::build_prelude;

        let root = crate::compile::full_compile("test_function(a = 1u)");
        let prelude = build_prelude();
        let mut stack = Stack::new(prelude);
        stack.insert_value("test_function", test_function.into());

        let product =
            execute_expression(&mut Vec::new(), &mut Vec::new(), &mut stack, &root).unwrap();

        assert_eq!(product, values::UnsignedInteger::from(3).into());
    }
}
