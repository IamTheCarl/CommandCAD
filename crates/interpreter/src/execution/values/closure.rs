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

use hashable_map::HashableMap;

use crate::{
    compile::{AstNode, ClosureDefinition, Expression, SourceReference},
    execute_expression,
    execution::{
        errors::{ExpressionResult, Raise},
        find_value,
        logging::{RuntimeLog, StackScope},
        stack::{ScopeType, Stack},
        values::{Dictionary, Value},
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

fn find_all_variable_accesses_in_closure_capture(
    closure: &crate::compile::ClosureDefinition,
    mut access_collector: &mut dyn FnMut(&AstNode<String>) -> ExpressionResult<()>,
) -> ExpressionResult<()> {
    let argument_names: Vec<&String> = {
        let mut argument_names = Vec::with_capacity(closure.argument_type.node.members.len());

        for argument in closure.argument_type.node.members.iter() {
            argument_names.push(&argument.node.name.node);
        }

        // We typically won't have more than 6 arguments, so a binary search will typically
        // outperform a hashset.
        argument_names.sort();

        argument_names
    };

    find_all_variable_accesses_in_expression(
        &closure.expression.node,
        &mut move |variable_name| {
            if argument_names.binary_search(&&variable_name.node).is_err() {
                // This is not an argument, which means it must be captured from the environment.
                access_collector(variable_name)?;
            }

            Ok(())
        },
    )?;

    Ok(())
}

fn find_all_variable_accesses_in_dictionary_construction(
    dictionary_construction: &crate::compile::DictionaryConstruction,
    access_collector: &mut dyn FnMut(&AstNode<String>) -> ExpressionResult<()>,
) -> ExpressionResult<()> {
    for assignment in dictionary_construction.assignments.iter() {
        find_all_variable_accesses_in_expression(
            &assignment.node.assignment.node,
            access_collector,
        )?;
    }

    Ok(())
}

fn find_all_variable_accesses_in_expression(
    expression: &Expression,
    access_collector: &mut dyn FnMut(&AstNode<String>) -> ExpressionResult<()>,
) -> ExpressionResult<()> {
    match expression {
        Expression::BinaryExpression(ast_node) => {
            find_all_variable_accesses_in_expression(&ast_node.node.a.node, access_collector)?;
            find_all_variable_accesses_in_expression(&ast_node.node.b.node, access_collector)?;

            Ok(())
        }
        Expression::ClosureDefinition(ast_node) => {
            find_all_variable_accesses_in_expression(
                &ast_node.node.return_type.node,
                access_collector,
            )?;
            find_all_variable_accesses_in_closure_capture(&ast_node.node, access_collector)?;

            Ok(())
        }
        Expression::DictionaryConstruction(ast_node) => {
            find_all_variable_accesses_in_dictionary_construction(&ast_node.node, access_collector)
        }
        Expression::If(ast_node) => {
            find_all_variable_accesses_in_expression(
                &ast_node.node.condition.node,
                access_collector,
            )?;
            find_all_variable_accesses_in_expression(
                &ast_node.node.on_true.node,
                access_collector,
            )?;
            find_all_variable_accesses_in_expression(
                &ast_node.node.on_false.node,
                access_collector,
            )?;

            Ok(())
        }
        Expression::List(ast_node) => {
            for expression in ast_node.node.iter() {
                find_all_variable_accesses_in_expression(&expression.node, access_collector)?;
            }

            Ok(())
        }
        Expression::Parenthesis(ast_node) => {
            find_all_variable_accesses_in_expression(&ast_node.node, access_collector)
        }
        Expression::IdentityPath(ast_node) => {
            // Only the top most parent matters.
            access_collector(&ast_node.node.path[0])
        }
        Expression::StructDefinition(ast_node) => {
            for member in ast_node.node.members.iter() {
                find_all_variable_accesses_in_expression(&member.node.ty.node, access_collector)?;
                if let Some(default) = member.node.default.as_ref() {
                    find_all_variable_accesses_in_expression(&default.node, access_collector)?;
                }
            }

            Ok(())
        }
        Expression::UnaryExpression(ast_node) => find_all_variable_accesses_in_expression(
            &ast_node.node.expression.node,
            access_collector,
        ),
        Expression::FunctionCall(ast_node) => {
            find_all_variable_accesses_in_expression(
                &ast_node.node.to_call.node,
                access_collector,
            )?;
            find_all_variable_accesses_in_dictionary_construction(
                &ast_node.node.argument.node,
                access_collector,
            )?;

            Ok(())
        }
        Expression::MethodCall(ast_node) => {
            find_all_variable_accesses_in_expression(
                &ast_node.node.self_dictionary.node,
                access_collector,
            )?;
            find_all_variable_accesses_in_dictionary_construction(
                &ast_node.node.argument.node,
                access_collector,
            )?;

            Ok(())
        }
        Expression::LetIn(ast_node) => {
            for assignment in ast_node.node.assignments.iter() {
                find_all_variable_accesses_in_expression(
                    &assignment.node.value.node,
                    access_collector,
                )?;
            }

            find_all_variable_accesses_in_expression(
                &ast_node.node.expression.node,
                access_collector,
            )?;

            Ok(())
        }
        Expression::Boolean(_)
        | Expression::Scalar(_)
        | Expression::Vector2(_)
        | Expression::Vector3(_)
        | Expression::Vector4(_)
        | Expression::SignedInteger(_)
        | Expression::String(_)
        | Expression::UnsignedInteger(_)
        | Expression::SelfPath(_) => Ok(()),
    }
}

/// Closures are immutable, meaning that all copies can reference the same data.
/// This is that common data.
#[derive(Debug, Eq, PartialEq)]
struct UserClosureInternals {
    signature: Arc<Signature>,
    captured_values: HashableMap<String, Value>,
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
                    StructDefinition::new(log, stack_trace, stack, &source.node.argument_type)?;

                Ok(argument_type)
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

        let mut captured_values = HashableMap::new();
        find_all_variable_accesses_in_closure_capture(&source.node, &mut |field_name| {
            let value = find_value(log, stack_trace, stack, [field_name])?;

            captured_values.insert(field_name.node.clone(), value);

            Ok(())
        })?;

        Ok(Self {
            data: Arc::new(UserClosureInternals {
                signature,
                captured_values,
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

        stack.scope(stack_trace, ScopeType::Inherited, |stack, stack_trace| {
            for (name, value) in argument.iter() {
                stack.insert_value(name, value.clone());
            }

            for (name, value) in self.data.captured_values.iter() {
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

pub trait Callable: Sync + Send {
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

#[macro_export]
macro_rules! build_member_from_sig {
    ($name:ident: $ty:ty) => {
        (
            String::from(stringify!($name)),
            $crate::execution::values::value_type::StructMember {
                ty: <$ty as crate::execution::values::StaticType>::static_type(),
                default: None,
            },
        )
    };
    ($name:ident: $ty:ty = $default:expr) => {
        (
            String::from(stringify!($name)),
            $crate::execution::values::value_type::StructMember {
                ty: <$ty as crate::execution::values::StaticType>::static_type(),
                default: Some($default),
            },
        )
    };
}

#[macro_export]
macro_rules! build_argument_signature_list {
    ($($arg:ident: $ty:path $(= $default:expr)?),*) => {{
        let list: [(String, crate::execution::values::value_type::StructMember); _] = [$($crate::build_member_from_sig!($arg: $ty $(= $default)?),)*];
        list
    }};
}

#[macro_export]
macro_rules! build_closure_signature {
    (($($arg:ident: $ty:path $(= $default:expr)?),*) -> $return_type:path) => {{
        let members = std::sync::Arc::new(hashable_map::HashableMap::from(std::collections::HashMap::from($crate::build_argument_signature_list!($($arg: $ty $(= $default)?),*))));

        std::sync::Arc::new(crate::execution::values::closure::Signature {
            argument_type: crate::execution::values::StructDefinition {
                members,
                variadic: false,
            },
            return_type: $return_type,
        })
    }};
}

#[macro_export]
macro_rules! build_closure_type {
    ($name:ident($($arg:ident: $ty:path $(= $default:expr)?),*) -> $return_type:path) => {
        struct $name(pub $crate::execution::values::UserClosure);

        impl $crate::execution::values::StaticType for $name {
            fn static_type() -> $crate::execution::values::ValueType {
                static TYPE: std::sync::OnceLock<
                    std::sync::Arc<crate::execution::values::closure::Signature>,
                > = std::sync::OnceLock::new();
                let signature = TYPE.get_or_init(|| $crate::build_closure_signature!(($($arg: $ty $(= $default)?),*) -> $return_type));

                $crate::execution::values::ValueType::Closure(signature.clone())
            }
        }

        impl $crate::execution::values::StaticTypeName for $name {
            fn static_type_name() -> &'static str {
                "Closure"
            }
        }

        impl enum_downcast::IntoVariant<$name> for $crate::execution::values::Value {
            fn into_variant(self) -> Result<$name, $crate::execution::values::Value> {
                Ok($name(self.into_variant()?))
            }
        }

        impl Into<$crate::execution::values::UserClosure> for $name {
            fn into(self) -> $crate::execution::values::UserClosure {
                self.0
            }
        }

        impl std::ops::Deref for $name {
            type Target = $crate::execution::values::UserClosure;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }
    };
}

#[macro_export]
macro_rules! build_function {
    ($name:ident ($log:ident: &mut dyn RuntimeLog, $stack_trace:ident: &mut Vec<SourceReference>, $stack:ident: &mut Stack $(, $($arg:ident: $ty:path $(= $default:expr)?),+)?) -> $return_type:path $code:block) => {{
        struct BuiltFunction<F: Fn(&mut dyn RuntimeLog, &mut Vec<SourceReference>, &mut Stack $(, $($ty),*)?) -> ExpressionResult<crate::execution::values::Value>> {
            function: F,
            signature: std::sync::Arc<crate::execution::values::closure::Signature>,
        }

        impl<F: Fn(&mut dyn RuntimeLog, &mut Vec<SourceReference>, &mut Stack $(, $($ty),*)?) -> ExpressionResult<crate::execution::values::Value> + Send + Sync> crate::execution::values::closure::Callable for BuiltFunction<F> {
            fn call(
                &self,
                runtime: &mut dyn RuntimeLog,
                stack_trace: &mut Vec<SourceReference>,
                stack: &mut Stack,
                argument: crate::execution::values::Dictionary,
            ) -> crate::execution::errors::ExpressionResult<crate::execution::values::Value> {
                use crate::execution::errors::Raise;
                self.signature
                    .argument_type
                    .check_other_qualifies(argument.struct_def())
                    .map_err(|error| error.to_error(stack_trace.iter()))?;

                // Argument is potentially unused if we take no arguments.
                let mut _argument = self.signature.argument_type.fill_defaults(argument);

                let _data = std::sync::Arc::make_mut(&mut _argument.data);
                $($(let $arg: $ty = _data.members.remove(stringify!($arg))
                        .expect("Argument was not present after argument check.").downcast(stack_trace)?;)*)?

                    (self.function)(runtime, stack_trace, stack $(, $($arg),*)?)
            }

            fn name(&self) -> &str {
                stringify!($name)
            }

            fn signature(&self) -> &std::sync::Arc<crate::execution::values::closure::Signature> {
                &self.signature
            }
        }

        crate::execution::values::closure::BuiltinFunction {
            callable: std::sync::Arc::new(BuiltFunction {
            function: move |$log: &mut dyn RuntimeLog, $stack_trace: &mut Vec<SourceReference>, $stack: &mut Stack $(, $($arg: $ty),*)?| -> ExpressionResult<crate::execution::values::Value> { $code },
            signature: $crate::build_closure_signature!(($($($arg: $ty $(= $default)?),*)*) -> $return_type),
        })
        }
    }};
}

#[macro_export]
macro_rules! build_method {
    ($name:ident ($log:ident: &mut dyn RuntimeLog, $stack_trace:ident: &mut Vec<SourceReference>, $stack:ident: &mut Stack, $this:ident: $this_type:ty $(, $($arg:ident: $ty:path $(= $default:expr)?),+)?) -> $return_type:path $code:block) => {{
        struct BuiltFunction<S, F: Fn(&mut dyn RuntimeLog, &mut Vec<SourceReference>, &mut crate::execution::Stack, S $(, $($ty),*)?) -> ExpressionResult<crate::execution::values::Value>> {
            function: F,
            signature: std::sync::Arc<crate::execution::values::closure::Signature>,
            _self_type: std::marker::PhantomData<S>
        }

        impl<S, F: Fn(&mut dyn RuntimeLog, &mut Vec<SourceReference>, &mut $crate::execution::Stack, S $(, $($ty),*)?) -> ExpressionResult<$crate::execution::values::Value> + Send + Sync> $crate::execution::values::closure::Callable for BuiltFunction<S, F>
        where
            S: Send + Sync + Clone + StaticTypeName,
            $crate::execution::values::Value: enum_downcast::AsVariant<S>
        {
            fn call(
                &self,
                runtime: &mut dyn RuntimeLog,
                stack_trace: &mut Vec<SourceReference>,
                stack: &mut $crate::execution::Stack,
                argument: $crate::execution::values::Dictionary,
            ) -> $crate::execution::errors::ExpressionResult<$crate::execution::values::Value> {
                use $crate::execution::errors::Raise;

                let this = stack.get_variable(
                    stack_trace,
                    $crate::execution::logging::LocatedStr {
                        location: stack_trace.last().unwrap().clone(),
                        string: "self",
                    },
                )?.downcast_ref::<S>(stack_trace)?.clone();

                self.signature
                    .argument_type
                    .check_other_qualifies(argument.struct_def())
                    .map_err(|error| error.to_error(stack_trace.iter()))?;

                // Argument is potentially unused if we take no arguments.
                let mut _argument = self.signature.argument_type.fill_defaults(argument);

                let _data = std::sync::Arc::make_mut(&mut _argument.data);
                $($(let $arg: $ty = _data.members.remove(stringify!($arg))
                        .expect("Argument was not present after argument check.").downcast(stack_trace)?;)*)?

                    (self.function)(runtime, stack_trace, stack, this $(, $($arg),*)?)
            }

            fn name(&self) -> &str {
                stringify!($name)
            }

            fn signature(&self) -> &std::sync::Arc<$crate::execution::values::closure::Signature> {
                &self.signature
            }
        }

        $crate::execution::values::closure::BuiltinFunction {
            callable: std::sync::Arc::new(BuiltFunction {
                function:
                    move |$log: &mut dyn RuntimeLog, $stack_trace: &mut Vec<SourceReference>, $stack: &mut $crate::execution::Stack, $this: $this_type $(, $($arg: $ty),*)?| -> $crate::execution::ExpressionResult<Value> { $code },
                signature: $crate::build_closure_signature!(($($($arg: $ty $(= $default)?),*)*) -> $return_type),
                _self_type: std::marker::PhantomData
            })
        }
    }};
}

#[macro_export]
macro_rules! static_method {
    ($name:ident ($log:ident: &mut dyn RuntimeLog, $stack_trace:ident: &mut Vec<SourceReference>, $stack:ident: &mut Stack, $this:ident: $this_type:path $(, $($arg:ident: $ty:path $(= $default:expr)?),+)?) -> $return_type:path $code:block) => {{
        static METHOD: std::sync::OnceLock<crate::execution::values::Value> = std::sync::OnceLock::new();
        METHOD.get_or_init(|| $crate::build_method!($name ($log: &mut dyn RuntimeLog, $stack_trace: &mut Vec<SourceReference>, $stack: &mut Stack, $this: $this_type $(, $($arg: $ty $(= $default)?),+)?) -> $return_type $code).into())
    }};
}

#[derive(Clone)]
pub struct BuiltinFunction {
    pub callable: Arc<dyn Callable>,
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
        values::{self, SignedInteger, StructMember, UnsignedInteger},
    };
    use hashable_map::HashableMap;
    use pretty_assertions::assert_eq;

    #[test]
    fn define_closure() {
        let product = test_run("() -> std.types.UInt 1u").unwrap();

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
                        return_type: ValueType::UnsignedInteger,
                    }),
                    captured_values: HashableMap::new(),
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
    fn call_closure_captured_variable() {
        let product =
            test_run("let value = 3u; my_function = (input: std.types.UInt) -> std.types.UInt: value + input; in my_function(input = 4u)")
                .unwrap();
        assert_eq!(product, values::UnsignedInteger::from(7).into());
    }

    #[test]
    fn call_custom_method() {
        let product = test_run(
            "let object = (value = 5u, method = () -> std.types.UInt: self.value;); in object:method()",
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

    #[test]
    fn builtin_method() {
        let test_method = build_method!(
            test_method(log: &mut dyn RuntimeLog, stack_trace: &mut Vec<SourceReference>, _stack: &mut Stack, this: Dictionary) -> ValueType::UnsignedInteger {
                this.get_attribute(log, stack_trace, "value")
            }
        );

        use crate::execution::standard_environment::build_prelude;

        let root = crate::compile::full_compile(
            "let object = (value = 5u, test_method = provided_test_method); in object:test_method()",
        );
        let prelude = build_prelude();
        let mut stack = Stack::new(prelude);
        stack.insert_value("provided_test_method", test_method.into());

        let product =
            execute_expression(&mut Vec::new(), &mut Vec::new(), &mut stack, &root).unwrap();

        assert_eq!(product, values::UnsignedInteger::from(5).into());
    }

    #[test]
    fn builtin_method_with_argument() {
        let test_method = build_method!(
            test_method(log: &mut dyn RuntimeLog, stack_trace: &mut Vec<SourceReference>, _stack: &mut Stack, this: Dictionary, to_add: UnsignedInteger) -> ValueType::UnsignedInteger {
                let value: UnsignedInteger = this.get_attribute(log, stack_trace, "value")?.downcast(stack_trace)?;

                Ok(values::UnsignedInteger::from(value.0 + to_add.0).into())
            }
        );

        use crate::execution::standard_environment::build_prelude;

        let root = crate::compile::full_compile(
            "let object = (value = 5u, test_method = provided_test_method); in object:test_method(to_add = 10u)",
        );
        let prelude = build_prelude();
        let mut stack = Stack::new(prelude);
        stack.insert_value("provided_test_method", test_method.into());

        let product =
            execute_expression(&mut Vec::new(), &mut Vec::new(), &mut stack, &root).unwrap();

        assert_eq!(product, values::UnsignedInteger::from(15).into());
    }
}
