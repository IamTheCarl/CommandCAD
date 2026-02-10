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

use std::{any::TypeId, borrow::Cow, collections::HashMap, fmt::Display, sync::Arc};

use hashable_map::HashableMap;
use imstr::ImString;

use crate::{
    compile::{AstNode, ClosureDefinition, Expression},
    execute_expression,
    execution::{
        errors::{ExpressionResult, Raise},
        find_all_variable_accesses_in_expression,
        logging::{LocatedStr, LogLevel, LogMessage},
        stack::ScopeType,
        values::{string::formatting::Style, Dictionary, Value},
        ExecutionContext,
    },
};

use super::{Object, StaticTypeName, StructDefinition, ValueType};

#[derive(Debug, Default)]
pub struct BuiltinCallableDatabase {
    callables: HashMap<TypeId, CallableStorage>,
    names: HashMap<String, TypeId>,
}

impl BuiltinCallableDatabase {
    pub fn new() -> Self {
        let mut database = Self::default();

        super::integer::register_methods(&mut database);
        super::scalar::register_methods(&mut database);
        super::vector::register_methods(&mut database);
        super::value_type::register_methods(&mut database);
        super::list::register_methods(&mut database);
        super::file::register_methods(&mut database);
        super::string::register_methods(&mut database);
        super::constraint_set::register_methods(&mut database);
        super::manifold_mesh::register_methods_and_functions(&mut database);

        database
    }

    pub fn register<T: 'static>(&mut self, callable: Box<dyn BuiltinCallable>) {
        if self
            .names
            .insert(callable.name().to_string(), TypeId::of::<T>())
            .is_some()
        {
            panic!("Duplicate bultin function name: {}", callable.name());
        }

        if self
            .callables
            .insert(TypeId::of::<T>(), CallableStorage { callable })
            .is_some()
        {
            panic!("Duplicate bultin function tag: {:?}", TypeId::of::<T>());
        }
    }

    fn get_callable(&self, id: TypeId) -> &CallableStorage {
        self.callables
            .get(&id)
            .expect("Forward callable was not present")
    }
}

#[derive(Debug)]
struct CallableStorage {
    callable: Box<dyn BuiltinCallable>,
}

impl std::ops::Deref for CallableStorage {
    type Target = dyn BuiltinCallable;

    fn deref(&self) -> &Self::Target {
        self.callable.as_ref()
    }
}

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

pub fn find_all_variable_accesses_in_closure_capture(
    closure: &crate::compile::ClosureDefinition,
    mut access_collector: &mut dyn FnMut(&AstNode<ImString>) -> ExpressionResult<()>,
) -> ExpressionResult<()> {
    let argument_names: Vec<&ImString> = {
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

/// Closures are immutable, meaning that all copies can reference the same data.
/// This is that common data.
#[derive(Debug, Eq, PartialEq)]
struct UserClosureInternals {
    signature: Arc<Signature>,
    captured_values: HashableMap<ImString, Value>,
    expression: Arc<AstNode<Expression>>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct UserClosure {
    data: Arc<UserClosureInternals>,
}

impl UserClosure {
    pub fn from_ast(
        context: &ExecutionContext,
        source: &AstNode<Box<ClosureDefinition>>,
    ) -> ExpressionResult<Self> {
        let argument_type =
            context.trace_scope(source.node.argument_type.reference.clone(), |context| {
                let argument_type = StructDefinition::new(context, &source.node.argument_type)?;

                Ok(argument_type)
            })?;

        let return_type =
            context.trace_scope(source.node.return_type.reference.clone(), |context| {
                execute_expression(context, &source.node.return_type)?
                    .downcast::<ValueType>(context.stack_trace)
            })?;

        let signature = Arc::new(Signature {
            argument_type,
            return_type,
        });

        let expression = source.node.expression.clone();

        let mut captured_values = HashableMap::new();
        find_all_variable_accesses_in_closure_capture(&source.node, &mut |field_name| {
            let local_variables = signature
                .argument_type
                .members
                .iter()
                .map(|(name, _value): (&ImString, _)| name.clone());

            let value = context
                .get_variable_for_closure(
                    local_variables,
                    LocatedStr {
                        location: field_name.reference.clone(),
                        string: field_name.node.as_str(),
                    },
                )?
                .clone();

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
    fn get_type(&self, _context: &ExecutionContext) -> ValueType {
        ValueType::Closure(self.data.signature.clone())
    }

    fn format(
        &self,
        context: &ExecutionContext,
        f: &mut dyn std::fmt::Write,
        style: Style,
        precision: Option<u8>,
    ) -> std::fmt::Result {
        if !matches!(style, Style::Default) {
            context.log.push_message(LogMessage {
                origin: context.stack_trace.bottom().clone(),
                level: LogLevel::Warning,
                message: "Boolean values only support default formatting".into(),
            });
        }

        if precision.is_some() {
            context.log.push_message(LogMessage {
                origin: context.stack_trace.bottom().clone(),
                level: LogLevel::Warning,
                message: "Boolean values cannot be formatted with precision".into(),
            });
        }

        write!(f, "{}", self.get_type(context))
    }

    fn call(&self, context: &ExecutionContext, argument: Dictionary) -> ExpressionResult<Value> {
        self.data
            .signature
            .argument_type
            .check_other_qualifies(argument.struct_def())
            .map_err(|error| error.to_error(context.stack_trace))?;

        let argument = self.data.signature.argument_type.fill_defaults(argument);

        let variables: HashMap<ImString, Value> = argument
            .iter()
            .chain(self.data.captured_values.iter())
            .map(|(name, value)| (name.clone(), value.clone()))
            .collect();

        context.stack_scope(ScopeType::Inherited, variables, |context| {
            let result = execute_expression(context, &self.data.expression)?;

            self.data
                .signature
                .return_type
                .check_other_qualifies(&result.get_type(context))
                .map_err(|error| error.to_error(context.stack_trace))?;

            Ok(result)
        })?
    }
}

impl StaticTypeName for UserClosure {
    fn static_type_name() -> Cow<'static, str> {
        "Closure".into()
    }
}

pub trait BuiltinCallable: Sync + Send {
    fn call(&self, context: &ExecutionContext, argument: Dictionary) -> ExpressionResult<Value>;

    fn name(&self) -> &str;

    fn signature(&self) -> &Arc<Signature>;

    fn scope_type(&self) -> ScopeType {
        ScopeType::Isolated
    }
}

impl std::fmt::Debug for dyn BuiltinCallable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Builtin")
            .field("name", &self.name())
            .field("signature", self.signature())
            .finish()
    }
}

#[macro_export]
macro_rules! build_member_from_sig {
    ($name:ident: $ty:ty) => {
        (
            imstr::ImString::from(stringify!($name)),
            $crate::execution::values::StructMember {
                ty: <$ty as crate::execution::values::StaticType>::static_type(),
                default: None,
            },
        )
    };
    ($name:ident: $ty:ty = $default:expr) => {
        (
            imstr::ImString::from(stringify!($name)),
            $crate::execution::values::StructMember {
                ty: <$ty as crate::execution::values::StaticType>::static_type(),
                default: Some($default),
            },
        )
    };
}

#[macro_export]
macro_rules! build_argument_signature_list {
    ($($arg:ident: $ty:path $(= $default:expr)?),*) => {{
        let list: [(imstr::ImString, $crate::execution::values::StructMember); _] = [$($crate::build_member_from_sig!($arg: $ty $(= $default)?),)*];
        list
    }};
}

#[macro_export]
macro_rules! build_struct_definition {
    (variadic: $variadic:literal, ($($arg:ident: $ty:path $(= $default:expr)?),*)) => {{
        $crate::execution::values::StructDefinition {
            members: std::sync::Arc::new(hashable_map::HashableMap::from(std::collections::HashMap::from($crate::build_argument_signature_list!($($arg: $ty $(= $default)?),*)))),
            variadic: $variadic,
        }
    }};
}

#[macro_export]
macro_rules! build_closure_signature {
    (($($arg:ident: $ty:path $(= $default:expr)?),*) -> $return_type:ty) => {{
        std::sync::Arc::new($crate::execution::values::closure::Signature {
            argument_type: $crate::build_struct_definition!(variadic: false, ($($arg: $ty $(= $default)?),*)),
            return_type: <$return_type as $crate::execution::values::StaticType>::static_type(),
        })
    }};
}

#[macro_export]
macro_rules! build_closure_type {
    ($name:ident($($arg:ident: $ty:path $(= $default:expr)?),*) -> $return_type:ty) => {
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
            fn static_type_name() -> std::borrow::Cow<'static, str> {
                "Closure".into()
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
macro_rules! build_function_callable {
    ($name:literal ($context:ident: &ExecutionContext $(, $($arg:ident: $ty:path $(= $default:expr)?),+)?) -> $return_type:ty $code:block) => {{
        struct BuiltFunction<F>
        where
            F: Fn(&$crate::execution::ExecutionContext, &$crate::execution::values::closure::Signature, $crate::values::Dictionary) -> $crate::execution::ExpressionResult<$crate::execution::values::Value>
        {
            function: F,
            signature: std::sync::Arc<$crate::execution::values::closure::Signature>,
            name: String,
        }

        impl<F> $crate::execution::values::closure::BuiltinCallable for BuiltFunction<F>
        where
            F: Fn(&$crate::execution::ExecutionContext, &$crate::execution::values::closure::Signature, $crate::values::Dictionary) -> $crate::execution::ExpressionResult<$crate::execution::values::Value> + Send + Sync,
        {
            fn call(
                &self,
                context: &$crate::execution::ExecutionContext,
                argument: $crate::execution::values::Dictionary,
            ) -> $crate::execution::errors::ExpressionResult<$crate::execution::values::Value> {
                (self.function)(context, &self.signature, argument)
            }

            fn name(&self) -> &str {
                &self.name
            }

            fn signature(&self) -> &std::sync::Arc<$crate::execution::values::closure::Signature> {
                &self.signature
            }
        }

        BuiltFunction {
            function: move |
                $context: &$crate::execution::ExecutionContext,
                signature: &$crate::execution::values::closure::Signature,
                argument: $crate::execution::values::Dictionary
            | -> $crate::execution::ExpressionResult<$crate::execution::values::Value> {
                use crate::execution::errors::Raise as _;

                signature
                    .argument_type
                    .check_other_qualifies(argument.struct_def())
                    .map_err(|error| error.to_error($context.stack_trace.iter()))?;

                // Argument is potentially unused if we take no arguments.
                let mut _argument = signature.argument_type.fill_defaults(argument);

                let _data = std::sync::Arc::make_mut(&mut _argument.data);
                $($(let $arg: $ty = _data.members.remove(stringify!($arg))
                        .expect("Argument was not present after argument check.").downcast::<$ty>($context.stack_trace)?;)*)?

                let result: $return_type = {
                    $code?
                };
                Ok(result.into())
            },
            signature: $crate::build_closure_signature!(($($($arg: $ty $(= $default)?),*)*) -> $return_type),
            name: $name.into(),
        }
    }};
}

#[macro_export]
macro_rules! build_function {
    ($database:ident,
        $ident:ty, $name:literal, ($context:ident: &ExecutionContext $(, $($arg:ident: $ty:path $(= $default:expr)?),+)?) -> $return_type:ty $code:block
    ) => {{
        let callable = $crate::build_function_callable!($name ($context: &ExecutionContext $(, $($arg: $ty $(= $default)?),+)?) -> $return_type $code);

        $database.register::<$ident>(Box::new(callable))
    }};
}

#[macro_export]
macro_rules! build_method_callable {
    ($name:expr,
        ($context:ident: &ExecutionContext, $this:ident: $this_type:ty $(, $($arg:ident: $ty:path $(= $default:expr)?),+)?) -> $return_type:ty $code:block
    ) => {{
        struct BuiltFunction<F>
        where
            F: Fn(&$crate::execution::ExecutionContext, &$crate::execution::values::closure::Signature, $crate::values::Dictionary) -> $crate::execution::ExpressionResult<$crate::execution::values::Value>
        {
            function: F,
            signature: std::sync::Arc<$crate::execution::values::closure::Signature>,
            name: String,
        }

        impl<F> $crate::execution::values::closure::BuiltinCallable for BuiltFunction<F>
        where
            F: Fn(&$crate::execution::ExecutionContext, &$crate::execution::values::closure::Signature, $crate::values::Dictionary) -> $crate::execution::ExpressionResult<$crate::execution::values::Value> + Send + Sync,
        {
            fn call(
                &self,
                context: &$crate::execution::ExecutionContext,
                argument: $crate::execution::values::Dictionary,
            ) -> $crate::execution::errors::ExpressionResult<$crate::execution::values::Value> {
                (self.function)(context, &self.signature, argument)
            }

            fn name(&self) -> &str {
                &self.name
            }

            fn signature(&self) -> &std::sync::Arc<$crate::execution::values::closure::Signature> {
                &self.signature
            }
        }

        BuiltFunction {
            function: move |
                $context: &$crate::execution::ExecutionContext,
                signature: &$crate::execution::values::closure::Signature,
                argument: $crate::execution::values::Dictionary
            | -> $crate::execution::ExpressionResult<$crate::execution::values::Value> {
                use crate::execution::errors::Raise as _;

                let $this = $context.get_variable(
                    $crate::execution::logging::LocatedStr {
                        location: $context.stack_trace.bottom().clone(),
                        string: "self",
                    },
                )?.downcast_ref::<$this_type>($context.stack_trace)?.clone();

                signature
                    .argument_type
                    .check_other_qualifies(argument.struct_def())
                    .map_err(|error| error.to_error($context.stack_trace.iter()))?;

                // Argument is potentially unused if we take no arguments.
                let mut _argument = signature.argument_type.fill_defaults(argument);

                let _data = std::sync::Arc::make_mut(&mut _argument.data);
                $($(let $arg: $ty = _data.members.remove(stringify!($arg))
                        .expect("Argument was not present after argument check.").downcast::<$ty>($context.stack_trace)?;)*)?

                let result: $return_type = {
                    $code?
                };
                Ok(result.into())
            },
            signature: $crate::build_closure_signature!(($($($arg: $ty $(= $default)?),*)*) -> $return_type),
            name: $name.into(),
        }
    }};
}

#[macro_export]
macro_rules! build_method {
    ($database:ident,
        $ident:ty, $name:expr, ($context:ident: &ExecutionContext, $this:ident: $this_type:ty $(, $($arg:ident: $ty:path $(= $default:expr)?),+)?) -> $return_type:path $code:block
    ) => {{
        let callable = $crate::build_method_callable!($name,
            ($context: &ExecutionContext, $this: $this_type $(, $($arg: $ty $(= $default)?),+)?) -> $return_type $code
        );

        $database.register::<$ident>(Box::new(callable))
    }};
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BuiltinFunction(pub TypeId);

impl BuiltinFunction {
    pub fn new<T: 'static>() -> Self {
        Self(TypeId::of::<T>())
    }
}

impl Object for BuiltinFunction {
    fn get_type(&self, context: &ExecutionContext) -> ValueType {
        ValueType::Closure(context.database.get_callable(self.0).signature().clone())
    }

    fn format(
        &self,
        context: &ExecutionContext,
        f: &mut dyn std::fmt::Write,
        style: Style,
        precision: Option<u8>,
    ) -> std::fmt::Result {
        if !matches!(style, Style::Default) {
            context.log.push_message(LogMessage {
                origin: context.stack_trace.bottom().clone(),
                level: LogLevel::Warning,
                message: "Closures only support default formatting".into(),
            });
        }

        if precision.is_some() {
            context.log.push_message(LogMessage {
                origin: context.stack_trace.bottom().clone(),
                level: LogLevel::Warning,
                message: "Closures cannot be formatted with precision".into(),
            });
        }

        write!(f, "{}", self.get_type(context))
    }

    fn call_scope_type(&self, context: &ExecutionContext) -> ScopeType {
        context.database.get_callable(self.0).callable.scope_type()
    }

    fn call(&self, context: &ExecutionContext, argument: Dictionary) -> ExpressionResult<Value> {
        context
            .database
            .get_callable(self.0)
            .call(context, argument)
    }
}

impl StaticTypeName for BuiltinFunction {
    fn static_type_name() -> Cow<'static, str> {
        "Builtin Function".into()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::execution::{
        test_context_custom_database, test_run,
        values::{self, SignedInteger, StructMember, UnsignedInteger},
    };
    use hashable_map::HashableMap;
    use pretty_assertions::assert_eq;

    #[test]
    fn define_closure() {
        let product = test_run("() -> std.types.UInt: 1u").unwrap();

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
            "let object = (value = 5u, method = () -> std.types.UInt: self.value); in object::method()",
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
                ImString::from("value"),
                StructMember {
                    ty: ValueType::SignedInteger,
                    default: None
                }
            )]
        );
        assert_eq!(
            build_argument_signature_list!(value: UnsignedInteger),
            [(
                ImString::from("value"),
                StructMember {
                    ty: ValueType::UnsignedInteger,
                    default: None
                }
            )]
        );
        assert_eq!(
            build_argument_signature_list!(value: UnsignedInteger = UnsignedInteger::from(23).into()),
            [(
                ImString::from("value"),
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
                    ImString::from("value"),
                    StructMember {
                        ty: ValueType::UnsignedInteger,
                        default: None
                    }
                ),
                (
                    ImString::from("value1"),
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
                    ImString::from("value"),
                    StructMember {
                        ty: ValueType::UnsignedInteger,
                        default: Some(UnsignedInteger::from(32).into())
                    }
                ),
                (
                    ImString::from("value1"),
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
        let mut database = BuiltinCallableDatabase::new();

        struct TestFunction;
        build_function!(
            database,
            TestFunction, "test_function", (
                _context: &ExecutionContext
            ) -> UnsignedInteger {
                Ok(values::UnsignedInteger::from(846))
            }
        );

        let root = crate::compile::full_compile("test_function()");
        test_context_custom_database(
            database,
            [(
                "test_function".into(),
                BuiltinFunction::new::<TestFunction>().into(),
            )],
            |context| {
                let product = execute_expression(&context, &root).unwrap();

                assert_eq!(product, values::UnsignedInteger::from(846).into());
            },
        )
    }

    #[test]
    fn builtin_function_with_args() {
        let mut database = BuiltinCallableDatabase::new();

        struct TestFunction;
        build_function!(
            database,
            TestFunction, "test_function", (
                _context: &ExecutionContext,
                a: UnsignedInteger,
                b: UnsignedInteger
            ) -> UnsignedInteger {
                Ok(values::UnsignedInteger::from(a.0 + b.0))
            }
        );

        let root = crate::compile::full_compile("test_function(a = 1u, b = 2u)");
        test_context_custom_database(
            database,
            [(
                "test_function".into(),
                BuiltinFunction::new::<TestFunction>().into(),
            )],
            |context| {
                let product = execute_expression(&context, &root).unwrap();

                assert_eq!(product, values::UnsignedInteger::from(3).into());
            },
        )
    }

    #[test]
    fn builtin_function_with_default_value() {
        let mut database = BuiltinCallableDatabase::new();
        struct TestFunction;
        build_function!(
            database,
            TestFunction, "test_function", (
                _context: &ExecutionContext,
                a: UnsignedInteger,
                b: UnsignedInteger = UnsignedInteger::from(2).into()
            ) -> UnsignedInteger {
                Ok(values::UnsignedInteger::from(a.0 + b.0))
            }
        );

        let root = crate::compile::full_compile("test_function(a = 1u)");
        test_context_custom_database(
            database,
            [(
                "test_function".into(),
                BuiltinFunction::new::<TestFunction>().into(),
            )],
            |context| {
                let product = execute_expression(&context, &root).unwrap();

                assert_eq!(product, values::UnsignedInteger::from(3).into());
            },
        )
    }

    #[test]
    fn builtin_function_captured_value() {
        let mut database = BuiltinCallableDatabase::new();
        let b = 2;

        struct TestFunction;
        build_function!(
            database,
            TestFunction, "test_function", (
                _context: &ExecutionContext,
                a: UnsignedInteger
            ) -> UnsignedInteger {
                Ok(values::UnsignedInteger::from(a.0 + b))
            }
        );

        let root = crate::compile::full_compile("test_function(a = 1u)");
        test_context_custom_database(
            database,
            [(
                "test_function".into(),
                BuiltinFunction::new::<TestFunction>().into(),
            )],
            |context| {
                let product = execute_expression(&context, &root).unwrap();

                assert_eq!(product, values::UnsignedInteger::from(3).into());
            },
        )
    }

    #[test]
    fn builtin_method() {
        let mut database = BuiltinCallableDatabase::new();
        struct TestMethod;
        build_method!(
            database,
            TestMethod, "test_method", (context: &ExecutionContext, this: Dictionary) -> Value {
                this.get_attribute(context, "value")
            }
        );

        let root = crate::compile::full_compile(
            "let object = (value = 5u, test_method = provided_test_method); in object::test_method()",
        );
        test_context_custom_database(
            database,
            [(
                "provided_test_method".into(),
                BuiltinFunction::new::<TestMethod>().into(),
            )],
            |context| {
                let product = execute_expression(&context, &root).unwrap();

                assert_eq!(product, values::UnsignedInteger::from(5).into());
            },
        )
    }

    #[test]
    fn builtin_method_with_argument() {
        let mut database = BuiltinCallableDatabase::new();
        struct TestMethod;

        build_method!(
            database,
            TestMethod, "test_method", (
                context: &ExecutionContext,
                this: Dictionary,
                to_add: UnsignedInteger
            ) -> UnsignedInteger {
                let value: UnsignedInteger = this.get_attribute(context, "value")?.downcast(context.stack_trace)?;

                Ok(values::UnsignedInteger::from(value.0 + to_add.0))
            }
        );

        let root = crate::compile::full_compile(
            "let object = (value = 5u, test_method = provided_test_method); in object::test_method(to_add = 10u)",
        );
        test_context_custom_database(
            database,
            [(
                "provided_test_method".into(),
                BuiltinFunction::new::<TestMethod>().into(),
            )],
            |context| {
                let product = execute_expression(&context, &root).unwrap();

                assert_eq!(product, values::UnsignedInteger::from(15).into());
            },
        )
    }
}
