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

use std::{
    any::TypeId,
    borrow::Cow,
    collections::HashMap,
    fmt::Display,
    sync::{Arc, OnceLock},
};

use imstr::ImString;
use indexmap::IndexMap;

use crate::{
    compile::{AstNode, ClosureDefinition, Expression},
    execute_expression,
    execution::{
        errors::{ExecutionResult, Raise},
        find_all_variable_accesses_in_expression,
        logging::{LocatedStr, LogLevel, LogMessage},
        stack::ScopeType,
        values::dictionary::ArgumentName,
        values::{string::formatting::Style, Dictionary, Value},
        ExecutionContext,
    },
};

use super::{
    implicit_surface::{Surface2D, Surface3D},
    MissingAttributeError, Object, StaticType, StaticTypeName, StructDefinition, ValueType,
};
use enum_downcast::IntoVariant;

#[derive(Debug, Default)]
pub struct BuiltinCallableDatabase {
    callables: HashMap<TypeId, CallableStorage>,
    names: HashMap<String, TypeId>,
}

impl BuiltinCallableDatabase {
    pub fn new() -> Self {
        let mut database = Self::default();

        super::integer::register_methods_and_functions(&mut database);
        super::scalar::register_methods(&mut database);
        super::vector::register_methods(&mut database);
        super::value_type::register_methods(&mut database);
        super::list::register_methods(&mut database);
        super::file::register_methods(&mut database);
        super::string::register_methods(&mut database);
        super::constraint_set::register_methods(&mut database);
        super::manifold_mesh::register_methods_and_functions(&mut database);
        crate::execution::register_methods_and_functions(&mut database);
        super::iterators::register_methods(&mut database);
        super::transform::register_methods(&mut database);
        super::polygon::register_methods_and_functions(&mut database);
        crate::execution::export::register_methods_and_functions(&mut database);
        register_closure_methods(&mut database);
        register_log_functions(&mut database);
        super::implicit_surface::surface3d::register_surface3d_methods(&mut database);

        super::implicit_surface::surface3d::register_implicits(&mut database);

        super::implicit_surface::surface2d::register_implicits(&mut database);
        super::implicit_surface::surface2d::register_surface2d_methods(&mut database);
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

        if let Some(old_callable) = self
            .callables
            .insert(TypeId::of::<T>(), CallableStorage { callable })
        {
            panic!(
                "Duplicate bultin function tag: {:?}, originally registered with function `{}`",
                TypeId::of::<T>(),
                old_callable.name()
            );
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
    mut access_collector: &mut dyn FnMut(&AstNode<ImString>) -> ExecutionResult<()>,
) -> ExecutionResult<()> {
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
    captured_values: IndexMap<ArgumentName, Value>,
    expression: Arc<AstNode<Expression>>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct UserClosure {
    data: Arc<UserClosureInternals>,
}

impl UserClosure {
    pub fn signature(&self) -> &Arc<Signature> {
        &self.data.signature
    }

    pub fn expression(&self) -> &std::sync::Arc<AstNode<Expression>> {
        &self.data.expression
    }

    pub fn captured_values(&self) -> &IndexMap<ArgumentName, Value> {
        &self.data.captured_values
    }

    pub fn from_ast(
        context: &ExecutionContext,
        source: &AstNode<Box<ClosureDefinition>>,
    ) -> ExecutionResult<Self> {
        let argument_type = context.trace_scope(
            None,
            source.node.argument_type.reference.clone(),
            |context| {
                let argument_type = StructDefinition::new(context, &source.node.argument_type)?;

                Ok(argument_type)
            },
        )?;

        let return_type =
            context.trace_scope(None, source.node.return_type.reference.clone(), |context| {
                execute_expression(context, &source.node.return_type)?
                    .downcast::<ValueType>(context)
            })?;

        let signature = Arc::new(Signature {
            argument_type,
            return_type,
        });

        let expression = source.node.expression.clone();

        let mut captured_values = IndexMap::new();
        find_all_variable_accesses_in_closure_capture(&source.node, &mut |field_name| {
            let local_variables = signature.argument_type.members.keys().filter_map(|name| {
                if let ArgumentName::Named(s) = name {
                    Some(s.clone())
                } else {
                    None
                }
            });

            let value = context
                .get_variable_for_closure(
                    local_variables,
                    LocatedStr {
                        location: field_name.reference.clone(),
                        string: field_name.node.as_str(),
                    },
                )?
                .clone();

            captured_values.insert(ArgumentName::Named(field_name.node.clone()), value);

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

    /// Convert this closure to a Fidget implicit surface shape.
    ///
    /// Validates:
    /// - Closure takes exactly one parameter (Vector2 or Vector3)
    /// - Closure returns Scalar
    /// - Body contains only supported expression types
    /// - All captured values are dimensionless
    pub fn to_implicit(&self, context: &ExecutionContext<'_>) -> ExecutionResult<Value> {
        use super::implicit_surface::{ast_to_shape, resolve_captured_values, ParamDim};

        // 1. Validate signature
        let members = &self.data.signature.argument_type.members;
        if members.len() != 1 {
            return Err(InvalidClosureSignatureError {
                message:
                    "Implicit surface closure must take exactly one parameter (Vector2 or Vector3)"
                        .into(),
            }
            .to_error(context));
        }

        let (_param_name, param_type) = members.iter().next().unwrap();
        let param_dim = match &param_type.ty {
            ValueType::Vector2(Some(dim)) => {
                if dim.length == 0 {
                    return Err(InvalidClosureSignatureError {
                        message: "Implicit surface closure parameter must have length dimension (e.g., std.vector.Vector2<std.scalar.Length>)".into(),
                    }.to_error(context));
                }
                ParamDim::Vec2
            }
            ValueType::Vector3(Some(dim)) => {
                if dim.length == 0 {
                    return Err(InvalidClosureSignatureError {
                        message: "Implicit surface closure parameter must have length dimension (e.g., std.vector.Vector3<std.scalar.Length>)".into(),
                    }.to_error(context));
                }
                ParamDim::Vec3
            }
            ValueType::Vector2(None) => {
                return Err(InvalidClosureSignatureError {
                    message: "Implicit surface closure parameter must have length dimension (e.g., std.vector.Vector2<std.scalar.Length>)".into(),
                }.to_error(context));
            }
            ValueType::Vector3(None) => {
                return Err(InvalidClosureSignatureError {
                    message: "Implicit surface closure parameter must have length dimension (e.g., std.vector.Vector3<std.scalar.Length>)".into(),
                }.to_error(context));
            }
            _ => {
                return Err(InvalidClosureSignatureError {
                    message: format!(
                        "Implicit surface closure parameter must be std.vector.Vector2 or std.vector.Vector3, got {}",
                        param_type.ty
                    ),
                }.to_error(context));
            }
        };

        // Check return type is Scalar with length dimension (SDF returns signed distance)
        match &self.data.signature.return_type {
            ValueType::Scalar(Some(dim)) if dim.length != 0 => {
                // Has length dimension — valid SDF return type
            }
            ValueType::Scalar(_) => {
                return Err(InvalidClosureSignatureError {
                    message: "Implicit surface closure must return std.scalar.Length (signed distance), not dimensionless scalar".into(),
                }.to_error(context));
            }
            _ => {
                return Err(InvalidClosureSignatureError {
                    message: "Implicit surface closure must return std.scalar.Length".into(),
                }
                .to_error(context));
            }
        }

        // 2. Resolve captured values from the current execution context
        let (captured_map, closures_map) =
            resolve_captured_values(&self.data.captured_values, context, &self.data.expression)
                .map_err(|e| {
                    ImplicitSurfaceError {
                        message: e.to_string(),
                    }
                    .to_error(context)
                })?;

        // 3. Convert AST expression to ImplicitSurface
        let shape = ast_to_shape(
            &self.data.expression,
            &captured_map,
            &closures_map,
            param_dim,
        )
        .map_err(|e| {
            ImplicitSurfaceError {
                message: e.to_string(),
            }
            .to_error(context)
        })?;

        Ok(shape)
    }
}

impl Object for UserClosure {
    fn get_type(&self, _context: &ExecutionContext) -> ValueType {
        ValueType::Closure(self.data.signature.clone())
    }

    fn get_attribute(
        &self,
        _context: &ExecutionContext,
        attribute: &str,
    ) -> ExecutionResult<Value> {
        match attribute {
            "to_implicit" => Ok(BuiltinFunction::new::<methods::ToImplicit>().into()),
            _ => Err(MissingAttributeError {
                name: attribute.into(),
            }
            .to_error(_context)),
        }
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

    fn call(&self, context: &ExecutionContext, argument: Dictionary) -> ExecutionResult<Value> {
        self.data
            .signature
            .argument_type
            .check_other_qualifies(argument.struct_def())
            .map_err(|error| error.to_error(context))?;

        let argument = self.data.signature.argument_type.fill_defaults(argument);

        let signature_members: Vec<ArgumentName> = self
            .data
            .signature
            .argument_type
            .members
            .iter()
            .map(|(name, _)| name.clone())
            .collect();

        let variables: HashMap<ImString, Value> = argument
            .iter()
            .chain(self.data.captured_values.iter())
            .filter_map(|(name, value)| match name {
                ArgumentName::Named(s) => Some((s.clone(), value.clone())),
                ArgumentName::Positional(idx) => {
                    if *idx < signature_members.len() {
                        if let ArgumentName::Named(s) = &signature_members[*idx] {
                            Some((s.clone(), value.clone()))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
            })
            .collect();

        context.stack_scope(ScopeType::Inherited, variables, |context| {
            let result = execute_expression(context, &self.data.expression)?;

            self.data
                .signature
                .return_type
                .check_other_qualifies(&result.get_type(context))
                .map_err(|error| error.to_error(context))?;

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
    fn call(&self, context: &ExecutionContext, argument: Dictionary) -> ExecutionResult<Value>;

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
            $crate::execution::values::dictionary::ArgumentName::Named(imstr::ImString::from(
                stringify!($name),
            )),
            $crate::execution::values::StructMember {
                ty: <$ty as $crate::execution::values::StaticType>::static_type(),
                default: None,
            },
        )
    };
    ($name:ident: $ty:ty = $default:expr) => {
        (
            $crate::execution::values::dictionary::ArgumentName::Named(imstr::ImString::from(
                stringify!($name),
            )),
            $crate::execution::values::StructMember {
                ty: <$ty as $crate::execution::values::StaticType>::static_type(),
                default: Some($default),
            },
        )
    };
}

#[macro_export]
macro_rules! build_argument_signature_list {
    ($($arg:ident: $ty:path $(= $default:expr)?),*) => {{
        let list: [($crate::execution::values::dictionary::ArgumentName, $crate::execution::values::StructMember); _] = [$($crate::build_member_from_sig!($arg: $ty $(= $default)?),)*];
        list
    }};
}

#[macro_export]
macro_rules! build_struct_definition {
    (variadic: $variadic:literal, ($($arg:ident: $ty:path $(= $default:expr)?),*)) => {{
        let list: [($crate::execution::values::dictionary::ArgumentName, $crate::execution::values::StructMember); _] = $crate::build_argument_signature_list!($($arg: $ty $(= $default)?),*);
        let converted: indexmap::IndexMap<$crate::execution::values::dictionary::ArgumentName, $crate::execution::values::StructMember> = list.into_iter().collect();
        $crate::execution::values::StructDefinition {
            members: std::sync::Arc::new(converted),
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
        #[derive(Debug, Eq, PartialEq, Clone)]
        struct $name(pub $crate::execution::values::UserClosure);

        impl $crate::execution::values::StaticType for $name {
            fn static_type() -> $crate::execution::values::ValueType {
                static TYPE: std::sync::OnceLock<
                    std::sync::Arc<$crate::execution::values::closure::Signature>,
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

        impl From<$name> for $crate::execution::values::UserClosure {
            fn from(value: $name) -> Self {
                value.0
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
            F: Fn(&$crate::execution::ExecutionContext, &$crate::execution::values::closure::Signature, $crate::values::Dictionary) -> $crate::execution::ExecutionResult<$crate::execution::values::Value>
        {
            function: F,
            signature: std::sync::Arc<$crate::execution::values::closure::Signature>,
            name: String,
        }

        impl<F> $crate::execution::values::closure::BuiltinCallable for BuiltFunction<F>
        where
            F: Fn(&$crate::execution::ExecutionContext, &$crate::execution::values::closure::Signature, $crate::values::Dictionary) -> $crate::execution::ExecutionResult<$crate::execution::values::Value> + Send + Sync,
        {
            fn call(
                &self,
                context: &$crate::execution::ExecutionContext,
                argument: $crate::execution::values::Dictionary,
            ) -> $crate::execution::errors::ExecutionResult<$crate::execution::values::Value> {
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
            | -> $crate::execution::ExecutionResult<$crate::execution::values::Value> {
                use $crate::execution::errors::Raise as _;

                signature
                    .argument_type
                    .check_other_qualifies(argument.struct_def())
                    .map_err(|error| error.to_error($context))?;

                // Argument is potentially unused if we take no arguments.
                let mut _argument = signature.argument_type.fill_defaults(argument);

                let _data = std::sync::Arc::make_mut(&mut _argument.data);
                $($(let $arg: $ty = _data.members.shift_remove(&$crate::execution::values::dictionary::ArgumentName::Named(stringify!($arg).into()))
                        .expect("Argument was not present after argument check.").downcast::<$ty>($context)?;)*)?

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
            F: Fn(&$crate::execution::ExecutionContext, &$crate::execution::values::closure::Signature, $crate::values::Dictionary) -> $crate::execution::ExecutionResult<$crate::execution::values::Value>
        {
            function: F,
            signature: std::sync::Arc<$crate::execution::values::closure::Signature>,
            name: String,
        }

        impl<F> $crate::execution::values::closure::BuiltinCallable for BuiltFunction<F>
        where
            F: Fn(&$crate::execution::ExecutionContext, &$crate::execution::values::closure::Signature, $crate::values::Dictionary) -> $crate::execution::ExecutionResult<$crate::execution::values::Value> + Send + Sync,
        {
            fn call(
                &self,
                context: &$crate::execution::ExecutionContext,
                argument: $crate::execution::values::Dictionary,
            ) -> $crate::execution::errors::ExecutionResult<$crate::execution::values::Value> {
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
            | -> $crate::execution::ExecutionResult<$crate::execution::values::Value> {
                use $crate::execution::errors::Raise as _;

                let $this = $context.get_variable(
                    $crate::execution::logging::LocatedStr {
                        location: $context.stack_trace.bottom().clone(),
                        string: "self",
                    },
                )?.downcast_ref::<$this_type>($context)?.clone();

                signature
                    .argument_type
                    .check_other_qualifies(argument.struct_def())
                    .map_err(|error| error.to_error($context))?;

                // Argument is potentially unused if we take no arguments.
                let mut _argument = signature.argument_type.fill_defaults(argument);

                let _data = std::sync::Arc::make_mut(&mut _argument.data);
                $($(let $arg: $ty = _data.members.shift_remove(&$crate::execution::values::dictionary::ArgumentName::Named(stringify!($arg).into()))
                        .expect("Argument was not present after argument check.").downcast::<$ty>($context)?;)*)?

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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct MessageClosure(pub UserClosure);

impl StaticType for MessageClosure {
    fn static_type() -> ValueType {
        static TYPE: OnceLock<Arc<Signature>> = OnceLock::new();
        let signature = TYPE.get_or_init(|| build_closure_signature!((v: Value) -> Value));
        ValueType::Closure(signature.clone())
    }
}

impl StaticTypeName for MessageClosure {
    fn static_type_name() -> Cow<'static, str> {
        "Closure".into()
    }
}

impl IntoVariant<MessageClosure> for Value {
    fn into_variant(self) -> Result<MessageClosure, Self> {
        Ok(MessageClosure(self.into_variant()?))
    }
}

impl From<MessageClosure> for UserClosure {
    fn from(value: MessageClosure) -> Self {
        value.0
    }
}

impl From<MessageClosure> for Value {
    fn from(value: MessageClosure) -> Self {
        value.0.into()
    }
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

    fn call(&self, context: &ExecutionContext, argument: Dictionary) -> ExecutionResult<Value> {
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

pub struct LogInfo;
pub struct LogWarn;

mod methods {
    pub struct ToImplicit;
}

pub fn register_closure_methods(database: &mut BuiltinCallableDatabase) {
    build_method!(
        database,
        methods::ToImplicit, "UserClosure::to_implicit", (
            context: &ExecutionContext,
            this: UserClosure) -> Value
        {
            let result = this.to_implicit(context)?;

            // Validate that implicit surfaces are closed (finite extent)
            if let Ok(surface) = result.clone().downcast::<Surface3D>(context) {
                if !surface.is_bounded() {
                    context.log.push_message(crate::execution::LogMessage {
                        origin: context.stack_trace.bottom().clone(),
                        level: crate::execution::LogLevel::Warning,
                        message: "Implicit surface is not closed (infinite extent in at least one direction). Shapes like planes or infinite cylinders may not mesh correctly.".into(),
                    });
                }
            } else if let Ok(surface) = result.clone().downcast::<Surface2D>(context) {
                if !surface.is_bounded() {
                    context.log.push_message(crate::execution::LogMessage {
                        origin: context.stack_trace.bottom().clone(),
                        level: crate::execution::LogLevel::Warning,
                        message: "Implicit surface is not closed (infinite extent in at least one direction). Shapes like half-planes or infinite strips may not mesh correctly.".into(),
                    });
                }
            }

            Ok(result)
        }
    );
}

pub fn register_log_functions(database: &mut BuiltinCallableDatabase) {
    build_function!(
        database,
        LogInfo, "log_info", (
            context: &ExecutionContext,
            expression: Value,
            message: MessageClosure
        ) -> Value {
            use crate::execution::values::IString;

            let arg_dict = Dictionary::new(
                context,
                HashMap::from([
                    (ArgumentName::Named("v".into()), expression.clone())
                ])
            );
            let result = message.0.call(context, arg_dict)?;

            if let Ok(msg) = result.downcast::<IString>(context) {
                context.log.push_message(LogMessage {
                    origin: context.stack_trace.bottom().clone(),
                    level: LogLevel::Info,
                    message: msg.0.to_string().into(),
                });
            }

            Ok(expression)
        }
    );

    build_function!(
        database,
        LogWarn, "log_warn", (
            context: &ExecutionContext,
            expression: Value,
            message: MessageClosure
        ) -> Value {
            use crate::execution::values::IString;

            let arg_dict = Dictionary::new(
                context,
                HashMap::from([
                    (ArgumentName::Named("v".into()), expression.clone())
                ])
            );
            let result = message.0.call(context, arg_dict)?;

            if let Ok(msg) = result.downcast::<IString>(context) {
                context.log.push_message(LogMessage {
                    origin: context.stack_trace.bottom().clone(),
                    level: LogLevel::Warning,
                    message: msg.0.to_string().into(),
                });
            }

            Ok(expression)
        }
    );
}

/// Error indicating a closure has an invalid signature for implicit surface conversion.
#[derive(Debug, Clone)]
pub struct InvalidClosureSignatureError {
    pub message: String,
}

impl std::error::Error for InvalidClosureSignatureError {}

impl Display for InvalidClosureSignatureError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Invalid closure signature: {}", self.message)
    }
}

/// Error indicating a problem during implicit surface conversion.
#[derive(Debug, Clone)]
pub struct ImplicitSurfaceError {
    pub message: String,
}

impl std::error::Error for ImplicitSurfaceError {}

impl Display for ImplicitSurfaceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Implicit surface error: {}", self.message)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        execution::{
            test_context_custom_database, test_run,
            values::{self, SignedInteger, StructMember, UnsignedInteger},
        },
        values::value_type::{MissmatchedField, TypeQualificationError},
    };

    use indexmap::IndexMap;
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
                            members: Arc::new(IndexMap::new()),
                            variadic: false,
                        },
                        return_type: ValueType::UnsignedInteger,
                    }),
                    captured_values: IndexMap::new(),
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
        let error = test_run(
            "let my_function = (a: std.types.UInt) -> std.types.UInt: a + 2u; in my_function(a = 3i)",
        )
        .unwrap_err();
        let error = error.ty.as_any();
        let error: &TypeQualificationError = error.downcast_ref().unwrap();
        assert_eq!(
            *error,
            TypeQualificationError::Fields {
                failed_feilds: vec![MissmatchedField {
                    name: "a".into(),
                    error: TypeQualificationError::This {
                        expected: ValueType::UnsignedInteger,
                        got: ValueType::SignedInteger,
                    },
                },],
            }
        );
    }

    #[test]
    fn call_closure_bad_result() {
        let error = test_run(
            "let my_function = (a: std.types.UInt) -> std.types.UInt: \"test\"; in my_function(a = 3u)",
        )
        .unwrap_err();
        let error = error.ty.as_any();
        let error: &TypeQualificationError = error.downcast_ref().unwrap();
        assert_eq!(
            *error,
            TypeQualificationError::This {
                expected: ValueType::UnsignedInteger,
                got: ValueType::String
            }
        );
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
                ArgumentName::Named(ImString::from("value")),
                StructMember {
                    ty: ValueType::SignedInteger,
                    default: None
                }
            )]
        );
        assert_eq!(
            build_argument_signature_list!(value: UnsignedInteger),
            [(
                ArgumentName::Named(ImString::from("value")),
                StructMember {
                    ty: ValueType::UnsignedInteger,
                    default: None
                }
            )]
        );
        assert_eq!(
            build_argument_signature_list!(value: UnsignedInteger = UnsignedInteger::from(23).into()),
            [(
                ArgumentName::Named(ImString::from("value")),
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
                    ArgumentName::Named(ImString::from("value")),
                    StructMember {
                        ty: ValueType::UnsignedInteger,
                        default: None
                    }
                ),
                (
                    ArgumentName::Named(ImString::from("value1")),
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
                    ArgumentName::Named(ImString::from("value")),
                    StructMember {
                        ty: ValueType::UnsignedInteger,
                        default: Some(UnsignedInteger::from(32).into())
                    }
                ),
                (
                    ArgumentName::Named(ImString::from("value1")),
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
                let product = execute_expression(context, &root).unwrap();

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
                let product = execute_expression(context, &root).unwrap();

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
                let product = execute_expression(context, &root).unwrap();

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
                let product = execute_expression(context, &root).unwrap();

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
                let product = execute_expression(context, &root).unwrap();

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
                let value: UnsignedInteger = this.get_attribute(context, "value")?.downcast(context)?;

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
                let product = execute_expression(context, &root).unwrap();

                assert_eq!(product, values::UnsignedInteger::from(15).into());
            },
        )
    }

    #[test]
    fn builtin_function_positional_args() {
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

        let root = crate::compile::full_compile("test_function(1u, 2u)");
        test_context_custom_database(
            database,
            [(
                "test_function".into(),
                BuiltinFunction::new::<TestFunction>().into(),
            )],
            |context| {
                let product = execute_expression(context, &root).unwrap();

                assert_eq!(product, values::UnsignedInteger::from(3).into());
            },
        )
    }

    #[test]
    fn builtin_function_mixed_args() {
        let mut database = BuiltinCallableDatabase::new();
        struct TestFunction;
        build_function!(
            database,
            TestFunction, "test_function", (
                _context: &ExecutionContext,
                a: UnsignedInteger,
                b: UnsignedInteger,
                c: UnsignedInteger
            ) -> UnsignedInteger {
                Ok(values::UnsignedInteger::from(a.0 + b.0 + c.0))
            }
        );

        let root = crate::compile::full_compile("test_function(1u, 2u, c = 3u)");
        test_context_custom_database(
            database,
            [(
                "test_function".into(),
                BuiltinFunction::new::<TestFunction>().into(),
            )],
            |context| {
                let product = execute_expression(context, &root).unwrap();

                assert_eq!(product, values::UnsignedInteger::from(6).into());
            },
        )
    }

    #[test]
    fn builtin_function_positional_with_default() {
        let mut database = BuiltinCallableDatabase::new();
        struct TestFunction;
        build_function!(
            database,
            TestFunction, "test_function", (
                _context: &ExecutionContext,
                a: UnsignedInteger,
                b: UnsignedInteger = UnsignedInteger::from(10).into()
            ) -> UnsignedInteger {
                Ok(values::UnsignedInteger::from(a.0 + b.0))
            }
        );

        let root = crate::compile::full_compile("test_function(5u)");
        test_context_custom_database(
            database,
            [(
                "test_function".into(),
                BuiltinFunction::new::<TestFunction>().into(),
            )],
            |context| {
                let product = execute_expression(context, &root).unwrap();

                assert_eq!(product, values::UnsignedInteger::from(15).into());
            },
        )
    }

    #[test]
    fn builtin_method_positional_args() {
        let mut database = BuiltinCallableDatabase::new();
        struct TestMethod;

        build_method!(
            database,
            TestMethod, "test_method", (
                context: &ExecutionContext,
                this: Dictionary,
                to_add: UnsignedInteger
            ) -> UnsignedInteger {
                let value: UnsignedInteger = this.get_attribute(context, "value")?.downcast(context)?;

                Ok(values::UnsignedInteger::from(value.0 + to_add.0))
            }
        );

        let root = crate::compile::full_compile(
            "let object = (value = 5u, test_method = provided_test_method); in object::test_method(10u)",
        );
        test_context_custom_database(
            database,
            [(
                "provided_test_method".into(),
                BuiltinFunction::new::<TestMethod>().into(),
            )],
            |context| {
                let product = execute_expression(context, &root).unwrap();

                assert_eq!(product, values::UnsignedInteger::from(15).into());
            },
        )
    }

    #[test]
    fn builtin_method_mixed_args() {
        let mut database = BuiltinCallableDatabase::new();
        struct TestMethod;

        build_method!(
            database,
            TestMethod, "test_method", (
                context: &ExecutionContext,
                this: Dictionary,
                to_add: UnsignedInteger,
                to_mul: UnsignedInteger
            ) -> UnsignedInteger {
                let value: UnsignedInteger = this.get_attribute(context, "value")?.downcast(context)?;

                Ok(values::UnsignedInteger::from((value.0 + to_add.0) * to_mul.0))
            }
        );

        let root = crate::compile::full_compile(
            "let object = (value = 5u, test_method = provided_test_method); in object::test_method(10u, to_mul = 2u)",
        );
        test_context_custom_database(
            database,
            [(
                "provided_test_method".into(),
                BuiltinFunction::new::<TestMethod>().into(),
            )],
            |context| {
                let product = execute_expression(context, &root).unwrap();

                assert_eq!(product, values::UnsignedInteger::from(30).into());
            },
        )
    }
}
