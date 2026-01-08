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

use crate::{
    compile::{AstNode, ClosureDefinition, Expression, SourceReference},
    execute_expression,
    execution::{
        errors::{ExpressionResult, Raise},
        find_value,
        logging::{RuntimeLog, StackScope},
        stack::{ScopeType, Stack},
        values::{Dictionary, MissingAttributeError, Value},
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

        database
    }

    fn register_internal<T: 'static>(
        &mut self,
        forward: Box<dyn BuiltinCallable>,
        inverse: Option<TypeId>,
    ) {
        if self
            .names
            .insert(forward.name().to_string(), TypeId::of::<T>())
            .is_some()
        {
            panic!("Duplicate bultin function name: {}", forward.name());
        }

        if self
            .callables
            .insert(
                TypeId::of::<T>(),
                CallableStorage {
                    forward,
                    reverse: inverse,
                },
            )
            .is_some()
        {
            panic!("Duplicate bultin function tag: {:?}", TypeId::of::<T>());
        }
    }

    pub fn register<T: 'static>(&mut self, callable: Box<dyn BuiltinCallable>) {
        self.register_internal::<T>(callable, Option::None);
    }

    pub fn register_with_inverse<ForwardT: 'static, InverseT: 'static>(
        &mut self,
        forward: Box<dyn BuiltinCallable>,
        inverse: Box<dyn BuiltinCallable>,
    ) {
        self.register_internal::<ForwardT>(forward, Some(TypeId::of::<InverseT>()));
        self.register_internal::<InverseT>(inverse, Some(TypeId::of::<ForwardT>()));
    }

    fn get_forward(&self, id: TypeId) -> &CallableStorage {
        self.callables
            .get(&id)
            .expect("Forward callable was not present")
    }

    fn get_inverse(&self, id: TypeId) -> Option<TypeId> {
        let forward = self
            .callables
            .get(&id)
            .expect("Forward callable was not present");

        forward.reverse.clone()
    }
}

#[derive(Debug)]
struct CallableStorage {
    forward: Box<dyn BuiltinCallable>,
    reverse: Option<TypeId>,
}

impl std::ops::Deref for CallableStorage {
    type Target = dyn BuiltinCallable;

    fn deref(&self) -> &Self::Target {
        self.forward.as_ref()
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
        database: &BuiltinCallableDatabase,
        source: &AstNode<Box<ClosureDefinition>>,
    ) -> ExpressionResult<Self> {
        let argument_type = stack_trace.stack_scope(
            source.node.argument_type.reference.clone(),
            |stack_trace| {
                let argument_type = StructDefinition::new(
                    log,
                    stack_trace,
                    stack,
                    database,
                    &source.node.argument_type,
                )?;

                Ok(argument_type)
            },
        )?;

        let return_type =
            stack_trace.stack_scope(source.node.return_type.reference.clone(), |stack_trace| {
                execute_expression(log, stack_trace, stack, database, &source.node.return_type)?
                    .downcast::<ValueType>(stack_trace)
            })?;

        let signature = Arc::new(Signature {
            argument_type,
            return_type,
        });

        let expression = source.node.expression.clone();

        let mut captured_values = HashableMap::new();
        find_all_variable_accesses_in_closure_capture(&source.node, &mut |field_name| {
            let value = find_value(log, stack_trace, stack, database, [field_name])?;

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
    fn get_type(&self, _callable_database: &BuiltinCallableDatabase) -> ValueType {
        ValueType::Closure(self.data.signature.clone())
    }

    fn call(
        &self,
        log: &mut dyn RuntimeLog,
        stack_trace: &mut Vec<SourceReference>,
        stack: &mut Stack,
        database: &BuiltinCallableDatabase,
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

            let result =
                execute_expression(log, stack_trace, stack, database, &self.data.expression)?;

            self.data
                .signature
                .return_type
                .check_other_qualifies(&result.get_type(database))
                .map_err(|error| error.to_error(stack_trace.iter()))?;

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
    fn call(
        &self,
        runtime: &mut dyn RuntimeLog,
        stack_trace: &mut Vec<SourceReference>,
        stack: &mut Stack,
        database: &BuiltinCallableDatabase,
        argument: Dictionary,
    ) -> ExpressionResult<Value>;

    fn name(&self) -> &str;

    fn signature(&self) -> &Arc<Signature>;
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
        let list: [(String, $crate::execution::values::value_type::StructMember); _] = [$($crate::build_member_from_sig!($arg: $ty $(= $default)?),)*];
        list
    }};
}

#[macro_export]
macro_rules! build_closure_signature {
    (($($arg:ident: $ty:path $(= $default:expr)?),*) -> $return_type:ty) => {{
        let members = std::sync::Arc::new(hashable_map::HashableMap::from(std::collections::HashMap::from($crate::build_argument_signature_list!($($arg: $ty $(= $default)?),*))));

        std::sync::Arc::new($crate::execution::values::closure::Signature {
            argument_type: crate::execution::values::StructDefinition {
                members,
                variadic: false,
            },
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
    ($name:literal ($log:ident: &mut dyn RuntimeLog, $stack_trace:ident: &mut Vec<SourceReference>, $stack:ident: &mut Stack, $database:ident: &BuiltinCallableDatabase $(, $($arg:ident: $ty:path $(= $default:expr)?),+)?) -> $return_type:ty $code:block) => {{
        struct BuiltFunction<F: Fn(&mut dyn RuntimeLog, &mut Vec<SourceReference>, &mut Stack, &BuiltinCallableDatabase $(, $($ty),*)?) -> ExpressionResult<$crate::execution::values::Value>> {
            function: F,
            signature: std::sync::Arc<$crate::execution::values::closure::Signature>,
        }

        impl<F: Fn(&mut dyn RuntimeLog, &mut Vec<SourceReference>, &mut Stack, &BuiltinCallableDatabase $(, $($ty),*)?) -> ExpressionResult<$crate::execution::values::Value> + Send + Sync> $crate::execution::values::closure::BuiltinCallable for BuiltFunction<F> {
            fn call(
                &self,
                runtime: &mut dyn RuntimeLog,
                stack_trace: &mut Vec<SourceReference>,
                stack: &mut Stack,
                database: &BuiltinCallableDatabase,
                argument: $crate::execution::values::Dictionary,
            ) -> $crate::execution::errors::ExpressionResult<$crate::execution::values::Value> {
                use $crate::execution::errors::Raise;
                self.signature
                    .argument_type
                    .check_other_qualifies(argument.struct_def())
                    .map_err(|error| error.to_error(stack_trace.iter()))?;

                // Argument is potentially unused if we take no arguments.
                let mut _argument = self.signature.argument_type.fill_defaults(argument);

                let _data = std::sync::Arc::make_mut(&mut _argument.data);
                $($(let $arg: $ty = _data.members.remove(stringify!($arg))
                        .expect("Argument was not present after argument check.").downcast(stack_trace)?;)*)?

                    (self.function)(runtime, stack_trace, stack, database $(, $($arg),*)?)
            }

            fn name(&self) -> &str {
                $name
            }

            fn signature(&self) -> &std::sync::Arc<$crate::execution::values::closure::Signature> {
                &self.signature
            }
        }

        BuiltFunction {
            function: move |$log: &mut dyn RuntimeLog, $stack_trace: &mut Vec<SourceReference>, $stack: &mut Stack, $database: &BuiltinCallableDatabase $(, $($arg: $ty),*)?| -> ExpressionResult<$crate::execution::values::Value> { let result: $return_type = $code?; Ok(result.into()) },
            signature: $crate::build_closure_signature!(($($($arg: $ty $(= $default)?),*)*) -> $return_type),
        }
    }};
}

#[macro_export]
macro_rules! build_function {
    ($database:ident,
        forward = $ident:ident, $name:literal, ($log:ident: &mut dyn RuntimeLog, $stack_trace:ident: &mut Vec<SourceReference>, $stack:ident: &mut Stack, $call_database:ident: &BuiltinCallableDatabase $(, $($arg:ident: $ty:path $(= $default:expr)?),+)?) -> $return_type:ty $code:block
    ) => {{
        let callable = $crate::build_function_callable!($name ($log: &mut dyn RuntimeLog, $stack_trace: &mut Vec<SourceReference>, $stack: &mut Stack, $call_database: &BuiltinCallableDatabase $(, $($arg: $ty $(= $default)?),+)?) -> $return_type $code);

        $database.register::<$ident>(Box::new(callable))
    }};
    ($database:ident,
        forward = $forward_ident:ident, $forward_name:literal, ($forward_log:ident: &mut dyn RuntimeLog, $forward_stack_trace:ident: &mut Vec<SourceReference>, $forward_stack:ident: &mut Stack, $forward_database:ident: &BuiltinCallableDatabase $(, $($forward_arg:ident: $forward_ty:path $(= $forward_default:expr)?),+)?) -> $forward_return_type:ty $forward_code:block,
        reverse = $reverse_ident:ident, $reverse_name:literal, ($reverse_log:ident: &mut dyn RuntimeLog, $reverse_stack_trace:ident: &mut Vec<SourceReference>, $reverse_stack:ident: &mut Stack, $reverse_database:ident: &BuiltinCallableDatabase $(, $($reverse_arg:ident: $reverse_ty:path $(= $reverse_default:expr)?),+)?) -> $reverse_return_type:ty $reverse_code:block
    ) => {{
        let forward = $crate::build_function_callable!($forward_name ($forward_log: &mut dyn RuntimeLog, $forward_stack_trace: &mut Vec<SourceReference>, $forward_stack: &mut Stack, $forward_database: &BuiltinCallableDatabase $(, $($forward_arg: $forward_ty $(= $forward_default)?),+)?) -> $forward_return_type $forward_code);
        let reverse = $crate::build_function_callable!($reverse_name ($reverse_log: &mut dyn RuntimeLog, $reverse_stack_trace: &mut Vec<SourceReference>, $reverse_stack: &mut Stack, $reverse_database: &BuiltinCallableDatabase $(, $($reverse_arg: $reverse_ty $(= $reverse_default)?),+)?) -> $reverse_return_type $reverse_code);

        $database.register_with_inverse::<$forward_ident, $reverse_ident>(Box::new(forward), Box::new(reverse))
    }};
}

#[macro_export]
macro_rules! build_method_callable {
    ($name:expr, ($log:ident: &mut dyn RuntimeLog, $stack_trace:ident: &mut Vec<SourceReference>, $stack:ident: &mut Stack, $database:ident: &BuiltinCallableDatabase, $this:ident: $this_type:ty $(, $($arg:ident: $ty:path $(= $default:expr)?),+)?) -> $return_type:ty $code:block) => {{
        struct BuiltFunction<S, F: Fn(&mut dyn RuntimeLog, &mut Vec<SourceReference>, &mut crate::execution::Stack, &BuiltinCallableDatabase, S $(, $($ty),*)?) -> ExpressionResult<crate::execution::values::Value>> {
            function: F,
            signature: std::sync::Arc<crate::execution::values::closure::Signature>,
            name: String,
            _self_type: std::marker::PhantomData<S>
        }

        impl<S, F: Fn(&mut dyn RuntimeLog, &mut Vec<SourceReference>, &mut $crate::execution::Stack, &BuiltinCallableDatabase, S $(, $($ty),*)?) -> ExpressionResult<$crate::execution::values::Value> + Send + Sync> $crate::execution::values::closure::BuiltinCallable for BuiltFunction<S, F>
        where
            S: Send + Sync + Clone + StaticTypeName,
            $crate::execution::values::Value: enum_downcast::AsVariant<S>
        {
            fn call(
                &self,
                runtime: &mut dyn RuntimeLog,
                stack_trace: &mut Vec<SourceReference>,
                stack: &mut $crate::execution::Stack,
                database: &BuiltinCallableDatabase,
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

                    (self.function)(runtime, stack_trace, stack, database, this $(, $($arg),*)?)
            }

            fn name(&self) -> &str {
                &self.name
            }

            fn signature(&self) -> &std::sync::Arc<$crate::execution::values::closure::Signature> {
                &self.signature
            }
        }

        BuiltFunction {
            function: move |$log: &mut dyn RuntimeLog, $stack_trace: &mut Vec<SourceReference>, $stack: &mut Stack, $database: &BuiltinCallableDatabase, $this: $this_type $(, $($arg: $ty),*)?| -> ExpressionResult<$crate::execution::values::Value> { let result: $return_type = $code?; Ok(result.into()) },
            signature: $crate::build_closure_signature!(($($($arg: $ty $(= $default)?),*)*) -> $return_type),
            name: $name.into(),
            _self_type: std::marker::PhantomData
        }
    }};
}

#[macro_export]
macro_rules! build_method {
    ($database:ident,
        forward = $ident:ty, $name:expr, ($log:ident: &mut dyn RuntimeLog, $stack_trace:ident: &mut Vec<SourceReference>, $stack:ident: &mut Stack, $call_database:ident: &BuiltinCallableDatabase, $this:ident: $this_type:ty $(, $($arg:ident: $ty:path $(= $default:expr)?),+)?) -> $return_type:path $code:block
    ) => {{
        let callable = $crate::build_method_callable!($name, ($log: &mut dyn RuntimeLog, $stack_trace: &mut Vec<SourceReference>, $stack: &mut Stack, $call_database: &BuiltinCallableDatabase, $this: $this_type $(, $($arg: $ty $(= $default)?),+)?) -> $return_type $code);

        $database.register::<$ident>(Box::new(callable))
    }};
    ($database:ident,
        forward = $forward_ident:ty, $forward_name:expr, ($forward_log:ident: &mut dyn RuntimeLog, $forward_stack_trace:ident: &mut Vec<SourceReference>, $forward_stack:ident: &mut Stack, $forward_call_database:ident: &BuiltinCallableDatabase, $forward_this:ident: $forward_this_type:ty $(, $($forward_arg:ident: $forward_ty:path $(= $forward_default:expr)?),+)?) -> $forward_return_type:path $forward_code:block,
        reverse = $reverse_ident:ty, $reverse_name:expr, ($reverse_log:ident: &mut dyn RuntimeLog, $reverse_stack_trace:ident: &mut Vec<SourceReference>, $reverse_stack:ident: &mut Stack, $reverse_call_database:ident: &BuiltinCallableDatabase, $reverse_this:ident: $reverse_this_type:ty $(, $($reverse_arg:ident: $reverse_ty:path $(= $reverse_default:expr)?),+)?) -> $reverse_return_type:path $reverse_code:block
    ) => {{
        let forward = $crate::build_method_callable!($forward_name, ($forward_log: &mut dyn RuntimeLog, $forward_stack_trace: &mut Vec<SourceReference>, $forward_stack: &mut Stack, $forward_call_database: &BuiltinCallableDatabase, $forward_this: $forward_this_type $(, $($forward_arg: $forward_ty $(= $forward_default)?),+)?) -> $forward_return_type $forward_code);
        let reverse = $crate::build_method_callable!($reverse_name, ($reverse_log: &mut dyn RuntimeLog, $reverse_stack_trace: &mut Vec<SourceReference>, $reverse_stack: &mut Stack, $reverse_call_database: &BuiltinCallableDatabase, $reverse_this: $reverse_this_type $(, $($reverse_arg: $reverse_ty $(= $reverse_default)?),+)?) -> $reverse_return_type $reverse_code);

        $database.register_with_inverse::<$forward_ident, $reverse_ident>(Box::new(forward), Box::new(reverse))
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
    fn get_type(&self, callable_database: &BuiltinCallableDatabase) -> ValueType {
        ValueType::Closure(callable_database.get_forward(self.0).signature().clone())
    }

    fn call(
        &self,
        log: &mut dyn RuntimeLog,
        stack_trace: &mut Vec<SourceReference>,
        stack: &mut Stack,
        callable_database: &BuiltinCallableDatabase,
        argument: Dictionary,
    ) -> ExpressionResult<Value> {
        callable_database.get_forward(self.0).call(
            log,
            stack_trace,
            stack,
            callable_database,
            argument,
        )
    }

    // TODO the user should be able to get the inverse function.
    fn get_attribute(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        callable_database: &BuiltinCallableDatabase,
        attribute: &str,
    ) -> ExpressionResult<Value> {
        match attribute {
            "inverse" => {
                if let Some(inverse) = callable_database.get_inverse(self.0) {
                    Ok(BuiltinFunction(inverse).into())
                } else {
                    todo!()
                }
            }
            _ => Err(MissingAttributeError {
                name: attribute.into(),
            }
            .to_error(stack_trace)),
        }
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
            "let object = (value = 5u, method = () -> std.types.UInt: self.value;); in object::method()",
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
        let mut database = BuiltinCallableDatabase::default();

        struct TestFunction;
        build_function!(
            database,
            forward = TestFunction, "test_function", (
                _log: &mut dyn RuntimeLog,
                _stack_trace: &mut Vec<SourceReference>,
                _stack: &mut Stack,
                _database: &BuiltinCallableDatabase
            ) -> UnsignedInteger            {
                Ok(values::UnsignedInteger::from(846))
            }
        );

        use crate::execution::standard_environment::build_prelude;

        let root = crate::compile::full_compile("test_function()");
        let prelude = build_prelude(&database);
        let mut stack = Stack::new(prelude);
        stack.insert_value(
            "test_function",
            BuiltinFunction::new::<TestFunction>().into(),
        );

        let product = execute_expression(
            &mut Vec::new(),
            &mut Vec::new(),
            &mut stack,
            &database,
            &root,
        )
        .unwrap();

        assert_eq!(product, values::UnsignedInteger::from(846).into());
    }

    #[test]
    fn builtin_function_with_args() {
        let mut database = BuiltinCallableDatabase::default();

        struct TestFunction;
        build_function!(
            database,
            forward = TestFunction, "test_function", (
                _log: &mut dyn RuntimeLog,
                _stack_trace: &mut Vec<SourceReference>,
                _stack: &mut Stack,
                _database: &BuiltinCallableDatabase,
                a: UnsignedInteger,
                b: UnsignedInteger
            ) -> UnsignedInteger {
                Ok(values::UnsignedInteger::from(a.0 + b.0))
            }
        );

        use crate::execution::standard_environment::build_prelude;

        let root = crate::compile::full_compile("test_function(a = 1u, b = 2u)");
        let prelude = build_prelude(&database);
        let mut stack = Stack::new(prelude);
        stack.insert_value(
            "test_function",
            BuiltinFunction::new::<TestFunction>().into(),
        );

        let product = execute_expression(
            &mut Vec::new(),
            &mut Vec::new(),
            &mut stack,
            &database,
            &root,
        )
        .unwrap();

        assert_eq!(product, values::UnsignedInteger::from(3).into());
    }

    #[test]
    fn builtin_function_with_inverse() {
        let mut database = BuiltinCallableDatabase::default();

        struct TestFunction;
        struct TestInverse;
        build_function!(
            database,
            forward = TestFunction, "test_function", (
                _log: &mut dyn RuntimeLog,
                _stack_trace: &mut Vec<SourceReference>,
                _stack: &mut Stack,
                _database: &BuiltinCallableDatabase,
                input: UnsignedInteger
            ) -> UnsignedInteger {
                Ok(values::UnsignedInteger::from(input.0 + 1))
            },
            reverse = TestInverse, "test_inverse", (
                _log: &mut dyn RuntimeLog,
                _stack_trace: &mut Vec<SourceReference>,
                _stack: &mut Stack,
                _database: &BuiltinCallableDatabase,
                input: UnsignedInteger
            ) -> UnsignedInteger {
                Ok(values::UnsignedInteger::from(input.0 - 1))
            }
        );

        use crate::execution::standard_environment::build_prelude;

        let root = crate::compile::full_compile("test_inverse(input = test_function(input = 1u))");
        let prelude = build_prelude(&database);
        let mut stack = Stack::new(prelude);
        stack.insert_value(
            "test_function",
            BuiltinFunction::new::<TestFunction>().into(),
        );
        stack.insert_value("test_inverse", BuiltinFunction::new::<TestInverse>().into());

        let product = execute_expression(
            &mut Vec::new(),
            &mut Vec::new(),
            &mut stack,
            &database,
            &root,
        )
        .unwrap();

        assert_eq!(product, values::UnsignedInteger::from(1).into());
    }

    #[test]
    fn builtin_function_with_default_value() {
        let mut database = BuiltinCallableDatabase::default();
        struct TestFunction;
        build_function!(
            database,
            forward = TestFunction, "test_function", (
                _log: &mut dyn RuntimeLog,
                _stack_trace: &mut Vec<SourceReference>,
                _stack: &mut Stack,
                _database: &BuiltinCallableDatabase,
                a: UnsignedInteger,
                b: UnsignedInteger = UnsignedInteger::from(2).into()
            ) -> UnsignedInteger {
                Ok(values::UnsignedInteger::from(a.0 + b.0))
            }
        );

        use crate::execution::standard_environment::build_prelude;

        let root = crate::compile::full_compile("test_function(a = 1u)");
        let prelude = build_prelude(&database);
        let mut stack = Stack::new(prelude);
        stack.insert_value(
            "test_function",
            BuiltinFunction::new::<TestFunction>().into(),
        );

        let product = execute_expression(
            &mut Vec::new(),
            &mut Vec::new(),
            &mut stack,
            &database,
            &root,
        )
        .unwrap();

        assert_eq!(product, values::UnsignedInteger::from(3).into());
    }

    #[test]
    fn builtin_function_captured_value() {
        let mut database = BuiltinCallableDatabase::default();
        let b = 2;

        struct TestFunction;
        build_function!(
            database,
            forward = TestFunction, "test_function", (
                _log: &mut dyn RuntimeLog,
                _stack_trace: &mut Vec<SourceReference>,
                _stack: &mut Stack,
                _database: &BuiltinCallableDatabase,
                a: UnsignedInteger
            ) -> UnsignedInteger {
                Ok(values::UnsignedInteger::from(a.0 + b))
            }
        );

        use crate::execution::standard_environment::build_prelude;

        let root = crate::compile::full_compile("test_function(a = 1u)");
        let prelude = build_prelude(&database);
        let mut stack = Stack::new(prelude);
        stack.insert_value(
            "test_function",
            BuiltinFunction::new::<TestFunction>().into(),
        );

        let product = execute_expression(
            &mut Vec::new(),
            &mut Vec::new(),
            &mut stack,
            &database,
            &root,
        )
        .unwrap();

        assert_eq!(product, values::UnsignedInteger::from(3).into());
    }

    #[test]
    fn builtin_method() {
        let mut database = BuiltinCallableDatabase::default();
        struct TestMethod;
        build_method!(
            database,
            forward = TestMethod, "test_method", (log: &mut dyn RuntimeLog, stack_trace: &mut Vec<SourceReference>, _stack: &mut Stack, database: &BuiltinCallableDatabase, this: Dictionary) -> Value {
                this.get_attribute(log, stack_trace, database, "value")
            }
        );

        use crate::execution::standard_environment::build_prelude;

        let root = crate::compile::full_compile(
            "let object = (value = 5u, test_method = provided_test_method); in object::test_method()",
        );
        let prelude = build_prelude(&database);
        let mut stack = Stack::new(prelude);
        stack.insert_value(
            "provided_test_method",
            BuiltinFunction::new::<TestMethod>().into(),
        );

        let product = execute_expression(
            &mut Vec::new(),
            &mut Vec::new(),
            &mut stack,
            &database,
            &root,
        )
        .unwrap();

        assert_eq!(product, values::UnsignedInteger::from(5).into());
    }

    #[test]
    fn builtin_method_with_argument() {
        let mut database = BuiltinCallableDatabase::default();
        struct TestMethod;

        build_method!(
            database,
            forward = TestMethod, "test_method", (
                log: &mut dyn RuntimeLog,
                stack_trace: &mut Vec<SourceReference>,
                _stack: &mut Stack,
                database: &BuiltinCallableDatabase,
                this: Dictionary,
                to_add: UnsignedInteger
            ) -> UnsignedInteger {
                let value: UnsignedInteger = this.get_attribute(log, stack_trace, database, "value")?.downcast(stack_trace)?;

                Ok(values::UnsignedInteger::from(value.0 + to_add.0))
            }
        );

        use crate::execution::standard_environment::build_prelude;

        let root = crate::compile::full_compile(
            "let object = (value = 5u, test_method = provided_test_method); in object::test_method(to_add = 10u)",
        );

        let prelude = build_prelude(&database);
        let mut stack = Stack::new(prelude);
        stack.insert_value(
            "provided_test_method",
            BuiltinFunction::new::<TestMethod>().into(),
        );

        let product = execute_expression(
            &mut Vec::new(),
            &mut Vec::new(),
            &mut stack,
            &database,
            &root,
        )
        .unwrap();

        assert_eq!(product, values::UnsignedInteger::from(15).into());
    }

    #[test]
    fn builtin_method_with_inverse() {
        let mut database = BuiltinCallableDatabase::default();
        struct TestMethod;
        struct InverseMethod;
        build_method!(
            database,
            forward = TestMethod, "test_method", (
                log: &mut dyn RuntimeLog,
                stack_trace: &mut Vec<SourceReference>,
                _stack: &mut Stack,
                database: &BuiltinCallableDatabase,
                this: Dictionary,
                input: UnsignedInteger
            ) -> UnsignedInteger {
                let value: UnsignedInteger = this.get_attribute(log, stack_trace, database, "value")?.downcast(stack_trace)?;

                Ok(UnsignedInteger::from(input.0 + value.0))
            },
            reverse = InverseMethod, "reverse_method", (
                log: &mut dyn RuntimeLog,
                stack_trace: &mut Vec<SourceReference>,
                _stack: &mut Stack,
                database: &BuiltinCallableDatabase,
                this: Dictionary,
                input: UnsignedInteger
            ) -> UnsignedInteger {
                let value: UnsignedInteger = this.get_attribute(log, stack_trace, database, "value")?.downcast(stack_trace)?;

                Ok(UnsignedInteger::from(input.0 - value.0))
            }
        );

        use crate::execution::standard_environment::build_prelude;

        let root = crate::compile::full_compile(
            "let object = (value = 5u, test_method = provided_test_method, inverse_method = provided_inverse_method); in object::inverse_method(input = object::test_method(input = 2u))",
        );
        let prelude = build_prelude(&database);
        let mut stack = Stack::new(prelude);
        stack.insert_value(
            "provided_test_method",
            BuiltinFunction::new::<TestMethod>().into(),
        );
        stack.insert_value(
            "provided_inverse_method",
            BuiltinFunction::new::<InverseMethod>().into(),
        );

        let product = execute_expression(
            &mut Vec::new(),
            &mut Vec::new(),
            &mut stack,
            &database,
            &root,
        )
        .unwrap();

        assert_eq!(product, values::UnsignedInteger::from(2).into());
    }
}
