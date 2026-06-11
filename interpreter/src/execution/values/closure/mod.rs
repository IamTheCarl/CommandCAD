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

use super::{Object, StaticType, StaticTypeName, StructDefinition, ValueType};
pub mod solve;
pub use solve::{differentiate, integrate, solve_for};
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
         super::manifold_mesh::register_methods_and_functions(&mut database);
        crate::execution::register_methods_and_functions(&mut database);
        super::iterators::register_methods(&mut database);
        super::transform::register_methods(&mut database);
        super::polygon::register_methods_and_functions(&mut database);
        crate::execution::export::register_methods_and_functions(&mut database);
        register_log_functions(&mut database);
        register_closure_methods(&mut database);

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
            .insert(TypeId::of::<T>(), CallableStorage { callable, inverse_type_id: None })
        {
            panic!(
                "Duplicate bultin function tag: {:?}, originally registered with function `{}`",
                TypeId::of::<T>(),
                old_callable.name()
            );
        }
    }

    /// Set the inverse TypeId for a registered callable.
    pub fn set_inverse<T: 'static>(&mut self, inverse: TypeId) {
        let forward_id = TypeId::of::<T>();
        if let Some(storage) = self.callables.get_mut(&forward_id) {
            storage.inverse_type_id = Some(inverse);
        }
    }

    /// Get the inverse TypeId for a given TypeId.
    pub fn get_inverse(&self, id: TypeId) -> Option<TypeId> {
        self.callables.get(&id).and_then(|s| s.inverse_type_id)
    }

    pub fn get_callable_id(&self, name: &str) -> Option<TypeId> {
        self.names.get(name).copied()
    }

    pub fn get_method_name(&self, id: TypeId) -> Option<ImString> {
        self.names
            .iter()
            .find(|(_, tid)| **tid == id)
            .map(|(name, _)| ImString::from(name.as_str()))
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
    inverse_type_id: Option<TypeId>,
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
pub struct UserClosureInternals {
    pub signature: Arc<Signature>,
    pub captured_values: IndexMap<ArgumentName, Value>,
    pub expression: Arc<AstNode<Expression>>,
    pub formula: Option<String>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct UserClosure {
    pub data: Arc<UserClosureInternals>,
}

impl UserClosure {
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
                formula: None,
            }),
        })
    }
}

impl Object for UserClosure {
    fn get_type(&self, _context: &ExecutionContext) -> ValueType {
        ValueType::Closure(self.data.signature.clone())
    }

    fn get_attribute(&self, context: &ExecutionContext, attribute: &str) -> ExecutionResult<Value> {
        match attribute {
            "inverse" => Ok(BuiltinFunction::new::<methods::Inverse>().into()),
            "derive" => Ok(BuiltinFunction::new::<methods::Derive>().into()),
            "integrate" => Ok(BuiltinFunction::new::<methods::Integrate>().into()),
            _ => Err(super::MissingAttributeError {
                name: attribute.into(),
            }
            .to_error(context)),
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
            let result = execute_expression(context, &self.data.expression).map_err(|e| {
                if let Some(formula) = &self.data.formula {
                    let msg = format!("Inverse body: {}\n{}", formula, e.ty);
                    crate::execution::errors::Error {
                        ty: Box::new(crate::execution::errors::StringError(msg)),
                        trace: e.trace,
                        failure_chain: e.failure_chain,
                    }
                } else {
                    e
                }
            })?;

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

mod methods {
    pub struct Inverse;
    pub struct Derive;
    pub struct Integrate;
}

/// The `Inverse` callable is invoked on a `UserClosure` via method syntax.
/// We cannot use the `build_method!` macro here because that macro is designed
/// for dictionary methods where `$this: Dictionary` is passed as the first
/// parameter. `inverse` operates on a `UserClosure` and must look up `self`
/// from the call stack rather than receiving it as an argument. The complex
/// logic (solve-for, synthetic AST node creation, captured-value forwarding)
/// also doesn't fit the macro's simple function-call pattern.
impl BuiltinCallable for methods::Inverse {
    fn call(&self, context: &ExecutionContext, argument: Dictionary) -> ExecutionResult<Value> {
        let wanted_output: crate::execution::values::IString = argument
            .get("wanted_output")
            .ok_or_else(|| super::MissingAttributeError {
                name: "wanted_output".into(),
            }
            .to_error(context))?
            .clone()
            .downcast::<crate::execution::values::IString>(context)?;

        let result_name: crate::execution::values::IString = argument
            .get("result_name")
            .map(|v| v.clone().downcast::<crate::execution::values::IString>(context))
            .transpose()?
            .unwrap_or_else(|| crate::execution::values::IString::from("original_result"));

        // Find the closure in the stack — the caller's "self"
        let closure = context
            .stack
            .get_variable(
                context.stack_trace,
                vec![],
                LocatedStr {
                    location: context.stack_trace.bottom().clone(),
                    string: "self",
                },
            )
            .map_err(|_| super::MissingAttributeError {
                name: "self".into(),
            }
            .to_error(context))?
            .clone()
            .downcast::<UserClosure>(context)
            .map_err(|_| super::DowncastError {
                expected: "UserClosure".into(),
                got: "Value".into(),
            }
            .to_error(context))?;

    // Infer the input parameter type from the body expression (captures actual dimension info).
        // When solving f(x) = y for x, the inverse is g(y) = x, so g takes what f returned.
        let target_param_type = Some(solve::ast_return_type(&closure.data.expression));

        // The return type of the inverse closure should be the type of the target parameter.
        // When solving f(x) = y for x, the inverse is g(y) = x, so the return type is x's type.
        let return_type_for_target = closure
            .data
            .signature
            .argument_type
            .members
            .iter()
            .find_map(|(name, member)| {
                if let ArgumentName::Named(s) = name {
                    if s.as_str() == wanted_output.0.as_str() {
                        return Some(member.ty.clone());
                    }
                }
                None
            });

        // Build a map of parameter names to their types from the original closure.
        // This is used to give the inverse closure's captured parameters the correct types.
        let param_types: IndexMap<ImString, crate::execution::values::ValueType> = closure
            .data
            .signature
            .argument_type
            .members
            .iter()
            .filter_map(|(name, member)| {
                if let ArgumentName::Named(s) = name {
                    Some((s.clone(), member.ty.clone()))
                } else {
                    None
                }
            })
            .collect();

       // Solve for the target variable
      let result = solve::solve_for(
            context,
            &closure.data.expression,
            &closure
                .data
                .signature
                .argument_type
                .members
                .keys()
                .filter_map(|name| match name {
                    ArgumentName::Named(s) => Some(s.clone()),
                    _ => None,
                })
                .collect::<Vec<_>>(),
            &closure.data.captured_values,
            &wanted_output.0,
            result_name.0.clone(),
            target_param_type,
            return_type_for_target,
            param_types,
        )?;

        let formula = result.body.to_string();
        let inverse_closure = result.into_closure(context, &closure).map_err(|e| {
            let msg = format!("Inverse body: {}\n{}", formula, e.ty);
            crate::execution::errors::Error {
                ty: Box::new(crate::execution::errors::StringError(msg)),
                trace: e.trace,
                failure_chain: e.failure_chain,
            }
        })?;

        Ok(inverse_closure.into())
    }

    fn name(&self) -> &str {
        "UserClosure::inverse"
    }

    fn signature(&self) -> &Arc<Signature> {
        static SIGNATURE: OnceLock<Arc<Signature>> = OnceLock::new();
        SIGNATURE.get_or_init(|| {
            Arc::new(Signature {
                argument_type: crate::build_struct_definition!(
                    variadic: false,
                    (wanted_output: crate::execution::values::IString, result_name: crate::execution::values::IString = crate::execution::values::IString::from("original_result").into())
                ),
                return_type: ValueType::Closure(Arc::new(Signature {
                    argument_type: crate::build_struct_definition!(
                        variadic: false,
                        (_dummy: crate::execution::values::IString)
                    ),
                    return_type: ValueType::Scalar(None),
                })),
            })
        })
    }

    fn scope_type(&self) -> ScopeType {
        ScopeType::Isolated
    }
}

impl BuiltinCallable for methods::Derive {
    fn call(&self, context: &ExecutionContext, argument: Dictionary) -> ExecutionResult<Value> {
        let wanted_output: crate::execution::values::IString = argument
            .get("wanted_output")
            .ok_or_else(|| super::MissingAttributeError {
                name: "wanted_output".into(),
            }
            .to_error(context))?
            .clone()
            .downcast::<crate::execution::values::IString>(context)?;

        let _result_name: crate::execution::values::IString = argument
            .get("result_name")
            .map(|v| v.clone().downcast::<crate::execution::values::IString>(context))
            .transpose()?
            .unwrap_or_else(|| crate::execution::values::IString::from("original_result"));

        let closure = context
            .stack
            .get_variable(
                context.stack_trace,
                vec![],
                crate::execution::logging::LocatedStr {
                    location: context.stack_trace.bottom().clone(),
                    string: "self",
                },
            )
            .map_err(|_| super::MissingAttributeError {
                name: "self".into(),
            }
            .to_error(context))?
            .clone()
            .downcast::<UserClosure>(context)
            .map_err(|_| super::DowncastError {
                expected: "UserClosure".into(),
                got: "Value".into(),
            }
            .to_error(context))?;

       let target_param_type = closure
            .data
            .signature
            .argument_type
            .members
            .iter()
            .find_map(|(name, member)| {
                if let ArgumentName::Named(s) = name {
                    if s.as_str() == wanted_output.0.as_str() {
                        return Some(member.ty.clone());
                    }
                }
                None
            });

        let param_types: IndexMap<ImString, crate::execution::values::ValueType> = closure
            .data
            .signature
            .argument_type
            .members
            .iter()
            .filter_map(|(name, member)| {
                if let ArgumentName::Named(s) = name {
                    Some((s.clone(), member.ty.clone()))
                } else {
                    None
                }
            })
            .collect();

        let sym_body = solve::expression_to_sym_expr(&closure.data.expression, context)?;
        let sym_body = solve::simplify_sym_expr(&sym_body);
        let derivative = solve::differentiate(&sym_body, &wanted_output.0);

        let formula = derivative.to_string();
        let derived_return_type = closure.data.signature.return_type.clone();
        let result = solve::SolveResult {
            body: derivative,
            captured: IndexMap::new(),
            result_name: wanted_output.0.clone(),
            target_param_name: wanted_output.0.clone(),
            target_param_type,
            return_type: Some(derived_return_type),
            param_types,
        };

        let derived_closure = result.into_closure(context, &closure).map_err(|e| {
            let msg = format!("Derivative body: {}\n{}", formula, e.ty);
            crate::execution::errors::Error {
                ty: Box::new(crate::execution::errors::StringError(msg)),
                trace: e.trace,
                failure_chain: e.failure_chain,
            }
        })?;

        Ok(derived_closure.into())
    }

    fn name(&self) -> &str {
        "UserClosure::derive"
    }

    fn signature(&self) -> &Arc<Signature> {
        static SIGNATURE: OnceLock<Arc<Signature>> = OnceLock::new();
        SIGNATURE.get_or_init(|| {
            Arc::new(Signature {
                argument_type: crate::build_struct_definition!(
                    variadic: false,
                    (wanted_output: crate::execution::values::IString, result_name: crate::execution::values::IString = crate::execution::values::IString::from("original_result").into())
                ),
                return_type: ValueType::Closure(Arc::new(Signature {
                    argument_type: crate::build_struct_definition!(
                        variadic: false,
                        (_dummy: crate::execution::values::IString)
                    ),
                    return_type: ValueType::Any,
                })),
            })
        })
    }

    fn scope_type(&self) -> ScopeType {
        ScopeType::Isolated
    }
}

impl BuiltinCallable for methods::Integrate {
    fn call(&self, context: &ExecutionContext, argument: Dictionary) -> ExecutionResult<Value> {
        let wanted_output: crate::execution::values::IString = argument
            .get("wanted_output")
            .ok_or_else(|| super::MissingAttributeError {
                name: "wanted_output".into(),
            }
            .to_error(context))?
            .clone()
            .downcast::<crate::execution::values::IString>(context)?;

        let max_recursion: i64 = argument
            .get("max_recursion")
            .map(|v| v.clone().downcast::<crate::execution::values::SignedInteger>(context))
            .transpose()?
            .map(|si| si.0)
            .unwrap_or(5);

        let max_recursion = max_recursion as usize;

        let _result_name: crate::execution::values::IString = argument
            .get("result_name")
            .map(|v| v.clone().downcast::<crate::execution::values::IString>(context))
            .transpose()?
            .unwrap_or_else(|| crate::execution::values::IString::from("original_result"));

        let closure = context
            .stack
            .get_variable(
                context.stack_trace,
                vec![],
                crate::execution::logging::LocatedStr {
                    location: context.stack_trace.bottom().clone(),
                    string: "self",
                },
            )
            .map_err(|_| super::MissingAttributeError {
                name: "self".into(),
            }
            .to_error(context))?
            .clone()
            .downcast::<UserClosure>(context)
            .map_err(|_| super::DowncastError {
                expected: "UserClosure".into(),
                got: "Value".into(),
            }
            .to_error(context))?;

        let target_param_type = closure
            .data
            .signature
            .argument_type
            .members
            .iter()
            .find_map(|(name, member)| {
                if let ArgumentName::Named(s) = name {
                    if s.as_str() == wanted_output.0.as_str() {
                        return Some(member.ty.clone());
                    }
                }
                None
            });

        let param_types: IndexMap<ImString, crate::execution::values::ValueType> = closure
            .data
            .signature
            .argument_type
            .members
            .iter()
            .filter_map(|(name, member)| {
                if let ArgumentName::Named(s) = name {
                    Some((s.clone(), member.ty.clone()))
                } else {
                    None
                }
            })
            .collect();

        let sym_body = solve::expression_to_sym_expr(&closure.data.expression, context)?;
        let sym_body = solve::simplify_sym_expr(&sym_body);

        let integral = solve::integrate(&sym_body, &wanted_output.0, max_recursion).map_err(|e| {
            crate::execution::errors::Error {
                ty: Box::new(crate::execution::errors::StringError(e)),
                trace: vec![context.stack_trace.bottom().clone()],
                failure_chain: vec![],
            }
        })?;

        let formula = integral.to_string();
        let integrated_return_type = closure.data.signature.return_type.clone();
        let result = solve::SolveResult {
            body: integral,
            captured: IndexMap::new(),
            result_name: wanted_output.0.clone(),
            target_param_name: wanted_output.0.clone(),
            target_param_type,
            return_type: Some(integrated_return_type),
            param_types,
        };

        let integrated_closure = result.into_closure(context, &closure).map_err(|e| {
            let msg = format!("Integral body: {}\n{}", formula, e.ty);
            crate::execution::errors::Error {
                ty: Box::new(crate::execution::errors::StringError(msg)),
                trace: e.trace,
                failure_chain: e.failure_chain,
            }
        })?;

        Ok(integrated_closure.into())
    }

    fn name(&self) -> &str {
        "UserClosure::integrate"
    }

    fn signature(&self) -> &Arc<Signature> {
        static SIGNATURE: OnceLock<Arc<Signature>> = OnceLock::new();
        SIGNATURE.get_or_init(|| {
            Arc::new(Signature {
                argument_type: crate::build_struct_definition!(
                    variadic: false,
                    (wanted_output: crate::execution::values::IString, max_recursion: crate::execution::values::SignedInteger = crate::execution::values::SignedInteger::from(5).into(), result_name: crate::execution::values::IString = crate::execution::values::IString::from("original_result").into())
                ),
                return_type: ValueType::Closure(Arc::new(Signature {
                    argument_type: crate::build_struct_definition!(
                        variadic: false,
                        (_dummy: crate::execution::values::IString)
                    ),
                    return_type: ValueType::Any,
                })),
            })
        })
    }

    fn scope_type(&self) -> ScopeType {
        ScopeType::Isolated
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
    ($database:ident,
        $ident:ty, $name:expr, ($context:ident: &ExecutionContext, $this:ident: $this_type:ty $(, $($arg:ident: $ty:path $(= $default:expr)?),+)?) -> $return_type:path $code:block, $inverse:expr
    ) => {{
        let callable = $crate::build_method_callable!($name,
            ($context: &ExecutionContext, $this: $this_type $(, $($arg: $ty $(= $default)?),+)?) -> $return_type $code
        );

        $database.register::<$ident>(Box::new(callable));
        $database.set_inverse::<$ident>($inverse);
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

fn register_closure_methods(database: &mut BuiltinCallableDatabase) {
    database.register::<methods::Inverse>(Box::new(methods::Inverse));
    database.register::<methods::Derive>(Box::new(methods::Derive));
    database.register::<methods::Integrate>(Box::new(methods::Integrate));
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
                    expression,
                    formula: None,
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

    #[test]
    fn inverse_x_squared() {
        let result = test_run(
            r#"let f = (x: std.scalar.Length) -> std.scalar.Length: x*x; in f::inverse(wanted_output = "x")"#,
        );
        assert!(result.is_ok());
        assert!(result.unwrap().as_userclosure().is_some());
    }

    #[test]
    fn inverse_x_doubled() {
        let result = test_run(
            r#"let f = (x: std.scalar.Length) -> std.scalar.Length: x+x; in f::inverse(wanted_output = "x")"#,
        );
        assert!(result.is_ok());
        assert!(result.unwrap().as_userclosure().is_some());
    }

    #[test]
    fn inverse_x_times_two() {
        let result = test_run(
            r#"let f = (x: std.scalar.Length) -> std.scalar.Length: x*2.0; in f::inverse(wanted_output = "x")"#,
        );
        assert!(result.is_ok());
        assert!(result.unwrap().as_userclosure().is_some());
    }

    #[test]
    fn inverse_x_minus_x_zero() {
        let result = test_run(
            r#"let f = (x: std.scalar.Length) -> std.scalar.Length: x-x; in f::inverse(wanted_output = "x")"#,
        );
        assert!(result.is_ok());
        assert!(result.unwrap().as_userclosure().is_some());
    }

    #[test]
    fn inverse_x_minus_x_plus_5() {
        let result = test_run(
            r#"let f = (x: std.scalar.Length) -> std.scalar.Length: x-x+5.0; in f::inverse(wanted_output = "x")"#,
        );
        assert!(result.is_err());
        let err_str = format!("{}", result.unwrap_err().ty);
        assert!(err_str.contains("no solution"));
    }

    #[test]
    fn inverse_sin() {
        let result = test_run(
            r#"let f = (x: std.scalar.Length) -> std.scalar.Length: x::sin(); in f::inverse(wanted_output = "x")"#,
        );
        assert!(result.is_ok());
        assert!(result.unwrap().as_userclosure().is_some());
    }

    #[test]
    fn inverse_sin_plus_one() {
        let result = test_run(
            r#"let f = (x: std.scalar.Length) -> std.scalar.Length: x::sin()+1.0; in f::inverse(wanted_output = "x")"#,
        );
        assert!(result.is_ok());
        assert!(result.unwrap().as_userclosure().is_some());
    }

    #[test]
    fn inverse_sinh() {
        let result = test_run(
            r#"let f = (x: std.scalar.Length) -> std.scalar.Length: x::sinh(); in f::inverse(wanted_output = "x")"#,
        );
        assert!(result.is_ok());
        assert!(result.unwrap().as_userclosure().is_some());
    }

    #[test]
    fn inverse_pow_error() {
        // pow(x, y) for x where y is not a constant is complex
        // This tests that non-invertible operations are handled
        let result = test_run(
            r#"let f = (x: std.scalar.Length, y: std.scalar.Length) -> std.scalar.Length: x::pow(y); in f::inverse(wanted_output = "x")"#,
        );
        assert!(result.is_err());
        let err_str = format!("{}", result.unwrap_err().ty);
        assert!(err_str.contains("not invertible"));
    }

    #[test]
    fn inverse_quadratic() {
        let result = test_run(
            r#"let f = (x: std.scalar.Length, y: std.scalar.Length) -> std.scalar.Length: x*x+y*y; in f::inverse(wanted_output = "x")"#,
        );
        assert!(result.is_ok());
        assert!(result.unwrap().as_userclosure().is_some());
    }

    #[test]
    fn inverse_dimension_mismatch_input_type() {
        // Body uses Angle + angle — should succeed at compile, but the inverse
        // should have Angle type for its wanted_output parameter
        let result = test_run(
            r#"let f = (x: std.scalar.Angle) -> std.scalar.Angle: x + 5deg; in f::inverse(wanted_output = "x")"#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn inverse_dimension_mismatch_caller_provides_length() {
        // Inverse expects Angle but caller provides Length — should fail
        let result = test_run(
            r#"let f = (x: std.scalar.Angle) -> std.scalar.Angle: x + 5m; in f::inverse(wanted_output = "x")(5m)"#,
        );
        assert!(result.is_err());
    }

    #[test]
    fn inverse_dimension_mismatch_caller_provides_time() {
        // Inverse expects Length but caller provides Time — should fail
        let result = test_run(
            r#"let f = (x: std.scalar.Length) -> std.scalar.Length: x + 5m; in f::inverse(wanted_output = "x")(5s)"#,
        );
        assert!(result.is_err());
    }

    #[test]
    fn inverse_dimension_mismatch_caller_provides_angle() {
        // Inverse expects Length but caller provides Angle — should fail
        let result = test_run(
            r#"let f = (x: std.scalar.Length) -> std.scalar.Length: x + 5m; in f::inverse(wanted_output = "x")(90deg)"#,
        );
        assert!(result.is_err());
    }

    #[test]
    fn inverse_dimension_correct_length() {
        // Same dimension — should succeed
        let result = test_run(
            r#"let f = (x: std.scalar.Length) -> std.scalar.Length: x + 5m; in f::inverse(wanted_output = "x")(5m)"#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn inverse_dimension_correct_angle() {
        // Same dimension — should succeed
        let result = test_run(
            r#"let f = (x: std.scalar.Angle) -> std.scalar.Angle: x + 5deg; in f::inverse(wanted_output = "x")(90deg)"#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn inverse_dimension_no_dimension_any_scalar() {
        // Scalar(None) with dimensionless value — should succeed
        let result = test_run(
            r#"let f = (x: std.scalar.Number) -> std.scalar.Number: x + 5.0; in f::inverse(wanted_output = "x")(5.0)"#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn inverse_with_two_params_solves_for_first() {
        // Solving x*x + y*y = result for x, y remains as a captured parameter
        let result = test_run(
            r#"let f = (x: std.scalar.Length, y: std.scalar.Length) -> std.scalar.Length: x*x + y*y; in f::inverse(wanted_output = "x")"#,
        );
        if let Err(e) = result {
            panic!("Two params test failed: {:?}", e);
        }
        assert!(result.is_ok());
        assert!(result.unwrap().as_userclosure().is_some());
    }

    #[test]
    fn inverse_with_two_params_wrong_dimension() {
        // Solving x*x + y*y = result for x, but y has wrong dimension — should fail
        let result = test_run(
            r#"let f = (x: std.scalar.Length, y: std.scalar.Length) -> std.scalar.Length: x*x + y*y; in f::inverse(wanted_output = "x")(y=1s, original_result=2cm)"#,
        );
        assert!(result.is_err());
    }

    #[test]
    fn inverse_x_squared_returns_sqrt() {
        // inverse of x*x = area for x: x = sqrt(area)
        // area = 4'm^2', sqrt(4'm^2') = 2m
        let result = test_run(
            r#"let f = (x: std.scalar.Length) -> std.scalar.Area: x*x; in f::inverse(wanted_output = "x")(original_result=4'm^2')"#,
        );
        if let Err(e) = result {
            panic!("inverse_x_squared_returns_sqrt failed: {:?}", e);
        }
        let val = result.unwrap();
        let scalar = val.as_scalar().expect("Expected Scalar");
        assert!((scalar.value - 2.0).abs() < 1e-10, "Expected 2.0, got {}", scalar.value);
    }

    #[test]
    fn inverse_pow_with_two_params() {
        // inverse of x*x + y*y = area for x: x = sqrt(area - y*y)
        // With y=3m, area=25'm^2' → sqrt(25-9) = 4m
        let result = test_run(
            r#"let f = (x: std.scalar.Length, y: std.scalar.Length) -> std.scalar.Area: x*x + y*y; in f::inverse(wanted_output = "x")(y=3m, original_result=25'm^2')"#,
        );
        if let Err(e) = result {
            panic!("inverse_pow_with_two_params failed: {:?}", e);
        }
        let val = result.unwrap();
        let scalar = val.as_scalar().expect("Expected Scalar");
        assert!((scalar.value - 4.0).abs() < 1e-10, "Expected 4.0, got {}", scalar.value);
    }

    #[test]
    fn inverse_pow_declared_return_type_mismatch() {
        // The declared return type is Length but the body produces Area.
        // The inverse should infer the type from the body (Area), not the declared type (Length).
        let result = test_run(
            r#"let f = (x: std.scalar.Length, y: std.scalar.Length) -> std.scalar.Length: x*x + y*y; in f::inverse(wanted_output = "x")(y=3m, original_result=25'm^2')"#,
        );
        if let Err(e) = result {
            panic!("inverse_pow_declared_return_type_mismatch failed: {:?}", e);
        }
        let val = result.unwrap();
        let scalar = val.as_scalar().expect("Expected Scalar");
        assert!((scalar.value - 4.0).abs() < 1e-10, "Expected 4.0, got {}", scalar.value);
    }

    #[test]
    fn inverse_sqrt_of_sum() {
        // inverse of sqrt(x*x + y*y) = result for x: x = sqrt(result^2 - y^2)
        // With y=3m, result=5m → sqrt(25-9) = 4m
        let result = test_run(
            r#"let f = (x: std.scalar.Length, y: std.scalar.Length) -> std.scalar.Length: (x*x + y*y)::sqrt(); in f::inverse(wanted_output = "x")(y=3m, original_result=5m)"#,
        );
        if let Err(e) = result {
            panic!("inverse_sqrt_of_sum failed: {:?}", e);
        }
        let val = result.unwrap();
        let scalar = val.as_scalar().expect("Expected Scalar");
        assert!((scalar.value - 4.0).abs() < 1e-10, "Expected 4.0, got {}", scalar.value);
    }

    #[test]
    fn inverse_body_inferred_area_type() {
        // When the body is x*x + y*y (which produces Area), the inverse closure's
        // original_result parameter should be typed as Area, not Length.
        // This tests that ast_return_type correctly infers the body's return type.
        let result = test_run(
            r#"let f = (x: std.scalar.Length, y: std.scalar.Length) -> std.scalar.Area: x*x + y*y; in f::inverse(wanted_output = "x")(y=3m, original_result=25'm^2')"#,
        );
        if let Err(e) = result {
            panic!("inverse_body_inferred_area_type failed: {:?}", e);
        }
        let val = result.unwrap();
        let scalar = val.as_scalar().expect("Expected Scalar");
        assert!((scalar.value - 4.0).abs() < 1e-10, "Expected 4.0, got {}", scalar.value);
    }
}
