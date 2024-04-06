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

use crate::script::execution::types::NoneType;

use self::{
    stack::ScopeType,
    types::{Object, OperatorResult, Scalar},
};

use super::{
    logging::{self, RuntimeLog, StandardLog, UnpackValidationWarnings},
    parsing::{self, Block, CallableBlock, MemberVariable},
    Runtime, Span,
};

pub mod types;
use common_data_types::ConversionFactor;
use fj_core::Core;
pub use types::print_all_supported_units;
use types::Value;

mod expressions;
mod failure_message;
mod module;
mod stack;
pub mod statements;
pub use module::Module;

pub use failure_message::Failure;
pub use stack::Stack;

pub type ExecutionResult<S, V> = std::result::Result<V, ControlFlow<S>>;

#[derive(Debug, PartialEq)]
pub enum ControlFlow<S: Span> {
    Failure(Failure<S>),
    Break {
        span: S,
        label: Option<S>,
        value: Value<S>,
    },
    Continue {
        span: S,
        label: Option<S>,
    },
    Return {
        value: Value<S>,
    },
}

impl<S: Span> From<Failure<S>> for ControlFlow<S> {
    fn from(value: Failure<S>) -> Self {
        Self::Failure(value)
    }
}

pub struct GlobalResources {
    pub fornjot_unit_conversion_factor: &'static ConversionFactor,
    pub fornjot_core: Core,
}

impl Default for GlobalResources {
    fn default() -> Self {
        Self {
            fornjot_unit_conversion_factor: Scalar::get_conversion_factor("mm").unwrap(),
            fornjot_core: Default::default(),
        }
    }
}

pub struct ExecutionContext<'a, S: Span> {
    pub global_resources: &'a mut GlobalResources,
    pub log: &'a mut dyn RuntimeLog<S>,
    pub stack: &'a mut Stack<S>,
}

impl<'a, S: Span> ExecutionContext<'a, S> {
    pub fn new<R>(runtime: &'a mut Runtime<S>, run: impl FnOnce(&mut Self) -> R) -> R {
        let mut context = Self {
            global_resources: &mut runtime.global_resources,
            log: StandardLog::global(),
            stack: &mut runtime.stack,
        };

        // FIXME this registers the global functions as part of the module,
        // which is not actually global. This is a bad way to do this because
        // other modules won't have access to the global functions.
        // TODO add constants: std::constants::PI, Angle::HALF_TURN, Angle::FULL_TURN, Scalar::ZERO for all measurement types, SolidAngle::SPHERE.
        types::register_globals(&mut context);
        logging::register_globals(&mut context);

        run(&mut context)
    }

    pub fn unpack_validation_warnings(&mut self, span: &S) {
        self.global_resources
            .unpack_validation_warnings(span, self.log)
    }

    pub fn new_scope<R>(&mut self, scope: impl FnOnce(&mut Self) -> R) -> R {
        // This doesn't copy any variables, so it can't fail.
        self.stack
            .push_scope(std::iter::empty(), ScopeType::Inherited)
            .unwrap();
        let result = scope(self);
        self.stack.pop_scope();

        result
    }

    pub fn new_isolated_scope<R>(&mut self, scope: impl FnOnce(&mut Self) -> R) -> R {
        // This doesn't copy any variables, so it can't fail.
        self.stack
            .push_scope(std::iter::empty(), ScopeType::Isolated)
            .unwrap();
        let result = scope(self);
        self.stack.pop_scope();

        result
    }

    pub fn new_closure_scope<'s, R>(
        &mut self,
        references: impl Iterator<Item = impl Into<compact_str::CompactString>>,
        copies: impl Iterator<Item = &'s S>,
        scope: impl FnOnce(&mut Self) -> R,
    ) -> OperatorResult<S, R> {
        self.stack.push_scope(
            copies,
            ScopeType::Closure {
                references: references.map(|s| s.into()).collect(),
            },
        )?;
        let result = scope(self);
        self.stack.pop_scope();

        Ok(result)
    }
}

pub fn validate_assignment_type<S: Span>(
    context: &mut ExecutionContext<S>,
    member: &MemberVariable<S>,
    variable_assignment: &S,
    value: Value<S>,
    value_name: &S,
) -> OperatorResult<S, Value<S>> {
    match value {
        Value::Default(_) => {
            // They want to use a default value.
            if let Some(default) = member.ty.default_value.as_ref() {
                Value::from_litteral(context, default)
            } else {
                Err(Failure::NoDefault(
                    variable_assignment.clone(),
                    variable_assignment.clone(),
                ))
            }
        }
        // No request for default. Check the type.
        value => {
            if value.matches_type(&member.ty.ty, context.log, value_name)? {
                Ok(value)
            } else {
                Err(Failure::ExpectedGot(
                    variable_assignment.clone(),
                    member.ty.ty.name(),
                    value.type_name(),
                ))
            }
        }
    }
}

fn run_block<S: Span>(
    context: &mut ExecutionContext<S>,
    block: &Block<S>,
) -> ExecutionResult<S, Value<S>> {
    let mut result = NoneType.into();

    for statement in block.statements.iter() {
        if let Some(statement) = statement.get() {
            result = statements::run_statement(context, statement)?;
        }
    }

    Ok(result)
}

fn run_callable_block<S: Span>(
    context: &mut ExecutionContext<S>,
    block: &CallableBlock<S>,
    arguments: Vec<Value<S>>,
    spans: &[parsing::Expression<S>],
    default_span: &S,
) -> Result<Value<S>, Failure<S>> {
    // We do not return a ControlFlow because control flow does not
    // pass through named blocks (we can't continue or break a for loop outside of this named block)
    match arguments.len().cmp(&block.parameters.len()) {
        std::cmp::Ordering::Equal => {
            let mut failures = Vec::new();

            // Validate the arguments and put them into scope..
            for (span, (argument, parameter)) in spans
                .iter()
                .map(|expression| expression.get_span())
                .chain(std::iter::repeat(default_span))
                .zip(arguments.into_iter().zip(&block.parameters))
            {
                match validate_assignment_type(context, parameter, span, argument, &parameter.name)
                {
                    Ok(value) => {
                        context.stack.new_variable(&parameter.name, value);
                    }
                    Err(failure) => failures.push(failure),
                }
            }

            if failures.is_empty() {
                match run_block(context, &block.block) {
                    Ok(value) => Ok(value),
                    Err(control_flow) => match control_flow {
                        ControlFlow::Return { value } => Ok(value), // Oh that's normal behavior.
                        ControlFlow::Failure(failure) => Err(failure),
                        ControlFlow::Break {
                            span,
                            label: None,
                            value: _,
                        } => Err(Failure::BreakOutsideOfLoop(span)),
                        ControlFlow::Break {
                            span,
                            label: Some(label),
                            value: _,
                        } => Err(Failure::BreakLabelNotFound(span, label)),
                        ControlFlow::Continue { span, label: None } => {
                            Err(Failure::ContinueOutsideOfLoop(span))
                        }
                        ControlFlow::Continue {
                            span,
                            label: Some(label),
                        } => Err(Failure::ContinueLabelNotFound(span, label)),
                    },
                }
            } else {
                Err(Failure::BadArgumentTypes(
                    block.parameter_span.clone(),
                    failures,
                ))
            }
        }
        std::cmp::Ordering::Less => Err(Failure::MissingArguments(block.parameter_span.clone())),
        std::cmp::Ordering::Greater => Err(Failure::MissingArguments(block.parameter_span.clone())),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::script::{
        execution::{expressions::run_expression, run_block, ExecutionContext},
        parsing::Expression,
    };
    use common_data_types::Number;

    #[test]
    fn functions() {
        let mut log = Vec::new();

        let module = Module::load(
            &mut log,
            "test_module.ccm",
            "function my_function(input: Number) -> Number { input }",
        )
        .unwrap();

        assert!(log.is_empty());

        ExecutionContext::new(&mut module.into(), |context| {
            let block = parsing::Block::parse("{ my_function(5) }").unwrap().1;

            let result = run_block(context, Box::leak(Box::new(block)));
            assert_eq!(result, Ok(Number::new(5.0).unwrap().into()));
        });
    }

    #[test]
    fn default_function() {
        let mut log = Vec::new();

        let module = Module::load(
            &mut log,
            "test_module.ccm",
            "function my_function(input: Number = 5) -> Number { input }",
        )
        .unwrap();

        assert!(log.is_empty());

        ExecutionContext::new(&mut module.into(), |context| {
            let block = parsing::Block::parse("{ my_function(default) }").unwrap().1;

            let result = run_block(context, Box::leak(Box::new(block)));
            assert_eq!(result, Ok(Number::new(5.0).unwrap().into()));
        });
    }

    #[test]
    fn function_scope() {
        let mut log = Vec::new();

        let module = Module::load(
            &mut log,
            "test_module.ccm",
            "function my_function(input: Number) -> Number { value = input; input }",
        )
        .unwrap();

        assert!(log.is_empty());

        ExecutionContext::new(&mut module.into(), |context| {
            let block = parsing::Block::parse("{ let value = 0; my_function(5); value }")
                .unwrap()
                .1;

            let result =
                context.new_scope(|context| run_block(context, Box::leak(Box::new(block))));
            assert_eq!(
                result,
                Err(ControlFlow::Failure(Failure::VariableNotInScope(
                    "value",
                    "value".into()
                )))
            );
        });
    }

    #[test]
    fn function_hygene() {
        let mut log = Vec::new();

        let module = Module::load(
            &mut log,
            "test_module.ccm",
            "struct MyStruct {} function my_function() -> struct MyStruct { MyStruct {} }",
        )
        .unwrap();

        assert!(log.is_empty());

        ExecutionContext::new(&mut module.into(), |context| {
            let block = parsing::Block::parse("{ my_function() }").unwrap().1;

            let result =
                context.new_scope(|context| run_block(context, Box::leak(Box::new(block))));
            assert_eq!(
                result.unwrap(),
                run_expression(
                    context,
                    Box::leak(Box::new(Expression::parse("MyStruct {}").unwrap().1))
                )
                .unwrap()
            );
        });
    }
}
