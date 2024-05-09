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

use crate::script::{
    execution::{run_callable_block, ExecutionContext},
    logging::RuntimeLog,
    parsing::{self, CapturedVariable, Expression, VariableType},
    Span,
};

use super::{NamedObject, Object, OperatorResult, Value};

#[derive(Clone)]
pub struct Closure<S: Span> {
    pub source: Rc<parsing::Closure<S>>,
}

impl<S: Span> Object<S> for Closure<S> {
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
        let referenced_variables = self.source.captured_variables.iter().filter_map(|v| {
            if let CapturedVariable::Reference(name) = v {
                Some(name.as_str())
            } else {
                None
            }
        });

        let copied_variables = self.source.captured_variables.iter().filter_map(|v| {
            if let CapturedVariable::Copy(name) = v {
                Some(name)
            } else {
                None
            }
        });

        context.new_closure_scope(referenced_variables, copied_variables, |context| {
            run_callable_block(context, &self.source.callable, arguments, spans, span)
        })?
    }
}

impl<'a, S: Span> From<&'a Rc<parsing::Closure<S>>> for Closure<S> {
    fn from(source: &'a Rc<parsing::Closure<S>>) -> Self {
        Self {
            source: Rc::clone(source),
        }
    }
}

impl<S: Span> std::fmt::Debug for Closure<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Closure")
            .field("address", &Rc::as_ptr(&self.source))
            .finish()
    }
}

impl<S: Span> PartialEq for Closure<S> {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl<S: Span> NamedObject for Closure<S> {
    fn static_type_name() -> &'static str {
        "Closure"
    }
}

#[cfg(test)]
mod test {
    use common_data_types::Float;

    use crate::script::{
        execution::{
            expressions::run_expression, run_block, types::NoneType, ControlFlow, Failure,
        },
        Runtime,
    };

    use super::*;

    #[test]
    fn call_closure() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(
                    context,
                    &Expression::parse("[]() -> Number { 1 }()").unwrap().1
                ),
                Ok(Float::new(1.0).unwrap().into())
            );
        });
    }

    #[test]
    fn closure_hygene() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            let block = parsing::Block::parse(
                "{ let closure = []() -> Number { let value = 1; }; closure(); }",
            )
            .unwrap()
            .1;

            assert_eq!(run_block(context, &block), Ok(NoneType.into()));

            assert_eq!(
                context.stack.get_variable(&"value"),
                Err(Failure::VariableNotInScope("value", "value".into()))
            );

            let block = parsing::Block::parse(
                "{ let closure = []() -> Number { value = 1; }; closure(); }",
            )
            .unwrap()
            .1;

            assert_eq!(
                run_block(context, &block),
                Err(ControlFlow::Failure(Failure::VariableNotInScope(
                    "value",
                    "value".into()
                )))
            );

            let block = parsing::Block::parse(
                "{ let value = 2; let closure = []() -> Number { value = 1; }; closure(); }",
            )
            .unwrap()
            .1;

            // Running the test right on the module scope creates issues, so we need to create a new scope.
            context.new_scope(|context| {
                assert_eq!(
                    run_block(context, &block),
                    Err(ControlFlow::Failure(Failure::VariableNotInScope(
                        "value",
                        "value".into()
                    )))
                );

                assert_eq!(
                    context.stack.get_variable(&"value"),
                    Ok(&Float::new(2.0).unwrap().into())
                );
            });
        });
    }

    #[test]
    fn pass_by_copy() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            let block = parsing::Block::parse(
            "{ let value = 1; let closure = [value]() -> Number { value = 2; value }; closure() }",
        )
        .unwrap()
        .1;

            let result = run_block(context, &block);
            assert_eq!(result, Ok(Float::new(2.0).unwrap().into()));

            assert_eq!(
                context.stack.get_variable(&"value"),
                Ok(&Float::new(1.0).unwrap().into())
            );
        });
    }

    #[test]
    fn pass_by_reference() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            let block = parsing::Block::parse(
            "{ let value = 1; let closure = [&value]() -> Number { value = 2; value }; closure() }",
        )
        .unwrap()
        .1;

            let result = run_block(context, &block);
            assert_eq!(result, Ok(Float::new(2.0).unwrap().into()));

            assert_eq!(
                context.stack.get_variable(&"value"),
                Ok(&Float::new(2.0).unwrap().into())
            );
        });
    }
}
