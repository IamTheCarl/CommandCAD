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

use crate::script::{
    execution::{run_callable_block, ExecutionContext},
    parsing::{self, CapturedVariable, Expression, FunctionSignature, VariableType},
    Span,
};

use super::{NamedObject, Object, OperatorResult, Value};

#[derive(Clone)]
pub struct Closure<'a, S: Span> {
    pub closure: &'a parsing::Closure<S>,
    pub signature: Rc<FunctionSignature<S>>,
}

impl<'a, S: Span> Object<'a, S> for Closure<'a, S> {
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
        let referenced_variables = self.closure.captured_variables.iter().filter_map(|v| {
            if let CapturedVariable::Reference(name) = v {
                Some(name.as_str())
            } else {
                None
            }
        });

        let copied_variables = self.closure.captured_variables.iter().filter_map(|v| {
            if let CapturedVariable::Copy(name) = v {
                Some(name)
            } else {
                None
            }
        });

        context.new_closure_scope(referenced_variables, copied_variables, |context| {
            run_callable_block(context, &self.closure.callable, arguments, spans, span)
        })?
    }
}

impl<'a, S: Span> From<&'a parsing::Closure<S>> for Closure<'a, S> {
    fn from(closure: &'a parsing::Closure<S>) -> Self {
        Self {
            signature: closure.signature.clone(),
            closure,
        }
    }
}

impl<'a, S: Span> Debug for Closure<'a, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("UserFunction").finish()
    }
}

impl<'a, S: Span> PartialEq for Closure<'a, S> {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl<'a, S: Span> NamedObject for Closure<'a, S> {
    fn static_type_name() -> &'static str {
        "Closure"
    }
}

#[cfg(test)]
mod test {
    use crate::script::execution::{
        expressions::run_expression,
        run_block,
        types::{NoneType, Number},
        ControlFlow, Failure,
    };

    use super::*;

    #[test]
    fn call_closure() {
        let mut context = ExecutionContext::default();

        assert_eq!(
            run_expression(
                &mut context,
                Box::leak(Box::new(
                    Expression::parse("[]() -> Number { 1 }()").unwrap().1
                ))
            ),
            Ok(Number::new(1.0).unwrap().into())
        );
    }

    #[test]
    fn closure_hygene() {
        let mut context = ExecutionContext::default();
        let block = parsing::Block::parse(
            "{ let closure = []() -> Number { let value = 1; }; closure(); }",
        )
        .unwrap()
        .1;

        assert_eq!(
            run_block(&mut context, Box::leak(Box::new(block))),
            Ok(NoneType.into())
        );

        assert_eq!(
            context.stack.get_variable(&"value"),
            Err(Failure::VariableNotInScope("value", "value".into()))
        );

        let block =
            parsing::Block::parse("{ let closure = []() -> Number { value = 1; }; closure(); }")
                .unwrap()
                .1;

        assert_eq!(
            run_block(&mut context, Box::leak(Box::new(block))),
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
                run_block(context, Box::leak(Box::new(block))),
                Err(ControlFlow::Failure(Failure::VariableNotInScope(
                    "value",
                    "value".into()
                )))
            );

            assert_eq!(
                context.stack.get_variable(&"value"),
                Ok(&Number::new(2.0).unwrap().into())
            );
        });
    }

    #[test]
    fn pass_by_copy() {
        let mut context = ExecutionContext::default();
        let block = parsing::Block::parse(
            "{ let value = 1; let closure = [value]() -> Number { value = 2; value }; closure() }",
        )
        .unwrap()
        .1;

        let result = run_block(&mut context, Box::leak(Box::new(block)));
        assert_eq!(result, Ok(Number::new(2.0).unwrap().into()));

        assert_eq!(
            context.stack.get_variable(&"value"),
            Ok(&Number::new(1.0).unwrap().into())
        );
    }

    #[test]
    fn pass_by_reference() {
        let mut context = ExecutionContext::default();
        let block = parsing::Block::parse(
            "{ let value = 1; let closure = [&value]() -> Number { value = 2; value }; closure() }",
        )
        .unwrap()
        .1;

        let result = run_block(&mut context, Box::leak(Box::new(block)));
        assert_eq!(result, Ok(Number::new(2.0).unwrap().into()));

        assert_eq!(
            context.stack.get_variable(&"value"),
            Ok(&Number::new(2.0).unwrap().into())
        );
    }
}
