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

use std::ops::Deref;

use crate::script::{
    execution::{
        types::{
            function::{AutoCall, IntoBuiltinFunction},
            Object, OperatorResult, Structure, Value,
        },
        ExecutionContext, Failure,
    },
    logging::RuntimeLog,
    parsing::VariableType,
    Span,
};

use fj_core::{
    operations::{build::BuildCycle, insert::Insert, reverse::Reverse},
    storage::Handle,
    topology::Cycle as FornjotCycle,
};

use super::{circle::unwrap_circle, handle_wrapper, polygon::unwrap_polygon};

pub fn register_globals<S: Span>(context: &mut ExecutionContext<S>) {
    context.stack.new_variable_str(
        "new_cycle",
        (|context: &mut ExecutionContext<S>, span: &S, configuration: Structure<S>| {
            match configuration.name() {
                "Circle" => {
                    let (center, radius) = unwrap_circle(context, span, configuration)?;

                    let circle = FornjotCycle::circle(
                        center,
                        radius,
                        &mut context.global_resources.fornjot_core,
                    );
                    let circle = circle.insert(&mut context.global_resources.fornjot_core);
                    context.unpack_validation_warnings(span);

                    Ok(Cycle { handle: circle }.into())
                }
                "Polygon" => {
                    let points = unwrap_polygon(context, span, configuration)?;

                    let polygone =
                        FornjotCycle::polygon(points, &mut context.global_resources.fornjot_core);
                    let polygon = polygone.insert(&mut context.global_resources.fornjot_core);
                    context.unpack_validation_warnings(span);

                    Ok(Cycle { handle: polygon }.into())
                }
                // "RawCycle" => {
                //     todo!() // TODO I want to be able to build a region from half-edges.
                // }
                _ => Err(Failure::ExpectedGot(
                    span.clone(),
                    "Empty, Circle, or Polygon".into(),
                    configuration.name().to_string().into(),
                )),
            }
        })
        .into_builtin_function()
        .into(),
    )
}

#[derive(Clone)]
pub struct Cycle {
    pub handle: Handle<FornjotCycle>,
}

impl<S: Span> Object<S> for Cycle {
    fn matches_type(
        &self,
        ty: &VariableType<S>,
        _log: &mut dyn RuntimeLog<S>,
        _variable_name_span: &S,
    ) -> OperatorResult<S, bool> {
        Ok(matches!(ty, VariableType::Cycle))
    }

    fn method_call(
        &self,
        context: &mut ExecutionContext<S>,
        span: &S,
        attribute: &S,
        arguments: Vec<Value<S>>,
        spans: &[crate::script::parsing::Expression<S>],
    ) -> OperatorResult<S, Value<S>> {
        match attribute.as_str() {
            "reverse" => {
                |context: &mut ExecutionContext<S>, _span: &S| -> OperatorResult<S, Value<S>> {
                    let reversed = self
                        .handle
                        .deref()
                        .reverse(&mut context.global_resources.fornjot_core);

                    let reversed = reversed.insert(&mut context.global_resources.fornjot_core);
                    context.unpack_validation_warnings(span);

                    Ok(Self::from(reversed).into())
                }
                .auto_call(context, span, arguments, spans)
            }
            _ => Err(Failure::UnknownAttribute(attribute.clone())),
        }
    }
}

handle_wrapper!(Cycle, FornjotCycle);

#[cfg(test)]
mod test {
    use crate::script::{
        execution::{expressions::run_expression, types::Value},
        parsing::Expression,
        Runtime,
    };

    use super::*;

    #[test]
    fn construct_circle() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert!(matches!(
                run_expression(
                    context,
                    &Expression::parse(
                        "new_cycle(Circle { center = vec2(1mm, 2mm), radius = 3mm })"
                    )
                    .unwrap()
                    .1
                ),
                Ok(Value::Cycle(_))
            ));
        });
    }

    #[test]
    fn construct_polygon() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert!(matches!(
            run_expression(
                context,
                    &Expression::parse(
                        "new_cycle(Polygon { points = [vec2(0m, 0m), vec2(0m, 1m), vec2(1m, 1m), vec2(1m, 0m)] })"
                    )
                    .unwrap()
                    .1
            ),
            Ok(Value::Cycle(_))
        ));
        });
    }
}
