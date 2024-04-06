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

use fj_core::{
    operations::{build::BuildRegion, insert::Insert, sweep::SweepSketch},
    storage::Handle,
    topology::{Region as FornjotRegion, Sketch as FornjotSketch},
};

use crate::script::{
    execution::{
        types::{
            function::{AutoCall, IntoBuiltinFunction},
            Object, OperatorResult, Value, Vector3,
        },
        ExecutionContext, Failure,
    },
    logging::RuntimeLog,
    parsing::{self, VariableType},
    Span,
};

use super::{
    circle::unwrap_circle, handle_wrapper, polygon::unwrap_polygon, region::Region, solid::Solid,
    surface::Surface,
};

pub fn register_globals<S: Span>(context: &mut ExecutionContext<S>) {
    context.stack.new_variable_str(
        "new_sketch",
        (|context: &mut ExecutionContext<S>,
          span: &S,
          argument: Value<S>|
         -> OperatorResult<S, Value<S>> {
            match argument {
                Value::List(regions) => {
                    let regions = regions
                        .unpack_dynamic_length::<Region>(span)?
                        .map(|region| region.handle);
                    let handle = FornjotSketch::new(regions)
                        .insert(&mut context.global_resources.fornjot_core);
                    context.unpack_validation_warnings(span);

                    Ok(Sketch { handle }.into())
                }
                Value::Structure(configuration) => match configuration.name() {
                    "Circle" => {
                        let (center, radius) = unwrap_circle(context, span, configuration)?;
                        let region = FornjotRegion::circle(
                            center,
                            radius,
                            &mut context.global_resources.fornjot_core,
                        )
                        .insert(&mut context.global_resources.fornjot_core);
                        context.unpack_validation_warnings(span);

                        let handle = FornjotSketch::new([region])
                            .insert(&mut context.global_resources.fornjot_core);
                        context.unpack_validation_warnings(span);

                        Ok(Sketch { handle }.into())
                    }
                    "Polygon" => {
                        let points = unwrap_polygon(context, span, configuration)?;

                        let polygon = FornjotRegion::polygon(
                            points,
                            &mut context.global_resources.fornjot_core,
                        );
                        let polygon = polygon;
                        let region = polygon.insert(&mut context.global_resources.fornjot_core);
                        context.unpack_validation_warnings(span);

                        let handle = FornjotSketch::new([region])
                            .insert(&mut context.global_resources.fornjot_core);
                        context.unpack_validation_warnings(span);

                        Ok(Sketch { handle }.into())
                    }
                    // TODO sketch from an SVG file.
                    _ => Err(Failure::ExpectedGot(
                        span.clone(),
                        "Empty, Circle, or Polygon".into(),
                        configuration.name().to_string().into(),
                    )),
                },
                value => Err(Failure::ExpectedGot(
                    span.clone(),
                    "Circle, Polygon, or a List of regions".into(),
                    value.type_name(),
                )),
            }
        })
        .into_builtin_function()
        .into(),
    )
}

#[derive(Clone)]
pub struct Sketch {
    pub handle: Handle<FornjotSketch>,
}

impl<S: Span> Object<S> for Sketch {
    fn matches_type(
        &self,
        ty: &VariableType<S>,
        _log: &mut dyn RuntimeLog<S>,
        _variable_name_span: &S,
    ) -> OperatorResult<S, bool> {
        Ok(matches!(ty, VariableType::Sketch))
    }

    fn method_call(
        &self,
        context: &mut ExecutionContext<S>,
        span: &S,
        attribute: &S,
        arguments: Vec<Value<S>>,
        spans: &[parsing::Expression<S>],
    ) -> OperatorResult<S, Value<S>> {
        match attribute.as_str() {
            "sweep" => |context: &mut ExecutionContext<S>,
                        span: &S,
                        surface: Surface,
                        path: Vector3|
             -> OperatorResult<S, Value<S>> {
                let surface = surface.handle;
                let path = path.as_fornjot_vector(context, span)?;

                let solid = self
                    .handle
                    .sweep_sketch(surface, path, &mut context.global_resources.fornjot_core)
                    .insert(&mut context.global_resources.fornjot_core);
                context.unpack_validation_warnings(span);

                Ok(Solid::from(solid).into())
            }
            .auto_call(context, span, arguments, spans),
            _ => Err(Failure::UnknownAttribute(attribute.clone())),
        }
    }
}

handle_wrapper!(Sketch, FornjotSketch);

#[cfg(test)]
mod test {
    use crate::script::{execution::expressions::run_expression, parsing::Expression, Runtime};

    use super::*;

    #[test]
    fn construct_circle() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert!(matches!(
                run_expression(
                    context,
                    &Expression::parse(
                        "new_sketch(Circle { center = vec2(1mm, 2mm), radius = 3mm })"
                    )
                    .unwrap()
                    .1
                ),
                Ok(Value::Sketch(_))
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
                        "new_sketch(Polygon { points = [vec2(0m, 0m), vec2(0m, 1m), vec2(1m, 1m), vec2(1m, 0m)] })"
                    )
                    .unwrap()
                    .1
		),
		Ok(Value::Sketch(_))
            ));
        });
    }

    #[test]
    fn construct_from_regions() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert!(matches!(
                run_expression(
                    context,
                    &Expression::parse(
                        "new_sketch([new_region(Circle { center = vec2(1mm, 2mm), radius = 3mm }),
new_region(Circle { center = vec2(4mm, 2mm), radius = 3mm })])"
                    )
                    .unwrap()
                    .1
                ),
                Ok(Value::Sketch(_))
            ));
        });
    }

    #[test]
    fn sweep() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert!(matches!(
		run_expression(
                    context,
                    &Expression::parse(
			"new_sketch(Circle { center = vec2(1mm, 2mm), radius = 3mm }).sweep(global_xz_plane(), vec3(0cm, 1cm, 0cm))"
                    )
		    .unwrap()
		    .1,
		),
		Ok(Value::Solid(_))
            ));
        });
    }

    // TODO validation failure test (Fornjot failure validation should result in a Failure type when constructing an invalid region)
}
