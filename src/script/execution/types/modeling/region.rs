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

use fj_core::{
    operations::{build::BuildRegion, insert::Insert},
    storage::Handle,
    topology::Region as FornjotRegion,
};

use crate::script::{
    execution::{
        types::{
            function::IntoBuiltinFunction, List, Object, OperatorResult, StructDefinition,
            Structure,
        },
        ExecutionContext, Failure,
    },
    logging::RuntimeLog,
    parsing::{self, MemberVariable, MemberVariableType, VariableType},
    Span,
};

use super::{
    cycle::Cycle,
    handle_wrapper,
    structs::{Circle, Polygon},
};

pub fn register_globals<S: Span>(context: &mut ExecutionContext<S>) {
    context.stack.new_variable_str(
        "RawRegion",
        StructDefinition {
            // TODO replace box leak with lazy static.
            definition: Rc::new(parsing::StructDefinition {
                name: S::from_str("RawRegion"),
                members: vec![
                    MemberVariable {
                        name: S::from_str("exterior"),
                        ty: MemberVariableType {
                            ty: VariableType::Cycle,
                            constraints: None,
                            default_value: None,
                        },
                    },
                    MemberVariable {
                        name: S::from_str("interiors"),
                        ty: MemberVariableType {
                            ty: VariableType::List,
                            constraints: None,
                            default_value: None,
                        },
                    },
                ],
            }),
        }
        .into(),
    );

    context.stack.new_variable_str(
        "new_region",
        (|context: &mut ExecutionContext<S>, span: &S, configuration: Structure<S>| {
            match configuration.name() {
                "Circle" => {
                    let circle = Circle::unpack_struct(span, configuration)?;

                    let region = Region::from(
                        FornjotRegion::circle(
                            circle.center.as_fornjot_point(context),
                            circle.radius.as_fornjot_scalar(context),
                            circle.surface.handle,
                            &mut context.global_resources.fornjot_core,
                        )
                        .insert(&mut context.global_resources.fornjot_core),
                    );

                    context.unpack_validation_warnings(span);

                    Ok(region.into())
                }
                "Polygon" => {
                    let polygon = Polygon::unpack_struct(span, configuration)?;

                    let region = Region::from(
                        FornjotRegion::polygon(
                            polygon.points(context, span)?,
                            polygon.surface.handle,
                            &mut context.global_resources.fornjot_core,
                        )
                        .insert(&mut context.global_resources.fornjot_core),
                    );

                    context.unpack_validation_warnings(span);

                    Ok(region.into())
                }
                "RawRegion" => {
                    let region = Region::build_raw(context, span, configuration)?;
                    Ok(region.into())
                }
                // TODO build a region from an SVG file.
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
pub struct Region {
    pub handle: Handle<FornjotRegion>,
}

impl Region {
    fn build_raw<S: Span>(
        context: &mut ExecutionContext<S>,
        span: &S,
        configuration: Structure<S>,
    ) -> OperatorResult<S, Self> {
        let mut members = Rc::unwrap_or_clone(configuration.members);

        // We assume that this is a `RawRegion` type.
        let exterior_cycle = members
            .remove("exterior")
            .unwrap()
            .downcast::<Cycle>(span)?;
        let exterior_cycle = exterior_cycle.handle;

        let interior_cylce_list = members
            .remove("interiors")
            .unwrap()
            .downcast::<List<S>>(span)?;

        let interior_cycles = interior_cylce_list
            .unpack_dynamic_length::<Cycle>(span)?
            .map(|cycle| cycle.handle);

        let region = Self::from(
            FornjotRegion::new(exterior_cycle, interior_cycles)
                .insert(&mut context.global_resources.fornjot_core),
        );

        context.unpack_validation_warnings(span);

        Ok(region)
    }
}

impl<S: Span> Object<S> for Region {
    fn matches_type(
        &self,
        ty: &VariableType<S>,
        _log: &mut dyn RuntimeLog<S>,
        _variable_name_span: &S,
    ) -> OperatorResult<S, bool> {
        Ok(matches!(ty, VariableType::Region))
    }
}

handle_wrapper!(Region, FornjotRegion);

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
                        "new_region(Circle { center = vec2(1mm, 2mm), radius = 3mm, surface = global_xy_plane() })"
                    )
                    .unwrap()
                    .1
                ),
                Ok(Value::Region(_))
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
                        "new_region(Polygon { points = [vec2(0m, 0m), vec2(0m, 1m), vec2(1m, 1m), vec2(1m, 0m)], surface = global_xy_plane() })"
                    )
                    .unwrap()
                    .1
            ),
            Ok(Value::Region(_))
        ));
        });
    }

    #[test]
    fn construct_raw() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert!(matches!(
            run_expression(
                context,
                &Expression::parse(
                    "new_region(RawRegion { exterior = new_cycle(Circle { center = vec2(1mm, 2mm), radius = 3mm, surface = global_xy_plane() }),
                     interiors = [new_cycle(Circle { center = vec2(1mm, 2mm), radius = 3mm / 2, surface = global_xy_plane() })] })"
                )
                .unwrap()
                .1,
            ),
            Ok(Value::Region(_))
        ));
        });
    }

    // TODO validation failure test (Fornjot failure validation should result in a Failure type when constructing an invalid region)
}
