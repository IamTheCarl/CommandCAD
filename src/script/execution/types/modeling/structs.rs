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

use bezier_rs::{Bezier, BezierHandles, TValueType};
use common_data_types::Float;
use fj_math::Point;
use glam::f64::DVec2;
use macros::Struct;
use parsing::Span;

use crate::script::{
    execution::{
        types::{
            math::{Angle, ConvertUnit},
            Length, LengthVector2, List, NamedObject, Object, OperatorResult, StructDefinition,
            Structure, Vector2,
        },
        ExecutionContext,
    },
    parsing::{self, MemberVariable, MemberVariableType},
    Failure,
};

use super::surface::Surface;

pub fn register_globals<S: Span>(context: &mut ExecutionContext<'_, S>) {
    Circle::define_struct(context);
    Polygon::define_struct(context);

    Segments::define_struct(context);
    CenterArc::define_struct(context);
    Line::define_struct(context);
    QuadraticBezier::define_struct(context);
    CubicBezier::define_struct(context);
}

/// A single, perfectly round, and closed circle.
#[derive(Debug, Struct)]
pub struct Circle {
    pub center: LengthVector2,
    pub radius: Length,
    pub surface: Surface,
}

/// A list of straight segments that may or may not be closed.
/// Some functions expect this to be closed and will give an error if it is not.
#[derive(Debug, Struct)]
pub struct Polygon<S: Span> {
    pub points: List<S>,
    pub surface: Surface,
}

impl<S: Span> Polygon<S> {
    pub fn points(
        &self,
        context: &ExecutionContext<S>,
        span: &S,
    ) -> OperatorResult<S, Vec<fj_math::Point<2>>> {
        let mut fornjot_points = Vec::with_capacity(self.points.len(span)?);
        for point in self.points.clone().unpack_dynamic_length::<Vector2>(span)? {
            let point = LengthVector2::try_from(point).map_err(|point| {
                Failure::ExpectedGot(
                    span.clone(),
                    LengthVector2::static_type_name().into(),
                    <Vector2 as Object<S>>::type_name(&point),
                )
            })?;
            let point = point.as_fornjot_point(context);

            fornjot_points.push(point);
        }

        Ok(fornjot_points)
    }
}

/// Defines a region made of complex segments, such as lines, arcs, and bezier curves.
#[derive(Debug, Struct)]
pub struct Segments<S: Span> {
    pub start: LengthVector2,
    pub segments: List<S>,
    #[default = "1mm"]
    pub distance_between_verticies: Length,
    pub surface: Surface,
}

impl<S: Span> Segments<S> {
    pub fn as_polygon(
        &self,
        context: &ExecutionContext<S>,
        span: &S,
    ) -> OperatorResult<S, Vec<Point<2>>> {
        let mut last_point = self.start.clone();
        let mut points = vec![last_point.as_fornjot_point(context)];
        let distance_per_step = self.distance_between_verticies;

        for edge in self.segments.iter(span)? {
            let structure = edge.downcast::<Structure<S>>(span)?;

            match structure.name() {
                "CenterArc" => {
                    let arc = CenterArc::unpack_struct(span, structure)?;
		    let direction = if arc.clockwise {
			ArcDirection::Clockwise
		    } else {
			ArcDirection::CounterClockwise
		    };
		    let end = arc_to_points(context, distance_per_step, last_point, arc.center, arc.angle, direction, &mut points);
		    last_point = end;
                }
		"Line" => {
		    let line = Line::unpack_struct(span, structure)?;
		    last_point = line.end;
		    points.push(last_point.as_fornjot_point(context));
		},
                "QuadraticBezier" => {
                    let bezier = QuadraticBezier::unpack_struct(span, structure)?;
		    let start = last_point.value;
		    let end = bezier.end.value;
		    last_point = bezier.end.clone();
		    let handle = bezier.handle.value;
		    let bezier = Bezier {
			start: DVec2::new(start.x, start.y),
			end: DVec2::new(end.x, end.y),
			handles: BezierHandles::Quadratic {
			    handle: DVec2::new(handle.x, handle.y),
			},
		    };
		    beizer_to_points(context, &bezier, distance_per_step, &mut points);
                }
                "CubicBezier" => {
                    let bezier = CubicBezier::unpack_struct(span, structure)?;

		    let start = last_point.value;
		    let start_handle = bezier.start_handle.value;

		    let end = bezier.end.value;
		    let end_handle = bezier.end_handle.value;
		    last_point = bezier.end.clone();
		    let bezier = Bezier {
			start: DVec2::new(start.x, start.y),
			end: DVec2::new(end.x, end.y),
			handles: BezierHandles::Cubic {
			    handle_start: DVec2::new(start_handle.x, start_handle.y),
			    handle_end: DVec2::new(end_handle.x, end_handle.y)
			},
		    };
		    beizer_to_points(context, &bezier, distance_per_step, &mut points);
                }
                name => {
                    return Err(Failure::ExpectedGot(
                        span.clone(),
                        "A Circle, Polygon, CenterArc, EndsArc, LengthArc, LinearBezier, QuadraticBezier, or a CubicBezier".into(),
                        name.to_string().into(),
                    ))
                }
            }
        }

        Ok(points)
    }
}

fn beizer_to_points<S: Span>(
    context: &ExecutionContext<S>,
    beizer: &Bezier,
    distance_per_step: Length,
    points: &mut Vec<Point<2>>,
) {
    let length = beizer.length(None);
    let steps = (length / distance_per_step.value.into_inner()).ceil() as usize;

    let lookup_table = beizer.compute_lookup_table(Some(steps), Some(TValueType::Euclidean));

    for vertex in &lookup_table[1..] {
        let x = context
            .global_resources
            .fornjot_unit_conversion_factor
            .convert_from_base_unit(Float::new(vertex.x).unwrap())
            .into_inner();
        let y = context
            .global_resources
            .fornjot_unit_conversion_factor
            .convert_from_base_unit(Float::new(vertex.y).unwrap())
            .into_inner();

        points.push(fj_math::Point::<2> {
            coords: fj_math::Vector::<2> {
                components: [x.into(), y.into()],
            },
        });
    }
}

#[derive(Debug, Clone, Copy)]
enum ArcDirection {
    Clockwise,
    CounterClockwise,
}

fn arc_to_points<S: Span>(
    context: &ExecutionContext<S>,
    distance_per_step: Length,
    start: LengthVector2,
    center: LengthVector2,
    angle: Angle,
    direction: ArcDirection,
    points: &mut Vec<Point<2>>,
) -> LengthVector2 {
    let distance_per_step = distance_per_step
        .as_fornjot_scale_float(context)
        .into_inner();
    let start = start.as_fornjot_scale_nalgebra_vector::<S, 2>(context);
    let center = center.as_fornjot_scale_nalgebra_vector::<S, 2>(context);

    let center_to_start = start - center;
    let radius = center_to_start.norm();

    let starting_angle = (start.y - center.y).atan2(start.x - center.x);
    let angle = angle.value.into_inner();

    let (sin, cos) = (starting_angle + angle).sin_cos();
    let end = center + nalgebra::Vector2::new(cos, sin) * radius;

    let arch_length = angle * radius;
    let steps = (arch_length / distance_per_step).ceil();

    let angle_direction = if matches!(direction, ArcDirection::Clockwise) {
        -1.0
    } else {
        1.0
    };

    let angle_step = (angle / steps) * angle_direction;

    let steps = steps as usize;

    // We skip the first step because that's already represented with the starting point.
    for step_index in 1..steps {
        let angle = starting_angle + angle_step * step_index as f64;

        let (sin, cos) = angle.sin_cos();
        let offset = nalgebra::Vector2::new(cos, sin) * radius;

        let new_position = center + offset;

        points.push(fj_math::Point::<2> {
            coords: fj_math::Vector::<2> {
                components: [new_position.x.into(), new_position.y.into()],
            },
        })
    }

    points.push(fj_math::Point::<2> {
        coords: fj_math::Vector::<2> {
            components: [end.x.into(), end.y.into()],
        },
    });

    LengthVector2 { value: end }
}

/// An arc defined by a senter point and a radius.
/// The radius is calulated by using the distance from the center point to the starting point/
/// The starting point is the previous end point.
#[derive(Debug, Struct)]
pub struct CenterArc {
    pub center: LengthVector2,
    pub angle: Angle,
    #[default = "true"]
    pub clockwise: bool,
}

/// A linear line segment.
/// The start point is assumed by the previous end point.
#[derive(Debug, Struct)]
pub struct Line {
    pub end: LengthVector2,
}

/// A quadratic Bezier curve.
/// The start point is assumed by the previous end point.
#[derive(Debug, Struct)]
pub struct QuadraticBezier {
    pub end: LengthVector2,
    pub handle: LengthVector2,
}

/// A cubic Bezier curve.
/// The start point is assumed by the previous end point.
#[derive(Debug, Struct)]
pub struct CubicBezier {
    pub start_handle: LengthVector2,
    pub end: LengthVector2,
    pub end_handle: LengthVector2,
}
