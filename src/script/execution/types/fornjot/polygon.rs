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

use fj::math::Point;
use parsing::Span;

use crate::script::{
    execution::{
        types::{List, OperatorResult, StructDefinition, Structure},
        ExecutionContext, Failure,
    },
    parsing::{self, MemberVariable, MemberVariableType, VariableType},
};

use super::{point_from_list, unpack_dynamic_length_list};

pub fn register_globals<S: Span>(context: &mut ExecutionContext<'_, S>) {
    context.stack.new_variable_str(
        "Polygon",
        StructDefinition {
            // TODO replace box leak with lazy static.
            definition: Box::leak(Box::new(parsing::StructDefinition {
                name: S::from_str("Polygon"),
                members: vec![MemberVariable {
                    name: S::from_str("points"),
                    ty: MemberVariableType {
                        ty: VariableType::List,
                        constraints: None,
                        default_value: None,
                    },
                }],
            })),
        }
        .into(),
    );
}

/// Unwraps a structure to be made into a polygon (assumes you have already checked that the structure is a polygon type)
pub fn unwrap_polygon<S: Span>(
    context: &ExecutionContext<S>,
    span: &S,
    polygon: Structure<S>,
) -> OperatorResult<S, Vec<Point<2>>> {
    let mut members = Rc::unwrap_or_clone(polygon.members);
    let provided_points = members
        .remove("points")
        .unwrap()
        .downcast::<List<S>>(span)?;
    let mut fornjot_points = Vec::with_capacity(provided_points.len());

    for (index, point) in
        unpack_dynamic_length_list::<S, List<S>>(span, provided_points)?.enumerate()
    {
        let point = point_from_list::<S, 2>(
            span,
            context.global_resources.convert_to_fornjot_units,
            point,
        )
        .map_err(|failure| Failure::ListElementFailure(span.clone(), index, Box::new(failure)))?;

        fornjot_points.push(point);
    }

    Ok(fornjot_points)
}
