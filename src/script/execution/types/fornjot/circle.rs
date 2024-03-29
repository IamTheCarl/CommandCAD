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

use fj_math::{Point, Scalar};
use parsing::Span;

use crate::script::{
    execution::{
        types::{Measurement, OperatorResult, StructDefinition, Structure, Vector2},
        ExecutionContext,
    },
    parsing::{self, MemberVariable, MemberVariableType, VariableType},
};

pub fn register_globals<S: Span>(context: &mut ExecutionContext<'_, S>) {
    context.stack.new_variable_str(
        "Circle",
        StructDefinition {
            // FIXME replace box leak with lazy static.
            definition: Box::leak(Box::new(parsing::StructDefinition {
                name: S::from_str("Circle"),
                members: vec![
                    MemberVariable {
                        name: S::from_str("center"),
                        ty: MemberVariableType {
                            ty: VariableType::Vector(2, S::from_str("Length")),
                            constraints: None,
                            default_value: None,
                        },
                    },
                    MemberVariable {
                        name: S::from_str("radius"),
                        ty: MemberVariableType {
                            ty: VariableType::Measurement(S::from_str("Length")),
                            constraints: None,
                            default_value: None,
                        },
                    },
                ],
            })),
        }
        .into(),
    );
}

/// Unwraps a structure to be made into a circle (assumes you have already checked that the structure is a circle type)
pub fn unwrap_circle<S: Span>(
    context: &ExecutionContext<S>,
    span: &S,
    circle: Structure<S>,
) -> OperatorResult<S, (Point<2>, Scalar)> {
    let mut members = Rc::unwrap_or_clone(circle.members);
    let center = members.remove("center").unwrap();
    let center = center.downcast::<Vector2>(span)?;
    let center = center.as_fornjot_point(context, span)?;

    let radius = members.remove("radius").unwrap();
    let radius = radius.downcast::<Measurement>(span)?;
    let radius = radius.as_scalar(context, span)?;

    Ok((center, radius))
}
