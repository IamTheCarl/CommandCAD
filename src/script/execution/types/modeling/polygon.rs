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

use parsing::Span;

use crate::script::{
    execution::{
        types::{
            LengthVector2, List, NamedObject, Object, OperatorResult, StructDefinition, Vector2,
        },
        ExecutionContext,
    },
    parsing::{self, MemberVariable, MemberVariableType},
    Failure,
};

use super::surface::Surface;

use macros::Struct;

#[derive(Struct)]
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

pub fn register_globals<S: Span>(context: &mut ExecutionContext<'_, S>) {
    Polygon::define_struct(context);
}
