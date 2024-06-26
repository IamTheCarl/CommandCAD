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

use fj_core::{storage::Handle, topology::Surface as FornjotSurface};

use crate::script::{
    execution::{
        types::{function::IntoBuiltinFunction, Object, OperatorResult, Value},
        ExecutionContext,
    },
    logging::RuntimeLog,
    parsing::VariableType,
    Span,
};

use super::handle_wrapper;

pub fn register_globals<S: Span>(context: &mut ExecutionContext<S>) {
    context.stack.new_variable_str(
        "sketch_plane",
        (|context: &mut ExecutionContext<S>, _span: &S| -> OperatorResult<S, Value<S>> {
            Ok(Surface::from(
                context
                    .global_resources
                    .fornjot_core
                    .layers
                    .topology
                    .surfaces
                    .space_2d(),
            )
            .into())
        })
        .into_builtin_function()
        .into(),
    );

    context.stack.new_variable_str(
        "global_xy_plane",
        (|context: &mut ExecutionContext<S>, _span: &S| -> OperatorResult<S, Value<S>> {
            Ok(Surface::from(
                context
                    .global_resources
                    .fornjot_core
                    .layers
                    .topology
                    .surfaces
                    .xy_plane(),
            )
            .into())
        })
        .into_builtin_function()
        .into(),
    );

    context.stack.new_variable_str(
        "global_xz_plane",
        (|context: &mut ExecutionContext<S>, _span: &S| -> OperatorResult<S, Value<S>> {
            Ok(Surface::from(
                context
                    .global_resources
                    .fornjot_core
                    .layers
                    .topology
                    .surfaces
                    .xz_plane(),
            )
            .into())
        })
        .into_builtin_function()
        .into(),
    );

    context.stack.new_variable_str(
        "global_yz_plane",
        (|context: &mut ExecutionContext<S>, _span: &S| -> OperatorResult<S, Value<S>> {
            Ok(Surface::from(
                context
                    .global_resources
                    .fornjot_core
                    .layers
                    .topology
                    .surfaces
                    .yz_plane(),
            )
            .into())
        })
        .into_builtin_function()
        .into(),
    );

    // TODO surface from plane points.
}

#[derive(Clone)]
pub struct Surface {
    pub handle: Handle<FornjotSurface>,
}

impl<S: Span> Object<S> for Surface {
    fn matches_type(
        &self,
        ty: &VariableType<S>,
        _log: &mut dyn RuntimeLog<S>,
        _variable_name_span: &S,
    ) -> OperatorResult<S, bool> {
        Ok(matches!(ty, VariableType::Surface))
    }
}

handle_wrapper!(Surface, FornjotSurface);
