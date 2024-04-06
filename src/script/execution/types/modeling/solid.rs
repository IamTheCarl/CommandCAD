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

use fj_core::{
    operations::{insert::Insert, update::UpdateSolid},
    storage::Handle,
    topology::Solid as FornjotSolid,
};

use crate::script::{
    execution::{
        types::{function::AutoCall, List, Object, OperatorResult, Value},
        ExecutionContext, Failure,
    },
    logging::RuntimeLog,
    parsing::VariableType,
    Span,
};

use super::{handle_wrapper, object_set::check_for_duplicates, shell::Shell};

pub fn register_globals<S: Span>(_context: &mut ExecutionContext<'_, S>) {}

#[derive(Clone)]
pub struct Solid {
    pub handle: Handle<FornjotSolid>,
}

impl<S: Span> Object<S> for Solid {
    fn matches_type(
        &self,
        ty: &VariableType<S>,
        _log: &mut dyn RuntimeLog<S>,
        _variable_name_span: &S,
    ) -> OperatorResult<S, bool> {
        Ok(matches!(ty, VariableType::Sketch))
    }

    fn attribute(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        _span: &S,
        attribute: &S,
    ) -> OperatorResult<S, Value<S>> {
        match attribute.as_str() {
            "shells" => Ok(Value::from_object_set(self.handle.shells()).into()),
            _ => Err(Failure::UnknownAttribute(attribute.clone())),
        }
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
            "update_shell" => |context: &mut ExecutionContext<S>,
                               span: &S,
                               shell: Shell,
                               update: Value<S>|
             -> OperatorResult<S, Value<S>> {
                // Update shell will panic if the shell isn't found in the solid, so check that it's in there.
                if !self.handle.deref().shells().contains(&shell.handle) {
                    return Err(Failure::ShellNotInSolid(span.clone()));
                }

                // Due to borrowing issues, we have to run the update call before we go into
                // the update function.
                let new_shells = update.call(context, span, vec![shell.clone().into()], &[])?;
                let new_shells = new_shells.downcast::<List<S>>(span)?;
                let num_shells = new_shells.len(span)?;
                let new_shells = new_shells
                    .unpack_dynamic_length::<Shell>(span)?
                    .map(|shell| shell.handle);

                // Update shell will panic if we insert a duplicate, so deduplicate it.
                let new_shells = check_for_duplicates(span, num_shells, new_shells)?;

                let new_solid = self.handle.deref().update_shell(
                    &shell.handle,
                    |_shell, _core| new_shells.into_iter(),
                    &mut context.global_resources.fornjot_core,
                );

                let new_solid =
                    Self::from(new_solid.insert(&mut context.global_resources.fornjot_core));
                context.unpack_validation_warnings(span);

                Ok(new_solid.into())
            }
            .auto_call(context, span, arguments, spans),
            "add_shells" => {
                |context: &mut ExecutionContext<S>,
                 span: &S,
                 new_shells: List<S>|
                 -> OperatorResult<S, Value<S>> {
                    let num_shells = new_shells.len(span)?;
                    let new_shells = new_shells
                        .unpack_dynamic_length::<Shell>(span)?
                        .map(|shell| shell.handle);

                    // Update shell will panic if we insert a duplicate, so deduplicate it.
                    let new_shells = check_for_duplicates(span, num_shells, new_shells)?;

                    let new_solid = self
                        .handle
                        .deref()
                        .add_shells(new_shells, &mut context.global_resources.fornjot_core);

                    let new_solid =
                        Self::from(new_solid.insert(&mut context.global_resources.fornjot_core));
                    context.unpack_validation_warnings(span);

                    Ok(new_solid.into())
                }
                .auto_call(context, span, arguments, spans)
            }
            _ => Err(Failure::UnknownAttribute(attribute.clone())),
        }
    }
}

handle_wrapper!(Solid, FornjotSolid);

// TODO test adding duplicate shells to the solid (through update and add_shells)
// TODO test updating a shell that did not exist in the solid
