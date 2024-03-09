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

use fj_core::{objects::Face as FornjotFace, storage::Handle};

use crate::script::{
    execution::{
        types::{function::AutoCall, NamedObject, Object, OperatorResult, UserFunction, Value},
        ExecutionContext, Failure,
    },
    logging::RuntimeLog,
    parsing::{Expression, VariableType},
    Span,
};

use super::handle_wrapper;

pub fn register_globals<S: Span>(_context: &mut ExecutionContext<S>) {
    // TODO we should have the power to build faces from surfaces and regions.
}

#[derive(Clone)]
pub struct Face {
    pub handle: Handle<FornjotFace>,
}

impl<'a, S: Span> Object<'a, S> for Face {
    fn matches_type(&self, ty: &VariableType<S>) -> bool {
        matches!(ty, VariableType::Face)
    }

    fn attribute(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        _span: &S,
        attribute: &S,
    ) -> OperatorResult<S, Value<'a, S>> {
        match attribute.as_str() {
            // "regions" => Ok(self.handle.faces().into()),
            _ => Err(Failure::UnknownAttribute(attribute.clone())),
        }
    }

    fn method_call(
        &self,
        context: &mut ExecutionContext<'a, S>,
        span: &S,
        attribute: &S,
        arguments: Vec<Value<'a, S>>,
        spans: &[Expression<S>],
    ) -> OperatorResult<S, Value<'a, S>> {
        match attribute.as_str() {
            // "update_region" => |context: &mut ExecutionContext<'a, S>,
            //                    span: &S,
            //                    shell: Shell,
            //                    update: UserFunction<'a, S>|
            //  -> OperatorResult<S, Value<S>> {
            //     // Update shell will panic if the shell isn't found in the solid, so check that it's in there.
            //     if !self.handle.deref().shells().contains(&shell.handle) {
            //         return Err(Failure::ShellNotInSolid(span.clone()));
            //     }

            //     // Due to borrowing issues, we have to run the update call before we go into
            //     // the update function.
            //     let new_shells = update.call(context, span, vec![shell.clone().into()], &[])?;
            //     let new_shells = new_shells.downcast::<List<S>>(span)?;
            //     let num_shells = new_shells.len();
            //     let new_shells = unpack_dynamic_length_list::<S, Shell>(span, new_shells)?
            //         .map(|shell| HandleWrapper::from(shell.handle));

            //     // Update shell will panic if we insert a duplicate, so deduplicate it.
            //     let new_shells = check_for_duplicates(span, num_shells, new_shells)?;

            //     let new_solid = self.handle.deref().update_shell(
            //         &shell.handle,
            //         |_shell, _core| new_shells.into_iter().map(|h| h.0),
            //         &mut context.global_resources.fornjot_core,
            //     );

            //     Ok(Self::from(new_solid.insert(&mut context.global_resources.fornjot_core)).into())
            // }
            // .auto_call(context, span, arguments, spans),
            // "add_regions" => {
            //     |context: &mut ExecutionContext<'a, S>,
            //      span: &S,
            //      new_shells: List<'a, S>|
            //      -> OperatorResult<S, Value<S>> {
            //         let num_shells = new_shells.len();
            //         let new_shells = unpack_dynamic_length_list::<S, Shell>(span, new_shells)?
            //             .map(|shell| HandleWrapper::from(shell.handle));

            //         // Update shell will panic if we insert a duplicate, so deduplicate it.
            //         let new_shells = check_for_duplicates(span, num_shells, new_shells)?;

            //         let new_solid = self.handle.deref().add_shells(
            //             new_shells.into_iter().map(|h| h.0),
            //             &mut context.global_resources.fornjot_core,
            //         );

            //         Ok(
            //             Self::from(new_solid.insert(&mut context.global_resources.fornjot_core))
            //                 .into(),
            //         )
            //     }
            //     .auto_call(context, span, arguments, spans)
            // }
            _ => Err(Failure::UnknownAttribute(attribute.clone())),
        }
    }
}

handle_wrapper!(Face, FornjotFace);

// TODO test adding duplicate regions to the face (through update and add_regions)
// TODO test updating a region that did not exist in the face
