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
    operations::{
        holes::{AddHole, HoleLocation},
        insert::Insert,
        update::UpdateShell,
    },
    storage::Handle,
    topology::Shell as FornjotShell,
};

use crate::script::{
    execution::{
        types::{function::AutoCall, List, Object, OperatorResult, Value, Vector2, Vector3},
        ExecutionContext, Failure,
    },
    logging::RuntimeLog,
    parsing::{Expression, VariableType},
    Scalar, Span,
};

use super::{face::Face, handle_wrapper, object_set::check_for_duplicates};

pub fn register_globals<S: Span>(_context: &mut ExecutionContext<S>) {
    // TODO we should have the power to build shells from faces.
}

#[derive(Clone)]
pub struct Shell {
    pub handle: Handle<FornjotShell>,
}

impl<S: Span> Object<S> for Shell {
    fn matches_type(
        &self,
        ty: &VariableType<S>,
        _log: &mut dyn RuntimeLog<S>,
        _variable_name_span: &S,
    ) -> OperatorResult<S, bool> {
        Ok(matches!(ty, VariableType::Shell))
    }

    fn attribute(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        _span: &S,
        attribute: &S,
    ) -> OperatorResult<S, Value<S>> {
        match attribute.as_str() {
            "faces" => Ok(Value::from_object_set(self.handle.faces())),
            _ => Err(Failure::UnknownAttribute(attribute.clone())),
        }
    }

    fn method_call(
        &self,
        context: &mut ExecutionContext<S>,
        span: &S,
        attribute: &S,
        arguments: Vec<Value<S>>,
        spans: &[Expression<S>],
    ) -> OperatorResult<S, Value<S>> {
        match attribute.as_str() {
            "add_blind_hole" => |context: &mut ExecutionContext<S>,
                                 span: &S,
                                 face: Face,
                                 position: Vector2,
                                 radius: Scalar,
                                 path: Vector3|
             -> OperatorResult<S, Value<S>> {
                if !self.handle.deref().faces().contains(&face.handle) {
                    return Err(Failure::FaceNotInShell(span.clone()));
                }

                let position = position.as_fornjot_point(context, span)?;
                let radius = radius.as_scalar(context, span)?;

                let path = path.as_fornjot_vector(context, span)?;

                let new_shell = self.handle.deref().add_blind_hole(
                    HoleLocation {
                        face: &face.handle,
                        position,
                    },
                    radius,
                    path,
                    &mut context.global_resources.fornjot_core,
                );

                let new_shell =
                    Self::from(new_shell.insert(&mut context.global_resources.fornjot_core));
                context.unpack_validation_warnings(span);

                Ok(new_shell.into())
            }
            .auto_call(context, span, arguments, spans),
            "add_through_hole" => |context: &mut ExecutionContext<S>,
                                   span: &S,
                                   front_face: Face,
                                   front_position: Vector2,

                                   back_face: Face,
                                   back_position: Vector2,

                                   radius: Scalar|
             -> OperatorResult<S, Value<S>> {
                let faces = self.handle.deref().faces();
                if !faces.contains(&front_face.handle) || !faces.contains(&front_face.handle) {
                    return Err(Failure::FaceNotInShell(span.clone()));
                }

                let front_position = front_position.as_fornjot_point(context, span)?;
                let back_position = back_position.as_fornjot_point(context, span)?;

                let radius = radius.as_scalar(context, span)?;

                let new_shell = self.handle.deref().add_through_hole(
                    [
                        HoleLocation {
                            face: &front_face.handle,
                            position: front_position,
                        },
                        HoleLocation {
                            face: &back_face.handle,
                            position: back_position,
                        },
                    ],
                    radius,
                    &mut context.global_resources.fornjot_core,
                );

                let new_shell =
                    Self::from(new_shell.insert(&mut context.global_resources.fornjot_core));
                context.unpack_validation_warnings(span);

                Ok(new_shell.into())
            }
            .auto_call(context, span, arguments, spans),
            "update_face" => |context: &mut ExecutionContext<S>,
                              span: &S,
                              face: Face,
                              update: Value<S>|
             -> OperatorResult<S, Value<S>> {
                // Update shell will panic if the shell isn't found in the solid, so check that it's in there.
                if !self.handle.deref().faces().contains(&face.handle) {
                    return Err(Failure::FaceNotInShell(span.clone()));
                }

                // Due to borrowing issues, we have to run the update call before we go into
                // the update function.
                let new_faces = update.call(context, span, vec![face.clone().into()], &[])?;
                let new_faces = new_faces.downcast::<List<S>>(span)?;
                let num_faces = new_faces.len(span)?;
                let new_faces = new_faces
                    .unpack_dynamic_length::<Face>(span)?
                    .map(|shell| shell.handle);

                // Update shell will panic if we insert a duplicate, so deduplicate it.
                let new_faces = check_for_duplicates(span, num_faces, new_faces)?;

                let new_shell = self.handle.deref().update_face(
                    &face.handle,
                    |_face, _core| new_faces.into_iter(),
                    &mut context.global_resources.fornjot_core,
                );

                let new_shell =
                    Self::from(new_shell.insert(&mut context.global_resources.fornjot_core));
                context.unpack_validation_warnings(span);

                Ok(new_shell.into())
            }
            .auto_call(context, span, arguments, spans),
            "add_faces" => {
                |context: &mut ExecutionContext<S>,
                 span: &S,
                 new_faces: List<S>|
                 -> OperatorResult<S, Value<S>> {
                    let num_faces = new_faces.len(span)?;
                    let new_faces = new_faces
                        .unpack_dynamic_length::<Face>(span)?
                        .map(|shell| shell.handle);

                    // Update shell will panic if we insert a duplicate, so deduplicate it.
                    let new_faces = check_for_duplicates(span, num_faces, new_faces)?;

                    let new_shell = self
                        .handle
                        .deref()
                        .add_faces(new_faces, &mut context.global_resources.fornjot_core);

                    let new_shell =
                        Self::from(new_shell.insert(&mut context.global_resources.fornjot_core));
                    context.unpack_validation_warnings(span);

                    Ok(new_shell.into())
                }
                .auto_call(context, span, arguments, spans)
            }
            _ => Err(Failure::UnknownAttribute(attribute.clone())),
        }
    }
}

handle_wrapper!(Shell, FornjotShell);

// TODO test that the hole creating functions correctly prevent non-existing faces from being used.
// TODO test adding duplicate faces to the shell (through update and add_faces)
// TODO test updating a face that did not exist in the shell
// TODO test blind hole
// TODO test through hole
