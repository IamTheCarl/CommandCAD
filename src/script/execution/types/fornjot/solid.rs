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

use fj::core::{objects::Solid as FornjotSolid, storage::Handle};

use crate::script::{
    execution::{
        types::{NamedObject, Object},
        ExecutionContext,
    },
    parsing::VariableType,
    Span,
};

pub fn register_globals<S: Span>(_context: &mut ExecutionContext<'_, S>) {}

#[derive(Clone)]
pub struct Solid {
    pub handle: Handle<FornjotSolid>,
}

impl<'a, S: Span> Object<'a, S> for Solid {
    fn matches_type(&self, ty: &VariableType<S>) -> bool {
        matches!(ty, VariableType::Sketch)
    }

    // TODO we need a way to get the faces of the solid, and from the faces, get the surfaces.
}

impl NamedObject for Solid {
    fn static_type_name() -> &'static str {
        "Solid"
    }
}

impl From<Handle<FornjotSolid>> for Solid {
    fn from(handle: Handle<FornjotSolid>) -> Self {
        Self { handle }
    }
}

impl PartialEq for Solid {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl std::fmt::Debug for Solid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Solid").finish()
    }
}
