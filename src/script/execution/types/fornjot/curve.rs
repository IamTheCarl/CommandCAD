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

use fj_core::{storage::Handle, topology::Curve as FornjotCurve};

use crate::script::{
    execution::{types::Object, ExecutionContext},
    parsing::VariableType,
    Span,
};

use super::handle_wrapper;

pub fn register_globals<S: Span>(_context: &mut ExecutionContext<S>) {
    // TODO we should have the power to build faces from surfaces and regions.
}

#[derive(Clone)]
pub struct Curve {
    pub handle: Handle<FornjotCurve>,
}

impl<'a, S: Span> Object<'a, S> for Curve {
    fn matches_type(&self, ty: &VariableType<S>) -> bool {
        matches!(ty, VariableType::Face)
    }
}

handle_wrapper!(Curve, FornjotCurve);
