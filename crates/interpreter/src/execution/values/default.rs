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

use crate::execution::logging::{RuntimeLog, StackPoint};

use super::{value_type::VariableType, Object, OperatorResult, StaticTypeName};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct DefaultValue;

impl Object for DefaultValue {
    fn matches_type(
        &self,
        _ty: &VariableType,
        _log: &mut dyn RuntimeLog,
        _stack_trace: &[StackPoint],
    ) -> OperatorResult<bool> {
        Ok(false)
    }
}

impl StaticTypeName for DefaultValue {
    fn static_type_name() -> &'static str {
        "Default"
    }
}
