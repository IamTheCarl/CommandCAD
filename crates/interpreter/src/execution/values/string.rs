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

use imstr::ImString;

use crate::{compile::SourceReference, execution::logging::RuntimeLog};

use super::{value_type::ValueType, ExpressionResult, Object, StaticTypeName, Value};

#[derive(Debug, Hash, Clone, Eq, PartialEq)]
pub struct IString(pub ImString);

impl Object for IString {
    fn get_type(&self) -> ValueType {
        ValueType::Boolean
    }

    fn eq(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: Value,
    ) -> ExpressionResult<bool> {
        let rhs: &Self = rhs.downcast_ref(stack_trace)?;
        Ok(self.0 == rhs.0)
    }

    // TODO we need to add a bunch of manipulation methods to this.
}

impl StaticTypeName for IString {
    fn static_type_name() -> &'static str {
        "String"
    }
}

impl<S> From<S> for IString
where
    S: Into<ImString>,
{
    fn from(value: S) -> Self {
        Self(value.into())
    }
}
