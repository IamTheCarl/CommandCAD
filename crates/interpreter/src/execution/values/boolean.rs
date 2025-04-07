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

use crate::{compile::SourceReference, execution::logging::RuntimeLog};

use super::{value_type::ValueType, ExpressionResult, Object, StaticTypeName, Value};

#[derive(Debug, Hash, Clone, Eq, PartialEq)]
pub struct Boolean(pub bool);

impl Object for Boolean {
    fn get_type(&self) -> ValueType {
        ValueType::Boolean
    }

    fn eq(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: &Value,
    ) -> ExpressionResult<bool> {
        let rhs: &Self = rhs.downcast_ref(stack_trace)?;
        Ok(self.0 == rhs.0)
    }
    fn and(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: &Value,
    ) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_ref(stack_trace)?;
        Ok(Self(self.0 && rhs.0).into())
    }

    fn or(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: &Value,
    ) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_ref(stack_trace)?;
        Ok(Self(self.0 || rhs.0).into())
    }

    fn xor(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: Value,
    ) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_ref(stack_trace)?;
        Ok(Self((self.0 && rhs.0) || (!self.0 && !rhs.0)).into())
    }

    fn unary_not(
        &self,
        _log: &mut dyn RuntimeLog,
        _stack_trace: &[SourceReference],
    ) -> ExpressionResult<Value> {
        Ok(Self(!self.0).into())
    }
}

impl StaticTypeName for Boolean {
    fn static_type_name() -> &'static str {
        "Boolean"
    }
}
