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

use crate::execution::{
    logging::{LogLevel, LogMessage},
    values::{string::formatting::Style, StaticType},
    ExecutionContext,
};

use super::{value_type::ValueType, ExpressionResult, Object, StaticTypeName, Value};

use std::borrow::Cow;

#[derive(Debug, Hash, Clone, Copy, Eq, PartialEq)]
pub struct Boolean(pub bool);

impl Object for Boolean {
    fn get_type(&self, _context: &ExecutionContext) -> ValueType {
        ValueType::Boolean
    }

    fn format(
        &self,
        context: &ExecutionContext,
        f: &mut dyn std::fmt::Write,
        style: Style,
        precision: Option<u8>,
    ) -> std::fmt::Result {
        if !matches!(style, Style::Default) {
            context.log.push_message(LogMessage {
                origin: context.stack_trace.bottom().clone(),
                level: LogLevel::Warning,
                message: "Boolean values only support default formatting".into(),
            });
        }

        if precision.is_some() {
            context.log.push_message(LogMessage {
                origin: context.stack_trace.bottom().clone(),
                level: LogLevel::Warning,
                message: "Boolean values cannot be formatted with precision".into(),
            });
        }

        write!(f, "{}", self.0)
    }

    fn eq(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<bool> {
        let rhs: &Self = rhs.downcast_for_binary_op_ref(context.stack_trace)?;
        Ok(self.0 == rhs.0)
    }
    fn and(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_for_binary_op_ref(context.stack_trace)?;
        Ok(Self(self.0 && rhs.0).into())
    }

    fn or(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_for_binary_op_ref(context.stack_trace)?;
        Ok(Self(self.0 || rhs.0).into())
    }

    fn xor(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_for_binary_op_ref(context.stack_trace)?;
        Ok(Self((self.0 && rhs.0) || (!self.0 && !rhs.0)).into())
    }

    fn unary_not(self, _context: &ExecutionContext) -> ExpressionResult<Value> {
        Ok(Self(!self.0).into())
    }
}

impl StaticTypeName for Boolean {
    fn static_type_name() -> Cow<'static, str> {
        "Boolean".into()
    }
}

impl StaticType for Boolean {
    fn static_type() -> ValueType {
        ValueType::Boolean
    }
}

#[cfg(test)]
mod test {
    use crate::execution::{test_run, values};

    #[test]
    fn format() {
        let product = test_run("\"{value}\"::format(value = true) == \"true\"").unwrap();
        assert_eq!(product, values::Boolean(true).into());

        let product = test_run("\"{value}\"::format(value = false) == \"false\"").unwrap();
        assert_eq!(product, values::Boolean(true).into());
    }
}
