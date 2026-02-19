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

use crate::{
    execution::{
        logging::{LogLevel, LogMessage},
        values::{string::formatting::Style, StaticType},
        ExecutionContext,
    },
    values::Value,
};

use super::{value_type::ValueType, Object, StaticTypeName};

use std::borrow::Cow;

#[derive(Debug, Hash, Clone, Copy, Eq, PartialEq)]
pub struct ValueNone;

impl Object for ValueNone {
    fn get_type(&self, _context: &ExecutionContext) -> ValueType {
        ValueType::TypeNone
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
                message: "None values only support default formatting".into(),
            });
        }

        if precision.is_some() {
            context.log.push_message(LogMessage {
                origin: context.stack_trace.bottom().clone(),
                level: LogLevel::Warning,
                message: "None values cannot be formatted with precision".into(),
            });
        }

        write!(f, "None")
    }

    fn eq(self, _context: &ExecutionContext, rhs: Value) -> crate::ExpressionResult<bool> {
        Ok(matches!(rhs, Value::ValueNone(_)))
    }
}

impl StaticTypeName for ValueNone {
    fn static_type_name() -> Cow<'static, str> {
        "None".into()
    }
}

impl StaticType for ValueNone {
    fn static_type() -> ValueType {
        ValueType::TypeNone
    }
}

#[cfg(test)]
mod test {
    use crate::execution::{test_run, values};

    #[test]
    fn format() {
        let product = test_run("\"{value}\"::format(value = std.consts.None) == \"None\"").unwrap();
        assert_eq!(product, values::Boolean(true).into());
    }
}
