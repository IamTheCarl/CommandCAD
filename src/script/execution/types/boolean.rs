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

use std::fmt::Write;

use crate::script::{logging::RuntimeLog, parsing::VariableType, Span};

use super::{
    serializable::SerializableValue,
    string::formatting::{Style, UnsupportedMessage, UnwrapFormattingResult},
    NamedObject, Object, OperatorResult, TypedObject, Value,
};

pub type Boolean = bool;

impl<S: Span> Object<S> for Boolean {
    fn matches_type(
        &self,
        ty: &VariableType<S>,
        _log: &mut dyn RuntimeLog<S>,
        _variable_name_span: &S,
    ) -> OperatorResult<S, bool> {
        Ok(matches!(ty, VariableType::Boolean))
    }

    fn format(
        &self,
        _context: &mut dyn RuntimeLog<S>,
        span: &S,
        f: &mut dyn Write,
        style: Style,
        precision: Option<u8>,
    ) -> OperatorResult<S, ()> {
        match (style, precision) {
            (Style::Default | Style::Debug, None) => {
                write!(f, "{}", self).unwrap_formatting_result(span)
            }
            (_, None) => style.unsupported_message(self, span),
            (Style::Default | Style::Debug, _) => style.unsupported_message(self, span),
            _ => {
                style.unsupported_message(self, span).ok();
                precision.unsupported_message(self, span)
            }
        }
    }

    fn eq(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        rhs: &Value<S>,
    ) -> OperatorResult<S, bool> {
        let rhs = rhs.downcast_ref::<Boolean>(span)?;
        Ok(*self == *rhs)
    }

    fn and(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        rhs: &Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        let rhs = rhs.downcast_ref(span)?;
        Ok((*self && *rhs).into())
    }

    fn or(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        rhs: &Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        let rhs = rhs.downcast_ref(span)?;
        Ok((*self || *rhs).into())
    }

    fn unary_logical_not(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        _span: &S,
    ) -> OperatorResult<S, Value<S>> {
        Ok((!self).into())
    }

    fn export(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        _span: &S,
    ) -> OperatorResult<S, SerializableValue> {
        Ok(SerializableValue::Boolean(*self))
    }
}

impl TypedObject for Boolean {
    fn get_type<S: Span>() -> VariableType<S> {
        VariableType::Boolean
    }
}

impl NamedObject for Boolean {
    fn static_type_name() -> &'static str {
        "Boolean"
    }
}
