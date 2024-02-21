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

use std::{cmp::Ordering, fmt::Write, rc::Rc};

use crate::script::{
    execution::{types::Number, ExecutionContext, Failure},
    parsing::{self, Expression, VariableType},
    RuntimeLog, Span,
};

use super::{
    function::AutoCall, number::UnwrapNotNan, serializable::SerializableValue, NamedObject, Object,
    OperatorResult, Range, Value,
};

pub mod formatting;
use self::formatting::{Style, UnsupportedMessage, UnwrapFormattingResult};

static ESCAPE_SEQUENCES: &[(&str, &str)] = &[("\\\"", "\""), ("\\n", "\n"), ("\\\\", "\\")];

#[derive(Debug, Clone, PartialEq)]
pub struct SString {
    string: Rc<String>,
}

impl<'a, S: Span> Object<'a, S> for SString {
    fn matches_type(&self, ty: &VariableType<S>) -> bool {
        matches!(ty, VariableType::String)
    }

    fn format(
        &self,
        _log: &mut RuntimeLog<S>,
        span: &S,
        f: &mut dyn Write,
        style: Style,
        precision: Option<u8>,
    ) -> OperatorResult<S, ()> {
        match (style, precision) {
            (Style::Default, None) => write!(f, "{}", self.string).unwrap_formatting_result(span),
            (Style::Debug, None) => {
                let mut sequence_iter = ESCAPE_SEQUENCES.iter();
                let (replace, find) = sequence_iter.next().unwrap(); // Should never fail since we static initalized that array.

                let mut to_print = self.string.replace(find, replace);

                for (replace, find) in sequence_iter {
                    to_print = to_print.replace(find, replace);
                }

                write!(f, "\"{}\"", to_print).unwrap_formatting_result(span)
            }
            (_, None) => style.unsupported_message(self, span),
            (Style::Default | Style::Debug, _) => style.unsupported_message(self, span),
            _ => {
                style.unsupported_message(self, span).ok();
                precision.unsupported_message(self, span)
            }
        }
    }

    fn index(
        &self,
        _log: &mut RuntimeLog<S>,
        span: &S,
        index: Value<'a, S>,
    ) -> OperatorResult<S, Value<'a, S>> {
        let range = index.downcast_ref::<Range>(span)?;

        // TODO could we keep an immutable reference to the original string to avoid a copy?
        let slice = match (
            range.lower_bound,
            range.upper_bound,
            range.upper_bound_is_inclusive,
        ) {
            (None, None, false) => self.string.get(..),
            (Some(lower_bound), None, false) => {
                let lower_bound = self.internalize_index(span, lower_bound)?;
                self.string.get(lower_bound..)
            }
            (None, Some(upper_bound), false) => {
                let upper_bound = self.internalize_index(span, upper_bound)?;
                self.string.get(..upper_bound)
            }
            (None, Some(upper_bound), true) => {
                let upper_bound = self.internalize_index(span, upper_bound)?;
                self.string.get(..=upper_bound)
            }
            (Some(lower_bound), Some(upper_bound), false) => {
                let lower_bound = self.internalize_index(span, lower_bound)?;
                let upper_bound = self.internalize_index(span, upper_bound)?;
                self.string.get(lower_bound..upper_bound)
            }
            (Some(lower_bound), Some(upper_bound), true) => {
                let lower_bound = self.internalize_index(span, lower_bound)?;
                let upper_bound = self.internalize_index(span, upper_bound)?;
                self.string.get(lower_bound..=upper_bound)
            }
            (_, None, true) => unreachable!(), // Inclusive ranges without an upper bound are illegal to construct.
        };

        // TOOD String has an identical error handling. We should probably move this to a common library.
        let range_type = if range.upper_bound_is_inclusive {
            "..="
        } else {
            ".."
        };

        slice
            .map(|slice| Self::from(slice.to_string()).into())
            .ok_or(Failure::SliceOutOfRange(
                span.clone(),
                range
                    .lower_bound
                    .map(|bound| bound.into_inner().trunc() as isize),
                range_type,
                range
                    .upper_bound
                    .map(|bound| bound.into_inner().trunc() as isize),
            ))
    }

    fn cmp(
        &self,
        _log: &mut RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> OperatorResult<S, Ordering> {
        let rhs = rhs.downcast_ref::<Self>(span)?;

        Ok(self.string.cmp(&rhs.string))
    }

    fn addition(
        &self,
        _log: &mut RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> OperatorResult<S, Value<'a, S>> {
        match rhs {
            Value::String(rhs) => {
                let mut string = self.unwrap_or_clone();

                string.push_str(rhs.as_str());

                Ok(Self::from(string).into())
            }
            Value::Number(rhs) => {
                // convert numbers to strings.
                let mut string = self.unwrap_or_clone();

                string += &format!("{}", rhs.into_inner());

                Ok(Self::from(string).into())
            }
            _ => Err(Failure::ExpectedGot(
                span.clone(),
                "string or number".into(),
                rhs.type_name(),
            )),
        }
    }

    fn method_call(
        &self,
        context: &mut ExecutionContext<'a, S>,
        span: &S,
        attribute: &S,
        arguments: Vec<Value<'a, S>>,
        expressions: &[Expression<S>],
    ) -> OperatorResult<S, Value<'a, S>> {
        match attribute.as_str() {
            "insert" => |_context: &mut ExecutionContext<'a, S>,
                         span: &S,
                         index: Number,
                         text: SString|
             -> OperatorResult<S, Value<S>> {
                let mut string = self.unwrap_or_clone();

                let index = self.internalize_index(span, index)?;

                string.insert_str(index, text.as_str());
                Ok(Self::from(string).into())
            }
            .auto_call(context, span, arguments, expressions), // insert_str
            "is_empty" => {
                |_context: &mut ExecutionContext<'a, S>, _span: &S| -> OperatorResult<S, Value<S>> {
                    Ok(self.string.is_empty().into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "len" => {
                |_context: &mut ExecutionContext<'a, S>, span: &S| -> OperatorResult<S, Value<S>> {
                    Number::new(self.string.len() as f64).unwrap_not_nan(span)
                }
                .auto_call(context, span, arguments, expressions)
            }
            "format" => {
                match formatting::Format::parse((*self.string).as_ref()) {
                    Ok((_, format)) => {
                        let mut output = String::new();
                        format.format(&mut context.log, span, &mut output, &arguments)?;

                        Ok(Self::from(output).into())
                    }
                    Err(_error) => {
                        // TODO Better context would be appreciated here.
                        Err(Failure::ParseFormatter(span.clone()))
                    }
                }
            }
            // "lines" => todo!(), // TODO when we have iterators.
            _ => Err(Failure::UnknownAttribute(attribute.clone())),
        }
    }

    fn export(&self, _log: &mut RuntimeLog<S>, _span: &S) -> OperatorResult<S, SerializableValue> {
        Ok(SerializableValue::String(String::clone(&self.string)))
    }
}

impl SString {
    pub fn from_parsed<'a, S: Span>(
        parsed: &parsing::PString<S>,
    ) -> OperatorResult<S, Value<'a, S>> {
        let mut sequence_iter = ESCAPE_SEQUENCES.iter();
        let (find, replace) = sequence_iter.next().unwrap(); // Should never fail since we static initalized that array.

        let mut string = parsed.value.as_str().replace(find, replace);

        for (find, replace) in sequence_iter {
            string = string.replace(find, replace);
        }

        Ok(Self {
            string: Rc::new(string),
        }
        .into())
    }

    fn unwrap_or_clone(&self) -> String {
        // FIXME we should be consuming self, rather than cloaning string.
        Rc::try_unwrap(self.string.clone()).unwrap_or_else(|string| String::clone(&string))
    }

    pub fn as_str(&self) -> &str {
        self.string.as_ref()
    }

    pub fn into_string(self) -> String {
        self.unwrap_or_clone()
    }

    fn internalize_index<S: Span>(&self, span: &S, index: Number) -> OperatorResult<S, usize> {
        let raw_index = index.trunc() as isize;

        let index = if raw_index >= 0 {
            Ok(raw_index as usize)
        } else if let Some(index) = self.string.len().checked_sub(raw_index.unsigned_abs()) {
            Ok(index)
        } else {
            Err(Failure::IndexOutOfRange(span.clone(), raw_index))
        }?;

        if index >= self.string.len() {
            Err(Failure::IndexOutOfRange(span.clone(), raw_index))
        } else if self.string.is_char_boundary(index) {
            Ok(index)
        } else if index < self.string.len() {
            Err(Failure::InvalidCharIndex(span.clone(), index as isize))
        } else {
            Err(Failure::IndexOutOfRange(span.clone(), index as isize))
        }
    }
}

impl NamedObject for SString {
    fn static_type_name() -> &'static str {
        "String"
    }
}

impl From<String> for SString {
    fn from(value: String) -> Self {
        Self {
            string: Rc::new(value),
        }
    }
}

impl<'a> From<&'a str> for SString {
    fn from(value: &'a str) -> Self {
        Self::from(String::from(value))
    }
}

#[cfg(test)]
mod test {
    use crate::script::execution::{expressions::run_expression, ExecutionContext};

    use super::*;

    #[test]
    fn string_concat() {
        let mut context = ExecutionContext::default();

        assert_eq!(
            run_expression(
                &mut context,
                &Expression::parse("\"test\" + \"test\" == \"testtest\"")
                    .unwrap()
                    .1
            ),
            Ok(true.into())
        );

        assert_eq!(
            run_expression(
                &mut context,
                &Expression::parse("\"test\" + 5 == \"test5\"").unwrap().1
            ),
            Ok(true.into())
        );
    }
}
