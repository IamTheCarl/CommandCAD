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

use common_data_types::Number;
use imstr::ImString;
use std::{cell::RefCell, cmp::Ordering, fmt::Write};

use crate::script::{
    execution::{ExecutionContext, Failure},
    logging::RuntimeLog,
    parsing::{self, Expression, VariableType},
    Span,
};

use super::{
    function::AutoCall, number::UnwrapNotNan, serializable::SerializableValue, NamedObject,
    NoneType, Object, OperatorResult, Range, Scalar, UnwrapBorrowFailure, Value,
};

pub mod formatting;
use formatting::{Style, UnsupportedMessage, UnwrapFormattingResult};

static ESCAPE_SEQUENCES: &[(&str, &str)] = &[("\\\"", "\""), ("\\n", "\n"), ("\\\\", "\\")];

#[derive(Debug, Clone, PartialEq)]
pub struct SString {
    string: RefCell<ImString>,
}

impl<S: Span> Object<S> for SString {
    fn matches_type(
        &self,
        ty: &VariableType<S>,
        _log: &mut dyn RuntimeLog<S>,
        _variable_name_span: &S,
    ) -> OperatorResult<S, bool> {
        Ok(matches!(ty, VariableType::String))
    }

    fn format(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        f: &mut dyn Write,
        style: Style,
        precision: Option<u8>,
    ) -> OperatorResult<S, ()> {
        let string = self.string.try_borrow().unwrap_borrow_failure(span)?;

        match (style, precision) {
            (Style::Default, None) => write!(f, "{}", string).unwrap_formatting_result(span),
            (Style::Debug, None) => {
                let mut sequence_iter = ESCAPE_SEQUENCES.iter();
                let (replace, find) = sequence_iter.next().unwrap(); // Should never fail since we static initalized that array.

                let mut to_print = string.replace(find, replace);

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
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        index: Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        let range = index.downcast::<Range>(span)?;
        let string = self.string.try_borrow().unwrap_borrow_failure(span)?;

        // TODO could we keep an immutable reference to the original string to avoid a copy?
        let slice = match (
            range.lower_bound,
            range.upper_bound,
            range.upper_bound_is_inclusive,
        ) {
            (None, None, false) => string.get(..).ok_or((None, None)),
            (Some(lower_bound), None, false) => {
                let signed_lower_bound = lower_bound.to_index(span)?;
                let lower_bound = self.internalize_index(span, signed_lower_bound)?;
                string
                    .get(lower_bound..)
                    .ok_or((Some(signed_lower_bound), None))
            }
            (None, Some(upper_bound), false) => {
                let signed_upper_bound = upper_bound.to_index(span)?;
                let upper_bound = self.internalize_index(span, signed_upper_bound)?;
                string
                    .get(..upper_bound)
                    .ok_or((None, Some(signed_upper_bound)))
            }
            (None, Some(upper_bound), true) => {
                let signed_upper_bound = upper_bound.to_index(span)?;
                let upper_bound = self.internalize_index(span, signed_upper_bound)?;
                string
                    .get(..=upper_bound)
                    .ok_or((None, Some(signed_upper_bound)))
            }
            (Some(lower_bound), Some(upper_bound), false) => {
                let signed_lower_bound = lower_bound.to_index(span)?;
                let lower_bound = self.internalize_index(span, signed_lower_bound)?;
                let signed_upper_bound = upper_bound.to_index(span)?;
                let upper_bound = self.internalize_index(span, signed_upper_bound)?;
                string
                    .get(lower_bound..upper_bound)
                    .ok_or((Some(signed_lower_bound), Some(signed_upper_bound)))
            }
            (Some(lower_bound), Some(upper_bound), true) => {
                let signed_lower_bound = lower_bound.to_index(span)?;
                let lower_bound = self.internalize_index(span, signed_lower_bound)?;
                let signed_upper_bound = upper_bound.to_index(span)?;
                let upper_bound = self.internalize_index(span, signed_upper_bound)?;
                string
                    .get(lower_bound..=upper_bound)
                    .ok_or((Some(signed_lower_bound), Some(signed_upper_bound)))
            }
            (_, None, true) => unreachable!(), // Inclusive ranges without an upper bound are illegal to construct.
        };

        // TODO List has an identical error handling. We should probably move this to a common library.
        let range_type = if range.upper_bound_is_inclusive {
            "..="
        } else {
            ".."
        };

        slice
            .map(|slice| Self::from(slice.to_string()).into())
            .map_err(|(lower_bound, upper_bound)| {
                Failure::SliceOutOfRange(span.clone(), lower_bound, range_type, upper_bound)
            })
    }

    fn cmp(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        rhs: &Value<S>,
    ) -> OperatorResult<S, Ordering> {
        let rhs = rhs.downcast_ref::<Self>(span)?;

        Ok(self.string.cmp(&rhs.string))
    }

    fn addition(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        rhs: &Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        let mut string = self
            .string
            .try_borrow()
            .unwrap_borrow_failure(span)?
            .clone()
            .into_std_string();

        match rhs {
            Value::String(rhs) => {
                string.push_str(&rhs.as_str(span)?);

                Ok(Self::from(string).into())
            }
            Value::Scalar(rhs) => {
                // convert numbers to strings.
                let rhs = rhs.to_number(span)?;

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
        context: &mut ExecutionContext<S>,
        span: &S,
        attribute: &S,
        arguments: Vec<Value<S>>,
        expressions: &[Expression<S>],
    ) -> OperatorResult<S, Value<S>> {
        match attribute.as_str() {
            "insert" => |_context: &mut ExecutionContext<S>,
                         span: &S,
                         index: Scalar,
                         text: SString|
             -> OperatorResult<S, Value<S>> {
                let index = index.to_index(span)?;
                let index = self.internalize_index(span, index)?;

                self.string
                    .try_borrow_mut()
                    .unwrap_borrow_failure(span)?
                    .insert_str(index, &text.as_str(span)?);
                Ok(NoneType.into())
            }
            .auto_call(context, span, arguments, expressions), // insert_str
            "is_empty" => {
                |_context: &mut ExecutionContext<S>, _span: &S| -> OperatorResult<S, Value<S>> {
                    Ok(self
                        .string
                        .try_borrow()
                        .unwrap_borrow_failure(span)?
                        .is_empty()
                        .into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "len" => {
                |_context: &mut ExecutionContext<S>, span: &S| -> OperatorResult<S, Value<S>> {
                    Number::new(self.string.try_borrow().unwrap_borrow_failure(span)?.len() as f64)
                        .unwrap_not_nan(span)
                        .map(|n| n.into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "format" => {
                let string = self.string.try_borrow().unwrap_borrow_failure(span)?;
                match formatting::Format::parse(string.as_str()) {
                    Ok((_, format)) => {
                        let mut output = String::new();
                        format.format(context.log, span, &mut output, &arguments)?;

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

    fn export(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
    ) -> OperatorResult<S, SerializableValue> {
        let string = self.to_string(span)?;
        Ok(SerializableValue::String(string))
    }
}

pub struct StrRef<'a> {
    reference: std::cell::Ref<'a, ImString>,
}

impl<'a> std::ops::Deref for StrRef<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.reference.as_str()
    }
}

impl SString {
    pub fn from_parsed<S: Span>(parsed: &parsing::PString<S>) -> OperatorResult<S, Value<S>> {
        let mut sequence_iter = ESCAPE_SEQUENCES.iter();
        let (find, replace) = sequence_iter.next().unwrap(); // Should never fail since we static initalized that array.

        let mut string = parsed.value.as_str().replace(find, replace);

        for (find, replace) in sequence_iter {
            string = string.replace(find, replace);
        }

        let string = RefCell::new(string.into());
        Ok(Self { string }.into())
    }

    pub fn as_str<'a, S: Span>(&'a self, span: &S) -> OperatorResult<S, StrRef<'a>> {
        let reference = self.string.try_borrow().unwrap_borrow_failure(span)?;
        let string_reference = StrRef { reference };

        Ok(string_reference)
    }

    pub fn to_string<S: Span>(&self, span: &S) -> OperatorResult<S, String> {
        Ok(self
            .string
            .try_borrow()
            .unwrap_borrow_failure(span)?
            .clone()
            .into_std_string())
    }

    fn internalize_index<S: Span>(&self, span: &S, index: isize) -> OperatorResult<S, usize> {
        let string = self.string.try_borrow().unwrap_borrow_failure(span)?;

        let new_index = if index >= 0 {
            Ok(index as usize)
        } else if let Some(index) = string.len().checked_sub(index.unsigned_abs()) {
            Ok(index)
        } else {
            Err(Failure::IndexOutOfRange(span.clone(), index))
        }?;

        if new_index >= string.len() {
            Err(Failure::IndexOutOfRange(span.clone(), index))
        } else if string.is_char_boundary(new_index) {
            Ok(new_index)
        } else if new_index < string.len() {
            Err(Failure::InvalidCharIndex(span.clone(), new_index as isize))
        } else {
            Err(Failure::IndexOutOfRange(span.clone(), new_index as isize))
        }
    }
}

impl NamedObject for SString {
    fn static_type_name() -> &'static str {
        "String"
    }
}

impl<S> From<S> for SString
where
    S: Into<ImString>,
{
    fn from(value: S) -> Self {
        Self {
            string: RefCell::new(value.into()),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::script::{
        execution::{expressions::run_expression, ExecutionContext},
        Runtime,
    };

    use super::*;

    #[test]
    fn string_concat() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(
                    context,
                    &Expression::parse("\"test\" + \"test\" == \"testtest\"")
                        .unwrap()
                        .1
                ),
                Ok(true.into())
            );

            assert_eq!(
                run_expression(
                    context,
                    &Expression::parse("\"test\" + 5 == \"test5\"").unwrap().1
                ),
                Ok(true.into())
            );
        });
    }
}
