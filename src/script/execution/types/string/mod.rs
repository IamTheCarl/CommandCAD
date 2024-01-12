use std::{cmp::Ordering, fmt::Write, rc::Rc};

use crate::script::{
    execution::{types::Number, ControlFlow, ExecutionResult},
    parsing::{self, Expression, VariableType},
    LogMessage, RuntimeLog, Span,
};

use super::{
    function::AutoCall, number::UnwrapNotNan, serializable::SerializableValue, NamedObject, Object,
    Range, Value,
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
        log: &mut RuntimeLog<S>,
        span: &S,
        f: &mut dyn Write,
        style: Style,
        precision: Option<u8>,
    ) -> ExecutionResult<'a, S, ()> {
        match (style, precision) {
            (Style::Default, None) => {
                write!(f, "{}", self.string).unwrap_formatting_result(log, span)
            }
            (Style::Debug, None) => {
                let mut sequence_iter = ESCAPE_SEQUENCES.iter();
                let (replace, find) = sequence_iter.next().unwrap(); // Should never fail since we static initalized that array.

                let mut to_print = self.string.replace(find, replace);

                for (replace, find) in sequence_iter {
                    to_print = to_print.replace(find, replace);
                }

                write!(f, "\"{}\"", to_print).unwrap_formatting_result(log, span)
            }
            (_, None) => style.unsupported_message(self, log, span),
            (Style::Default | Style::Debug, _) => style.unsupported_message(self, log, span),
            _ => {
                style.unsupported_message(self, log, span).ok();
                precision.unsupported_message(self, log, span)
            }
        }
    }

    fn index(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        index: Value<'a, S>,
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        let range = index.downcast_ref::<Range>(log, span)?;

        // TODO could we keep an immutable reference to the original string to avoid a copy?
        let slice = match (
            range.lower_bound,
            range.upper_bound,
            range.upper_bound_is_inclusive,
        ) {
            (None, None, false) => self.string.get(..),
            (Some(lower_bound), None, false) => {
                let lower_bound = self.internalize_index(log, span, lower_bound)?;
                self.string.get(lower_bound..)
            }
            (None, Some(upper_bound), false) => {
                let upper_bound = self.internalize_index(log, span, upper_bound)?;
                self.string.get(..upper_bound)
            }
            (None, Some(upper_bound), true) => {
                let upper_bound = self.internalize_index(log, span, upper_bound)?;
                self.string.get(..=upper_bound)
            }
            (Some(lower_bound), Some(upper_bound), false) => {
                let lower_bound = self.internalize_index(log, span, lower_bound)?;
                let upper_bound = self.internalize_index(log, span, upper_bound)?;
                self.string.get(lower_bound..upper_bound)
            }
            (Some(lower_bound), Some(upper_bound), true) => {
                let lower_bound = self.internalize_index(log, span, lower_bound)?;
                let upper_bound = self.internalize_index(log, span, upper_bound)?;
                self.string.get(lower_bound..=upper_bound)
            }
            (_, None, true) => unreachable!(), // Inclusive ranges without an upper bound are illegal to construct.
        };

        slice
            .map(|slice| Self::from(slice).into())
            .ok_or(ControlFlow::Failure)
    }

    fn cmp(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> ExecutionResult<'a, S, Ordering> {
        let rhs = rhs.downcast_ref::<Self>(log, span)?;

        Ok(self.string.cmp(&rhs.string))
    }

    fn addition(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
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
            _ => {
                log.push(LogMessage::ExpectedGot(
                    span.clone(),
                    "string or number".into(),
                    rhs.type_name(),
                ));
                Err(ControlFlow::Failure)
            }
        }
    }

    fn method_call(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        attribute: &S,
        arguments: Vec<Value<'a, S>>,
        expressions: &[Expression<S>],
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        match attribute.as_str() {
            "insert" => |log: &mut RuntimeLog<S>,
                         span: &S,
                         index: Number,
                         text: SString|
             -> ExecutionResult<S, Value<S>> {
                let mut string = self.unwrap_or_clone();

                let index = self.internalize_index(log, span, index)?;

                string.insert_str(index, text.as_str());
                Ok(Self::from(string).into())
            }
            .auto_call(log, span, arguments, expressions), // insert_str
            "is_empty" => |_log: &mut RuntimeLog<S>, _span: &S| -> ExecutionResult<S, Value<S>> {
                Ok(self.string.is_empty().into())
            }
            .auto_call(log, span, arguments, expressions),
            "len" => |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<S, Value<S>> {
                Number::new(self.string.len() as f64).unwrap_not_nan(log, span)
            }
            .auto_call(log, span, arguments, expressions),
            "format" => {
                match formatting::Format::parse((*self.string).as_ref()) {
                    Ok((_, format)) => {
                        let mut output = String::new();
                        format.format(log, span, &mut output, &arguments)?;

                        Ok(Self::from(output).into())
                    }
                    Err(_error) => {
                        // TODO Better context would be appreciated here.
                        log.push(LogMessage::ParseFormatter(span.clone()));
                        Err(ControlFlow::Failure)
                    }
                }
            }
            // "lines" => todo!(), // TODO when we have iterators.
            _ => {
                log.push(LogMessage::UnknownAttribute(attribute.clone()));
                Err(ControlFlow::Failure)
            }
        }
    }

    fn export(
        &self,
        _log: &mut RuntimeLog<S>,
        _span: &S,
    ) -> ExecutionResult<'a, S, SerializableValue> {
        Ok(SerializableValue::String(String::clone(&self.string)))
    }
}

impl SString {
    pub fn from_parsed<'a, S: Span>(
        parsed: &parsing::PString<S>,
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
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

    fn internalize_index<'a, S: Span>(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        index: Number,
    ) -> ExecutionResult<'a, S, usize> {
        let raw_index = index.trunc() as isize;

        let index = if raw_index >= 0 {
            Ok(raw_index as usize)
        } else if let Some(index) = self.string.len().checked_sub(raw_index.unsigned_abs()) {
            Ok(index)
        } else {
            log.push(LogMessage::IndexOutOfRange(span.clone(), raw_index));
            Err(ControlFlow::Failure)
        }?;

        if index >= self.string.len() {
            log.push(LogMessage::IndexOutOfRange(span.clone(), raw_index));
            Err(ControlFlow::Failure)
        } else if self.string.is_char_boundary(index) {
            Ok(index)
        } else {
            if index < self.string.len() {
                log.push(LogMessage::InvalidCharIndex(span.clone(), index as isize));
            } else {
                log.push(LogMessage::IndexOutOfRange(span.clone(), index as isize));
            }
            Err(ControlFlow::Failure)
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
        let mut context = ExecutionContext {
            log: Default::default(),
            stack: Default::default(),
        };

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
