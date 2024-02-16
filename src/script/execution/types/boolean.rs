use std::fmt::Write;

use crate::script::{parsing::VariableType, RuntimeLog, Span};

use super::{
    serializable::SerializableValue,
    string::formatting::{Style, UnsupportedMessage, UnwrapFormattingResult},
    NamedObject, Object, OperatorResult, Value,
};

pub type Boolean = bool;

impl<'a, S: Span> Object<'a, S> for Boolean {
    fn matches_type(&self, ty: &VariableType<S>) -> bool {
        matches!(ty, VariableType::Boolean)
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
        _log: &mut RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> OperatorResult<S, bool> {
        let rhs = rhs.downcast_ref::<Boolean>(span)?;
        Ok(*self == *rhs)
    }

    fn and(
        &self,
        _log: &mut RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> OperatorResult<S, Value<'a, S>> {
        let rhs = rhs.downcast_ref(span)?;
        Ok((*self && *rhs).into())
    }

    fn or(
        &self,
        _log: &mut RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> OperatorResult<S, Value<'a, S>> {
        let rhs = rhs.downcast_ref(span)?;
        Ok((*self || *rhs).into())
    }

    fn unary_logical_not(
        &self,
        _log: &mut RuntimeLog<S>,
        _span: &S,
    ) -> OperatorResult<S, Value<'a, S>> {
        Ok((!(*self)).into())
    }

    fn export(&self, _log: &mut RuntimeLog<S>, _span: &S) -> OperatorResult<S, SerializableValue> {
        Ok(SerializableValue::Boolean(*self))
    }
}

impl NamedObject for Boolean {
    fn static_type_name() -> &'static str {
        "Boolean"
    }
}
