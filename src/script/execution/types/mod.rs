use std::{borrow::Cow, cmp::Ordering, fmt::Write};

use enum_dispatch::enum_dispatch;
use enum_downcast::{AsVariant, EnumDowncast};

use crate::script::{
    parsing::{Expression, Litteral, VariableType},
    LogMessage, RuntimeLog, Span,
};

use super::{ControlFlow, ExecutionContext, ExecutionResult};

mod none;
pub use none::NoneType;

mod default;
pub use default::Default;

mod boolean;
pub use boolean::Boolean;

mod number;
pub use number::Number;

pub mod function;
pub use function::{BuiltinFunction, UserFunction};

mod structures;
pub use structures::{StructDefinition, Structure};

mod list;
pub use list::List;

mod string;
pub use string::SString;

mod measurement;
pub use self::measurement::Measurement;
use self::string::formatting::Style;

mod range;
pub use range::Range;

mod serializable;
pub use serializable::SerializableValue;

fn unsupported_operation_message<'a, S: Span, R, O: Object<'a, S>>(
    object: &O,
    log: &mut RuntimeLog<S>,
    span: &S,
    operation_name: &'static str,
) -> ExecutionResult<'a, S, R> {
    log.push(LogMessage::UnsupportedOperation(
        span.clone(),
        object.type_name(),
        operation_name,
    ));
    Err(ControlFlow::Failure)
}

pub trait NamedObject {
    fn static_type_name() -> &'static str;
}

#[enum_dispatch]
pub trait Object<'a, S: Span>: Sized + Clone + NamedObject {
    fn matches_type(&self, ty: &VariableType<S>) -> bool;

    fn format(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        _f: &mut dyn Write,
        _style: Style,
        _precision: Option<u8>,
    ) -> ExecutionResult<'a, S, ()> {
        unsupported_operation_message(self, log, span, "format")
    }

    fn type_name(&self) -> Cow<'static, str> {
        Self::static_type_name().into()
    }

    fn and(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        _rhs: &Value<'a, S>,
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        unsupported_operation_message(self, log, span, "and")
    }
    fn or(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        _rhs: &Value<'a, S>,
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        unsupported_operation_message(self, log, span, "or")
    }
    fn cmp(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        _rhs: &Value<'a, S>,
    ) -> ExecutionResult<'a, S, Ordering> {
        unsupported_operation_message(self, log, span, "compare")
    }
    fn eq(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> ExecutionResult<'a, S, bool> {
        Ok(matches!(self.cmp(log, span, rhs)?, Ordering::Equal))
    }
    fn addition(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        _rhs: &Value<'a, S>,
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        unsupported_operation_message(self, log, span, "addition")
    }
    fn subtraction(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        _rhs: &Value<'a, S>,
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        unsupported_operation_message(self, log, span, "subtraction")
    }
    fn multiply(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        _rhs: &Value<'a, S>,
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        unsupported_operation_message(self, log, span, "multiply")
    }
    fn divide(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        _rhs: &Value<'a, S>,
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        unsupported_operation_message(self, log, span, "divide")
    }
    fn attribute(
        &self,
        log: &mut RuntimeLog<S>,
        _span: &S,
        attribute: &S,
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        log.push(LogMessage::UnknownAttribute(attribute.clone()));
        Err(ControlFlow::Failure)
    }
    fn call(
        &self,
        context: &mut ExecutionContext<'a, S>,
        span: &S,
        _arguments: Vec<Value<'a, S>>,
        _spans: &[Expression<S>],
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        unsupported_operation_message(self, &mut context.log, span, "call")
    }
    fn method_call(
        &self,
        log: &mut RuntimeLog<S>,
        _span: &S,
        attribute: &S,
        _arguments: Vec<Value<'a, S>>,
        _spans: &[Expression<S>],
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        log.push(LogMessage::UnknownAttribute(attribute.clone()));
        Err(ControlFlow::Failure)
    }
    fn index(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        _index: Value<'a, S>,
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        unsupported_operation_message(self, log, span, "index")
    }
    fn iterate(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
    ) -> ExecutionResult<'a, S, Box<dyn Iterator<Item = Value<'a, S>> + '_>> {
        unsupported_operation_message(self, log, span, "iterate")
    }
    fn unary_plus(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        unsupported_operation_message(self, log, span, "unary plus")
    }
    fn unary_minus(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        unsupported_operation_message(self, log, span, "unary minus")
    }
    fn unary_logical_not(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        unsupported_operation_message(self, log, span, "unary logical not")
    }

    fn export(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
    ) -> ExecutionResult<'a, S, SerializableValue> {
        unsupported_operation_message(self, log, span, "export")
    }
}

#[enum_dispatch(Object<S>, FormatArgument)]
#[derive(Debug, Clone, PartialEq, EnumDowncast)]
pub enum Value<'a, S: Span> {
    NoneType,
    Default,
    Boolean,
    Number,
    BuiltinFunction(BuiltinFunction<'a, S>),
    UserFunction(UserFunction<'a, S>),
    Structure(Structure<'a, S>),
    StructDefinition(StructDefinition<'a, S>),
    List(List<'a, S>),
    String(SString),
    Range(Range),
    Measurement(Measurement),
}

impl<'a, S: Span> NamedObject for Value<'a, S> {
    fn static_type_name() -> &'static str {
        "Value"
    }
}

impl<'a, S: Span> Value<'a, S> {
    pub fn downcast_ref<T>(&self, log: &mut RuntimeLog<S>, span: &S) -> ExecutionResult<'a, S, &T>
    where
        T: NamedObject,
        Self: AsVariant<T>,
    {
        if let Some(value) = self.enum_downcast_ref() {
            Ok(value)
        } else {
            log.push(LogMessage::ExpectedGot(
                span.clone(),
                T::static_type_name().into(),
                self.type_name(),
            ));
            Err(ControlFlow::Failure)
        }
    }

    pub fn downcast<T>(self, log: &mut RuntimeLog<S>, span: &S) -> ExecutionResult<'a, S, T>
    where
        T: NamedObject,
        Self: TryInto<T>,
    {
        let type_name = self.type_name();

        match self.try_into() {
            Ok(value) => Ok(value),
            Err(_original) => {
                log.push(LogMessage::ExpectedGot(
                    span.clone(),
                    T::static_type_name().into(),
                    type_name,
                ));
                Err(ControlFlow::Failure)
            }
        }
    }

    pub fn from_litteral(
        context: &mut ExecutionContext<'a, S>,
        value: &Litteral<S>,
    ) -> ExecutionResult<'a, S, Self> {
        match value {
            Litteral::Measurement(measurement) => {
                Measurement::from_parsed(&mut context.log, measurement)
            }
            Litteral::Number(number) => number::from_parsed(&mut context.log, number),
            Litteral::String(string) => SString::from_parsed(string),
            Litteral::List(list) => List::from_parsed(context, list),
            Litteral::Boolean(_span, value) => Ok(Self::Boolean(*value)),
            Litteral::Default(_span) => Ok(Default.into()),
        }
    }
}
