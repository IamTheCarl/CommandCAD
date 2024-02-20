use std::{cmp::Ordering, fmt::Write};

use ordered_float::{FloatIsNan, NotNan};

use crate::script::{
    execution::{ExecutionContext, Failure},
    parsing::{self, Expression, VariableType},
    LogMessage, RuntimeLog, Span,
};

use super::{
    function::AutoCall,
    serializable::SerializableValue,
    string::formatting::{Style, UnwrapFormattingResult},
    List, Measurement, NamedObject, Object, OperatorResult, SString, Value,
};

pub type RawNumber = f64;
pub type Number = NotNan<RawNumber>;

pub trait UnwrapNotNan: Sized {
    fn unwrap_not_nan_raw<S: Span>(self, span: &S) -> OperatorResult<S, Number>;
    fn unwrap_not_nan<'a, S: Span>(self, span: &S) -> OperatorResult<S, Value<'a, S>> {
        self.unwrap_not_nan_raw(span).map(|number| number.into())
    }
}

pub fn unwrap_float<S: Span>(span: S, number: &parsing::Number<S>) -> OperatorResult<S, Number> {
    match number.to_float::<Number>() {
        Ok(number) => Ok(number),
        Err(error) => Err(Failure::NumberConversion(span, error)),
    }
}

pub fn from_parsed<'a, S: Span>(number: &parsing::Number<S>) -> OperatorResult<S, Value<'a, S>> {
    let number = unwrap_float(number.get_span().clone(), number)?;

    Ok(number.into())
}

impl UnwrapNotNan for std::result::Result<Number, FloatIsNan> {
    fn unwrap_not_nan_raw<S: Span>(self, span: &S) -> OperatorResult<S, Number> {
        match self {
            Ok(number) => Ok(number),
            Err(_float_is_nan) => Err(Failure::ResultIsNan(span.clone())),
        }
    }
}

impl<'a, S: Span> Object<'a, S> for Number {
    fn matches_type(&self, ty: &VariableType<S>) -> bool {
        matches!(ty, VariableType::Number)
    }
    fn format(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        f: &mut dyn Write,
        style: Style,
        precision: Option<u8>,
    ) -> OperatorResult<S, ()> {
        match (style, precision) {
            (Style::Default, None) => write!(f, "{}", self).unwrap_formatting_result(span),
            (Style::Default, Some(precision)) => {
                write!(f, "{:.1$}", self, precision as usize).unwrap_formatting_result(span)
            }
            (Style::Debug, None) => write!(f, "{}", self).unwrap_formatting_result(span),
            (Style::Debug, Some(precision)) => {
                write!(f, "{:.1$}", self, precision as usize).unwrap_formatting_result(span)
            }
            (Style::Octal, _) => {
                if precision.is_some() {
                    log.push(LogMessage::FormatIntegerPrecision(span.clone()));
                }
                write!(f, "{:o}", self.into_inner() as usize).unwrap_formatting_result(span)
            }
            (Style::Hex, _) => {
                if precision.is_some() {
                    log.push(LogMessage::FormatIntegerPrecision(span.clone()));
                }
                write!(f, "{:x}", self.into_inner() as usize).unwrap_formatting_result(span)
            }
            (Style::CapitalizedHex, _) => {
                if precision.is_some() {
                    log.push(LogMessage::FormatIntegerPrecision(span.clone()));
                }
                write!(f, "{:X}", self.into_inner() as usize).unwrap_formatting_result(span)
            }
            (Style::Exponent, None) => {
                write!(f, "{:e}", self.into_inner() as usize).unwrap_formatting_result(span)
            }
            (Style::Exponent, Some(precision)) => {
                write!(f, "{:.1$e}", self.into_inner(), precision as usize)
                    .unwrap_formatting_result(span)
            }
            (Style::CapitalizedExponent, None) => {
                write!(f, "{:E}", self.into_inner()).unwrap_formatting_result(span)
            }
            (Style::CapitalizedExponent, Some(precision)) => {
                write!(f, "{:.1$E}", self.into_inner(), precision as usize)
                    .unwrap_formatting_result(span)
            }
        }
    }
    fn cmp(
        &self,
        _log: &mut RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> OperatorResult<S, Ordering> {
        let rhs = rhs.downcast_ref(span)?;

        Ok(Ord::cmp(self, rhs))
    }
    fn addition(
        &self,
        _log: &mut RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> OperatorResult<S, Value<'a, S>> {
        let rhs: &Self = rhs.downcast_ref(span)?;
        NotNan::new(self.into_inner() + rhs.into_inner()).unwrap_not_nan(span)
    }
    fn subtraction(
        &self,
        _log: &mut RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> OperatorResult<S, Value<'a, S>> {
        let rhs: &Self = rhs.downcast_ref(span)?;
        NotNan::new(self.into_inner() - rhs.into_inner()).unwrap_not_nan(span)
    }
    fn multiply(
        &self,
        _log: &mut RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> OperatorResult<S, Value<'a, S>> {
        let rhs: &Self = rhs.downcast_ref(span)?;
        NotNan::new(self.into_inner() * rhs.into_inner()).unwrap_not_nan(span)
    }
    fn divide(
        &self,
        _log: &mut RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> OperatorResult<S, Value<'a, S>> {
        let rhs: &Self = rhs.downcast_ref(span)?;
        NotNan::new(self.into_inner() / rhs.into_inner()).unwrap_not_nan(span)
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
            "abs" => |_context: &mut ExecutionContext<'a, S>,
                      span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                Self::new(self.abs()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "acos" => |_context: &mut ExecutionContext<'a, S>,
                       span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.acos()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "acosh" => |_context: &mut ExecutionContext<'a, S>,
                        span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.acosh()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "asin" => |_context: &mut ExecutionContext<'a, S>,
                       span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.asin()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "asinh" => |_context: &mut ExecutionContext<'a, S>,
                        span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.asinh()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "atan" => |_context: &mut ExecutionContext<'a, S>,
                       span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.atan()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "atanh" => |_context: &mut ExecutionContext<'a, S>,
                        span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.atanh()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "cbrt" => |_context: &mut ExecutionContext<'a, S>,
                       span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.cbrt()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "ceil" => |_context: &mut ExecutionContext<'a, S>,
                       span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.ceil()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "clamp" => |_context: &mut ExecutionContext<'a, S>,
                        _span: &S,
                        min: Number,
                        max: Number|
             -> OperatorResult<S, Value<'a, S>> {
                Ok((*self).clamp(min, max).into())
            }
            .auto_call(context, span, arguments, expressions),
            "copysign" => |_context: &mut ExecutionContext<'a, S>,
                           span: &S,
                           sign: Number|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.copysign(*sign)).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "cos" => |_context: &mut ExecutionContext<'a, S>,
                      span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.cos()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "cosh" => |_context: &mut ExecutionContext<'a, S>,
                       span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.cosh()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "div_euclid" => |_context: &mut ExecutionContext<'a, S>,
                             span: &S,
                             rhs: Number|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.div_euclid(*rhs)).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "exp" => |_context: &mut ExecutionContext<'a, S>,
                      span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.exp()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "exp2" => |_context: &mut ExecutionContext<'a, S>,
                       span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.exp2()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "exp_m1" => |_context: &mut ExecutionContext<'a, S>,
                         span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.exp_m1()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "floor" => |_context: &mut ExecutionContext<'a, S>,
                        span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.floor()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "fract" => |_context: &mut ExecutionContext<'a, S>,
                        span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.fract()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "hypot" => |_context: &mut ExecutionContext<'a, S>,
                        span: &S,
                        other: Number|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.hypot(*other)).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "is_finite" => {
                |_context: &mut ExecutionContext<'a, S>,
                 _span: &S|
                 -> OperatorResult<S, Value<'a, S>> { Ok(self.is_finite().into()) }
                .auto_call(context, span, arguments, expressions)
            }
            "is_infinite" => |_context: &mut ExecutionContext<'a, S>,
                              _span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                Ok(self.is_infinite().into())
            }
            .auto_call(context, span, arguments, expressions),
            "is_sign_negative" => |_context: &mut ExecutionContext<'a, S>,
                                   _span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                Ok(self.is_sign_negative().into())
            }
            .auto_call(context, span, arguments, expressions),
            "is_sign_posative" => |_context: &mut ExecutionContext<'a, S>,
                                   _span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                Ok(self.is_sign_positive().into())
            }
            .auto_call(context, span, arguments, expressions),
            "ln" => |_context: &mut ExecutionContext<'a, S>,
                     span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.ln()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "ln_1p" => |_context: &mut ExecutionContext<'a, S>,
                        span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.ln_1p()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "context" => |_context: &mut ExecutionContext<'a, S>,
                          span: &S,
                          base: Number|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.log(*base)).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "context2" => |_context: &mut ExecutionContext<'a, S>,
                           span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.log2()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "context10" => |_context: &mut ExecutionContext<'a, S>,
                            span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.log10()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "max" => |_context: &mut ExecutionContext<'a, S>,
                      _span: &S,
                      other: Number|
             -> OperatorResult<S, Value<'a, S>> {
                Ok((*self).max(other).into())
            }
            .auto_call(context, span, arguments, expressions),
            "min" => |_context: &mut ExecutionContext<'a, S>,
                      _span: &S,
                      other: Number|
             -> OperatorResult<S, Value<'a, S>> {
                Ok((*self).min(other).into())
            }
            .auto_call(context, span, arguments, expressions),
            "mul_add" => |_context: &mut ExecutionContext<'a, S>,
                          span: &S,
                          a: Number,
                          b: Number|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.mul_add(*a, *b)).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "powf" => |_context: &mut ExecutionContext<'a, S>,
                       span: &S,
                       n: Number|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.powf(*n)).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "recip" => |_context: &mut ExecutionContext<'a, S>,
                        span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.recip()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "rem_euclid" => |_context: &mut ExecutionContext<'a, S>,
                             span: &S,
                             rhs: Number|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.rem_euclid(*rhs)).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "round" => |_context: &mut ExecutionContext<'a, S>,
                        span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.round()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "signum" => |_context: &mut ExecutionContext<'a, S>,
                         span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.signum()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "sin" => |_context: &mut ExecutionContext<'a, S>,
                      span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.sin()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "sin_cos" => |_context: &mut ExecutionContext<'a, S>,
                          _span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                let (sin, cos) = self.sin_cos();
                let sin = NotNan::new(sin).unwrap_not_nan(span)?;
                let cos = NotNan::new(cos).unwrap_not_nan(span)?;

                Ok(List::from([sin, cos]).into())
            }
            .auto_call(context, span, arguments, expressions),
            "sinh" => |_context: &mut ExecutionContext<'a, S>,
                       span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.sinh()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "sqrt" => |_context: &mut ExecutionContext<'a, S>,
                       span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.sqrt()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "tan" => |_context: &mut ExecutionContext<'a, S>,
                      span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.tan()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "tanh" => |_context: &mut ExecutionContext<'a, S>,
                       span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.tanh()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "trunc" => |_context: &mut ExecutionContext<'a, S>,
                        span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                NotNan::new(self.trunc()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "to_measurement" => |_context: &mut ExecutionContext<'a, S>,
                                 span: &S,
                                 ty: SString|
             -> OperatorResult<S, Value<'a, S>> {
                Measurement::from_number(span, *self, ty)
            }
            .auto_call(context, span, arguments, expressions),
            _ => Err(Failure::UnknownAttribute(attribute.clone())),
        }
    }
    fn unary_plus(&self, _log: &mut RuntimeLog<S>, _span: &S) -> OperatorResult<S, Value<'a, S>> {
        Ok((*self).into())
    }
    fn unary_minus(&self, _log: &mut RuntimeLog<S>, span: &S) -> OperatorResult<S, Value<'a, S>> {
        NotNan::new(-self.into_inner()).unwrap_not_nan(span)
    }

    fn export(&self, _log: &mut RuntimeLog<S>, _span: &S) -> OperatorResult<S, SerializableValue> {
        Ok(SerializableValue::Number(self.into_inner()))
    }
}

impl NamedObject for Number {
    fn static_type_name() -> &'static str {
        "Number"
    }
}
