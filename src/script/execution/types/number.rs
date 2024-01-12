use std::{cmp::Ordering, fmt::Write};

use ordered_float::{FloatIsNan, NotNan};

use crate::script::{
    execution::{ControlFlow, ExecutionResult},
    parsing::{self, Expression, VariableType},
    LogMessage, RuntimeLog, Span,
};

use super::{
    function::AutoCall,
    serializable::SerializableValue,
    string::formatting::{Style, UnwrapFormattingResult},
    List, Measurement, NamedObject, Object, SString, Value,
};

pub type RawNumber = f64;
pub type Number = NotNan<RawNumber>;

pub trait UnwrapNotNan: Sized {
    fn unwrap_not_nan_raw<'a, S: Span>(
        self,
        log: &mut RuntimeLog<S>,
        span: &S,
    ) -> ExecutionResult<'a, S, Number>;
    fn unwrap_not_nan<'a, S: Span>(
        self,
        log: &mut RuntimeLog<S>,
        span: &S,
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        self.unwrap_not_nan_raw(log, span)
            .map(|number| number.into())
    }
}

pub fn unwrap_float<'a, S: Span>(
    log: &mut RuntimeLog<S>,
    span: S,
    number: &parsing::Number<S>,
) -> ExecutionResult<'a, S, Number> {
    match number.to_float::<Number>() {
        Ok(number) => Ok(number),
        Err(error) => {
            log.push(LogMessage::NumberConversion(span, error));
            Err(ControlFlow::Failure)
        }
    }
}

pub fn from_parsed<'a, S: Span>(
    log: &mut RuntimeLog<S>,
    number: &parsing::Number<S>,
) -> ExecutionResult<'a, S, Value<'a, S>> {
    let number = unwrap_float(log, number.get_span().clone(), number)?;

    Ok(number.into())
}

impl UnwrapNotNan for Result<Number, FloatIsNan> {
    fn unwrap_not_nan_raw<'a, S: Span>(
        self,
        log: &mut RuntimeLog<S>,
        span: &S,
    ) -> ExecutionResult<'a, S, Number> {
        match self {
            Ok(number) => Ok(number),
            Err(_float_is_nan) => {
                log.push(LogMessage::ResultIsNan(span.clone()));
                Err(ControlFlow::Failure)
            }
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
    ) -> ExecutionResult<'a, S, ()> {
        match (style, precision) {
            (Style::Default, None) => write!(f, "{}", self).unwrap_formatting_result(log, span),
            (Style::Default, Some(precision)) => {
                write!(f, "{:.1$}", self, precision as usize).unwrap_formatting_result(log, span)
            }
            (Style::Debug, None) => write!(f, "{}", self).unwrap_formatting_result(log, span),
            (Style::Debug, Some(precision)) => {
                write!(f, "{:.1$}", self, precision as usize).unwrap_formatting_result(log, span)
            }
            (Style::Octal, _) => {
                if precision.is_some() {
                    log.push(LogMessage::FormatIntegerPrecision(span.clone()));
                }
                write!(f, "{:o}", self.into_inner() as usize).unwrap_formatting_result(log, span)
            }
            (Style::Hex, _) => {
                if precision.is_some() {
                    log.push(LogMessage::FormatIntegerPrecision(span.clone()));
                }
                write!(f, "{:x}", self.into_inner() as usize).unwrap_formatting_result(log, span)
            }
            (Style::CapitalizedHex, _) => {
                if precision.is_some() {
                    log.push(LogMessage::FormatIntegerPrecision(span.clone()));
                }
                write!(f, "{:X}", self.into_inner() as usize).unwrap_formatting_result(log, span)
            }
            (Style::Exponent, None) => {
                write!(f, "{:e}", self.into_inner() as usize).unwrap_formatting_result(log, span)
            }
            (Style::Exponent, Some(precision)) => {
                write!(f, "{:.1$e}", self.into_inner(), precision as usize)
                    .unwrap_formatting_result(log, span)
            }
            (Style::CapitalizedExponent, None) => {
                write!(f, "{:E}", self.into_inner()).unwrap_formatting_result(log, span)
            }
            (Style::CapitalizedExponent, Some(precision)) => {
                write!(f, "{:.1$E}", self.into_inner(), precision as usize)
                    .unwrap_formatting_result(log, span)
            }
        }
    }
    fn cmp(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> ExecutionResult<'a, S, Ordering> {
        let rhs = rhs.downcast_ref(log, span)?;

        Ok(Ord::cmp(self, rhs))
    }
    fn addition(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        let rhs: &Self = rhs.downcast_ref(log, span)?;
        NotNan::new(self.into_inner() + rhs.into_inner()).unwrap_not_nan(log, span)
    }
    fn subtraction(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        let rhs: &Self = rhs.downcast_ref(log, span)?;
        NotNan::new(self.into_inner() - rhs.into_inner()).unwrap_not_nan(log, span)
    }
    fn multiply(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        let rhs: &Self = rhs.downcast_ref(log, span)?;
        NotNan::new(self.into_inner() * rhs.into_inner()).unwrap_not_nan(log, span)
    }
    fn divide(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        let rhs: &Self = rhs.downcast_ref(log, span)?;
        NotNan::new(self.into_inner() / rhs.into_inner()).unwrap_not_nan(log, span)
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
            "abs" => |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                Self::new(self.abs()).unwrap_not_nan(log, span)
            }
            .auto_call(log, span, arguments, expressions),
            "acos" => |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                NotNan::new(self.acos()).unwrap_not_nan(log, span)
            }
            .auto_call(log, span, arguments, expressions),
            "acosh" => {
                |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                    NotNan::new(self.acosh()).unwrap_not_nan(log, span)
                }
                .auto_call(log, span, arguments, expressions)
            }
            "asin" => |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                NotNan::new(self.asin()).unwrap_not_nan(log, span)
            }
            .auto_call(log, span, arguments, expressions),
            "asinh" => {
                |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                    NotNan::new(self.asinh()).unwrap_not_nan(log, span)
                }
                .auto_call(log, span, arguments, expressions)
            }
            "atan" => |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                NotNan::new(self.atan()).unwrap_not_nan(log, span)
            }
            .auto_call(log, span, arguments, expressions),
            "atanh" => {
                |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                    NotNan::new(self.atanh()).unwrap_not_nan(log, span)
                }
                .auto_call(log, span, arguments, expressions)
            }
            "cbrt" => |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                NotNan::new(self.cbrt()).unwrap_not_nan(log, span)
            }
            .auto_call(log, span, arguments, expressions),
            "ceil" => |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                NotNan::new(self.ceil()).unwrap_not_nan(log, span)
            }
            .auto_call(log, span, arguments, expressions),
            "clamp" => |_log: &mut RuntimeLog<S>,
                        _span: &S,
                        min: Number,
                        max: Number|
             -> ExecutionResult<'a, S, Value<'a, S>> {
                Ok((*self).clamp(min, max).into())
            }
            .auto_call(log, span, arguments, expressions),
            "copysign" => |log: &mut RuntimeLog<S>,
                           span: &S,
                           sign: Number|
             -> ExecutionResult<'a, S, Value<'a, S>> {
                NotNan::new(self.copysign(*sign)).unwrap_not_nan(log, span)
            }
            .auto_call(log, span, arguments, expressions),
            "cos" => |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                NotNan::new(self.cos()).unwrap_not_nan(log, span)
            }
            .auto_call(log, span, arguments, expressions),
            "cosh" => |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                NotNan::new(self.cosh()).unwrap_not_nan(log, span)
            }
            .auto_call(log, span, arguments, expressions),
            "div_euclid" => |log: &mut RuntimeLog<S>,
                             span: &S,
                             rhs: Number|
             -> ExecutionResult<'a, S, Value<'a, S>> {
                NotNan::new(self.div_euclid(*rhs)).unwrap_not_nan(log, span)
            }
            .auto_call(log, span, arguments, expressions),
            "exp" => |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                NotNan::new(self.exp()).unwrap_not_nan(log, span)
            }
            .auto_call(log, span, arguments, expressions),
            "exp2" => |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                NotNan::new(self.exp2()).unwrap_not_nan(log, span)
            }
            .auto_call(log, span, arguments, expressions),
            "exp_m1" => {
                |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                    NotNan::new(self.exp_m1()).unwrap_not_nan(log, span)
                }
                .auto_call(log, span, arguments, expressions)
            }
            "floor" => {
                |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                    NotNan::new(self.floor()).unwrap_not_nan(log, span)
                }
                .auto_call(log, span, arguments, expressions)
            }
            "fract" => {
                |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                    NotNan::new(self.fract()).unwrap_not_nan(log, span)
                }
                .auto_call(log, span, arguments, expressions)
            }
            "hypot" => |log: &mut RuntimeLog<S>,
                        span: &S,
                        other: Number|
             -> ExecutionResult<'a, S, Value<'a, S>> {
                NotNan::new(self.hypot(*other)).unwrap_not_nan(log, span)
            }
            .auto_call(log, span, arguments, expressions),
            "is_finite" => {
                |_log: &mut RuntimeLog<S>, _span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                    Ok(self.is_finite().into())
                }
                .auto_call(log, span, arguments, expressions)
            }
            "is_infinite" => {
                |_log: &mut RuntimeLog<S>, _span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                    Ok(self.is_infinite().into())
                }
                .auto_call(log, span, arguments, expressions)
            }
            "is_sign_negative" => {
                |_log: &mut RuntimeLog<S>, _span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                    Ok(self.is_sign_negative().into())
                }
                .auto_call(log, span, arguments, expressions)
            }
            "is_sign_posative" => {
                |_log: &mut RuntimeLog<S>, _span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                    Ok(self.is_sign_positive().into())
                }
                .auto_call(log, span, arguments, expressions)
            }
            "ln" => |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                NotNan::new(self.ln()).unwrap_not_nan(log, span)
            }
            .auto_call(log, span, arguments, expressions),
            "ln_1p" => {
                |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                    NotNan::new(self.ln_1p()).unwrap_not_nan(log, span)
                }
                .auto_call(log, span, arguments, expressions)
            }
            "log" => |log: &mut RuntimeLog<S>,
                      span: &S,
                      base: Number|
             -> ExecutionResult<'a, S, Value<'a, S>> {
                NotNan::new(self.log(*base)).unwrap_not_nan(log, span)
            }
            .auto_call(log, span, arguments, expressions),
            "log2" => |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                NotNan::new(self.log2()).unwrap_not_nan(log, span)
            }
            .auto_call(log, span, arguments, expressions),
            "log10" => {
                |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                    NotNan::new(self.log10()).unwrap_not_nan(log, span)
                }
                .auto_call(log, span, arguments, expressions)
            }
            "max" => |_log: &mut RuntimeLog<S>,
                      _span: &S,
                      other: Number|
             -> ExecutionResult<'a, S, Value<'a, S>> {
                Ok((*self).max(other).into())
            }
            .auto_call(log, span, arguments, expressions),
            "min" => |_log: &mut RuntimeLog<S>,
                      _span: &S,
                      other: Number|
             -> ExecutionResult<'a, S, Value<'a, S>> {
                Ok((*self).min(other).into())
            }
            .auto_call(log, span, arguments, expressions),
            "mul_add" => |log: &mut RuntimeLog<S>,
                          span: &S,
                          a: Number,
                          b: Number|
             -> ExecutionResult<'a, S, Value<'a, S>> {
                NotNan::new(self.mul_add(*a, *b)).unwrap_not_nan(log, span)
            }
            .auto_call(log, span, arguments, expressions),
            "powf" => |log: &mut RuntimeLog<S>,
                       span: &S,
                       n: Number|
             -> ExecutionResult<'a, S, Value<'a, S>> {
                NotNan::new(self.powf(*n)).unwrap_not_nan(log, span)
            }
            .auto_call(log, span, arguments, expressions),
            "recip" => {
                |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                    NotNan::new(self.recip()).unwrap_not_nan(log, span)
                }
                .auto_call(log, span, arguments, expressions)
            }
            "rem_euclid" => |log: &mut RuntimeLog<S>,
                             span: &S,
                             rhs: Number|
             -> ExecutionResult<'a, S, Value<'a, S>> {
                NotNan::new(self.rem_euclid(*rhs)).unwrap_not_nan(log, span)
            }
            .auto_call(log, span, arguments, expressions),
            "round" => {
                |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                    NotNan::new(self.round()).unwrap_not_nan(log, span)
                }
                .auto_call(log, span, arguments, expressions)
            }
            "signum" => {
                |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                    NotNan::new(self.signum()).unwrap_not_nan(log, span)
                }
                .auto_call(log, span, arguments, expressions)
            }
            "sin" => |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                NotNan::new(self.sin()).unwrap_not_nan(log, span)
            }
            .auto_call(log, span, arguments, expressions),
            "sin_cos" => {
                |log: &mut RuntimeLog<S>, _span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                    let (sin, cos) = self.sin_cos();
                    let sin = NotNan::new(sin).unwrap_not_nan(log, span)?;
                    let cos = NotNan::new(cos).unwrap_not_nan(log, span)?;

                    Ok(List::from([sin, cos]).into())
                }
                .auto_call(log, span, arguments, expressions)
            }
            "sinh" => |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                NotNan::new(self.sinh()).unwrap_not_nan(log, span)
            }
            .auto_call(log, span, arguments, expressions),
            "sqrt" => |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                NotNan::new(self.sqrt()).unwrap_not_nan(log, span)
            }
            .auto_call(log, span, arguments, expressions),
            "tan" => |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                NotNan::new(self.tan()).unwrap_not_nan(log, span)
            }
            .auto_call(log, span, arguments, expressions),
            "tanh" => |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                NotNan::new(self.tanh()).unwrap_not_nan(log, span)
            }
            .auto_call(log, span, arguments, expressions),
            "trunc" => {
                |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<'a, S, Value<'a, S>> {
                    NotNan::new(self.trunc()).unwrap_not_nan(log, span)
                }
                .auto_call(log, span, arguments, expressions)
            }
            "to_measurement" => |log: &mut RuntimeLog<S>,
                                 span: &S,
                                 ty: SString|
             -> ExecutionResult<'a, S, Value<'a, S>> {
                Measurement::from_number(log, span, *self, ty)
            }
            .auto_call(log, span, arguments, expressions),
            _ => {
                log.push(LogMessage::UnknownAttribute(attribute.clone()));
                Err(ControlFlow::Failure)
            }
        }
    }
    fn unary_plus(
        &self,
        _log: &mut RuntimeLog<S>,
        _span: &S,
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        Ok((*self).into())
    }
    fn unary_minus(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        NotNan::new(-self.into_inner()).unwrap_not_nan(log, span)
    }

    fn export(
        &self,
        _log: &mut RuntimeLog<S>,
        _span: &S,
    ) -> ExecutionResult<'a, S, SerializableValue> {
        Ok(SerializableValue::Number(self.into_inner()))
    }
}

impl NamedObject for Number {
    fn static_type_name() -> &'static str {
        "Number"
    }
}
