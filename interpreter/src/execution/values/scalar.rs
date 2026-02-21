/*
 * Copyright 2025 James Carl
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
use std::{borrow::Cow, cmp::Ordering, f64::consts::PI};

use common_data_types::{Dimension, Float, FloatIsNan};

use crate::{
    build_method,
    execution::{
        errors::{ExpressionResult, GenericFailure, Raise},
        logging::{LogLevel, LogMessage, StackTrace},
        values::{
            self, closure::BuiltinCallableDatabase, string::formatting::Style, Boolean,
            BuiltinFunction, MissingAttributeError, SignedInteger, StaticType, UnsignedInteger,
            Vector2,
        },
        ExecutionContext,
    },
    values::DowncastError,
};

use super::{value_type::ValueType, DowncastForBinaryOpError, Object, StaticTypeName, Value};

pub trait UnwrapNotNan: Sized {
    fn unwrap_not_nan(self, stack_trace: &StackTrace) -> ExpressionResult<Float>;
}

impl UnwrapNotNan for std::result::Result<Float, FloatIsNan> {
    fn unwrap_not_nan(self, stack_trace: &StackTrace) -> ExpressionResult<Float> {
        match self {
            Ok(number) => Ok(number),
            Err(_float_is_nan) => Err(GenericFailure(
                "Result of arithmetic operation is NaN".into(),
            )
            .to_error(stack_trace)),
        }
    }
}

#[derive(Debug, Hash, Clone, Copy, Eq, PartialEq)]
pub struct Scalar {
    pub dimension: Dimension,
    pub value: Float,
}

impl Object for Scalar {
    fn get_type(&self, _context: &ExecutionContext) -> ValueType {
        ValueType::Scalar(Some(self.dimension))
    }

    fn type_name(&self) -> Cow<'static, str> {
        units::get_dimension_name(&self.dimension)
    }

    fn format(
        &self,
        context: &ExecutionContext,
        f: &mut dyn std::fmt::Write,
        style: Style,
        precision: Option<u8>,
    ) -> std::fmt::Result {
        // This just takes a reference to the unit name, so it's pretty cheap. I don't mind if it's not always used.
        // In the rare case that a unit name is generated and memory is allocated on the heap, well we're clearly
        // not about to format a number, so it's clear that we're going to use this.
        let unit_name = units::get_base_unit_name(&self.dimension);

        match (style, precision, unit_name) {
            (Style::Default, None, None) => {
                write!(f, "{}", self.value)
            }
            (Style::Default, None, Some(unit_name)) => {
                write!(f, "{}{unit_name}", self.value)
            }
            (Style::Default, Some(precision), None) => {
                write!(f, "{:.1$}", self.value, precision as usize)
            }
            (Style::Default, Some(precision), Some(unit_name)) => {
                write!(f, "{:.1$}{unit_name}", self.value, precision as usize)
            }
            (Style::Debug, None, None) => {
                write!(f, "{}", self.value)
            }
            (Style::Debug, None, Some(unit_name)) => {
                write!(f, "{}{unit_name}", self.value)
            }
            (Style::Debug, Some(precision), None) => {
                write!(f, "{:.1$}", self.value, precision as usize)
            }
            (Style::Debug, Some(precision), Some(unit_name)) => {
                write!(f, "{:.1$}{unit_name}", self.value, precision as usize)
            }
            (Style::Octal | Style::Hex | Style::CapitalizedHex, _, _) => {
                context.log.push_message(LogMessage {
                    origin: context.stack_trace.bottom().clone(),
                    level: LogLevel::Warning,
                    message: "Scalar values cannot be formatted as integers".into(),
                });

                // Don't stop execution. Just use default formatting.
                self.format(context, f, Style::Default, precision)
            }
            (Style::Exponent, None, None) => {
                write!(f, "{:e}", self.value.into_inner() as usize)
            }
            (Style::Exponent, None, Some(unit_name)) => {
                write!(f, "{:e}{unit_name}", self.value.into_inner() as usize)
            }
            (Style::Exponent, Some(precision), None) => {
                write!(f, "{:.1$e}", self.value.into_inner(), precision as usize)
            }
            (Style::Exponent, Some(precision), Some(unit_name)) => write!(
                f,
                "{:.1$e}{unit_name}",
                self.value.into_inner(),
                precision as usize
            ),
            (Style::CapitalizedExponent, None, None) => {
                write!(f, "{:E}", self.value.into_inner())
            }
            (Style::CapitalizedExponent, None, Some(unit_name)) => {
                write!(f, "{:E}{unit_name}", self.value.into_inner())
            }
            (Style::CapitalizedExponent, Some(precision), None) => {
                write!(f, "{:.1$E}", self.value.into_inner(), precision as usize)
            }
            (Style::CapitalizedExponent, Some(precision), Some(unit_name)) => write!(
                f,
                "{:.1$E}{unit_name}",
                self.value.into_inner(),
                precision as usize
            ),
        }
    }

    fn addition(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let rhs = self.unpack_same_dimension(context.stack_trace, rhs)?;

        let value = Float::new(*self.value + *rhs.value).unwrap_not_nan(context.stack_trace)?;

        Ok(Self { value, ..self }.into())
    }
    fn subtraction(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let rhs = self.unpack_same_dimension(context.stack_trace, rhs)?;

        let value = Float::new(*self.value - *rhs.value).unwrap_not_nan(context.stack_trace)?;

        Ok(Self { value, ..self }.into())
    }
    fn multiply(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let rhs = rhs.downcast_for_binary_op_ref::<Scalar>(context.stack_trace)?;
        self.multiply_by_scalar(context.stack_trace, rhs)
            .map(|rhs| rhs.into())
    }
    fn divide(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let rhs = rhs.downcast_for_binary_op_ref::<Scalar>(context.stack_trace)?;
        self.divide_by_measurement(context.stack_trace, rhs)
            .map(|rhs| rhs.into())
    }
    fn exponent(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let rhs = rhs.downcast::<Zero>(context.stack_trace)?;

        if rhs.dimension == Dimension::zero() {
            Ok(Scalar {
                dimension: self.dimension * *rhs.value as i8,
                value: Float::new(self.value.powf(*rhs.value))
                    .unwrap_not_nan(context.stack_trace)?,
            }
            .into())
        } else {
            Err(DowncastError {
                expected: ValueType::Scalar(Some(Dimension::zero())).name(),
                got: rhs.get_type(context).name(),
            }
            .to_error(context.stack_trace))
        }
    }
    fn unary_plus(self, _context: &ExecutionContext) -> ExpressionResult<Value> {
        Ok(self.into())
    }
    fn unary_minus(self, _context: &ExecutionContext) -> ExpressionResult<Value> {
        Ok(Self {
            value: -self.value,
            ..self
        }
        .into())
    }
    fn cmp(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Ordering> {
        let rhs = rhs.downcast_for_binary_op_ref::<Self>(context.stack_trace)?;
        if self.dimension == rhs.dimension {
            Ok(std::cmp::Ord::cmp(&self.value, &rhs.value))
        } else {
            Err(DowncastForBinaryOpError {
                expected: self.type_name(),
                got: rhs.type_name(),
            }
            .to_error(context.stack_trace))
        }
    }

    fn get_attribute(
        &self,
        context: &ExecutionContext,
        attribute: &str,
    ) -> ExpressionResult<Value> {
        match attribute {
            "to_signed_integer" => Ok(BuiltinFunction::new::<methods::ToSignedInteger>().into()),
            "to_unsigned_integer" => {
                Ok(BuiltinFunction::new::<methods::ToUnsignedInteger>().into())
            }
            "abs" => Ok(BuiltinFunction::new::<methods::Abs>().into()),
            "clamp" => Ok(BuiltinFunction::new::<methods::Clamp>().into()),
            "copysign" => Ok(BuiltinFunction::new::<methods::Copysign>().into()),
            "hypot" => Ok(BuiltinFunction::new::<methods::Hypot>().into()),
            "is_finite" => Ok(BuiltinFunction::new::<methods::IsFinite>().into()),
            "is_infinite" => Ok(BuiltinFunction::new::<methods::IsInfinite>().into()),
            "is_normal" => Ok(BuiltinFunction::new::<methods::IsNormal>().into()),
            "cbrt" => Ok(BuiltinFunction::new::<methods::Cbrt>().into()),
            "pow" => Ok(BuiltinFunction::new::<methods::Pow>().into()),
            "sqrt" => Ok(BuiltinFunction::new::<methods::Sqrt>().into()),
            "is_sign_negative" => Ok(BuiltinFunction::new::<methods::IsSignNegative>().into()),
            "is_sign_positive" => Ok(BuiltinFunction::new::<methods::IsSignPositive>().into()),
            "recip" => Ok(BuiltinFunction::new::<methods::Recip>().into()),
            "round" => Ok(BuiltinFunction::new::<methods::Round>().into()),
            "trunc" => Ok(BuiltinFunction::new::<methods::Trunc>().into()),
            "fract" => Ok(BuiltinFunction::new::<methods::Fract>().into()),
            "floor" => Ok(BuiltinFunction::new::<methods::Floor>().into()),
            "ceil" => Ok(BuiltinFunction::new::<methods::Ceil>().into()),
            "max" => Ok(BuiltinFunction::new::<methods::Max>().into()),
            "min" => Ok(BuiltinFunction::new::<methods::Min>().into()),
            "signum" => Ok(BuiltinFunction::new::<methods::Signum>().into()),
            "acos" => Ok(BuiltinFunction::new::<methods::Acos>().into()),
            "acosh" => Ok(BuiltinFunction::new::<methods::Acosh>().into()),
            "cos" => Ok(BuiltinFunction::new::<methods::Cos>().into()),
            "cosh" => Ok(BuiltinFunction::new::<methods::Cosh>().into()),
            "asin" => Ok(BuiltinFunction::new::<methods::Asin>().into()),
            "asinh" => Ok(BuiltinFunction::new::<methods::Asinh>().into()),
            "sin" => Ok(BuiltinFunction::new::<methods::Sin>().into()),
            "sinh" => Ok(BuiltinFunction::new::<methods::Sinh>().into()),
            "cossin" => Ok(BuiltinFunction::new::<methods::CosSin>().into()),
            "atan" => Ok(BuiltinFunction::new::<methods::Atan>().into()),
            "atanh" => Ok(BuiltinFunction::new::<methods::Atanh>().into()),
            "tan" => Ok(BuiltinFunction::new::<methods::Tan>().into()),
            "tanh" => Ok(BuiltinFunction::new::<methods::Tanh>().into()),
            _ => Err(MissingAttributeError {
                name: attribute.into(),
            }
            .to_error(context.stack_trace)),
        }
    }

    // fn export(
    //     &self,
    //     _log: &dyn RuntimeLog,
    //     _stack_trace: &StackTrace,
    // ) -> ExpressionResult<SerializableValue> {
    //     Ok(SerializableValue::Scalar(self.clone()))
    // }
}

impl StaticTypeName for Scalar {
    fn static_type_name() -> Cow<'static, str> {
        "Scalar".into()
    }
}

impl StaticType for Scalar {
    fn static_type() -> ValueType {
        ValueType::Scalar(None)
    }
}

impl Scalar {
    fn multiply_by_scalar(&self, stack_trace: &StackTrace, rhs: &Self) -> ExpressionResult<Self> {
        let value = Float::new(*self.value * *rhs.value).unwrap_not_nan(stack_trace)?;
        let dimension = self.dimension + rhs.dimension;

        Ok(Self { dimension, value })
    }

    fn divide_by_measurement(
        &self,
        stack_trace: &StackTrace,
        rhs: &Self,
    ) -> ExpressionResult<Self> {
        let value = Float::new(*self.value / *rhs.value).unwrap_not_nan(stack_trace)?;
        let dimension = self.dimension - rhs.dimension;

        Ok(Self { dimension, value })
    }

    fn check_inverse_trig_compatible(&self, stack_trace: &StackTrace) -> ExpressionResult<()> {
        if self.dimension.is_zero_dimension() {
            Ok(())
        } else {
            Err(GenericFailure("Inverse trigonometric functions can only be used with zero dimensional types (Angles, Ratios)".into()).to_error(stack_trace))
        }
    }

    fn check_trig_compatible(&self, stack_trace: &StackTrace) -> ExpressionResult<()> {
        if self.dimension.is_zero_dimension() && self.dimension.ratio_type_hint.is_angle() {
            Ok(())
        } else {
            Err(
                GenericFailure("Trigonometric functions can only be used with angles".into())
                    .to_error(stack_trace),
            )
        }
    }

    fn unpack_same_dimension(self, stack_trace: &StackTrace, rhs: Value) -> ExpressionResult<Self> {
        if let Value::Scalar(rhs) = rhs {
            if self.dimension == rhs.dimension {
                Ok(rhs)
            } else {
                Err(DowncastForBinaryOpError {
                    expected: self.type_name(),
                    got: rhs.type_name(),
                }
                .to_error(stack_trace))
            }
        } else {
            Err(DowncastForBinaryOpError {
                expected: self.type_name(),
                got: rhs.type_name(),
            }
            .to_error(stack_trace))
        }
    }
}

mod methods {
    pub struct ToSignedInteger;
    pub struct ToUnsignedInteger;
    pub struct Abs;
    pub struct Clamp;
    pub struct Copysign;
    pub struct Hypot;
    pub struct IsFinite;
    pub struct IsInfinite;
    pub struct IsNormal;
    pub struct Cbrt;
    pub struct Pow;
    pub struct Sqrt;
    pub struct IsSignNegative;
    pub struct IsSignPositive;
    pub struct Recip;
    pub struct Round;
    pub struct Trunc;
    pub struct Fract;
    pub struct Floor;
    pub struct Ceil;
    pub struct Max;
    pub struct Min;
    pub struct Signum;
    pub struct Acos;
    pub struct Acosh;
    pub struct Cos;
    pub struct Cosh;
    pub struct Asin;
    pub struct Asinh;
    pub struct Sin;
    pub struct Sinh;
    pub struct CosSin;
    pub struct Atan;
    pub struct Atanh;
    pub struct Tan;
    pub struct Tanh;
}

pub fn register_methods(database: &mut BuiltinCallableDatabase) {
    build_method!(
        database,
        methods::ToSignedInteger, "Scalar::to_signed_integer", (
            context: &ExecutionContext,
            this: Scalar) -> SignedInteger
        {
            if this.dimension.is_zero_dimension() {
                Ok(values::SignedInteger::from(*this.value as i64))
            } else {
                Err(GenericFailure("Only zero dimensional scalars can be converted into an integer".into())
                    .to_error(context.stack_trace))
            }
        }
    );
    build_method!(
        database,
        methods::ToUnsignedInteger, "Scalar::to_unsigned_integer", (
            context: &ExecutionContext,
            this: Scalar) -> UnsignedInteger
        {
            if this.dimension.is_zero_dimension() {
                if *this.value >= 0.0 {
                    Ok(values::UnsignedInteger::from(*this.value as u64))
                } else {
                    Err(GenericFailure("Negative values cannot be converted to signed integers".into())
                        .to_error(context.stack_trace))
                }
            } else {
                Err(GenericFailure("Only zero dimensional scalars can be converted into an integer".into())
                    .to_error(context.stack_trace))
            }
        }
    );
    build_method!(
        database,
        methods::Abs, "Scalar::abs", (
            context: &ExecutionContext,
            this: Scalar) -> Scalar
        {
            Ok(Scalar {
                dimension: this.dimension,
                value: Float::new(this.value.abs()).unwrap_not_nan(context.stack_trace)?
            })
        }
    );
    build_method!(
        database,
        methods::Clamp, "Scalar::clamp", (
            _context: &ExecutionContext,
            this: Scalar,
            min: Scalar,
            max: Scalar) -> Scalar
        {
            Ok(Scalar {
                dimension: this.dimension,
                value: this.value.clamp(min.value, max.value)
            })
        }
    );
    build_method!(
        database,
        methods::Copysign, "Scalar::copysign", (
            context: &ExecutionContext,
            this: Scalar,
            sign: Zero) -> Scalar
        {
            Ok(Scalar {
                dimension: Dimension::zero(),
                value: Float::new(this.value.copysign(*sign.value)).unwrap_not_nan(context.stack_trace)?
            })
        }
    );
    build_method!(
        database,
        methods::Hypot, "Scalar::hypot", (
            context: &ExecutionContext,
            this: Scalar,
            other: Scalar) -> Scalar
        {
            Ok(Scalar {
                dimension: Dimension::zero(),
                value: Float::new(this.value.hypot(*other.value)).unwrap_not_nan(context.stack_trace)?
            })
        }
    );
    build_method!(
        database,
        methods::IsFinite, "Scalar::is_finite", (
            _context: &ExecutionContext,
            this: Scalar) -> Boolean
        {
            Ok(values::Boolean(this.value.is_finite()))
        }
    );
    build_method!(
        database,
        methods::IsInfinite, "Scalar::is_infinite", (
            _context: &ExecutionContext,
            this: Scalar) -> Boolean
        {
            Ok(values::Boolean(this.value.is_infinite()))
        }
    );
    build_method!(
        database,
        methods::IsNormal, "Scalar::is_normal", (
            _context: &ExecutionContext,
            this: Scalar) -> Boolean
        {
            Ok(values::Boolean(this.value.is_normal()))
        }
    );
    build_method!(
        database,
        methods::Cbrt, "Scalar::cbrt", (
            context: &ExecutionContext,
            this: Scalar) -> Scalar
        {
            Ok(Scalar {
                dimension: this.dimension / 3,
                value: Float::new(this.value.cbrt()).unwrap_not_nan(context.stack_trace)?
            })
        }
    );
    build_method!(
        database,
        methods::Pow, "Scalar::pow", (
            context: &ExecutionContext,
            this: Scalar,
            exp: Zero) -> Scalar
        {
            Ok(Scalar {
                dimension: this.dimension * *exp.value as i8,
                value: Float::new(this.value.powf(*exp.value)).unwrap_not_nan(context.stack_trace)?
            })
        }
    );
    build_method!(
        database,
        methods::Sqrt, "Scalar::sqrt", (
            context: &ExecutionContext,
            this: Scalar) -> Scalar
        {
            Ok(Scalar {
                dimension: this.dimension / 2,
                value: Float::new(this.value.sqrt()).unwrap_not_nan(context.stack_trace)?
            })
        }
    );
    build_method!(
        database,
        methods::IsSignNegative, "Scalar::is_sign_negative", (
            _context: &ExecutionContext,
            this: Scalar) -> Boolean
        {
            Ok(Boolean(this.value.is_sign_negative()))
        }
    );
    build_method!(
        database,
        methods::IsSignPositive, "Scalar::is_sign_positive", (
            _context: &ExecutionContext,
            this: Scalar) -> Boolean
        {
            Ok(Boolean(this.value.is_sign_positive()))
        }
    );
    build_method!(
        database,
        methods::Recip, "Scalar::recip", (
            context: &ExecutionContext,
            this: Scalar) -> Scalar
        {
            Ok(Scalar {
                dimension: -this.dimension,
                value: Float::new(this.value.recip()).unwrap_not_nan(context.stack_trace)?
            })
        }
    );
    build_method!(
        database,
        methods::Round, "Scalar::round", (
            context: &ExecutionContext,
            this: Scalar,
            unit: Scalar) -> Scalar
        {
            let unit = this.unpack_same_dimension(context.stack_trace, unit.into())?;

            let value = this.value / unit.value;

            Ok(Scalar {
                dimension: this.dimension,
                value: Float::new(value.round() * *unit.value).unwrap_not_nan(context.stack_trace)?
            })
        }
    );
    build_method!(
        database,
        methods::Trunc, "Scalar::trunc", (
            context: &ExecutionContext,
            this: Scalar,
            unit: Scalar) -> Scalar
        {
            let value = this.value / unit.value;

            Ok(Scalar {
                dimension: this.dimension,
                value: Float::new(value.trunc() * *unit.value).unwrap_not_nan(context.stack_trace)?
            })
        }
    );
    build_method!(
        database,
        methods::Fract, "Scalar::fract", (
            context: &ExecutionContext,
            this: Scalar,
            unit: Scalar) -> Scalar
        {
            let value = this.value / unit.value;

            Ok(Scalar {
                dimension: this.dimension,
                value: Float::new(value.fract() * *unit.value).unwrap_not_nan(context.stack_trace)?
            })
        }
    );
    build_method!(
        database,
        methods::Floor, "Scalar::floor", (
            context: &ExecutionContext,
            this: Scalar,
            unit: Scalar) -> Scalar
        {
            let value = this.value / unit.value;

            Ok(Scalar {
                dimension: this.dimension,
                value: Float::new(value.floor() * *unit.value).unwrap_not_nan(context.stack_trace)?
            })
        }
    );
    build_method!(
        database,
        methods::Ceil, "Scalar::ceil", (
            context: &ExecutionContext,
            this: Scalar,
            unit: Scalar) -> Scalar
        {
            let value = this.value / unit.value;

            Ok(Scalar {
                dimension: this.dimension,
                value: Float::new(value.ceil() * *unit.value).unwrap_not_nan(context.stack_trace)?
            })
        }
    );
    build_method!(
        database,
        methods::Max, "Scalar::max", (
            _context: &ExecutionContext,
            this: Scalar,
            other: Scalar) -> Scalar
        {
            Ok(Scalar {
                dimension: this.dimension,
                value: this.value.max(other.value)
            })
        }
    );
    build_method!(
        database,
        methods::Min, "Scalar::min", (
            _context: &ExecutionContext,
            this: Scalar,
            other: Scalar) -> Scalar
        {
            Ok(Scalar {
                dimension: this.dimension,
                value: this.value.min(other.value)
            })
        }
    );
    build_method!(
        database,
        methods::Signum, "Scalar::signum", (
            context: &ExecutionContext,
            this: Scalar) -> Scalar
        {
            Ok(Scalar {
                dimension: Dimension::zero(),
                value: Float::new(this.value.signum()).unwrap_not_nan(context.stack_trace)?
            })
        }
    );
    build_method!(
        database,
        methods::Acos, "Scalar::acos", (
            context: &ExecutionContext,
            this: Scalar) -> Scalar
        {
            this.check_inverse_trig_compatible(context.stack_trace)?;

            Ok(Scalar {
                dimension: Dimension::angle(),
                value: Float::new((this.value * PI).acos()).unwrap_not_nan(context.stack_trace)?
            })
        }
    );
    build_method!(
        database,
        methods::Acosh, "Scalar::acosh", (
            context: &ExecutionContext,
            this: Scalar) -> Scalar
        {
            this.check_inverse_trig_compatible(context.stack_trace)?;

            Ok(Scalar {
                dimension: Dimension::angle(),
                value: Float::new((this.value).acosh() / PI).unwrap_not_nan(context.stack_trace)?
            })
        }
    );
    build_method!(
        database,
        methods::Cos, "Scalar::cos", (
            context: &ExecutionContext,
            this: Scalar) -> Scalar
        {
            this.check_trig_compatible(context.stack_trace)?;

            Ok(Scalar {
                dimension: Dimension::zero(),
                value: Float::new((this.value * PI).cos()).unwrap_not_nan(context.stack_trace)?
            })
        }
    );
    build_method!(
        database,
        methods::Cosh, "Scalar::cosh", (
            context: &ExecutionContext,
            this: Scalar) -> Scalar
        {
            this.check_trig_compatible(context.stack_trace)?;

            Ok(Scalar {
                dimension: Dimension::zero(),
                value: Float::new((this.value * PI).cosh()).unwrap_not_nan(context.stack_trace)?
            })
        }
    );
    build_method!(
        database,
        methods::Asin, "Scalar::asin", (
            context: &ExecutionContext,
            this: Scalar) -> Scalar
        {
            this.check_inverse_trig_compatible(context.stack_trace)?;

            Ok(Scalar {
                dimension: Dimension::angle(),
                value: Float::new((this.value * PI).asin()).unwrap_not_nan(context.stack_trace)?
            })
        }
    );
    build_method!(
        database,
        methods::Asinh, "Scalar::asinh", (
            context: &ExecutionContext,
            this: Scalar) -> Scalar
        {
            this.check_inverse_trig_compatible(context.stack_trace)?;

            Ok(Scalar {
                dimension: Dimension::angle(),
                value: Float::new((this.value).asinh() / PI).unwrap_not_nan(context.stack_trace)?
            })
        }
    );
    build_method!(
        database,
        methods::Sin, "Scalar::sin", (
            context: &ExecutionContext,
            this: Scalar) -> Scalar
        {
            this.check_trig_compatible(context.stack_trace)?;

            Ok(Scalar {
                dimension: Dimension::zero(),
                value: Float::new((this.value * PI).sin()).unwrap_not_nan(context.stack_trace)?
            })
        }
    );
    build_method!(
        database,
        methods::Sinh, "Scalar::sinh", (
            context: &ExecutionContext,
            this: Scalar) -> Scalar
        {
            this.check_trig_compatible(context.stack_trace)?;

            Ok(Scalar {
                dimension: Dimension::zero(),
                value: Float::new((this.value * PI).sinh()).unwrap_not_nan(context.stack_trace)?
            })
        }
    );
    build_method!(
        database,
        methods::CosSin, "Scalar::cossin", (
            context: &ExecutionContext,
            this: Scalar) -> Vector2
        {
            this.check_trig_compatible(context.stack_trace)?;

            let (sin, cos) = (this.value * PI).sin_cos();
            Vector2::new(context, Dimension::zero(), [cos, sin])
        }
    );
    build_method!(
        database,
        methods::Atan, "Scalar::atan", (
            context: &ExecutionContext,
            this: Scalar) -> Scalar
        {
            this.check_inverse_trig_compatible(context.stack_trace)?;

            Ok(Scalar {
                dimension: Dimension::angle(),
                value: Float::new((this.value * PI).atan()).unwrap_not_nan(context.stack_trace)?
            })
        }
    );
    build_method!(
        database,
        methods::Atanh, "Scalar::atanh", (
            context: &ExecutionContext,
            this: Scalar) -> Scalar
        {
            this.check_inverse_trig_compatible(context.stack_trace)?;

            Ok(Scalar {
                dimension: Dimension::angle(),
                value: Float::new((this.value).atanh() / PI).unwrap_not_nan(context.stack_trace)?
            })
        }
    );
    build_method!(
        database,
        methods::Tan, "Scalar::tan", (
            context: &ExecutionContext,
            this: Scalar) -> Scalar
        {
            this.check_trig_compatible(context.stack_trace)?;

            Ok(Scalar {
                dimension: Dimension::zero(),
                value: Float::new((this.value * PI).tan()).unwrap_not_nan(context.stack_trace)?
            })
        }
    );
    build_method!(
        database,
        methods::Tanh, "Scalar::tanh", (
            context: &ExecutionContext,
            this: Scalar) -> Scalar
        {
            this.check_trig_compatible(context.stack_trace)?;

            Ok(Scalar {
                dimension: Dimension::zero(),
                value: Float::new((this.value * PI).tanh()).unwrap_not_nan(context.stack_trace)?
            })
        }
    );
}

macro_rules! build_scalar_type {
    ($name:ident = $dimension:expr) => {
        #[derive(Debug, Hash, Clone)]
        pub struct $name(Scalar);

        impl StaticType for $name {
            fn static_type() -> ValueType {
                ValueType::Scalar(Some($dimension))
            }
        }

        impl StaticTypeName for $name {
            fn static_type_name() -> Cow<'static, str> {
                stringify!($name).into()
            }
        }

        impl enum_downcast::IntoVariant<$name> for Value {
            fn into_variant(self) -> Result<$name, Value> {
                Ok($name(self.into_variant()?))
            }
        }

        impl From<$name> for Scalar {
            fn from(value: $name) -> Scalar {
                value.0
            }
        }

        impl std::ops::Deref for $name {
            type Target = Scalar;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl From<$name> for common_data_types::RawFloat {
            fn from(value: $name) -> common_data_types::RawFloat {
                *value.value
            }
        }
    };
}

build_scalar_type!(Zero = Dimension::zero());
build_scalar_type!(Angle = Dimension::angle());
build_scalar_type!(Length = Dimension::length());

#[cfg(test)]
mod test {

    use crate::execution::{test_run, values::Boolean};

    use super::*;

    #[test]
    fn addition() {
        let product = test_run("3m + 2m").unwrap();
        assert_eq!(
            product,
            Scalar {
                dimension: Dimension::length(),
                value: Float::new(5.0).unwrap(),
            }
            .into()
        );
    }

    #[test]
    fn subtraction() {
        let product = test_run("3m - 2m").unwrap();
        assert_eq!(
            product,
            Scalar {
                dimension: Dimension::length(),
                value: Float::new(1.0).unwrap(),
            }
            .into()
        );
    }

    #[test]
    fn multiplication() {
        let product = test_run("3m * 2m").unwrap();
        assert_eq!(
            product,
            Scalar {
                dimension: Dimension::area(),
                value: Float::new(6.0).unwrap(),
            }
            .into()
        );
    }

    #[test]
    fn division() {
        let product = test_run("6'm^2' / 2m").unwrap();
        assert_eq!(
            product,
            Scalar {
                dimension: Dimension::length(),
                value: Float::new(3.0).unwrap(),
            }
            .into()
        );
    }

    #[test]
    fn comparisions() {
        let product = test_run("6m > 2m").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("2m > 6m").unwrap();
        assert_eq!(product, Boolean(false).into());

        let product = test_run("6m >= 2m").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("6m >= 6m").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("2m >= 6m").unwrap();
        assert_eq!(product, Boolean(false).into());

        let product = test_run("6m == 6m").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("6m == 5m").unwrap();
        assert_eq!(product, Boolean(false).into());

        let product = test_run("6m <= 5m").unwrap();
        assert_eq!(product, Boolean(false).into());

        let product = test_run("5m <= 5m").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("5m <= 6m").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("5m < 6m").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("6m < 6m").unwrap();
        assert_eq!(product, Boolean(false).into());

        let product = test_run("6m != 6m").unwrap();
        assert_eq!(product, Boolean(false).into());

        let product = test_run("6m != 5m").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn conversions() {
        let product = test_run("1m + 100cm == 2m").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("2m * 2m == 4'm^2'").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn to_signed_integer() {
        let product = test_run("100::to_signed_integer() == 100i").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("(-100)::to_signed_integer() == -100i").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("(100m / 1m)::to_signed_integer() == 100i").unwrap();
        assert_eq!(product, Boolean(true).into());

        test_run("100m::to_signed_integer()").unwrap_err();
    }

    #[test]
    fn to_unsigned_integer() {
        let product = test_run("100::to_unsigned_integer() == 100u").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("(100m / 1m)::to_unsigned_integer() == 100u").unwrap();
        assert_eq!(product, Boolean(true).into());

        test_run("(-100)::to_unsigned_integer()").unwrap_err();
        test_run("100m::to_unsigned_integer()").unwrap_err();
    }

    #[test]
    fn abs() {
        let product = test_run("(-100)::abs() == 100").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn clamp() {
        let product = test_run("(-100)::clamp(min = -50, max = 50) == -50").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("(100)::clamp(min = -50, max = 50) == 50").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn copysign() {
        let product = test_run("100::copysign(sign = -50) == -100").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn hypot() {
        // 141.4213562373095 was calculated Using `math.sqrt(100 ** 2 + 100 ** 2)` in Python 3.13.9.
        let product = test_run("100::hypot(other = 100) - 141.4213562373095 < 0.0001").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn is_finite() {
        let product = test_run("std.consts.Infinity::is_finite()").unwrap();
        assert_eq!(product, Boolean(false).into());

        let product = test_run("5::is_finite()").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("(std.consts.Infinity * 5)::is_finite()").unwrap();
        assert_eq!(product, Boolean(false).into());
    }

    #[test]
    fn is_infinite() {
        let product = test_run("std.consts.Infinity::is_infinite()").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("5::is_infinite()").unwrap();
        assert_eq!(product, Boolean(false).into());

        let product = test_run("(std.consts.Infinity * 5)::is_infinite()").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn is_normal() {
        let product = test_run("std.consts.Infinity::is_normal()").unwrap();
        assert_eq!(product, Boolean(false).into());

        let product = test_run("0::is_normal()").unwrap();
        assert_eq!(product, Boolean(false).into());

        let product = test_run("5::is_normal()").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("(-5)::is_normal()").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn cbrt() {
        let product = test_run("(64 'm^3')::cbrt() == 4m").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn pow() {
        let product = test_run("2m::pow(exp = 2) == 4 'm^2'").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn sqrt() {
        let product = test_run("(16 'm^2')::sqrt() == 4m").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn is_sign_negative() {
        let product = test_run("0::is_sign_negative()").unwrap();
        assert_eq!(product, Boolean(false).into());

        let product = test_run("(-0)::is_sign_negative()").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("5::is_sign_negative()").unwrap();
        assert_eq!(product, Boolean(false).into());

        let product = test_run("(-5)::is_sign_negative()").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn is_sign_positive() {
        let product = test_run("0::is_sign_positive()").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("(-0)::is_sign_positive()").unwrap();
        assert_eq!(product, Boolean(false).into());

        let product = test_run("5::is_sign_positive()").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("(-5)::is_sign_positive()").unwrap();
        assert_eq!(product, Boolean(false).into());
    }

    #[test]
    fn recip() {
        let product = test_run("16ft::recip() == 1 / 16ft").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn round() {
        let product = test_run("16.8ft::round(unit = 1ft) == 17ft").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("16.2ft::round(unit = 1ft) == 16ft").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn trunc() {
        let product = test_run("16.8ft::trunc(unit = 1ft) == 16ft").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("16.2ft::trunc(unit = 1ft) == 16ft").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn fract() {
        let product = test_run("16.8ft::fract(unit = 1ft) - 0.8ft < 0.0000000000001ft").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn floor() {
        let product = test_run("16.5ft::floor(unit = 1ft) == 16ft").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn ceil() {
        let product = test_run("16.5ft::ceil(unit = 1ft) == 17ft").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn max() {
        let product = test_run("16m::max(other = 15m) == 16m").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn min() {
        let product = test_run("16m::min(other = 15m) == 15m").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn signum() {
        let product = test_run("16m::signum() == 1").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("(-16m)::signum() == -1").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("0m::signum() == 1").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("(-0m)::signum() == -1").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn acos() {
        let product = test_run("0::acos() - 90deg < 0.000000000001deg").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn cosh_acosh() {
        let product = test_run("1rad::cosh()::acosh() - 1rad < 0.000000000001rad").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn cos() {
        let product = test_run("90deg::cos() - 1 < 0.000000000001").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn asin() {
        let product = test_run("0::asin() - 90deg < 0.000000000001deg").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn sinh_asinh() {
        let product = test_run("1rad::sinh()::asinh() - 1rad < 0.000000000001rad").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn sin() {
        let product = test_run("90deg::sin() - 1 < 0.000000000001").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn cossin() {
        let product = test_run("let angle = 45deg; in (angle::cossin() - <(angle::cos(), angle::sin())>)::norm() < 0.0000000001").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn atan() {
        let product = test_run("0::atan() == 0deg").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn tanh_atanh() {
        let product = test_run("1rad::tanh()::atanh() - 1rad < 0.000000000001rad").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn tan() {
        let product = test_run("0deg::tan() == 0").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn format() {
        let product = test_run(
            "\"{a} {b} {c:.2}\"::format(a = 10, b = 10m, c = 10.1234) == \"10 10m 10.12\"",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run(
            "\"{a:?} {b:?} {c:?.2}\"::format(a = 10, b = 10m, c = 10.1234) == \"10 10m 10.12\"",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());

        let product =
            test_run("\"{a:e} {b:e} {c:e.2}\"::format(a = 1000, b = 1000m, c = 1234.1234) == \"1e3 1e3m 1.23e3\"")
                .unwrap();
        assert_eq!(product, Boolean(true).into());

        let product =
            test_run("\"{a:E} {b:E} {c:E.2}\"::format(a = 1000, b = 1000m, c = 1234.1234) == \"1E3 1E3m 1.23E3\"")
                .unwrap();
        assert_eq!(product, Boolean(true).into());
    }
}
