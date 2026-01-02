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
    compile::SourceReference,
    execution::{
        errors::{ExpressionResult, GenericFailure, Raise},
        logging::RuntimeLog,
        values::{self, MissingAttributeError, StaticType, Vector2},
    },
    static_method,
};

use super::{value_type::ValueType, DowncastError, Object, StaticTypeName, Value};

pub trait UnwrapNotNan: Sized {
    fn unwrap_not_nan(self, stack_trace: &[SourceReference]) -> ExpressionResult<Float>;
}

impl UnwrapNotNan for std::result::Result<Float, FloatIsNan> {
    fn unwrap_not_nan(self, stack_trace: &[SourceReference]) -> ExpressionResult<Float> {
        match self {
            Ok(number) => Ok(number),
            Err(_float_is_nan) => {
                Err(GenericFailure("Result of arithmetic operation is NaN").to_error(stack_trace))
            }
        }
    }
}

#[derive(Debug, Hash, Clone, Copy, Eq, PartialEq)]
pub struct Scalar {
    pub dimension: Dimension,
    pub value: Float,
}

impl Object for Scalar {
    fn get_type(&self) -> ValueType {
        ValueType::Scalar(Some(self.dimension))
    }

    fn type_name(&self) -> Cow<'static, str> {
        units::get_dimension_name(&self.dimension)
    }

    // fn format(
    //     &self,
    //     log: &mut dyn RuntimeLog,
    //     stack_trace: [SourceReference],
    //     f: &mut dyn fmt::Write,
    //     style: Style,
    //     precision: Option<u8>,
    // ) -> ExpressionResult<S, ()> {
    //     // This just takes a reference to the unit name, so it's pretty cheap. I don't mind if it's not always used.
    //     // In the rare case that a unit name is generated and memory is allocated on the heap, well we're clearly
    //     // not about to format a number, so it's clear that we're going to use this.
    //     let unit_name = BASE_UNITS
    //         .get(&self.dimension)
    //         .cloned()
    //         .unwrap_or_else(|| format_dimension(&self.dimension));

    //     match (style, precision, self.is_number()) {
    //         (Style::Default, None, true) => {
    //             write!(f, "{}", self.value).unwrap_formatting_result(span)
    //         }
    //         (Style::Default, None, false) => {
    //             write!(f, "{} {unit_name}", self.value).unwrap_formatting_result(span)
    //         }
    //         (Style::Default, Some(precision), true) => {
    //             write!(f, "{:.1$}", self.value, precision as usize).unwrap_formatting_result(span)
    //         }
    //         (Style::Default, Some(precision), false) => {
    //             write!(f, "{:.1$} {unit_name}", self.value, precision as usize)
    //                 .unwrap_formatting_result(span)
    //         }
    //         (Style::Debug, None, true) => {
    //             write!(f, "{}", self.value).unwrap_formatting_result(span)
    //         }
    //         (Style::Debug, None, false) => {
    //             write!(f, "{} {unit_name}", self.value).unwrap_formatting_result(span)
    //         }
    //         (Style::Debug, Some(precision), true) => {
    //             write!(f, "{:.1$}", self.value, precision as usize).unwrap_formatting_result(span)
    //         }
    //         (Style::Debug, Some(precision), false) => {
    //             write!(f, "{:.1$} {unit_name}", self.value, precision as usize)
    //                 .unwrap_formatting_result(span)
    //         }
    //         (Style::Octal, _, true) => {
    //             if precision.is_some() {
    //                 log.push(LogMessage::FormatIntegerPrecision(span.clone()));
    //             }
    //             write!(f, "{:o}", self.value.into_inner() as usize).unwrap_formatting_result(span)
    //         }
    //         (Style::Octal, _, false) => {
    //             if precision.is_some() {
    //                 log.push(LogMessage::FormatIntegerPrecision(span.clone()));
    //             }
    //             write!(f, "{:o} {unit_name}", self.value.into_inner() as usize)
    //                 .unwrap_formatting_result(span)
    //         }
    //         (Style::Hex, _, true) => {
    //             if precision.is_some() {
    //                 log.push(LogMessage::FormatIntegerPrecision(span.clone()));
    //             }
    //             write!(f, "{:x}", self.value.into_inner() as usize).unwrap_formatting_result(span)
    //         }
    //         (Style::Hex, _, false) => {
    //             if precision.is_some() {
    //                 log.push(LogMessage::FormatIntegerPrecision(span.clone()));
    //             }
    //             write!(f, "{:x} {unit_name}", self.value.into_inner() as usize)
    //                 .unwrap_formatting_result(span)
    //         }
    //         (Style::CapitalizedHex, _, true) => {
    //             if precision.is_some() {
    //                 log.push(LogMessage::FormatIntegerPrecision(span.clone()));
    //             }
    //             write!(f, "{:X}", self.value.into_inner() as usize).unwrap_formatting_result(span)
    //         }
    //         (Style::CapitalizedHex, _, false) => {
    //             if precision.is_some() {
    //                 log.push(LogMessage::FormatIntegerPrecision(span.clone()));
    //             }
    //             write!(f, "{:X} {unit_name}", self.value.into_inner() as usize)
    //                 .unwrap_formatting_result(span)
    //         }
    //         (Style::Exponent, None, true) => {
    //             write!(f, "{:e}", self.value.into_inner() as usize).unwrap_formatting_result(span)
    //         }
    //         (Style::Exponent, None, false) => {
    //             write!(f, "{:e} {unit_name}", self.value.into_inner() as usize)
    //                 .unwrap_formatting_result(span)
    //         }
    //         (Style::Exponent, Some(precision), true) => {
    //             write!(f, "{:.1$e}", self.value.into_inner(), precision as usize)
    //                 .unwrap_formatting_result(span)
    //         }
    //         (Style::Exponent, Some(precision), false) => write!(
    //             f,
    //             "{:.1$e} {unit_name}",
    //             self.value.into_inner(),
    //             precision as usize
    //         )
    //         .unwrap_formatting_result(span),
    //         (Style::CapitalizedExponent, None, true) => {
    //             write!(f, "{:E}", self.value.into_inner()).unwrap_formatting_result(span)
    //         }
    //         (Style::CapitalizedExponent, None, false) => {
    //             write!(f, "{:E} {unit_name}", self.value.into_inner())
    //                 .unwrap_formatting_result(span)
    //         }
    //         (Style::CapitalizedExponent, Some(precision), true) => {
    //             write!(f, "{:.1$E}", self.value.into_inner(), precision as usize)
    //                 .unwrap_formatting_result(span)
    //         }
    //         (Style::CapitalizedExponent, Some(precision), false) => write!(
    //             f,
    //             "{:.1$E} {unit_name}",
    //             self.value.into_inner(),
    //             precision as usize
    //         )
    //         .unwrap_formatting_result(span),
    //     }
    // }

    fn addition(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: Value,
    ) -> ExpressionResult<Value> {
        let rhs = self.unpack_same_dimension(stack_trace, rhs)?;

        let value = Float::new(*self.value + *rhs.value).unwrap_not_nan(stack_trace)?;

        Ok(Self {
            value,
            ..self.clone()
        }
        .into())
    }
    fn subtraction(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: Value,
    ) -> ExpressionResult<Value> {
        let rhs = self.unpack_same_dimension(stack_trace, rhs)?;

        let value = Float::new(*self.value - *rhs.value).unwrap_not_nan(stack_trace)?;

        Ok(Self {
            value,
            ..self.clone()
        }
        .into())
    }
    fn multiply(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: Value,
    ) -> ExpressionResult<Value> {
        let rhs = rhs.downcast_ref::<Scalar>(stack_trace)?;
        self.multiply_by_scalar(stack_trace, rhs)
            .map(|rhs| rhs.into())
    }
    fn divide(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: Value,
    ) -> ExpressionResult<Value> {
        let rhs = rhs.downcast_ref::<Scalar>(stack_trace)?;
        self.divide_by_measurement(stack_trace, rhs)
            .map(|rhs| rhs.into())
    }
    fn unary_plus(
        self,
        _log: &mut dyn RuntimeLog,
        _stack_trace: &[SourceReference],
    ) -> ExpressionResult<Value> {
        Ok(self.clone().into())
    }
    fn unary_minus(
        self,
        _log: &mut dyn RuntimeLog,
        _stack_trace: &[SourceReference],
    ) -> ExpressionResult<Value> {
        Ok(Self {
            value: -self.value,
            ..self.clone()
        }
        .into())
    }
    fn cmp(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: Value,
    ) -> ExpressionResult<Ordering> {
        let rhs = rhs.downcast_ref::<Self>(stack_trace)?;
        if self.dimension == rhs.dimension {
            Ok(std::cmp::Ord::cmp(&self.value, &rhs.value))
        } else {
            Err(DowncastError {
                expected: self.type_name(),
                got: rhs.type_name(),
            }
            .to_error(stack_trace))
        }
    }

    fn get_attribute(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        attribute: &str,
    ) -> ExpressionResult<Value> {
        match attribute {
            "to_signed_integer" => Ok(static_method!(
                Scalar_to_signed_integer(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self) -> ValueType::SignedInteger
                {
                    if this.dimension.is_zero_dimension() {
                        Ok(values::SignedInteger::from(*this.value as i64).into())
                    } else {
                        Err(GenericFailure("Only zero dimensional scalars can be converted into an integer")
                            .to_error(stack_trace.iter()))
                    }
                }).clone()),
            "to_unsigned_integer" => Ok(static_method!(
                Scalar_to_unsigned_integer(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self) -> ValueType::UnsignedInteger
                {
                    if this.dimension.is_zero_dimension() {
                        if *this.value >= 0.0 {
                            Ok(values::UnsignedInteger::from(*this.value as u64).into())
                        } else {
                            Err(GenericFailure("Negative values cannot be converted to signed integers")
                                .to_error(stack_trace.iter()))
                        }
                    } else {
                        Err(GenericFailure("Only zero dimensional scalars can be converted into an integer")
                            .to_error(stack_trace.iter()))
                    }
                }).clone()),
            "abs" => Ok(static_method!(
                Scalar_abs(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self) -> ValueType::UnsignedInteger
                {
                    Ok(Self {
                        dimension: this.dimension,
                        value: Float::new(this.value.abs()).unwrap_not_nan(stack_trace)?
                    }.into())
                }).clone()),
            "clamp" => Ok(static_method!(
                Scalar_clamp(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self,
                    min: Value,
                    max: Value) -> ValueType::UnsignedInteger
                {
                    let min = this.unpack_same_dimension(stack_trace, min)?;
                    let max = this.unpack_same_dimension(stack_trace, max)?;
    
                    Ok(Self {
                        dimension: this.dimension,
                        value: this.value.clamp(min.value, max.value)
                    }.into())
                }).clone()),
            "copysign" => Ok(static_method!(
                Scalar_copysign(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self,
                    sign: Zero) -> ValueType::UnsignedInteger
                {
                    Ok(Self {
                        dimension: Dimension::zero(),
                        value: Float::new(this.value.copysign(*sign.value)).unwrap_not_nan(stack_trace)?
                    }.into())
                }).clone()),
            "hypot" => Ok(static_method!(
                Scalar_hypot(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self,
                    other: Value) -> ValueType::UnsignedInteger
                {
                    let other = this.unpack_same_dimension(stack_trace, other)?;

                    Ok(Self {
                        dimension: Dimension::zero(),
                        value: Float::new(this.value.hypot(*other.value)).unwrap_not_nan(stack_trace)?
                    }.into())
                }).clone()),
            "is_finite" => Ok(static_method!(
                Scalar_is_finite(
                    _log: &mut dyn RuntimeLog,
                    _stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self) -> ValueType::Boolean
                {
                    Ok(values::Boolean(this.value.is_finite()).into())
                }).clone()),
            "is_infinite" => Ok(static_method!(
                Scalar_is_infinite(
                    _log: &mut dyn RuntimeLog,
                    _stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self) -> ValueType::Boolean
                {
                    Ok(values::Boolean(this.value.is_infinite()).into())
                }).clone()),
            "is_normal" => Ok(static_method!(
                Scalar_is_normal(
                    _log: &mut dyn RuntimeLog,
                    _stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self) -> ValueType::Boolean
                {
                    Ok(values::Boolean(this.value.is_normal()).into())
                }).clone()),
            "cbrt" => Ok(static_method!(
                Scalar_cbrt(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self) -> ValueType::Boolean
                {
                    Ok(Self {
                        dimension: this.dimension / 3,
                        value: Float::new(this.value.cbrt()).unwrap_not_nan(stack_trace)?
                    }.into())
                }).clone()),
            "pow" => Ok(static_method!(
                Scalar_pow(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self,
                    exp: Zero) -> ValueType::Boolean
                {
                    Ok(Self {
                        dimension: this.dimension * *exp.value as i8,
                        value: Float::new(this.value.powf(*exp.value)).unwrap_not_nan(stack_trace)?
                    }.into())
                }).clone()),
            "sqrt" => Ok(static_method!(
                Scalar_sqrt(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self) -> ValueType::Boolean
                {
                    Ok(Self {
                        dimension: this.dimension / 2,
                        value: Float::new(this.value.sqrt()).unwrap_not_nan(stack_trace)?
                    }.into())
                }).clone()),
            "is_sign_negative" => Ok(static_method!(
                Scalar_is_sign_negative(
                    _log: &mut dyn RuntimeLog,
                    _stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self) -> ValueType::Boolean
                {
                    Ok(values::Boolean(this.value.is_sign_negative()).into())
                }).clone()),
            "is_sign_positive" => Ok(static_method!(
                Scalar_is_sign_positive(
                    _log: &mut dyn RuntimeLog,
                    _stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self) -> ValueType::Boolean
                {
                    Ok(values::Boolean(this.value.is_sign_positive()).into())
                }).clone()),
            "recip" => Ok(static_method!(
                Scalar_recip(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self) -> ValueType::Boolean
                {
                    Ok(Self {
                        dimension: -this.dimension,
                        value: Float::new(this.value.recip()).unwrap_not_nan(stack_trace)?
                    }.into())
                }).clone()),
            "round" => Ok(static_method!(
                Scalar_round(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self,
                    unit: Value) -> ValueType::Boolean
                {
                    let unit = this.unpack_same_dimension(stack_trace, unit)?;
                   
                    let value = this.value / unit.value;

                    Ok(Self {
                        dimension: this.dimension,
                        value: Float::new(value.round() * *unit.value).unwrap_not_nan(stack_trace)?
                    }.into())
                }).clone()),
            "trunc" => Ok(static_method!(
                Scalar_trunc(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self,
                    unit: Value) -> ValueType::Boolean
                {
                    let unit = this.unpack_same_dimension(stack_trace, unit)?;
                   
                    let value = this.value / unit.value;

                    Ok(Self {
                        dimension: this.dimension,
                        value: Float::new(value.trunc() * *unit.value).unwrap_not_nan(stack_trace)?
                    }.into())
                }).clone()),
            "fract" => Ok(static_method!(
                Scalar_fract(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self,
                    unit: Value) -> ValueType::Boolean
                {
                    let unit = this.unpack_same_dimension(stack_trace, unit)?;
                   
                    let value = this.value / unit.value;

                    Ok(Self {
                        dimension: this.dimension,
                        value: Float::new(value.fract() * *unit.value).unwrap_not_nan(stack_trace)?
                    }.into())
                }).clone()),
            "floor" => Ok(static_method!(
                Scalar_floor(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self,
                    unit: Value) -> ValueType::Boolean
                {
                    let unit = this.unpack_same_dimension(stack_trace, unit)?;
                   
                    let value = this.value / unit.value;

                    Ok(Self {
                        dimension: this.dimension,
                        value: Float::new(value.floor() * *unit.value).unwrap_not_nan(stack_trace)?
                    }.into())
                }).clone()),
            "ceil" => Ok(static_method!(
                Scalar_ceil(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self,
                    unit: Value) -> ValueType::Boolean
                {
                    let unit = this.unpack_same_dimension(stack_trace, unit)?;
                   
                    let value = this.value / unit.value;

                    Ok(Self {
                        dimension: this.dimension,
                        value: Float::new(value.ceil() * *unit.value).unwrap_not_nan(stack_trace)?
                    }.into())
                }).clone()),
            "max" => Ok(static_method!(
                Scalar_max(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self,
                    other: Value) -> ValueType::Boolean
                {
                    let other = this.unpack_same_dimension(stack_trace, other)?;
                   
                    Ok(Self {
                        dimension: this.dimension,
                        value: this.value.max(other.value)
                    }.into())
                }).clone()),
            "min" => Ok(static_method!(
                Scalar_min(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self,
                    other: Value) -> ValueType::Boolean
                {
                    let other = this.unpack_same_dimension(stack_trace, other)?;
                   
                    Ok(Self {
                        dimension: this.dimension,
                        value: this.value.min(other.value)
                    }.into())
                }).clone()),
            "signum" => Ok(static_method!(
                Scalar_signum(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self) -> ValueType::Boolean
                {
                    Ok(Self {
                        dimension: Dimension::zero(),
                        value: Float::new(this.value.signum()).unwrap_not_nan(stack_trace)?
                    }.into())
                }).clone()),
            "acos" => Ok(static_method!(
                Scalar_acos(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self) -> ValueType::Boolean
                {
                    this.check_inverse_trig_compatible(stack_trace)?;

                    Ok(Self {
                        dimension: Dimension::angle(),
                        value: Float::new((this.value * PI).acos()).unwrap_not_nan(stack_trace)?
                    }.into())
                }).clone()),
            "acosh" => Ok(static_method!(
                Scalar_acosh(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self) -> ValueType::Boolean
                {
                    this.check_inverse_trig_compatible(stack_trace)?;

                    Ok(Self {
                        dimension: Dimension::angle(),
                        value: Float::new((this.value).acosh() / PI).unwrap_not_nan(stack_trace)?
                    }.into())
                }).clone()),
            "cos" => Ok(static_method!(
                Scalar_cos(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self) -> ValueType::Boolean
                {
                    this.check_trig_compatible(stack_trace)?;

                    Ok(Self {
                        dimension: Dimension::zero(),
                        value: Float::new((this.value * PI).cos()).unwrap_not_nan(stack_trace)?
                    }.into())
                }).clone()),
            "cosh" => Ok(static_method!(
                Scalar_cosh(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self) -> ValueType::Boolean
                {
                    this.check_trig_compatible(stack_trace)?;

                    Ok(Self {
                        dimension: Dimension::zero(),
                        value: Float::new((this.value * PI).cosh()).unwrap_not_nan(stack_trace)?
                    }.into())
                }).clone()),
            "asin" => Ok(static_method!(
                Scalar_asin(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self) -> ValueType::Boolean
                {
                    this.check_inverse_trig_compatible(stack_trace)?;

                    Ok(Self {
                        dimension: Dimension::angle(),
                        value: Float::new((this.value * PI).asin()).unwrap_not_nan(stack_trace)?
                    }.into())
                }).clone()),
            "asinh" => Ok(static_method!(
                Scalar_asinh(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self) -> ValueType::Boolean
                {
                    this.check_inverse_trig_compatible(stack_trace)?;

                    Ok(Self {
                        dimension: Dimension::angle(),
                        value: Float::new((this.value).asinh() / PI).unwrap_not_nan(stack_trace)?
                    }.into())
                }).clone()),
            "sin" => Ok(static_method!(
                Scalar_sin(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self) -> ValueType::Boolean
                {
                    this.check_trig_compatible(stack_trace)?;

                    Ok(Self {
                        dimension: Dimension::zero(),
                        value: Float::new((this.value * PI).sin()).unwrap_not_nan(stack_trace)?
                    }.into())
                }).clone()),
            "sinh" => Ok(static_method!(
                Scalar_sinh(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self) -> ValueType::Boolean
                {
                    this.check_trig_compatible(stack_trace)?;

                    Ok(Self {
                        dimension: Dimension::zero(),
                        value: Float::new((this.value * PI).sinh()).unwrap_not_nan(stack_trace)?
                    }.into())
                }).clone()),
            "cossin" => Ok(static_method!(
                Scalar_cossin(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self) -> ValueType::Boolean
                {
                    this.check_trig_compatible(stack_trace)?;

                    let (sin, cos) = (this.value * PI).sin_cos();
                    Ok(Vector2::new(stack_trace, Dimension::zero(), [cos, sin])?.into())
                }).clone()),
            "atan" => Ok(static_method!(
                Scalar_atan(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self) -> ValueType::Boolean
                {
                    this.check_inverse_trig_compatible(stack_trace)?;

                    Ok(Self {
                        dimension: Dimension::angle(),
                        value: Float::new((this.value * PI).atan()).unwrap_not_nan(stack_trace)?
                    }.into())
                }).clone()),
            "atanh" => Ok(static_method!(
                Scalar_atanh(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self) -> ValueType::Boolean
                {
                    this.check_inverse_trig_compatible(stack_trace)?;

                    Ok(Self {
                        dimension: Dimension::angle(),
                        value: Float::new((this.value).atanh() / PI).unwrap_not_nan(stack_trace)?
                    }.into())
                }).clone()),
            "tan" => Ok(static_method!(
                Scalar_tan(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self) -> ValueType::Boolean
                {
                    this.check_trig_compatible(stack_trace)?;

                    Ok(Self {
                        dimension: Dimension::zero(),
                        value: Float::new((this.value * PI).tan()).unwrap_not_nan(stack_trace)?
                    }.into())
                }).clone()),
            "tanh" => Ok(static_method!(
                Scalar_tanh(
                    _log: &mut dyn RuntimeLog,
                    stack_trace: &mut Vec<SourceReference>,
                    _stack: &mut Stack,
                    this: Self) -> ValueType::Boolean
                {
                    this.check_trig_compatible(stack_trace)?;

                    Ok(Self {
                        dimension: Dimension::zero(),
                        value: Float::new((this.value * PI).tanh()).unwrap_not_nan(stack_trace)?
                    }.into())
                }).clone()),
            _ => Err(MissingAttributeError {
                name: attribute.into(),
            }.to_error(stack_trace)),
        }
    }

    // fn export(
    //     &self,
    //     _log: &mut dyn RuntimeLog,
    //     _stack_trace: &[SourceReference],
    // ) -> ExpressionResult<SerializableValue> {
    //     Ok(SerializableValue::Scalar(self.clone()))
    // }
}

impl StaticTypeName for Scalar {
    fn static_type_name() -> &'static str {
        "Scalar"
    }
}

impl StaticType for Scalar {
    fn static_type() -> ValueType {
        ValueType::Scalar(None)
    }
}

impl Scalar {
    fn multiply_by_scalar(
        &self,
        stack_trace: &[SourceReference],
        rhs: &Self,
    ) -> ExpressionResult<Self> {
        let value = Float::new(*self.value * *rhs.value).unwrap_not_nan(stack_trace)?;
        let dimension = self.dimension + rhs.dimension;

        Ok(Self { dimension, value })
    }

    fn divide_by_measurement(
        &self,
        stack_trace: &[SourceReference],
        rhs: &Self,
    ) -> ExpressionResult<Self> {
        let value = Float::new(*self.value / *rhs.value).unwrap_not_nan(stack_trace)?;
        let dimension = self.dimension - rhs.dimension;

        Ok(Self { dimension, value })
    }

    fn check_inverse_trig_compatible(
        &self,
        stack_trace: &[SourceReference],
    ) -> ExpressionResult<()> {
        if self.dimension.is_zero_dimension() {
            Ok(())
        } else {
            Err(GenericFailure("Inverse trigonometric functions can only be used with zero dimensional types (Angles, Ratios)").to_error(stack_trace))
        }
    }

    fn check_trig_compatible(&self, stack_trace: &[SourceReference]) -> ExpressionResult<()> {
        if self.dimension.is_zero_dimension() && self.dimension.ratio_type_hint.is_angle() {
            Ok(())
        } else {
            Err(
                GenericFailure("Trigonometric functions can only be used with angles")
                    .to_error(stack_trace),
            )
        }
    }

    fn unpack_same_dimension(
        self,
        stack_trace: &[SourceReference],
        rhs: Value,
    ) -> ExpressionResult<Self> {
        if let Value::Scalar(rhs) = rhs {
            if self.dimension == rhs.dimension {
                Ok(rhs)
            } else {
                Err(DowncastError {
                    expected: self.type_name(),
                    got: rhs.type_name(),
                }
                .to_error(stack_trace))
            }
        } else {
            Err(DowncastError {
                expected: self.type_name(),
                got: rhs.type_name(),
            }
            .to_error(stack_trace))
        }
    }
}

macro_rules! build_scalar_type {
    ($name:ident = $dimension:expr) => {
        struct $name(Scalar);

        impl StaticType for $name {
            fn static_type() -> ValueType {
                ValueType::Scalar(Some($dimension))
            }
        }

        impl StaticTypeName for $name {
            fn static_type_name() -> &'static str {
                stringify!($name)
            }
        }

        impl enum_downcast::IntoVariant<$name> for Value {
            fn into_variant(self) -> Result<$name, Value> {
                Ok($name(self.into_variant()?))
            }
        }

        impl Into<Scalar> for $name {
            fn into(self) -> Scalar {
                self.0
            }
        }

        impl std::ops::Deref for $name {
            type Target = Scalar;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }
    };
}

build_scalar_type!(Zero = Dimension::zero());
build_scalar_type!(Angle = Dimension::angle());

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
}
