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
use std::{borrow::Cow, cmp::Ordering, fmt, str::FromStr};

use enum_downcast::EnumDowncast;
use fj_math::Scalar as FornjotScalar;
use imstr::ImString;
use nalgebra::Const;
use serde::{
    de::{self, Visitor},
    Deserialize, Serialize,
};
use uom::{
    si::{Dimension as UomDimension, Quantity, Units},
    typenum::ToInt,
};

use common_data_types::{consts, ConversionFactor, Dimension, Float, RatioTypeHint, RawFloat};

use crate::script::{
    execution::{
        types::{
            function::AutoCall,
            number::{unwrap_float, UnwrapNotNan},
            NamedObject, Object, OperatorResult, SString, SerializableValue, Style,
            UnwrapFormattingResult, Value,
        },
        ExecutionContext, Failure,
    },
    logging::{LogMessage, RuntimeLog},
    parsing::{self, Expression, VariableType},
    Span,
};

use super::{
    format_dimension, get_dimension_name, vector::NVector, ConvertUnit, Vector2, BASE_UNITS,
    CONVERSION_FACTORS,
};

macro_rules! define_fixed_dimension_scalar {
    ($name:ident, $dimension:expr) => {
        #[derive(Debug, Clone, Copy, Eq, PartialEq)]
        pub struct $name {
            pub value: Float,
        }

        impl From<$name> for Scalar {
            fn from(value: $name) -> Scalar {
                Scalar {
                    dimension: $dimension,
                    value: value.value,
                }
            }
        }

        impl<S: Span> From<$name> for Value<S> {
            fn from(value: $name) -> Value<S> {
                Value::from(Scalar::from(value))
            }
        }

        impl<S: Span> TryFrom<Value<S>> for $name {
            type Error = ();

            fn try_from(value: Value<S>) -> Result<Self, ()> {
                let scalar: Scalar = value.enum_downcast().map_err(|_| ())?;
                Self::try_from(scalar)
            }
        }

        impl TryFrom<Scalar> for $name {
            type Error = ();

            fn try_from(value: Scalar) -> Result<Self, ()> {
                if value.dimension == $dimension {
                    Ok(Self { value: value.value })
                } else {
                    Err(())
                }
            }
        }

        impl<'a> TryFrom<&'a Scalar> for $name {
            type Error = ();

            fn try_from(value: &'a Scalar) -> Result<Self, ()> {
                if value.dimension == $dimension {
                    Ok(Self { value: value.value })
                } else {
                    Err(())
                }
            }
        }

        impl NamedObject for $name {
            fn static_type_name() -> &'static str {
                "Scalar"
            }
        }
    };
}

define_fixed_dimension_scalar!(Length, Dimension::length());
define_fixed_dimension_scalar!(Angle, Dimension::angle());
define_fixed_dimension_scalar!(Number, Dimension::zero());

impl Number {
    pub fn to_index(self) -> isize {
        self.value.round() as isize
    }
}

impl From<Float> for Number {
    fn from(value: Float) -> Self {
        Self { value }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scalar {
    pub(super) dimension: Dimension,
    pub(super) value: Float,
}

impl<S: Span> Object<S> for Scalar {
    fn matches_type(
        &self,
        ty: &VariableType<S>,
        _log: &mut dyn RuntimeLog<S>,
        _variable_name_span: &S,
    ) -> OperatorResult<S, bool> {
        Ok(if let VariableType::Scalar(name) = ty {
            name.as_str() == Object::<S>::type_name(self).as_ref()
        } else {
            false
        })
    }

    fn type_name(&self) -> Cow<'static, str> {
        get_dimension_name(&self.dimension)
    }

    fn format(
        &self,
        log: &mut dyn RuntimeLog<S>,
        span: &S,
        f: &mut dyn fmt::Write,
        style: Style,
        precision: Option<u8>,
    ) -> OperatorResult<S, ()> {
        // This just takes a reference to the unit name, so it's pretty cheap. I don't mind if it's not always used.
        // In the rare case that a unit name is generated and memory is allocated on the heap, well we're clearly
        // not about to format a number, so it's clear that we're going to use this.
        let unit_name = BASE_UNITS
            .get(&self.dimension)
            .cloned()
            .unwrap_or_else(|| format_dimension(&self.dimension));

        match (style, precision, self.is_number()) {
            (Style::Default, None, true) => {
                write!(f, "{}", self.value).unwrap_formatting_result(span)
            }
            (Style::Default, None, false) => {
                write!(f, "{} {unit_name}", self.value).unwrap_formatting_result(span)
            }
            (Style::Default, Some(precision), true) => {
                write!(f, "{:.1$}", self.value, precision as usize).unwrap_formatting_result(span)
            }
            (Style::Default, Some(precision), false) => {
                write!(f, "{:.1$} {unit_name}", self.value, precision as usize)
                    .unwrap_formatting_result(span)
            }
            (Style::Debug, None, true) => {
                write!(f, "{}", self.value).unwrap_formatting_result(span)
            }
            (Style::Debug, None, false) => {
                write!(f, "{} {unit_name}", self.value).unwrap_formatting_result(span)
            }
            (Style::Debug, Some(precision), true) => {
                write!(f, "{:.1$}", self.value, precision as usize).unwrap_formatting_result(span)
            }
            (Style::Debug, Some(precision), false) => {
                write!(f, "{:.1$} {unit_name}", self.value, precision as usize)
                    .unwrap_formatting_result(span)
            }
            (Style::Octal, _, true) => {
                if precision.is_some() {
                    log.push(LogMessage::FormatIntegerPrecision(span.clone()));
                }
                write!(f, "{:o}", self.value.into_inner() as usize).unwrap_formatting_result(span)
            }
            (Style::Octal, _, false) => {
                if precision.is_some() {
                    log.push(LogMessage::FormatIntegerPrecision(span.clone()));
                }
                write!(f, "{:o} {unit_name}", self.value.into_inner() as usize)
                    .unwrap_formatting_result(span)
            }
            (Style::Hex, _, true) => {
                if precision.is_some() {
                    log.push(LogMessage::FormatIntegerPrecision(span.clone()));
                }
                write!(f, "{:x}", self.value.into_inner() as usize).unwrap_formatting_result(span)
            }
            (Style::Hex, _, false) => {
                if precision.is_some() {
                    log.push(LogMessage::FormatIntegerPrecision(span.clone()));
                }
                write!(f, "{:x} {unit_name}", self.value.into_inner() as usize)
                    .unwrap_formatting_result(span)
            }
            (Style::CapitalizedHex, _, true) => {
                if precision.is_some() {
                    log.push(LogMessage::FormatIntegerPrecision(span.clone()));
                }
                write!(f, "{:X}", self.value.into_inner() as usize).unwrap_formatting_result(span)
            }
            (Style::CapitalizedHex, _, false) => {
                if precision.is_some() {
                    log.push(LogMessage::FormatIntegerPrecision(span.clone()));
                }
                write!(f, "{:X} {unit_name}", self.value.into_inner() as usize)
                    .unwrap_formatting_result(span)
            }
            (Style::Exponent, None, true) => {
                write!(f, "{:e}", self.value.into_inner() as usize).unwrap_formatting_result(span)
            }
            (Style::Exponent, None, false) => {
                write!(f, "{:e} {unit_name}", self.value.into_inner() as usize)
                    .unwrap_formatting_result(span)
            }
            (Style::Exponent, Some(precision), true) => {
                write!(f, "{:.1$e}", self.value.into_inner(), precision as usize)
                    .unwrap_formatting_result(span)
            }
            (Style::Exponent, Some(precision), false) => write!(
                f,
                "{:.1$e} {unit_name}",
                self.value.into_inner(),
                precision as usize
            )
            .unwrap_formatting_result(span),
            (Style::CapitalizedExponent, None, true) => {
                write!(f, "{:E}", self.value.into_inner()).unwrap_formatting_result(span)
            }
            (Style::CapitalizedExponent, None, false) => {
                write!(f, "{:E} {unit_name}", self.value.into_inner())
                    .unwrap_formatting_result(span)
            }
            (Style::CapitalizedExponent, Some(precision), true) => {
                write!(f, "{:.1$E}", self.value.into_inner(), precision as usize)
                    .unwrap_formatting_result(span)
            }
            (Style::CapitalizedExponent, Some(precision), false) => write!(
                f,
                "{:.1$E} {unit_name}",
                self.value.into_inner(),
                precision as usize
            )
            .unwrap_formatting_result(span),
        }
    }

    fn addition(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        rhs: &Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        let rhs = self.unpack_for_addition_or_subtraction(span, rhs)?;

        let value = Float::new(*self.value + *rhs.value).unwrap_not_nan(span)?;

        Ok(Self {
            value,
            ..self.clone()
        }
        .into())
    }
    fn subtraction(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        rhs: &Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        let rhs = self.unpack_for_addition_or_subtraction(span, rhs)?;

        let value = Float::new(*self.value - *rhs.value).unwrap_not_nan(span)?;

        Ok(Self {
            value,
            ..self.clone()
        }
        .into())
    }
    fn multiply(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        rhs: &Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        let rhs = rhs.downcast_ref::<Scalar>(span)?;
        self.multiply_by_scalar(span, rhs).map(|rhs| rhs.into())
    }
    fn divide(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        rhs: &Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        let rhs = rhs.downcast_ref::<Scalar>(span)?;
        self.divide_by_measurement(span, rhs).map(|rhs| rhs.into())
    }
    fn unary_plus(&self, _log: &mut dyn RuntimeLog<S>, _span: &S) -> OperatorResult<S, Value<S>> {
        Ok(self.clone().into())
    }
    fn unary_minus(&self, _log: &mut dyn RuntimeLog<S>, _span: &S) -> OperatorResult<S, Value<S>> {
        Ok(Self {
            value: -self.value,
            ..self.clone()
        }
        .into())
    }
    fn cmp(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        rhs: &Value<S>,
    ) -> OperatorResult<S, Ordering> {
        let rhs = rhs.downcast_ref::<Self>(span)?;
        if self.dimension == rhs.dimension {
            Ok(std::cmp::Ord::cmp(&self.value, &rhs.value))
        } else {
            Err(Failure::ExpectedGot(
                span.clone(),
                Object::<S>::type_name(self),
                Object::<S>::type_name(rhs),
            ))
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
            "to_number" => |_context: &mut ExecutionContext<S>,
                            span: &S,
                            ty: SString|
             -> OperatorResult<S, Value<S>> {
                self.convert_to_number(span, &ty.as_str(span)?)
            }
            .auto_call(context, span, arguments, expressions),
            "abs" => {
                |_context: &mut ExecutionContext<S>, span: &S| -> OperatorResult<S, Value<S>> {
                    Ok(Self {
                        value: Float::new(self.value.abs()).unwrap_not_nan(span)?,
                        ..self.clone()
                    }
                    .into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "clamp" => |_context: &mut ExecutionContext<S>,
                        span: &S,
                        min: Value<S>,
                        max: Value<S>|
             -> OperatorResult<S, Value<S>> {
                let min = self.unpack_for_addition_or_subtraction(span, &min)?;
                let max = self.unpack_for_addition_or_subtraction(span, &max)?;

                Ok(Self {
                    value: self.value.clamp(min.value, max.value),
                    ..self.clone()
                }
                .into())
            }
            .auto_call(context, span, arguments, expressions),
            "copysign" => |_context: &mut ExecutionContext<S>,
                           span: &S,
                           sign: Number|
             -> OperatorResult<S, Value<S>> {
                let sign = sign.to_index();

                Ok(Self {
                    value: Float::new(self.value.copysign(sign as RawFloat))
                        .unwrap_not_nan(span)?,
                    ..self.clone()
                }
                .into())
            }
            .auto_call(context, span, arguments, expressions),
            "hypot" => |_context: &mut ExecutionContext<S>,
                        span: &S,
                        other: Value<S>|
             -> OperatorResult<S, Value<S>> {
                let other = self.unpack_for_addition_or_subtraction(span, &other)?;

                Ok(Self {
                    value: Float::new(self.value.hypot(*other.value)).unwrap_not_nan(span)?,
                    ..self.clone()
                }
                .into())
            }
            .auto_call(context, span, arguments, expressions),
            "is_finite" => {
                |_context: &mut ExecutionContext<S>, _span: &S| -> OperatorResult<S, Value<S>> {
                    Ok(self.value.is_finite().into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "is_infinite" => {
                |_context: &mut ExecutionContext<S>, _span: &S| -> OperatorResult<S, Value<S>> {
                    Ok(self.value.is_infinite().into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "is_normal" => {
                |_context: &mut ExecutionContext<S>, _span: &S| -> OperatorResult<S, Value<S>> {
                    Ok(self.value.is_normal().into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "cbrt" => {
                |_context: &mut ExecutionContext<S>, span: &S| -> OperatorResult<S, Value<S>> {
                    Ok(Self {
                        dimension: self.dimension / 3,
                        value: Float::new(self.value.cbrt()).unwrap_not_nan(span)?,
                    }
                    .into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "pow" => |_context: &mut ExecutionContext<S>,
                      _span: &S,
                      exponent: Scalar|
             -> OperatorResult<S, Value<S>> {
                let exponent = exponent.to_number(span)?;

                self.check_is_zero_dimension(span)?;

                Ok(Self {
                    dimension: self.dimension,
                    value: Float::new(self.value.powf(exponent.into_inner()))
                        .unwrap_not_nan(span)?,
                }
                .into())
            }
            .auto_call(context, span, arguments, expressions),
            "powi" => |_context: &mut ExecutionContext<S>,
                       _span: &S,
                       exponent: Number|
             -> OperatorResult<S, Value<S>> {
                let exponent = exponent.to_index() as i8;

                Ok(Self {
                    dimension: self.dimension * exponent,
                    value: Float::new(self.value.powi(exponent as i32)).unwrap_not_nan(span)?,
                }
                .into())
            }
            .auto_call(context, span, arguments, expressions),
            "sqrt" => {
                |_context: &mut ExecutionContext<S>, _span: &S| -> OperatorResult<S, Value<S>> {
                    Ok(Self {
                        dimension: self.dimension / 2,
                        value: Float::new(self.value.sqrt()).unwrap_not_nan(span)?,
                    }
                    .into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "is_sign_negative" => {
                |_context: &mut ExecutionContext<S>, _span: &S| -> OperatorResult<S, Value<S>> {
                    Ok(self.value.is_sign_negative().into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "is_sign_positive" => {
                |_context: &mut ExecutionContext<S>, _span: &S| -> OperatorResult<S, Value<S>> {
                    Ok(self.value.is_sign_positive().into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "recip" => {
                |_context: &mut ExecutionContext<S>, _span: &S| -> OperatorResult<S, Value<S>> {
                    Ok(Self {
                        dimension: -self.dimension,
                        value: Float::new(1.0 / self.value.into_inner()).unwrap_not_nan(span)?,
                    }
                    .into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "round" => |_context: &mut ExecutionContext<S>,
                        span: &S,
                        unit: Value<S>|
             -> OperatorResult<S, Value<S>> {
                let conversion_factor = Self::get_operation_conversion_factor(span, unit)?;

                let value = conversion_factor.convert_from_measurement_to_number(span, self)?;
                let value = Float::new(value.round()).unwrap_not_nan(span)?;

                Ok(Self {
                    dimension: self.dimension,
                    value: conversion_factor.convert_to_base_unit(value),
                }
                .into())
            }
            .auto_call_optional(context, span, arguments, expressions),
            "trunc" => |_context: &mut ExecutionContext<S>,
                        span: &S,
                        unit: Value<S>|
             -> OperatorResult<S, Value<S>> {
                let conversion_factor = Self::get_operation_conversion_factor(span, unit)?;

                let value = conversion_factor.convert_from_measurement_to_number(span, self)?;
                let value = Float::new(value.trunc()).unwrap_not_nan(span)?;

                Ok(Self {
                    dimension: self.dimension,
                    value: conversion_factor.convert_to_base_unit(value),
                }
                .into())
            }
            .auto_call_optional(context, span, arguments, expressions),
            "fract" => |_context: &mut ExecutionContext<S>,
                        span: &S,
                        unit: Value<S>|
             -> OperatorResult<S, Value<S>> {
                let conversion_factor = Self::get_operation_conversion_factor(span, unit)?;

                let value = conversion_factor.convert_from_measurement_to_number(span, self)?;
                let value = Float::new(value.fract()).unwrap_not_nan(span)?;

                Ok(Self {
                    dimension: self.dimension,
                    value: conversion_factor.convert_to_base_unit(value),
                }
                .into())
            }
            .auto_call_optional(context, span, arguments, expressions),
            "floor" => |_context: &mut ExecutionContext<S>,
                        span: &S,
                        unit: Value<S>|
             -> OperatorResult<S, Value<S>> {
                let conversion_factor = Self::get_operation_conversion_factor(span, unit)?;

                let value = conversion_factor.convert_from_measurement_to_number(span, self)?;
                let value = Float::new(value.floor()).unwrap_not_nan(span)?;

                Ok(Self {
                    dimension: self.dimension,
                    value: conversion_factor.convert_to_base_unit(value),
                }
                .into())
            }
            .auto_call_optional(context, span, arguments, expressions),
            "ceil" => |_context: &mut ExecutionContext<S>,
                       span: &S,
                       unit: Value<S>|
             -> OperatorResult<S, Value<S>> {
                let conversion_factor = Self::get_operation_conversion_factor(span, unit)?;

                let value = conversion_factor.convert_from_measurement_to_number(span, self)?;
                let value = Float::new(value.ceil()).unwrap_not_nan(span)?;

                Ok(Self {
                    dimension: self.dimension,
                    value: conversion_factor.convert_to_base_unit(value),
                }
                .into())
            }
            .auto_call_optional(context, span, arguments, expressions),
            "max" => |_context: &mut ExecutionContext<S>,
                      _span: &S,
                      other: Value<S>|
             -> OperatorResult<S, Value<S>> {
                let other = self.unpack_for_addition_or_subtraction(span, &other)?;
                Ok(Float::new(*self.value.max(other.value))
                    .unwrap_not_nan(span)?
                    .into())
            }
            .auto_call(context, span, arguments, expressions),
            "min" => |_context: &mut ExecutionContext<S>,
                      _span: &S,
                      other: Value<S>|
             -> OperatorResult<S, Value<S>> {
                let other = self.unpack_for_addition_or_subtraction(span, &other)?;
                Ok(Float::new(*self.value.min(other.value))
                    .unwrap_not_nan(span)?
                    .into())
            }
            .auto_call(context, span, arguments, expressions),
            "mul_add" => |context: &mut ExecutionContext<S>,
                          span: &S,
                          a: Value<S>,
                          b: Value<S>|
             -> OperatorResult<S, Value<S>> {
                let multiply_result = self.multiply(context.log, span, &b)?;
                let add_result = multiply_result.addition(context.log, span, &a)?;

                Ok(add_result)
            }
            .auto_call(context, span, arguments, expressions),
            "signum" => {
                |_context: &mut ExecutionContext<S>, span: &S| -> OperatorResult<S, Value<S>> {
                    Float::new(self.value.signum())
                        .unwrap_not_nan(span)
                        .map(|n| n.into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "acos" => {
                |_context: &mut ExecutionContext<S>, span: &S| -> OperatorResult<S, Value<S>> {
                    self.check_inverse_trig_compatible(span)?;
                    Float::new((self.value * consts::PI).acos())
                        .unwrap_not_nan(span)
                        .map(|n| n.into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "acosh" => {
                |_context: &mut ExecutionContext<S>, span: &S| -> OperatorResult<S, Value<S>> {
                    self.check_inverse_trig_compatible(span)?;
                    Float::new((self.value * consts::PI).acosh())
                        .unwrap_not_nan(span)
                        .map(|n| n.into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "asin" => {
                |_context: &mut ExecutionContext<S>, span: &S| -> OperatorResult<S, Value<S>> {
                    self.check_inverse_trig_compatible(span)?;
                    Float::new((self.value * consts::PI).asin())
                        .unwrap_not_nan(span)
                        .map(|n| n.into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "asinh" => {
                |_context: &mut ExecutionContext<S>, span: &S| -> OperatorResult<S, Value<S>> {
                    self.check_inverse_trig_compatible(span)?;
                    Float::new((self.value * consts::PI).asinh())
                        .unwrap_not_nan(span)
                        .map(|n| n.into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "atan" => {
                |_context: &mut ExecutionContext<S>, span: &S| -> OperatorResult<S, Value<S>> {
                    self.check_inverse_trig_compatible(span)?;
                    Float::new((self.value * consts::PI).atan())
                        .unwrap_not_nan(span)
                        .map(|n| n.into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "atanh" => {
                |_context: &mut ExecutionContext<S>, span: &S| -> OperatorResult<S, Value<S>> {
                    self.check_inverse_trig_compatible(span)?;
                    Float::new((self.value * consts::PI).atanh())
                        .unwrap_not_nan(span)
                        .map(|n| n.into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "cos" => {
                |_context: &mut ExecutionContext<S>, span: &S| -> OperatorResult<S, Value<S>> {
                    self.check_trig_compatible(span)?;
                    Float::new((self.value * consts::PI).cos())
                        .unwrap_not_nan(span)
                        .map(|n| n.into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "cosh" => {
                |_context: &mut ExecutionContext<S>, span: &S| -> OperatorResult<S, Value<S>> {
                    self.check_trig_compatible(span)?;
                    Float::new((self.value * consts::PI).cosh())
                        .unwrap_not_nan(span)
                        .map(|n| n.into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "sin" => {
                |_context: &mut ExecutionContext<S>, span: &S| -> OperatorResult<S, Value<S>> {
                    self.check_trig_compatible(span)?;
                    Float::new((self.value * consts::PI).sin())
                        .unwrap_not_nan(span)
                        .map(|n| n.into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "sin_cos" => {
                |_context: &mut ExecutionContext<S>, _span: &S| -> OperatorResult<S, Value<S>> {
                    self.check_trig_compatible(span)?;
                    let (sin, cos) = (self.value * consts::PI).sin_cos();

                    Ok(Vector2 {
                        dimension: Dimension::zero(),
                        value: NVector::<Const<2>>::new(cos, sin),
                    }
                    .into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "sinh" => {
                |_context: &mut ExecutionContext<S>, span: &S| -> OperatorResult<S, Value<S>> {
                    self.check_trig_compatible(span)?;
                    Float::new((self.value * consts::PI).sinh())
                        .unwrap_not_nan(span)
                        .map(|n| n.into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "tan" => {
                |_context: &mut ExecutionContext<S>, span: &S| -> OperatorResult<S, Value<S>> {
                    self.check_trig_compatible(span)?;
                    Float::new((self.value * consts::PI).tan())
                        .unwrap_not_nan(span)
                        .map(|n| n.into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "tanh" => {
                |_context: &mut ExecutionContext<S>, span: &S| -> OperatorResult<S, Value<S>> {
                    self.check_trig_compatible(span)?;
                    Float::new((self.value * consts::PI).tanh())
                        .unwrap_not_nan(span)
                        .map(|n| n.into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            // TODO we need functions to convert zero dimensional types to: Angles, Constitute Concentration, Information, Solid Angle, and Temperature.
            _ => Err(Failure::UnknownAttribute(attribute.clone())),
        }
    }

    fn export(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        _span: &S,
    ) -> OperatorResult<S, SerializableValue> {
        Ok(SerializableValue::Scalar(self.clone()))
    }
}

impl NamedObject for Scalar {
    fn static_type_name() -> &'static str {
        "Scalar"
    }
}

impl Scalar {
    pub(super) fn check_dimension<S: Span>(
        &self,
        span: &S,
        dimension: Dimension,
    ) -> OperatorResult<S, ()> {
        if self.dimension == dimension {
            Ok(())
        } else {
            Err(Failure::ExpectedGot(
                span.clone(),
                get_dimension_name(&dimension),
                get_dimension_name(&self.dimension),
            ))
        }
    }

    fn multiply_by_scalar<S: Span>(&self, span: &S, rhs: &Self) -> OperatorResult<S, Self> {
        let value = Float::new(*self.value * *rhs.value).unwrap_not_nan(span)?;
        let dimension = self.dimension + rhs.dimension;

        Ok(Self { dimension, value })
    }

    fn divide_by_measurement<S: Span>(&self, span: &S, rhs: &Self) -> OperatorResult<S, Self> {
        let value = Float::new(*self.value / *rhs.value).unwrap_not_nan(span)?;
        let dimension = self.dimension - rhs.dimension;

        Ok(Self { dimension, value })
    }

    fn check_is_zero_dimension<S: Span>(&self, span: &S) -> OperatorResult<S, ()> {
        if self.dimension.is_zero_dimension() {
            Ok(())
        } else {
            Err(Failure::ExpectedZeroDimension(span.clone()))
        }
    }

    fn check_inverse_trig_compatible<S: Span>(&self, span: &S) -> OperatorResult<S, ()> {
        if self.dimension.is_zero_dimension() {
            Ok(())
        } else {
            Err(Failure::InverseTrigIncompatible(span.clone()))
        }
    }

    fn check_trig_compatible<S: Span>(&self, span: &S) -> OperatorResult<S, ()> {
        if self.dimension.is_zero_dimension() && self.dimension.ratio_type_hint.is_angle() {
            Ok(())
        } else {
            Err(Failure::TrigIncompatible(span.clone()))
        }
    }

    fn is_number(&self) -> bool {
        self.dimension.is_zero_dimension() && self.dimension.ratio_type_hint.0 == 0
    }

    fn unpack_for_addition_or_subtraction<'b, S: Span>(
        &'b self,
        span: &S,
        rhs: &'b Value<S>,
    ) -> OperatorResult<S, &Self> {
        if let Value::Scalar(rhs) = rhs {
            if self.dimension == rhs.dimension {
                Ok(rhs)
            } else {
                Err(Failure::ExpectedGot(
                    span.clone(),
                    <Self as Object<S>>::type_name(self),
                    <Self as Object<S>>::type_name(rhs),
                ))
            }
        } else {
            Err(Failure::ExpectedGot(
                span.clone(),
                <Self as Object<S>>::type_name(self),
                rhs.type_name(),
            ))
        }
    }

    // pub fn convert_from_number<S: Span>(
    //     span: &S,
    //     value: Number,
    //     ty: SString,
    // ) -> OperatorResult<S, Value<S>> {
    //     if let Some(conversion_factor) = CONVERSION_FACTORS.get(ty.as_str()) {
    //         let value = conversion_factor.convert_to_base_unit(value);
    //         let dimension = conversion_factor.dimension;

    //         Ok(Self { dimension, value }.into())
    //     } else {
    //         Err(Failure::UnknownUnitType(
    //             span.clone(),
    //             ty.into_string().into(),
    //         ))
    //     }
    // }

    pub fn convert_to_number<S: Span>(&self, span: &S, ty: &str) -> OperatorResult<S, Value<S>> {
        if let Some(conversion_factor) = CONVERSION_FACTORS.get(ty) {
            if self.dimension == conversion_factor.dimension {
                Ok(conversion_factor.convert_from_base_unit(self.value).into())
            } else {
                Err(Failure::DimensionalMissmatch(
                    span.clone(),
                    Object::<S>::type_name(self),
                    ty.to_string().into(),
                ))
            }
        } else {
            Err(Failure::UnknownUnitType(
                span.clone(),
                ty.to_string().into(),
            ))
        }
    }

    pub fn to_number<S: Span>(&self, span: &S) -> OperatorResult<S, Float> {
        self.check_is_zero_dimension(span)?;
        if self.dimension.ratio_type_hint.0 == 0 {
            // It's just a number/ratio, we can use that.
            Ok(self.value)
        } else {
            Err(Failure::ExpectedGot(
                span.clone(),
                "Number".into(),
                Object::<S>::type_name(self),
            ))
        }
    }

    pub fn from_number(value: Float) -> Self {
        let dimension = Dimension {
            length: 0,
            mass: 0,
            time: 0,
            electric_current: 0,
            thermodynamic_temprature: 0,
            amount_of_substance: 0,
            luminous_intensity: 0,
            ratio_type_hint: RatioTypeHint(0),
        };

        Self { dimension, value }
    }

    pub fn from_parsed_raw<S: Span>(measurement: &parsing::Scalar<S>) -> OperatorResult<S, Self> {
        if let Some(conversion_factor) = CONVERSION_FACTORS.get(measurement.ty.as_str()) {
            let value = unwrap_float(measurement.number.get_span().clone(), &measurement.number)?;
            let value = conversion_factor.convert_to_base_unit(value);

            let dimension = conversion_factor.dimension;

            Ok(Self { dimension, value })
        } else {
            Err(Failure::UnknownUnitType(
                measurement.ty.clone(),
                measurement.ty.to_string().into(),
            ))
        }
    }

    pub fn from_parsed<S: Span>(measurement: &parsing::Scalar<S>) -> OperatorResult<S, Value<S>> {
        Self::from_parsed_raw(measurement).map(|measurement| measurement.into())
    }

    pub fn get_conversion_factor(
        keyboard_friendly_abbreviation: &str,
    ) -> Option<&'static ConversionFactor> {
        CONVERSION_FACTORS.get(keyboard_friendly_abbreviation)
    }

    fn get_operation_conversion_factor<S: Span>(
        span: &S,
        unit: Value<S>,
    ) -> OperatorResult<S, &'static ConversionFactor> {
        let unit_specification = unit
            .downcast_optional::<SString>(span)?
            .map(|s| s.to_string(span));

        Ok(match unit_specification {
            Some(unit_specification) => {
                let unit_specification = unit_specification?;
                Self::get_conversion_factor(unit_specification.as_ref()).ok_or(
                    Failure::UnknownUnitType(span.clone(), unit_specification.into()),
                )?
            }
            None => Self::get_conversion_factor("").unwrap(),
        })
    }

    pub fn as_scalar<S: Span>(
        &self,
        context: &ExecutionContext<S>,
        span: &S,
    ) -> OperatorResult<S, FornjotScalar> {
        let length = context
            .global_resources
            .fornjot_unit_conversion_factor
            .convert_from_measurement_to_number(span, self)?;

        Ok(FornjotScalar::from_f64(length.into_inner()))
    }
}

impl<D, U> TryFrom<Quantity<D, U, f64>> for Scalar
where
    D: UomDimension + ?Sized,
    D::L: ToInt<i8>,
    D::M: ToInt<i8>,
    D::T: ToInt<i8>,
    D::I: ToInt<i8>,
    D::Th: ToInt<i8>,
    D::N: ToInt<i8>,
    D::J: ToInt<i8>,
    U: Units<f64> + ?Sized,
{
    type Error = common_data_types::FloatIsNan;

    fn try_from(value: Quantity<D, U, f64>) -> std::result::Result<Self, Self::Error> {
        // Due to language limitations in Rust, we are incapable of getting ratio type hints.
        // https://stackoverflow.com/questions/30274091/is-it-possible-to-check-if-an-object-implements-a-trait-at-runtime

        let length = D::L::INT;
        let mass = D::M::INT;
        let time = D::T::INT;
        let electric_current = D::I::INT;
        let thermodynamic_temprature = D::Th::INT;
        let amount_of_substance = D::N::INT;
        let luminous_intensity = D::J::INT;

        let dimension = Dimension {
            length,
            mass,
            time,
            electric_current,
            thermodynamic_temprature,
            amount_of_substance,
            luminous_intensity,
            ratio_type_hint: RatioTypeHint::default(),
        };

        let value = Float::new(value.value)?;

        Ok(Self { dimension, value })
    }
}

impl<D, U> TryInto<Quantity<D, U, f64>> for Scalar
where
    D: UomDimension + ?Sized,
    D::L: ToInt<i8>,
    D::M: ToInt<i8>,
    D::T: ToInt<i8>,
    D::I: ToInt<i8>,
    D::Th: ToInt<i8>,
    D::N: ToInt<i8>,
    D::J: ToInt<i8>,
    U: Units<f64> + ?Sized,
{
    type Error = ();

    fn try_into(self) -> std::result::Result<Quantity<D, U, f64>, Self::Error> {
        // Due to language limitations in Rust, we are incapable of getting ratio type hints.
        // https://stackoverflow.com/questions/30274091/is-it-possible-to-check-if-an-object-implements-a-trait-at-runtime

        if D::L::INT == self.dimension.length
            && D::M::INT == self.dimension.mass
            && D::T::INT == self.dimension.time
            && D::I::INT == self.dimension.electric_current
            && D::Th::INT == self.dimension.thermodynamic_temprature
            && D::N::INT == self.dimension.amount_of_substance
            && D::J::INT == self.dimension.luminous_intensity
        {
            Ok(Quantity {
                dimension: std::marker::PhantomData,
                units: std::marker::PhantomData,
                value: self.value.into_inner(),
            })
        } else {
            // We are not the same measurement type.
            Err(())
        }
    }
}

impl FromStr for Scalar {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (_leftover, measurement) = parsing::Scalar::parse(ImString::from(s))
            .map_err(|error| anyhow::anyhow!("Failed to parse measurement: {}", error))?;
        let measurement = Self::from_parsed_raw(&measurement)
            .map_err(|failure| anyhow::anyhow!("{}", failure))?;

        Ok(measurement)
    }
}

impl From<Float> for Scalar {
    fn from(value: Float) -> Self {
        Self {
            dimension: Dimension {
                length: 0,
                mass: 0,
                time: 0,
                electric_current: 0,
                thermodynamic_temprature: 0,
                amount_of_substance: 0,
                luminous_intensity: 0,
                ratio_type_hint: RatioTypeHint::default(),
            },
            value,
        }
    }
}

impl Serialize for Scalar {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if self.is_number() {
            serializer.serialize_f64(self.value.into_inner())
        } else {
            let unit_name = BASE_UNITS
                .get(&self.dimension)
                .cloned()
                .unwrap_or_else(|| format_dimension(&self.dimension));

            serializer.serialize_str(&format!("{} {}", self.value, unit_name))
        }
    }
}

impl<'de> Deserialize<'de> for Scalar {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_any(ScalarVisitor)
    }
}

struct ScalarVisitor;

impl<'de> Visitor<'de> for ScalarVisitor {
    type Value = Scalar;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a measurement or number")
    }

    fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        self.visit_f64(v as f64)
    }

    fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        self.visit_f64(v as f64)
    }

    fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        let v = Float::new(v).map_err(|e| E::custom(e))?;
        Ok(Scalar::from_number(v))
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Scalar::from_str(v).map_err(|e| E::custom(e))
    }
}

#[cfg(test)]
mod test {
    use uom::si::f64 as si;

    use crate::script::{
        execution::{expressions::run_expression, ExecutionContext},
        logging::StandardLog,
        Runtime,
    };

    use super::*;

    use uom::si::length::meter;

    #[test]
    fn addition() {
        let mut log = StandardLog;

        let a = si::Length::new::<meter>(3.0);
        let b = si::Length::new::<meter>(2.0);
        let value_a: Value<&str> = Scalar::try_from(a).unwrap().into();
        let value_b: Value<&str> = Scalar::try_from(b).unwrap().into();
        assert_eq!(
            value_a.addition(&mut log, &"span", &value_b),
            Ok(Scalar::try_from(a + b).unwrap().into())
        );
    }

    #[test]
    fn subtraction() {
        let mut log = StandardLog;

        let a = si::Length::new::<meter>(3.0);
        let b = si::Length::new::<meter>(2.0);
        let value_a: Value<&str> = Scalar::try_from(a).unwrap().into();
        let value_b: Value<&str> = Scalar::try_from(b).unwrap().into();
        assert_eq!(
            value_a.subtraction(&mut log, &"span", &value_b),
            Ok(Scalar::try_from(a - b).unwrap().into())
        );
    }

    #[test]
    fn multiplication() {
        let mut log = StandardLog;

        let a = si::Length::new::<meter>(3.0);
        let b = si::Length::new::<meter>(2.0);
        let c = a * b;
        let value_a: Value<&str> = Scalar::try_from(a).unwrap().into();
        let value_b: Value<&str> = Scalar::try_from(b).unwrap().into();
        let value_c: Value<&str> = Scalar::try_from(c).unwrap().into();
        assert_eq!(
            value_a.clone().multiply(&mut log, &"span", &value_b),
            Ok(Scalar::try_from(a * b).unwrap().into())
        );
        assert_eq!(
            value_a.multiply(&mut log, &"span", &value_c),
            Ok(Scalar::try_from(a * c).unwrap().into())
        );
    }

    #[test]
    fn division() {
        let mut log = StandardLog;

        let a = si::Length::new::<meter>(3.0);
        let b = si::Length::new::<meter>(2.0);
        let value_a: Value<&str> = Scalar::try_from(a).unwrap().into();
        let value_b: Value<&str> = Scalar::try_from(b).unwrap().into();

        assert_eq!(
            value_a.clone().divide(&mut log, &"span", &value_b),
            Ok(Scalar::try_from(a / b).unwrap().into())
        );

        let c: si::Area = a * b;
        let value_c: Value<&str> = Scalar::try_from(c).unwrap().into();
        assert_eq!(
            value_c.divide(&mut log, &"span", &value_a),
            Ok(Scalar::try_from(c / a).unwrap().into())
        );
    }

    #[test]
    fn comparisions() {
        let mut log = StandardLog;

        let a = si::Length::new::<meter>(3.0);
        let b = si::Length::new::<meter>(2.0);
        let value_a: Value<&str> = Scalar::try_from(a).unwrap().into();
        let value_b: Value<&str> = Scalar::try_from(b).unwrap().into();
        assert_eq!(
            value_a.cmp(&mut log, &"span", &value_b),
            Ok(Ordering::Greater)
        );
        assert_eq!(
            value_a.cmp(&mut log, &"span", &value_a),
            Ok(Ordering::Equal)
        );
        assert_eq!(value_b.cmp(&mut log, &"span", &value_a), Ok(Ordering::Less));
    }

    #[test]
    fn conversions() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(context, &Expression::parse("1m + 100cm == 2m").unwrap().1),
                Ok(true.into())
            );
            assert_eq!(
                run_expression(context, &Expression::parse("2m * 2m == 4m^2").unwrap().1),
                Ok(true.into())
            );
            assert_eq!(
                run_expression(
                    context,
                    &Expression::parse("(2m).to_number(\"cm\") == 200")
                        .unwrap()
                        .1
                ),
                { Ok(true.into()) }
            );
        });
    }
}
