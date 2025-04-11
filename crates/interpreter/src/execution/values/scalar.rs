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
use std::{borrow::Cow, cmp::Ordering};

use common_data_types::{Dimension, Float, FloatIsNan};

use crate::{
    compile::SourceReference,
    execution::{
        errors::{ExpressionResult, GenericFailure, Raise},
        heap::Heap,
        logging::RuntimeLog,
    },
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

#[derive(Debug, Hash, Clone, Eq, PartialEq)]
pub struct Scalar {
    pub dimension: Dimension,
    pub value: Float,
}

impl Object for Scalar {
    fn get_type(&self) -> ValueType {
        ValueType::Scalar(self.dimension)
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
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        _heap: &Heap,
        rhs: &Value,
    ) -> ExpressionResult<Value> {
        let rhs = self.unpack_for_addition_or_subtraction(stack_trace, rhs)?;

        let value = Float::new(*self.value + *rhs.value).unwrap_not_nan(stack_trace)?;

        Ok(Self {
            value,
            ..self.clone()
        }
        .into())
    }
    fn subtraction(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        _heap: &Heap,
        rhs: &Value,
    ) -> ExpressionResult<Value> {
        let rhs = self.unpack_for_addition_or_subtraction(stack_trace, rhs)?;

        let value = Float::new(*self.value - *rhs.value).unwrap_not_nan(stack_trace)?;

        Ok(Self {
            value,
            ..self.clone()
        }
        .into())
    }
    fn multiply(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        _heap: &Heap,
        rhs: &Value,
    ) -> ExpressionResult<Value> {
        let rhs = rhs.downcast_ref::<Scalar>(stack_trace)?;
        self.multiply_by_scalar(stack_trace, rhs)
            .map(|rhs| rhs.into())
    }
    fn divide(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        _heap: &Heap,
        rhs: &Value,
    ) -> ExpressionResult<Value> {
        let rhs = rhs.downcast_ref::<Scalar>(stack_trace)?;
        self.divide_by_measurement(stack_trace, rhs)
            .map(|rhs| rhs.into())
    }
    fn unary_plus(
        &self,
        _log: &mut dyn RuntimeLog,
        _stack_trace: &[SourceReference],
        _heap: &Heap,
    ) -> ExpressionResult<Value> {
        Ok(self.clone().into())
    }
    fn unary_minus(
        &self,
        _log: &mut dyn RuntimeLog,
        _stack_trace: &[SourceReference],
        _heap: &Heap,
    ) -> ExpressionResult<Value> {
        Ok(Self {
            value: -self.value,
            ..self.clone()
        }
        .into())
    }
    fn cmp(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        _heap: &Heap,
        rhs: &Value,
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
    // fn method_call(
    //     &self,
    //     context: &mut ExecutionContext,
    //     stack_trace: &[SourceReference],
    //     attribute: &S,
    //     arguments: Vec<Value>,
    //     expressions: &[Expression],
    // ) -> ExpressionResult<Value> {
    //     match attribute.as_str() {
    //         "to_number" => |_context: &mut ExecutionContext,
    //                         stack_trace: &[SourceReference],
    //                         ty: SString|
    //          -> ExpressionResult<S, Value> {
    //             self.convert_to_number(span, &ty.as_str(span)?)
    //         }
    //         .auto_call(context, span, arguments, expressions),
    //         "abs" => {
    //             |_context: &mut ExecutionContext, stack_trace: &[SourceReference]| -> ExpressionResult<S, Value> {
    //                 Ok(Self {
    //                     value: Float::new(self.value.abs()).unwrap_not_nan(span)?,
    //                     ..self.clone()
    //                 }
    //                 .into())
    //             }
    //             .auto_call(context, span, arguments, expressions)
    //         }
    //         "clamp" => |_context: &mut ExecutionContext,
    //                     stack_trace: &[SourceReference],
    //                     min: Value,
    //                     max: Value|
    //          -> ExpressionResult<S, Value> {
    //             let min = self.unpack_for_addition_or_subtraction(span, &min)?;
    //             let max = self.unpack_for_addition_or_subtraction(span, &max)?;

    //             Ok(Self {
    //                 value: self.value.clamp(min.value, max.value),
    //                 ..self.clone()
    //             }
    //             .into())
    //         }
    //         .auto_call(context, span, arguments, expressions),
    //         "copysign" => |_context: &mut ExecutionContext,
    //                        stack_trace: &[SourceReference],
    //                        sign: Number|
    //          -> ExpressionResult<S, Value> {
    //             let sign = sign.to_index();

    //             Ok(Self {
    //                 value: Float::new(self.value.copysign(sign as RawFloat))
    //                     .unwrap_not_nan(span)?,
    //                 ..self.clone()
    //             }
    //             .into())
    //         }
    //         .auto_call(context, span, arguments, expressions),
    //         "hypot" => |_context: &mut ExecutionContext,
    //                     stack_trace: &[SourceReference],
    //                     other: Value|
    //          -> ExpressionResult<S, Value> {
    //             let other = self.unpack_for_addition_or_subtraction(span, &other)?;

    //             Ok(Self {
    //                 value: Float::new(self.value.hypot(*other.value)).unwrap_not_nan(span)?,
    //                 ..self.clone()
    //             }
    //             .into())
    //         }
    //         .auto_call(context, span, arguments, expressions),
    //         "is_finite" => {
    //             |_context: &mut ExecutionContext, _stack_trace: &[SourceReference]| -> ExpressionResult<S, Value> {
    //                 Ok(self.value.is_finite().into())
    //             }
    //             .auto_call(context, span, arguments, expressions)
    //         }
    //         "is_infinite" => {
    //             |_context: &mut ExecutionContext, _stack_trace: &[SourceReference]| -> ExpressionResult<S, Value> {
    //                 Ok(self.value.is_infinite().into())
    //             }
    //             .auto_call(context, span, arguments, expressions)
    //         }
    //         "is_normal" => {
    //             |_context: &mut ExecutionContext, _stack_trace: &[SourceReference]| -> ExpressionResult<S, Value> {
    //                 Ok(self.value.is_normal().into())
    //             }
    //             .auto_call(context, span, arguments, expressions)
    //         }
    //         "cbrt" => {
    //             |_context: &mut ExecutionContext, stack_trace: &[SourceReference]| -> ExpressionResult<S, Value> {
    //                 Ok(Self {
    //                     dimension: self.dimension / 3,
    //                     value: Float::new(self.value.cbrt()).unwrap_not_nan(span)?,
    //                 }
    //                 .into())
    //             }
    //             .auto_call(context, span, arguments, expressions)
    //         }
    //         "pow" => |_context: &mut ExecutionContext,
    //                   _stack_trace: &[SourceReference],
    //                   exponent: Scalar|
    //          -> ExpressionResult<S, Value> {
    //             let exponent = exponent.to_number(span)?;

    //             self.check_is_zero_dimension(span)?;

    //             Ok(Self {
    //                 dimension: self.dimension,
    //                 value: Float::new(self.value.powf(exponent.into_inner()))
    //                     .unwrap_not_nan(span)?,
    //             }
    //             .into())
    //         }
    //         .auto_call(context, span, arguments, expressions),
    //         "powi" => |_context: &mut ExecutionContext,
    //                    _stack_trace: &[SourceReference],
    //                    exponent: Number|
    //          -> ExpressionResult<S, Value> {
    //             let exponent = exponent.to_index() as i8;

    //             Ok(Self {
    //                 dimension: self.dimension * exponent,
    //                 value: Float::new(self.value.powi(exponent as i32)).unwrap_not_nan(span)?,
    //             }
    //             .into())
    //         }
    //         .auto_call(context, span, arguments, expressions),
    //         "sqrt" => {
    //             |_context: &mut ExecutionContext, _stack_trace: &[SourceReference]| -> ExpressionResult<S, Value> {
    //                 Ok(Self {
    //                     dimension: self.dimension / 2,
    //                     value: Float::new(self.value.sqrt()).unwrap_not_nan(span)?,
    //                 }
    //                 .into())
    //             }
    //             .auto_call(context, span, arguments, expressions)
    //         }
    //         "is_sign_negative" => {
    //             |_context: &mut ExecutionContext, _stack_trace: &[SourceReference]| -> ExpressionResult<S, Value> {
    //                 Ok(self.value.is_sign_negative().into())
    //             }
    //             .auto_call(context, span, arguments, expressions)
    //         }
    //         "is_sign_positive" => {
    //             |_context: &mut ExecutionContext, _stack_trace: &[SourceReference]| -> ExpressionResult<S, Value> {
    //                 Ok(self.value.is_sign_positive().into())
    //             }
    //             .auto_call(context, span, arguments, expressions)
    //         }
    //         "recip" => {
    //             |_context: &mut ExecutionContext, _stack_trace: &[SourceReference]| -> ExpressionResult<S, Value> {
    //                 Ok(Self {
    //                     dimension: -self.dimension,
    //                     value: Float::new(1.0 / self.value.into_inner()).unwrap_not_nan(span)?,
    //                 }
    //                 .into())
    //             }
    //             .auto_call(context, span, arguments, expressions)
    //         }
    //         "round" => |_context: &mut ExecutionContext,
    //                     stack_trace: &[SourceReference],
    //                     unit: Value|
    //          -> ExpressionResult<S, Value> {
    //             let conversion_factor = Self::get_operation_conversion_factor(span, unit)?;

    //             let value = conversion_factor.convert_from_measurement_to_number(span, self)?;
    //             let value = Float::new(value.round()).unwrap_not_nan(span)?;

    //             Ok(Self {
    //                 dimension: self.dimension,
    //                 value: conversion_factor.convert_to_base_unit(value),
    //             }
    //             .into())
    //         }
    //         .auto_call_optional(context, span, arguments, expressions),
    //         "trunc" => |_context: &mut ExecutionContext,
    //                     stack_trace: &[SourceReference],
    //                     unit: Value|
    //          -> ExpressionResult<S, Value> {
    //             let conversion_factor = Self::get_operation_conversion_factor(span, unit)?;

    //             let value = conversion_factor.convert_from_measurement_to_number(span, self)?;
    //             let value = Float::new(value.trunc()).unwrap_not_nan(span)?;

    //             Ok(Self {
    //                 dimension: self.dimension,
    //                 value: conversion_factor.convert_to_base_unit(value),
    //             }
    //             .into())
    //         }
    //         .auto_call_optional(context, span, arguments, expressions),
    //         "fract" => |_context: &mut ExecutionContext,
    //                     stack_trace: &[SourceReference],
    //                     unit: Value|
    //          -> ExpressionResult<S, Value> {
    //             let conversion_factor = Self::get_operation_conversion_factor(span, unit)?;

    //             let value = conversion_factor.convert_from_measurement_to_number(span, self)?;
    //             let value = Float::new(value.fract()).unwrap_not_nan(span)?;

    //             Ok(Self {
    //                 dimension: self.dimension,
    //                 value: conversion_factor.convert_to_base_unit(value),
    //             }
    //             .into())
    //         }
    //         .auto_call_optional(context, span, arguments, expressions),
    //         "floor" => |_context: &mut ExecutionContext,
    //                     stack_trace: &[SourceReference],
    //                     unit: Value|
    //          -> ExpressionResult<S, Value> {
    //             let conversion_factor = Self::get_operation_conversion_factor(span, unit)?;

    //             let value = conversion_factor.convert_from_measurement_to_number(span, self)?;
    //             let value = Float::new(value.floor()).unwrap_not_nan(span)?;

    //             Ok(Self {
    //                 dimension: self.dimension,
    //                 value: conversion_factor.convert_to_base_unit(value),
    //             }
    //             .into())
    //         }
    //         .auto_call_optional(context, span, arguments, expressions),
    //         "ceil" => |_context: &mut ExecutionContext,
    //                    stack_trace: &[SourceReference],
    //                    unit: Value|
    //          -> ExpressionResult<S, Value> {
    //             let conversion_factor = Self::get_operation_conversion_factor(span, unit)?;

    //             let value = conversion_factor.convert_from_measurement_to_number(span, self)?;
    //             let value = Float::new(value.ceil()).unwrap_not_nan(span)?;

    //             Ok(Self {
    //                 dimension: self.dimension,
    //                 value: conversion_factor.convert_to_base_unit(value),
    //             }
    //             .into())
    //         }
    //         .auto_call_optional(context, span, arguments, expressions),
    //         "max" => |_context: &mut ExecutionContext,
    //                   _stack_trace: &[SourceReference],
    //                   other: Value|
    //          -> ExpressionResult<S, Value> {
    //             let other = self.unpack_for_addition_or_subtraction(span, &other)?;
    //             Ok(Float::new(*self.value.max(other.value))
    //                 .unwrap_not_nan(span)?
    //                 .into())
    //         }
    //         .auto_call(context, span, arguments, expressions),
    //         "min" => |_context: &mut ExecutionContext,
    //                   _stack_trace: &[SourceReference],
    //                   other: Value|
    //          -> ExpressionResult<S, Value> {
    //             let other = self.unpack_for_addition_or_subtraction(span, &other)?;
    //             Ok(Float::new(*self.value.min(other.value))
    //                 .unwrap_not_nan(span)?
    //                 .into())
    //         }
    //         .auto_call(context, span, arguments, expressions),
    //         "mul_add" => |context: &mut ExecutionContext,
    //                       stack_trace: &[SourceReference],
    //                       a: Value,
    //                       b: Value|
    //          -> ExpressionResult<S, Value> {
    //             let multiply_result = self.multiply(context.log, span, &b)?;
    //             let add_result = multiply_result.addition(context.log, span, &a)?;

    //             Ok(add_result)
    //         }
    //         .auto_call(context, span, arguments, expressions),
    //         "signum" => {
    //             |_context: &mut ExecutionContext, stack_trace: &[SourceReference]| -> ExpressionResult<S, Value> {
    //                 Float::new(self.value.signum())
    //                     .unwrap_not_nan(span)
    //                     .map(|n| n.into())
    //             }
    //             .auto_call(context, span, arguments, expressions)
    //         }
    //         "acos" => {
    //             |_context: &mut ExecutionContext, stack_trace: &[SourceReference]| -> ExpressionResult<S, Value> {
    //                 self.check_inverse_trig_compatible(span)?;
    //                 Float::new((self.value * consts::PI).acos())
    //                     .unwrap_not_nan(span)
    //                     .map(|n| n.into())
    //             }
    //             .auto_call(context, span, arguments, expressions)
    //         }
    //         "acosh" => {
    //             |_context: &mut ExecutionContext, stack_trace: &[SourceReference]| -> ExpressionResult<S, Value> {
    //                 self.check_inverse_trig_compatible(span)?;
    //                 Float::new((self.value * consts::PI).acosh())
    //                     .unwrap_not_nan(span)
    //                     .map(|n| n.into())
    //             }
    //             .auto_call(context, span, arguments, expressions)
    //         }
    //         "asin" => {
    //             |_context: &mut ExecutionContext, stack_trace: &[SourceReference]| -> ExpressionResult<S, Value> {
    //                 self.check_inverse_trig_compatible(span)?;
    //                 Float::new((self.value * consts::PI).asin())
    //                     .unwrap_not_nan(span)
    //                     .map(|n| n.into())
    //             }
    //             .auto_call(context, span, arguments, expressions)
    //         }
    //         "asinh" => {
    //             |_context: &mut ExecutionContext, stack_trace: &[SourceReference]| -> ExpressionResult<S, Value> {
    //                 self.check_inverse_trig_compatible(span)?;
    //                 Float::new((self.value * consts::PI).asinh())
    //                     .unwrap_not_nan(span)
    //                     .map(|n| n.into())
    //             }
    //             .auto_call(context, span, arguments, expressions)
    //         }
    //         "atan" => {
    //             |_context: &mut ExecutionContext, stack_trace: &[SourceReference]| -> ExpressionResult<S, Value> {
    //                 self.check_inverse_trig_compatible(span)?;
    //                 Float::new((self.value * consts::PI).atan())
    //                     .unwrap_not_nan(span)
    //                     .map(|n| n.into())
    //             }
    //             .auto_call(context, span, arguments, expressions)
    //         }
    //         "atanh" => {
    //             |_context: &mut ExecutionContext, stack_trace: &[SourceReference]| -> ExpressionResult<S, Value> {
    //                 self.check_inverse_trig_compatible(span)?;
    //                 Float::new((self.value * consts::PI).atanh())
    //                     .unwrap_not_nan(span)
    //                     .map(|n| n.into())
    //             }
    //             .auto_call(context, span, arguments, expressions)
    //         }
    //         "cos" => {
    //             |_context: &mut ExecutionContext, stack_trace: &[SourceReference]| -> ExpressionResult<S, Value> {
    //                 self.check_trig_compatible(span)?;
    //                 Float::new((self.value * consts::PI).cos())
    //                     .unwrap_not_nan(span)
    //                     .map(|n| n.into())
    //             }
    //             .auto_call(context, span, arguments, expressions)
    //         }
    //         "cosh" => {
    //             |_context: &mut ExecutionContext, stack_trace: &[SourceReference]| -> ExpressionResult<S, Value> {
    //                 self.check_trig_compatible(span)?;
    //                 Float::new((self.value * consts::PI).cosh())
    //                     .unwrap_not_nan(span)
    //                     .map(|n| n.into())
    //             }
    //             .auto_call(context, span, arguments, expressions)
    //         }
    //         "sin" => {
    //             |_context: &mut ExecutionContext, stack_trace: &[SourceReference]| -> ExpressionResult<S, Value> {
    //                 self.check_trig_compatible(span)?;
    //                 Float::new((self.value * consts::PI).sin())
    //                     .unwrap_not_nan(span)
    //                     .map(|n| n.into())
    //             }
    //             .auto_call(context, span, arguments, expressions)
    //         }
    //         "sin_cos" => {
    //             |_context: &mut ExecutionContext, _stack_trace: &[SourceReference]| -> ExpressionResult<S, Value> {
    //                 self.check_trig_compatible(span)?;
    //                 let (sin, cos) = (self.value * consts::PI).sin_cos();

    //                 Ok(Vector2 {
    //                     dimension: Dimension::zero(),
    //                     value: NVector::<Const<2>>::new(cos, sin),
    //                 }
    //                 .into())
    //             }
    //             .auto_call(context, span, arguments, expressions)
    //         }
    //         "sinh" => {
    //             |_context: &mut ExecutionContext, stack_trace: &[SourceReference]| -> ExpressionResult<S, Value> {
    //                 self.check_trig_compatible(span)?;
    //                 Float::new((self.value * consts::PI).sinh())
    //                     .unwrap_not_nan(span)
    //                     .map(|n| n.into())
    //             }
    //             .auto_call(context, span, arguments, expressions)
    //         }
    //         "tan" => {
    //             |_context: &mut ExecutionContext, stack_trace: &[SourceReference]| -> ExpressionResult<S, Value> {
    //                 self.check_trig_compatible(span)?;
    //                 Float::new((self.value * consts::PI).tan())
    //                     .unwrap_not_nan(span)
    //                     .map(|n| n.into())
    //             }
    //             .auto_call(context, span, arguments, expressions)
    //         }
    //         "tanh" => {
    //             |_context: &mut ExecutionContext, stack_trace: &[SourceReference]| -> ExpressionResult<S, Value> {
    //                 self.check_trig_compatible(span)?;
    //                 Float::new((self.value * consts::PI).tanh())
    //                     .unwrap_not_nan(span)
    //                     .map(|n| n.into())
    //             }
    //             .auto_call(context, span, arguments, expressions)
    //         }
    //         // TODO we need functions to convert zero dimensional types to: Angles, Constitute Concentration, Information, Solid Angle, and Temperature.
    //         _ => Err(Failure::UnknownAttribute(attribute.clone())),
    //     }
    // }

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

    fn check_is_zero_dimension(&self, stack_trace: &[SourceReference]) -> ExpressionResult<()> {
        if self.dimension.is_zero_dimension() {
            Ok(())
        } else {
            Err(GenericFailure("Expected zero dimensional type").to_error(stack_trace))
        }
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

    fn is_number(&self) -> bool {
        self.dimension.is_zero_dimension() && self.dimension.ratio_type_hint.0 == 0
    }

    fn unpack_for_addition_or_subtraction<'b>(
        &'b self,
        stack_trace: &[SourceReference],
        rhs: &'b Value,
    ) -> ExpressionResult<&'b Self> {
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

#[cfg(test)]
mod test {

    use crate::execution::{test_run, values::Boolean};

    use super::*;

    #[test]
    fn addition() {
        let product = test_run("3m + 2m").unwrap().0;
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
        let product = test_run("3m - 2m").unwrap().0;
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
        let product = test_run("3m * 2m").unwrap().0;
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
        let product = test_run("6'm^2' / 2m").unwrap().0;
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
        let product = test_run("6m > 2m").unwrap().0;
        assert_eq!(product, Boolean(true).into());

        let product = test_run("2m > 6m").unwrap().0;
        assert_eq!(product, Boolean(false).into());

        let product = test_run("6m >= 2m").unwrap().0;
        assert_eq!(product, Boolean(true).into());

        let product = test_run("6m >= 6m").unwrap().0;
        assert_eq!(product, Boolean(true).into());

        let product = test_run("2m >= 6m").unwrap().0;
        assert_eq!(product, Boolean(false).into());

        let product = test_run("6m == 6m").unwrap().0;
        assert_eq!(product, Boolean(true).into());

        let product = test_run("6m == 5m").unwrap().0;
        assert_eq!(product, Boolean(false).into());

        let product = test_run("6m <= 5m").unwrap().0;
        assert_eq!(product, Boolean(false).into());

        let product = test_run("5m <= 5m").unwrap().0;
        assert_eq!(product, Boolean(true).into());

        let product = test_run("5m <= 6m").unwrap().0;
        assert_eq!(product, Boolean(true).into());

        let product = test_run("5m < 6m").unwrap().0;
        assert_eq!(product, Boolean(true).into());

        let product = test_run("6m < 6m").unwrap().0;
        assert_eq!(product, Boolean(false).into());

        let product = test_run("6m != 6m").unwrap().0;
        assert_eq!(product, Boolean(false).into());

        let product = test_run("6m != 5m").unwrap().0;
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn conversions() {
        let product = test_run("1m + 100cm == 2m").unwrap().0;
        assert_eq!(product, Boolean(true).into());

        let product = test_run("2m * 2m == 4'm^2'").unwrap().0;
        assert_eq!(product, Boolean(true).into());
    }
}
