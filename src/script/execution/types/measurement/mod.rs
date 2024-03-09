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

use std::{borrow::Cow, cmp::Ordering, str::FromStr};

use uom::{
    si::{Dimension as UomDimension, Quantity, Units},
    typenum::ToInt,
};

use common_data_types::{
    ConversionFactor, ConversionFactorDatabase, Dimension, DimensionNameDatabase, RatioTypeHint,
};

use crate::script::{
    execution::{ExecutionContext, Failure},
    logging::RuntimeLog,
    parsing::{self, Expression, VariableType},
    Span,
};

use super::{
    function::AutoCall,
    number::{consts, unwrap_float, Number, UnwrapNotNan},
    List, NamedObject, Object, OperatorResult, SString, Value,
};

lazy_static::lazy_static! {
    static ref CONVERSION_FACTORS: ConversionFactorDatabase = include!(concat!(env!("OUT_DIR"), "/conversion_factors.rs"));
    static ref DIMENSION_NAMES: DimensionNameDatabase = include!(concat!(env!("OUT_DIR"), "/dimension_names.rs"));
}

fn get_dimension_name(dimension: &Dimension) -> Cow<'static, str> {
    if let Some(name) = DIMENSION_NAMES.get(dimension) {
        name.clone()
    } else {
        format!(
            "Measurement<L{}, M{}, T{}, I{}, Th{}, N{}, J{}>",
            dimension.length,
            dimension.mass,
            dimension.time,
            dimension.electric_current,
            dimension.thermodynamic_temprature,
            dimension.amount_of_substance,
            dimension.luminous_intensity
        )
        .into()
    }
}

pub trait ConvertUnit {
    fn to_base_unit(&self, input: Number) -> Number;
    fn from_base_unit(&self, input: Number) -> Number;
    fn from_measurement_to_number<S: Span>(
        &self,
        span: &S,
        measurement: &Measurement,
    ) -> OperatorResult<S, Number>;
}

impl ConvertUnit for ConversionFactor {
    fn to_base_unit(&self, input: Number) -> Number {
        input * self.coefficient + self.constant
    }

    fn from_base_unit(&self, input: Number) -> Number {
        (input - self.constant) / self.coefficient
    }

    fn from_measurement_to_number<S: Span>(
        &self,
        span: &S,
        measurement: &Measurement,
    ) -> OperatorResult<S, Number> {
        if measurement.dimension == self.dimension {
            Ok(self.from_base_unit(measurement.value))
        } else {
            Err(Failure::ExpectedGot(
                span.clone(),
                get_dimension_name(&self.dimension),
                get_dimension_name(&measurement.dimension),
            ))
        }
    }
}

pub fn print_all_supported_units() {}

#[derive(Debug, Clone, PartialEq)]
pub struct Measurement {
    dimension: Dimension,
    value: Number,
}

impl<'a, S: Span> Object<'a, S> for Measurement {
    fn matches_type(&self, ty: &VariableType<S>) -> bool {
        if let VariableType::Measurement(name) = ty {
            name.as_str() == Object::<S>::type_name(self).as_ref()
        } else {
            false
        }
    }

    fn type_name(&self) -> Cow<'static, str> {
        get_dimension_name(&self.dimension)
    }

    fn addition(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> OperatorResult<S, Value<'a, S>> {
        let rhs = self.unpack_for_addition_or_subtraction(span, rhs)?;

        let value = Number::new(*self.value + *rhs.value).unwrap_not_nan_raw(span)?;

        Ok(Self { value, ..*self }.into())
    }
    fn subtraction(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> OperatorResult<S, Value<'a, S>> {
        let rhs = self.unpack_for_addition_or_subtraction(span, rhs)?;

        let value = Number::new(*self.value - *rhs.value).unwrap_not_nan_raw(span)?;

        Ok(Self { value, ..*self }.into())
    }
    fn multiply(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> OperatorResult<S, Value<'a, S>> {
        match rhs {
            Value::Measurement(rhs) => Ok(self.multiply_by_measurement(span, rhs)?.into()),
            Value::Number(rhs) => Ok(self.multiply_by_number(span, rhs)?.into()),
            _ => Err(Failure::ExpectedGot(
                span.clone(),
                "Measurement or Number".into(),
                rhs.type_name(),
            )),
        }
    }
    fn divide(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> OperatorResult<S, Value<'a, S>> {
        match rhs {
            Value::Measurement(rhs) => Ok(self.divide_by_measurement(span, rhs)?.into()),
            Value::Number(rhs) => Ok(self.divide_by_number(span, rhs)?.into()),
            _ => Err(Failure::ExpectedGot(
                span.clone(),
                "Measurement or Number".into(),
                rhs.type_name(),
            )),
        }
    }
    fn unary_plus(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        _span: &S,
    ) -> OperatorResult<S, Value<'a, S>> {
        Ok(self.clone().into())
    }
    fn unary_minus(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        _span: &S,
    ) -> OperatorResult<S, Value<'a, S>> {
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
        rhs: &Value<'a, S>,
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
        context: &mut ExecutionContext<'a, S>,
        span: &S,
        attribute: &S,
        arguments: Vec<Value<'a, S>>,
        expressions: &[Expression<S>],
    ) -> OperatorResult<S, Value<'a, S>> {
        match attribute.as_str() {
            "to_number" => |_context: &mut ExecutionContext<'a, S>,
                            span: &S,
                            ty: SString|
             -> OperatorResult<S, Value<S>> {
                self.to_number(span, ty.as_str())
            }
            .auto_call(context, span, arguments, expressions),
            "abs" => |_context: &mut ExecutionContext<'a, S>,
                      span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                Ok(Self {
                    value: Number::new(self.value.abs()).unwrap_not_nan_raw(span)?,
                    ..self.clone()
                }
                .into())
            }
            .auto_call(context, span, arguments, expressions),
            "clamp" => |_context: &mut ExecutionContext<'a, S>,
                        span: &S,
                        min: Value<'a, S>,
                        max: Value<'a, S>|
             -> OperatorResult<S, Value<'a, S>> {
                let min = self.unpack_for_addition_or_subtraction(span, &min)?;
                let max = self.unpack_for_addition_or_subtraction(span, &max)?;

                Ok(Self {
                    value: self.value.clamp(min.value, max.value),
                    ..self.clone()
                }
                .into())
            }
            .auto_call(context, span, arguments, expressions),
            "copysign" => |_context: &mut ExecutionContext<'a, S>,
                           span: &S,
                           sign: Number|
             -> OperatorResult<S, Value<'a, S>> {
                Ok(Self {
                    value: Number::new(self.value.copysign(*sign)).unwrap_not_nan_raw(span)?,
                    ..self.clone()
                }
                .into())
            }
            .auto_call(context, span, arguments, expressions),
            "hypot" => |_context: &mut ExecutionContext<'a, S>,
                        span: &S,
                        other: Value<'a, S>|
             -> OperatorResult<S, Value<'a, S>> {
                let other = self.unpack_for_addition_or_subtraction(span, &other)?;

                Ok(Self {
                    value: Number::new(self.value.hypot(*other.value)).unwrap_not_nan_raw(span)?,
                    ..self.clone()
                }
                .into())
            }
            .auto_call(context, span, arguments, expressions),
            "is_finite" => |_context: &mut ExecutionContext<'a, S>,
                            _span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                Ok(self.value.is_finite().into())
            }
            .auto_call(context, span, arguments, expressions),
            "is_infinite" => |_context: &mut ExecutionContext<'a, S>,
                              _span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                Ok(self.value.is_infinite().into())
            }
            .auto_call(context, span, arguments, expressions),
            "is_sign_negative" => |_context: &mut ExecutionContext<'a, S>,
                                   _span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                Ok(self.value.is_sign_negative().into())
            }
            .auto_call(context, span, arguments, expressions),
            "is_sign_posative" => |_context: &mut ExecutionContext<'a, S>,
                                   _span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                Ok(self.value.is_sign_positive().into())
            }
            .auto_call(context, span, arguments, expressions),
            "max" => |_context: &mut ExecutionContext<'a, S>,
                      _span: &S,
                      other: Value<'a, S>|
             -> OperatorResult<S, Value<'a, S>> {
                let other = self.unpack_for_addition_or_subtraction(span, &other)?;
                Ok(Number::new(*self.value.max(other.value))
                    .unwrap_not_nan_raw(span)?
                    .into())
            }
            .auto_call(context, span, arguments, expressions),
            "min" => |_context: &mut ExecutionContext<'a, S>,
                      _span: &S,
                      other: Value<'a, S>|
             -> OperatorResult<S, Value<'a, S>> {
                let other = self.unpack_for_addition_or_subtraction(span, &other)?;
                Ok(Number::new(*self.value.min(other.value))
                    .unwrap_not_nan_raw(span)?
                    .into())
            }
            .auto_call(context, span, arguments, expressions),
            "mul_add" => |context: &mut ExecutionContext<'a, S>,
                          span: &S,
                          a: Value<'a, S>,
                          b: Value<'a, S>|
             -> OperatorResult<S, Value<'a, S>> {
                let multiply_result = self.multiply(context.log, span, &b)?;
                let add_result = multiply_result.addition(context.log, span, &a)?;

                Ok(add_result)
            }
            .auto_call(context, span, arguments, expressions),
            "signum" => |_context: &mut ExecutionContext<'a, S>,
                         span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                Number::new(self.value.signum()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "acos" => |_context: &mut ExecutionContext<'a, S>,
                       span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                self.check_inverse_trig_compatible(span)?;
                Number::new((self.value * consts::PI).acos()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "acosh" => |_context: &mut ExecutionContext<'a, S>,
                        span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                self.check_inverse_trig_compatible(span)?;
                Number::new((self.value * consts::PI).acosh()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "asin" => |_context: &mut ExecutionContext<'a, S>,
                       span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                self.check_inverse_trig_compatible(span)?;
                Number::new((self.value * consts::PI).asin()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "asinh" => |_context: &mut ExecutionContext<'a, S>,
                        span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                self.check_inverse_trig_compatible(span)?;
                Number::new((self.value * consts::PI).asinh()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "atan" => |_context: &mut ExecutionContext<'a, S>,
                       span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                self.check_inverse_trig_compatible(span)?;
                Number::new((self.value * consts::PI).atan()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "atanh" => |_context: &mut ExecutionContext<'a, S>,
                        span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                self.check_inverse_trig_compatible(span)?;
                Number::new((self.value * consts::PI).atanh()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "cos" => |_context: &mut ExecutionContext<'a, S>,
                      span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                self.check_trig_compatible(span)?;
                Number::new((self.value * consts::PI).cos()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "cosh" => |_context: &mut ExecutionContext<'a, S>,
                       span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                self.check_trig_compatible(span)?;
                Number::new((self.value * consts::PI).cosh()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "sin" => |_context: &mut ExecutionContext<'a, S>,
                      span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                self.check_trig_compatible(span)?;
                Number::new((self.value * consts::PI).sin()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "sin_cos" => |_context: &mut ExecutionContext<'a, S>,
                          _span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                self.check_trig_compatible(span)?;
                let (sin, cos) = (self.value * consts::PI).sin_cos();
                let sin = Number::new(sin).unwrap_not_nan(span)?;
                let cos = Number::new(cos).unwrap_not_nan(span)?;

                Ok(List::from([sin, cos]).into())
            }
            .auto_call(context, span, arguments, expressions),
            "sinh" => |_context: &mut ExecutionContext<'a, S>,
                       span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                self.check_trig_compatible(span)?;
                Number::new((self.value * consts::PI).sinh()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "tan" => |_context: &mut ExecutionContext<'a, S>,
                      span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                self.check_trig_compatible(span)?;
                Number::new((self.value * consts::PI).tan()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            "tanh" => |_context: &mut ExecutionContext<'a, S>,
                       span: &S|
             -> OperatorResult<S, Value<'a, S>> {
                self.check_trig_compatible(span)?;
                Number::new((self.value * consts::PI).tanh()).unwrap_not_nan(span)
            }
            .auto_call(context, span, arguments, expressions),
            // TODO we need functions to convert zero dimensional types to: Angles, Constitute Concentration, Information, Solid Angle, and Temperature.
            _ => Err(Failure::UnknownAttribute(attribute.clone())),
        }
    }
}

impl NamedObject for Measurement {
    fn static_type_name() -> &'static str {
        "Measurement"
    }
}

impl Measurement {
    pub fn multiply_by_measurement<S: Span>(
        &self,
        span: &S,
        rhs: &Self,
    ) -> OperatorResult<S, Self> {
        let value = Number::new(*self.value * *rhs.value).unwrap_not_nan_raw(span)?;

        let dimension = Dimension {
            length: self.dimension.length + rhs.dimension.length,
            mass: self.dimension.mass + rhs.dimension.mass,
            time: self.dimension.time + rhs.dimension.time,
            electric_current: self.dimension.electric_current + rhs.dimension.electric_current,
            thermodynamic_temprature: self.dimension.thermodynamic_temprature
                + rhs.dimension.thermodynamic_temprature,
            amount_of_substance: self.dimension.amount_of_substance
                + rhs.dimension.amount_of_substance,
            luminous_intensity: self.dimension.luminous_intensity
                + rhs.dimension.luminous_intensity,
            ratio_type_hint: RatioTypeHint(
                self.dimension.ratio_type_hint.0 | rhs.dimension.ratio_type_hint.0,
            ),
        };

        Ok(Self { dimension, value })
    }

    pub fn multiply_by_number<S: Span>(&self, span: &S, rhs: &Number) -> OperatorResult<S, Self> {
        let value = Number::new(*self.value * **rhs).unwrap_not_nan_raw(span)?;

        Ok(Self { value, ..*self })
    }

    pub fn divide_by_measurement<S: Span>(&self, span: &S, rhs: &Self) -> OperatorResult<S, Self> {
        let value = Number::new(*self.value / *rhs.value).unwrap_not_nan_raw(span)?;

        let dimension = Dimension {
            length: self.dimension.length - rhs.dimension.length,
            mass: self.dimension.mass - rhs.dimension.mass,
            time: self.dimension.time - rhs.dimension.time,
            electric_current: self.dimension.electric_current - rhs.dimension.electric_current,
            thermodynamic_temprature: self.dimension.thermodynamic_temprature
                - rhs.dimension.thermodynamic_temprature,
            amount_of_substance: self.dimension.amount_of_substance
                - rhs.dimension.amount_of_substance,
            luminous_intensity: self.dimension.luminous_intensity
                - rhs.dimension.luminous_intensity,
            ratio_type_hint: RatioTypeHint(
                self.dimension.ratio_type_hint.0 | rhs.dimension.ratio_type_hint.0,
            ),
        };

        Ok(Self { dimension, value })
    }

    pub fn divide_by_number<S: Span>(&self, span: &S, rhs: &Number) -> OperatorResult<S, Self> {
        let value = Number::new(*self.value / **rhs).unwrap_not_nan_raw(span)?;

        Ok(Self { value, ..*self })
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

    fn unpack_for_addition_or_subtraction<'b, S: Span>(
        &'b self,
        span: &S,
        rhs: &'b Value<'_, S>,
    ) -> OperatorResult<S, &Self> {
        if let Value::Measurement(rhs) = rhs {
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

    pub fn from_number<'a, S: Span>(
        span: &S,
        value: Number,
        ty: SString,
    ) -> OperatorResult<S, Value<'a, S>> {
        if let Some(conversion_factor) = CONVERSION_FACTORS.get(ty.as_str()) {
            let value = conversion_factor.to_base_unit(value);
            let dimension = conversion_factor.dimension.clone();

            Ok(Self { dimension, value }.into())
        } else {
            Err(Failure::UnknownUnitType(
                span.clone(),
                ty.into_string().into(),
            ))
        }
    }

    pub fn to_number<'a, S: Span>(&self, span: &S, ty: &str) -> OperatorResult<S, Value<'a, S>> {
        if let Some(conversion_factor) = CONVERSION_FACTORS.get(ty) {
            if self.dimension == conversion_factor.dimension {
                Ok(conversion_factor.from_base_unit(self.value).into())
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

    pub fn from_parsed_raw<S: Span>(
        measurement: &parsing::Measurement<S>,
    ) -> OperatorResult<S, Self> {
        if let Some(conversion_factor) = CONVERSION_FACTORS.get(measurement.ty.as_str()) {
            let value = unwrap_float(measurement.number.get_span().clone(), &measurement.number)?;
            let value = conversion_factor.to_base_unit(value);

            let dimension = conversion_factor.dimension.clone();

            Ok(Self { dimension, value })
        } else {
            Err(Failure::UnknownUnitType(
                measurement.ty.clone(),
                measurement.ty.to_string().into(),
            ))
        }
    }

    pub fn from_parsed<'a, S: Span>(
        measurement: &parsing::Measurement<S>,
    ) -> OperatorResult<S, Value<'a, S>> {
        Self::from_parsed_raw(measurement).map(|measurement| measurement.into())
    }

    pub fn get_conversion_factor(
        keyboard_friendly_abbreviation: &str,
    ) -> Option<&'static ConversionFactor> {
        CONVERSION_FACTORS.get(keyboard_friendly_abbreviation)
    }
}

impl<D, U> TryFrom<Quantity<D, U, f64>> for Measurement
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
    type Error = ordered_float::FloatIsNan;

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

        let value = Number::new(value.value)?;

        Ok(Self { dimension, value })
    }
}

impl<D, U> TryInto<Quantity<D, U, f64>> for Measurement
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

impl FromStr for Measurement {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (_leftover, measurement) = parsing::Measurement::parse(s)
            .map_err(|error| anyhow::anyhow!("Failed to parse measurement: {}", error))?;
        let measurement = Self::from_parsed_raw(&measurement)
            .map_err(|failure| anyhow::anyhow!("{}", failure))?;

        Ok(measurement)
    }
}

#[cfg(test)]
mod test {
    use uom::si::f64 as si;

    use crate::script::{
        execution::{expressions::run_expression, ExecutionContext},
        logging::StandardLog,
    };

    use super::*;

    use uom::si::length::meter;

    #[test]
    fn addition() {
        let mut log = StandardLog;

        let a = si::Length::new::<meter>(3.0);
        let b = si::Length::new::<meter>(2.0);
        let value_a: Value<&str> = Measurement::try_from(a).unwrap().into();
        let value_b: Value<&str> = Measurement::try_from(b).unwrap().into();
        assert_eq!(
            value_a.addition(&mut log, &"span", &value_b),
            Ok(Measurement::try_from(a + b).unwrap().into())
        );
    }

    #[test]
    fn subtraction() {
        let mut log = StandardLog;

        let a = si::Length::new::<meter>(3.0);
        let b = si::Length::new::<meter>(2.0);
        let value_a: Value<&str> = Measurement::try_from(a).unwrap().into();
        let value_b: Value<&str> = Measurement::try_from(b).unwrap().into();
        assert_eq!(
            value_a.subtraction(&mut log, &"span", &value_b),
            Ok(Measurement::try_from(a - b).unwrap().into())
        );
    }

    #[test]
    fn multiplication() {
        let mut log = StandardLog;

        let a = si::Length::new::<meter>(3.0);
        let b = si::Length::new::<meter>(2.0);
        let c = a * b;
        let value_a: Value<&str> = Measurement::try_from(a).unwrap().into();
        let value_b: Value<&str> = Measurement::try_from(b).unwrap().into();
        let value_c: Value<&str> = Measurement::try_from(c).unwrap().into();
        assert_eq!(
            value_a.multiply(&mut log, &"span", &value_b),
            Ok(Measurement::try_from(a * b).unwrap().into())
        );
        assert_eq!(
            value_a.multiply(&mut log, &"span", &value_c),
            Ok(Measurement::try_from(a * c).unwrap().into())
        );
    }

    #[test]
    fn division() {
        let mut log = StandardLog;

        let a = si::Length::new::<meter>(3.0);
        let b = si::Length::new::<meter>(2.0);
        let value_a: Value<&str> = Measurement::try_from(a).unwrap().into();
        let value_b: Value<&str> = Measurement::try_from(b).unwrap().into();
        assert_eq!(
            value_a.divide(&mut log, &"span", &value_b),
            Ok(Measurement::try_from(a / b).unwrap().into())
        );

        let c: si::Area = a * b;
        let value_c: Value<&str> = Measurement::try_from(c).unwrap().into();
        assert_eq!(
            value_c.divide(&mut log, &"span", &value_a),
            Ok(Measurement::try_from(c / a).unwrap().into())
        );
    }

    // TODO test multiplication for AngleKind, ConstituentConcentrationKind, InformationKind, SolidAngleKind, TemperatureKind

    #[test]
    fn comparisions() {
        let mut log = StandardLog;

        let a = si::Length::new::<meter>(3.0);
        let b = si::Length::new::<meter>(2.0);
        let value_a: Value<&str> = Measurement::try_from(a).unwrap().into();
        let value_b: Value<&str> = Measurement::try_from(b).unwrap().into();
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
        let mut context = ExecutionContext::default();

        assert_eq!(
            run_expression(
                &mut context,
                Box::leak(Box::new(Expression::parse("1m + 100cm == 2m").unwrap().1))
            ),
            Ok(true.into())
        );
        assert_eq!(
            run_expression(
                &mut context,
                Box::leak(Box::new(Expression::parse("2m * 2m == 4m^2").unwrap().1))
            ),
            Ok(true.into())
        );
        assert_eq!(
            run_expression(
                &mut context,
                Box::leak(Box::new(
                    Expression::parse("(2m).to_number(\"cm\") == 200")
                        .unwrap()
                        .1
                ))
            ),
            { Ok(true.into()) }
        );
        assert_eq!(
            run_expression(
                &mut context,
                Box::leak(Box::new(
                    Expression::parse("(200).to_measurement(\"mm\") == 200mm")
                        .unwrap()
                        .1
                ))
            ),
            { Ok(true.into()) }
        );
    }
}
