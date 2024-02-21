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
    si::{self, Dimension, Quantity, Units},
    typenum::ToInt,
};

use crate::script::{
    execution::{ExecutionContext, Failure},
    parsing::{self, Expression, VariableType},
    RuntimeLog, Span,
};

use super::{
    function::AutoCall, number::UnwrapNotNan, NamedObject, Number, Object, OperatorResult, SString,
    Value,
};

mod from_parsed;

#[derive(Debug, Clone, PartialEq)]
pub struct Measurement {
    // Meter
    length: i8,

    // Kilogram
    mass: i8,

    // Second
    time: i8,

    // Apere
    electric_current: i8,

    // Kelvin
    thermodynamic_temprature: i8,

    // Mole
    amount_of_substance: i8,

    // Candela
    luminous_intensity: i8,

    // Hints of type for ratios.
    angle_kind: bool,
    constituent_concentration_kind: bool,
    information_kind: bool,
    solid_angle_kind: bool,
    temperature_kind: bool,

    value: Number,
}

macro_rules! named_measurement_branchs {
    ( $self:ident, $($name:ident),* ) => {

	{
            $(paste::paste! { const [<$name:snake:upper _IS_ANGLE_KIND>]: bool = has_impl::has_impl!(<si:: [< $name:snake:lower >] ::Dimension as Dimension>::Kind: si::marker::AngleKind); })*;
            $(paste::paste! { const [<$name:snake:upper _IS_CONSTITUENT_CONCENTRATION_KIND>]: bool = has_impl::has_impl!(<si:: [< $name:snake:lower >] ::Dimension as Dimension>::Kind: si::marker::ConstituentConcentrationKind); })*;
            $(paste::paste! { const [<$name:snake:upper _IS_INFORMATION_KIND>]: bool = has_impl::has_impl!(<si:: [< $name:snake:lower >] ::Dimension as Dimension>::Kind: si::marker::InformationKind); })*;
            $(paste::paste! { const [<$name:snake:upper _IS_SOLID_ANGLE_KIND>]: bool = has_impl::has_impl!(<si:: [< $name:snake:lower >] ::Dimension as Dimension>::Kind: si::marker::SolidAngleKind); })*;
            $(paste::paste! { const [<$name:snake:upper _IS_TEMPERATURE_KIND>]: bool = has_impl::has_impl!(<si:: [< $name:snake:lower >] ::Dimension as Dimension>::Kind: si::marker::TemperatureKind); })*;

            match (
       		$self.length,
       		$self.mass,
       		$self.time,
       		$self.electric_current,
       		$self.thermodynamic_temprature,
       		$self.amount_of_substance,
       		$self.luminous_intensity,
       		$self.angle_kind,
		$self.constituent_concentration_kind,
		$self.information_kind,
		$self.solid_angle_kind,
		$self.temperature_kind,
            ) {
       		$((
       		    <paste::paste! { si:: [< $name:snake:lower >] ::Dimension} as Dimension>::L::INT,
       		    <paste::paste! { si:: [< $name:snake:lower >] ::Dimension} as Dimension>::M::INT,
       		    <paste::paste! { si:: [< $name:snake:lower >] ::Dimension} as Dimension>::T::INT,
       		    <paste::paste! { si:: [< $name:snake:lower >] ::Dimension} as Dimension>::I::INT,
       		    <paste::paste! { si:: [< $name:snake:lower >] ::Dimension} as Dimension>::Th::INT,
       		    <paste::paste! { si:: [< $name:snake:lower >] ::Dimension} as Dimension>::N::INT,
       		    <paste::paste! { si:: [< $name:snake:lower >] ::Dimension} as Dimension>::J::INT,
       		    paste::paste! { [<$name:snake:upper _IS_ANGLE_KIND>] },
		    paste::paste! { [<$name:snake:upper _IS_CONSTITUENT_CONCENTRATION_KIND>] },
		    paste::paste! { [<$name:snake:upper _IS_INFORMATION_KIND>] },
		    paste::paste! { [<$name:snake:upper _IS_SOLID_ANGLE_KIND>] },
		    paste::paste! { [<$name:snake:upper _IS_TEMPERATURE_KIND>] },
       		) => stringify!($name).into(),)*
       		_ => format!(
                    "Measurement<L{}, M{}, T{}, I{}, Th{}, N{}, J{}>",
                    $self.length,
                    $self.mass,
                    $self.time,
                    $self.electric_current,
                    $self.thermodynamic_temprature,
                    $self.amount_of_substance,
                    $self.luminous_intensity
       		).into(),
            }
	}
    }
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
        named_measurement_branchs!(
            self,
            Absement,
            Acceleration,
            Action,
            AmountOfSubstance,
            Angle,
            AngularAbsement,
            AngularAcceleration,
            AngularJerk,
            AngularVelocity,
            Area,
            ArealDensityOfStates,
            ArealMassDensity,
            ArealNumberDensity,
            ArealNumberRate,
            AvailableEnergy,
            Capacitance,
            CatalyticActivity,
            CatalyticActivityConcentration,
            Curvature,
            DiffusionCoefficient,
            DynamicViscosity,
            ElectricCharge,
            ElectricChargeArealDensity,
            ElectricChargeLinearDensity,
            ElectricChargeVolumetricDensity,
            ElectricCurrent,
            ElectricCurrentDensity,
            ElectricDipoleMoment,
            ElectricDisplacementField,
            ElectricField,
            ElectricFlux,
            ElectricPermittivity,
            ElectricPotential,
            ElectricQuadrupoleMoment,
            ElectricalConductance,
            ElectricalConductivity,
            ElectricalMobility,
            ElectricalResistance,
            ElectricalResistivity,
            Energy,
            Force,
            Frequency,
            FrequencyDrift,
            HeatCapacity,
            HeatFluxDensity,
            HeatTransfer,
            Inductance,
            Information,
            InformationRate,
            InverseVelocity,
            Jerk,
            Length,
            LinearDensityOfStates,
            LinearMassDensity,
            LinearNumberDensity,
            LinearNumberRate,
            LinearPowerDensity,
            Luminance,
            LuminousIntensity,
            MagneticFieldStrength,
            MagneticFlux,
            MagneticFluxDensity,
            MagneticMoment,
            MagneticPermeability,
            Mass,
            MassConcentration,
            MassDensity,
            MassFlux,
            MassPerEnergy,
            MassRate,
            Molality,
            MolarConcentration,
            MolarEnergy,
            MolarFlux,
            MolarHeatCapacity,
            MolarMass,
            MolarRadioactivity,
            MolarVolume,
            MomentOfInertia,
            Momentum,
            Power,
            PowerRate,
            Pressure,
            RadiantExposure,
            Radioactivity,
            Ratio,
            ReciprocalLength,
            SolidAngle,
            SpecificArea,
            SpecificHeatCapacity,
            SpecificPower,
            SpecificRadioactivity,
            SpecificVolume,
            SurfaceElectricCurrentDensity,
            TemperatureCoefficient,
            TemperatureGradient,
            TemperatureInterval,
            ThermalConductance,
            ThermalConductivity,
            ThermodynamicTemperature,
            Time,
            Torque,
            Velocity,
            Volume,
            VolumeRate,
            VolumetricDensityOfStates,
            VolumetricHeatCapacity,
            VolumetricNumberDensity,
            VolumetricNumberRate,
            VolumetricPowerDensity
        )
    }

    fn addition(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> OperatorResult<S, Value<'a, S>> {
        let rhs = self.unpack_for_addition_or_subtraction(log, span, rhs)?;

        let value = Number::new(*self.value + *rhs.value).unwrap_not_nan_raw(span)?;

        Ok(Self { value, ..*self }.into())
    }
    fn subtraction(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> OperatorResult<S, Value<'a, S>> {
        let rhs = self.unpack_for_addition_or_subtraction(log, span, rhs)?;

        let value = Number::new(*self.value - *rhs.value).unwrap_not_nan_raw(span)?;

        Ok(Self { value, ..*self }.into())
    }
    fn multiply(
        &self,
        _log: &mut RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> OperatorResult<S, Value<'a, S>> {
        match rhs {
            Value::Measurement(rhs) => {
                let value = Number::new(*self.value * *rhs.value).unwrap_not_nan_raw(span)?;

                Ok(Self {
                    length: self.length + rhs.length,
                    mass: self.mass + rhs.mass,
                    time: self.time + rhs.time,
                    electric_current: self.electric_current + rhs.electric_current,
                    thermodynamic_temprature: self.thermodynamic_temprature
                        + rhs.thermodynamic_temprature,
                    amount_of_substance: self.amount_of_substance + rhs.amount_of_substance,
                    luminous_intensity: self.luminous_intensity + rhs.luminous_intensity,
                    angle_kind: self.angle_kind | rhs.angle_kind,
                    constituent_concentration_kind: self.constituent_concentration_kind
                        | rhs.constituent_concentration_kind,
                    information_kind: self.information_kind | rhs.information_kind,
                    solid_angle_kind: self.solid_angle_kind | rhs.solid_angle_kind,
                    temperature_kind: self.temperature_kind | rhs.temperature_kind,
                    value,
                }
                .into())
            }
            Value::Number(rhs) => {
                let value = Number::new(*self.value * **rhs).unwrap_not_nan_raw(span)?;

                Ok(Self { value, ..*self }.into())
            }
            _ => Err(Failure::ExpectedGot(
                span.clone(),
                "Measurement or Number".into(),
                rhs.type_name(),
            )),
        }
    }
    fn divide(
        &self,
        _log: &mut RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> OperatorResult<S, Value<'a, S>> {
        match rhs {
            Value::Measurement(rhs) => {
                let value = Number::new(*self.value / *rhs.value).unwrap_not_nan_raw(span)?;

                Ok(Self {
                    length: self.length - rhs.length,
                    mass: self.mass - rhs.mass,
                    time: self.time - rhs.time,
                    electric_current: self.electric_current - rhs.electric_current,
                    thermodynamic_temprature: self.thermodynamic_temprature
                        - rhs.thermodynamic_temprature,
                    amount_of_substance: self.amount_of_substance - rhs.amount_of_substance,
                    luminous_intensity: self.luminous_intensity - rhs.luminous_intensity,
                    angle_kind: self.angle_kind | rhs.angle_kind,
                    constituent_concentration_kind: self.constituent_concentration_kind
                        | rhs.constituent_concentration_kind,
                    information_kind: self.information_kind | rhs.information_kind,
                    solid_angle_kind: self.solid_angle_kind | rhs.solid_angle_kind,
                    temperature_kind: self.temperature_kind | rhs.temperature_kind,
                    value,
                }
                .into())
            }
            Value::Number(rhs) => {
                let value = Number::new(*self.value / **rhs).unwrap_not_nan_raw(span)?;

                Ok(Self { value, ..*self }.into())
            }
            _ => Err(Failure::ExpectedGot(
                span.clone(),
                "Measurement or Number".into(),
                rhs.type_name(),
            )),
        }
    }
    fn unary_plus(&self, _log: &mut RuntimeLog<S>, _span: &S) -> OperatorResult<S, Value<'a, S>> {
        Ok(self.clone().into())
    }
    fn unary_minus(&self, _log: &mut RuntimeLog<S>, _span: &S) -> OperatorResult<S, Value<'a, S>> {
        Ok(Self {
            value: -self.value,
            ..self.clone()
        }
        .into())
    }
    fn cmp(
        &self,
        _log: &mut RuntimeLog<S>,
        span: &S,
        rhs: &Value<'a, S>,
    ) -> OperatorResult<S, Ordering> {
        let rhs = rhs.downcast_ref::<Self>(span)?;
        if self.length == rhs.length
            && self.mass == rhs.mass
            && self.time == rhs.time
            && self.electric_current == rhs.electric_current
            && self.thermodynamic_temprature == rhs.thermodynamic_temprature
            && self.amount_of_substance == rhs.amount_of_substance
            && self.luminous_intensity == rhs.luminous_intensity
            && self.angle_kind == rhs.angle_kind
            && self.constituent_concentration_kind == rhs.constituent_concentration_kind
            && self.information_kind == rhs.information_kind
            && self.solid_angle_kind == rhs.solid_angle_kind
            && self.temperature_kind == rhs.temperature_kind
        {
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
    fn unpack_for_addition_or_subtraction<'b, S: Span>(
        &'b self,
        _log: &mut RuntimeLog<S>,
        span: &S,
        rhs: &'b Value<'_, S>,
    ) -> OperatorResult<S, &Self> {
        if let Value::Measurement(rhs) = rhs {
            if rhs.length == self.length
                && rhs.mass == self.mass
                && rhs.time == self.time
                && rhs.electric_current == self.electric_current
                && rhs.thermodynamic_temprature == self.thermodynamic_temprature
                && rhs.amount_of_substance == self.amount_of_substance
                && rhs.luminous_intensity == self.luminous_intensity
                && rhs.angle_kind == self.angle_kind
            {
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
}

impl<D, U> TryFrom<Quantity<D, U, f64>> for Measurement
where
    D: Dimension + ?Sized,
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
        let angle_kind: bool = has_impl::has_impl!(D::Kind: si::marker::AngleKind);
        let constituent_concentration_kind: bool =
            has_impl::has_impl!(D::Kind: si::marker::ConstituentConcentrationKind);
        let information_kind: bool = has_impl::has_impl!(D::Kind: si::marker::InformationKind);
        let solid_angle_kind: bool = has_impl::has_impl!(D::Kind: si::marker::SolidAngleKind);
        let temperature_kind: bool = has_impl::has_impl!(D::Kind: si::marker::TemperatureKind);

        let length = D::L::INT;
        let mass = D::M::INT;
        let time = D::T::INT;
        let electric_current = D::I::INT;
        let thermodynamic_temprature = D::Th::INT;
        let amount_of_substance = D::N::INT;
        let luminous_intensity = D::J::INT;

        let value = Number::new(value.value)?;

        Ok(Self {
            length,
            mass,
            time,
            electric_current,
            thermodynamic_temprature,
            amount_of_substance,
            luminous_intensity,
            angle_kind,
            constituent_concentration_kind,
            information_kind,
            solid_angle_kind,
            temperature_kind,
            value,
        })
    }
}

impl<D, U> TryInto<Quantity<D, U, f64>> for Measurement
where
    D: Dimension + ?Sized,
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
        if D::L::INT == self.length
            && D::M::INT == self.mass
            && D::T::INT == self.time
            && D::I::INT == self.electric_current
            && D::Th::INT == self.thermodynamic_temprature
            && D::N::INT == self.amount_of_substance
            && D::J::INT == self.luminous_intensity
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

    use crate::script::execution::{expressions::run_expression, ExecutionContext};

    use super::*;

    use uom::si::length::meter;

    #[test]
    fn addition() {
        let mut log = RuntimeLog::default();

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
        let mut log = RuntimeLog::default();

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
        let mut log = RuntimeLog::default();

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
        let mut log = RuntimeLog::default();

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
        let mut log = RuntimeLog::default();

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
                &Expression::parse("1m + 100cm == 2m").unwrap().1
            ),
            Ok(true.into())
        );
        assert_eq!(
            run_expression(
                &mut context,
                &Expression::parse("2m * 2m == 4m^2").unwrap().1
            ),
            Ok(true.into())
        );
        assert_eq!(
            run_expression(
                &mut context,
                &Expression::parse("(2m).to_number(\"cm\") == 200")
                    .unwrap()
                    .1
            ),
            { Ok(true.into()) }
        );
        assert_eq!(
            run_expression(
                &mut context,
                &Expression::parse("(200).to_measurement(\"mm\") == 200mm")
                    .unwrap()
                    .1
            ),
            { Ok(true.into()) }
        );
    }
}
