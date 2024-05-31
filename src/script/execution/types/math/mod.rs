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

use std::{borrow::Cow, io};

use common_data_types::{
    BaseUnits, ConversionFactor, ConversionFactorDatabase, Dimension, DimensionNameDatabase, Float,
    RatioTypeHint, RawFloat, UnitDescription, UnitList,
};
use nalgebra::{DefaultAllocator, DimName};

use crate::script::{
    execution::{ExecutionContext, Failure},
    Span,
};

mod scalar;
pub use scalar::{Angle, Length, Number, Scalar};

mod vector;
use self::vector::Vector;
pub use vector::{LengthVector2, LengthVector3, Vector2, Vector3, Vector4};

mod transform;
pub use transform::{Transform2D, Transform3D};

mod quaternion;
pub use quaternion::Quaternion;

use super::OperatorResult;

lazy_static::lazy_static! {
    static ref CONVERSION_FACTORS: ConversionFactorDatabase = include!(concat!(env!("OUT_DIR"), "/conversion_factors.rs"));
    static ref DIMENSION_NAMES: DimensionNameDatabase = include!(concat!(env!("OUT_DIR"), "/dimension_names.rs"));
    static ref UNIT_LIST: UnitList = include!(concat!(env!("OUT_DIR"), "/unit_list.rs"));
    static ref BASE_UNITS: BaseUnits = include!(concat!(env!("OUT_DIR"), "/base_units.rs"));
}

pub fn register_globals<S: Span>(context: &mut ExecutionContext<S>) {
    // TODO this would be a good point call into scalar and create some global constants.
    vector::register_globals(context);
    transform::register_globals(context);
    quaternion::register_globals(context);
}

trait CheckNan {
    fn check_nan<S: Span>(self, span: &S) -> OperatorResult<S, ()>;
}

impl<
        R: nalgebra::base::Dim,
        C: nalgebra::base::Dim,
        S: nalgebra::base::storage::RawStorage<RawFloat, R, C>,
    > CheckNan for nalgebra::Matrix<RawFloat, R, C, S>
{
    fn check_nan<SP: Span>(self, span: &SP) -> OperatorResult<SP, ()> {
        if !self.iter().any(|c| c.is_nan()) {
            Ok(())
        } else {
            Err(Failure::ResultIsNan(span.clone()))
        }
    }
}

fn format_dimension(dimension: &Dimension) -> Cow<'static, str> {
    format!(
        "Scalar<L{}, M{}, T{}, I{}, Th{}, N{}, J{}>",
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

fn get_dimension_name(dimension: &Dimension) -> Cow<'static, str> {
    if let Some(name) = DIMENSION_NAMES.get(dimension) {
        name.clone()
    } else {
        format_dimension(dimension)
    }
}

pub trait ConvertUnit {
    fn convert_to_base_unit(&self, input: Float) -> Float;
    fn convert_from_base_unit(&self, input: Float) -> Float;
    fn convert_from_measurement_to_number<S: Span>(
        &self,
        span: &S,
        measurement: &Scalar,
    ) -> OperatorResult<S, Float>;
    fn convert_from_vector_to_iter<S: Span, D: DimName>(
        &self,
        span: &S,
        vector: &Vector<D>,
    ) -> OperatorResult<S, impl Iterator<Item = Float>>
    where
        DefaultAllocator: nalgebra::allocator::Allocator<f64, D>;
    fn convert_from_vector_to_iter_without_dimension_check<D: DimName>(
        &self,
        vector: &vector::NVector<D>,
    ) -> impl Iterator<Item = Float>
    where
        DefaultAllocator: nalgebra::allocator::Allocator<f64, D>;
}

impl ConvertUnit for ConversionFactor {
    fn convert_to_base_unit(&self, input: Float) -> Float {
        input * self.coefficient + self.constant
    }

    fn convert_from_base_unit(&self, input: Float) -> Float {
        (input - self.constant) / self.coefficient
    }

    fn convert_from_measurement_to_number<S: Span>(
        &self,
        span: &S,
        measurement: &Scalar,
    ) -> OperatorResult<S, Float> {
        if measurement.dimension == self.dimension {
            Ok(self.convert_from_base_unit(measurement.value))
        } else {
            Err(Failure::ExpectedGot(
                span.clone(),
                get_dimension_name(&self.dimension),
                get_dimension_name(&measurement.dimension),
            ))
        }
    }

    fn convert_from_vector_to_iter_without_dimension_check<D: DimName>(
        &self,
        vector: &vector::NVector<D>,
    ) -> impl Iterator<Item = Float>
    where
        DefaultAllocator: nalgebra::allocator::Allocator<f64, D>,
    {
        vector
            .iter()
            .copied()
            .map(|c| self.convert_from_base_unit(Float::new(c).unwrap()))
    }

    fn convert_from_vector_to_iter<S: Span, D: DimName>(
        &self,
        span: &S,
        vector: &Vector<D>,
    ) -> OperatorResult<S, impl Iterator<Item = Float>>
    where
        DefaultAllocator: nalgebra::allocator::Allocator<f64, D>,
    {
        if vector.dimension == self.dimension {
            Ok(vector
                .value
                .iter()
                .copied()
                .map(|c| self.convert_from_base_unit(Float::new(c).unwrap())))
        } else {
            Err(Failure::ExpectedGot(
                span.clone(),
                get_dimension_name(&self.dimension),
                get_dimension_name(&vector.dimension),
            ))
        }
    }
}

pub fn print_all_supported_units(
    f: &mut dyn io::Write,
    filter: Option<&str>,
) -> std::result::Result<(), io::Error> {
    for (dimension, units) in UNIT_LIST.iter() {
        let units = units.iter().filter(|unit| {
            if let Some(filter) = filter {
                unit.name.contains(filter) || unit.abbreviation.contains(filter)
            } else {
                true
            }
        });

        let name = "Name";
        let abbreviation = "Abbreviation";
        let keyboard_friendly_abbreviation = "Keyboard Friendly Abbreviation";
        let plural_name = "Plural Name";

        let mut name_length = name.len();
        let mut abbreviation_length = abbreviation.len();
        let mut keyboard_friendly_abbreviation_length = keyboard_friendly_abbreviation.len();
        let mut plural_name_length = plural_name.len();

        let mut contained_any = false;

        for unit in units.clone() {
            contained_any = true;

            name_length = name_length.max(unit.name.len());
            abbreviation_length = abbreviation_length.max(unit.abbreviation.len());
            keyboard_friendly_abbreviation_length = keyboard_friendly_abbreviation_length
                .max(unit.keyboard_friendly_abbreviation.len());
            plural_name_length = plural_name_length.max(unit.plural_name.len());
        }

        if contained_any {
            writeln!(f, "{}:\n", dimension)?;
            let header = format!(
            "[{:<name_length$}|{:<abbreviation_length$}|{:<keyboard_friendly_abbreviation_length$}|{:<plural_name_length$}]",
            name, abbreviation, keyboard_friendly_abbreviation, plural_name
            );

            writeln!(f, "{}", header)?;
            writeln!(f, "{:-<1$}", "", header.len())?;

            for unit in units {
                let keyboard_abbreviation = if !unit.keyboard_friendly_abbreviation.contains(' ')
                    && !unit.keyboard_friendly_abbreviation.contains('.')
                    && !unit.keyboard_friendly_abbreviation.contains('(')
                    && !unit.keyboard_friendly_abbreviation.contains(')')
                {
                    Cow::Borrowed(&unit.keyboard_friendly_abbreviation)
                } else {
                    Cow::Owned(format!("<{}>", unit.keyboard_friendly_abbreviation))
                };

                writeln!(
                f,
                " {:<name_length$}|{:<abbreviation_length$}|{:<keyboard_friendly_abbreviation_length$}|{:<plural_name_length$} ",
                unit.name, unit.abbreviation, keyboard_abbreviation, unit.plural_name
            )?;
            }
        }
    }

    Ok(())
}
