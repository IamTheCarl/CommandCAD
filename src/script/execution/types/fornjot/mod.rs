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

use enum_downcast::{AsVariant, EnumDowncast, IntoVariant};
use fj_math::{Point, Scalar, Vector};

use crate::script::{
    execution::{
        types::{number::RawNumber, Measurement, Object},
        ExecutionContext, Failure,
    },
    Span,
};

use super::{List, NamedObject, OperatorResult, Value};

// TODO I want a box type that can be a square or a rectangle.
mod circle;
pub mod cycle;
pub mod face;
pub mod object_set;
mod polygon;
pub mod region;
pub mod shell;
pub mod sketch;
pub mod solid;
pub mod surface;

pub fn register_globals<S: Span>(context: &mut ExecutionContext<'_, S>) {
    circle::register_globals(context);
    polygon::register_globals(context);

    cycle::register_globals(context);
    face::register_globals(context);
    region::register_globals(context);
    shell::register_globals(context);
    sketch::register_globals(context);
    solid::register_globals(context);
    surface::register_globals(context);
}

fn unpack_dynamic_length_list<'a, S, T>(
    span: &S,
    list: List<'a, S>,
) -> OperatorResult<S, impl Iterator<Item = T> + Clone + 'a>
where
    S: Span,
    T: NamedObject + Clone,
    Value<'a, S>: IntoVariant<T> + AsVariant<T> + TryInto<T>,
{
    // Verify that they're all of the right type.
    for (index, item) in list.iter().enumerate() {
        if item.enum_downcast_ref::<T>().is_none() {
            return Err(Failure::ListElementFailure(
                span.clone(),
                index,
                Box::new(Failure::ExpectedGot(
                    span.clone(),
                    T::static_type_name().into(),
                    item.type_name(),
                )),
            ));
        }
    }

    // Okay, we've validated them. Now we can really take them.
    // The unwraps will not fail because we've already validated the types.
    let iter = list.consume().map(|v| v.enum_downcast::<T>().unwrap());
    Ok(iter)
}

macro_rules! handle_wrapper {
    ($name:ident, $handle:ident) => {
        impl<'a, S: Span> From<Handle<$handle>> for crate::script::execution::types::Value<'a, S> {
            fn from(handle: Handle<$handle>) -> Self {
                $name::from(handle).into()
            }
        }

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                use std::ops::Deref;
                f.debug_struct(stringify!($name))
                    .field("id", &self.handle.id())
                    .field("content", &self.handle.deref())
                    .finish()
            }
        }

        impl<'a, S: Span> TryFrom<crate::script::execution::types::Value<'a, S>>
            for Handle<$handle>
        {
            type Error = crate::script::execution::types::Value<'a, S>;

            fn try_from(
                value: crate::script::execution::types::Value<'a, S>,
            ) -> Result<Self, Self::Error> {
                use enum_downcast::EnumDowncast;
                let value = value.enum_downcast::<$name>()?;
                Ok(value.handle)
            }
        }

        impl From<Handle<$handle>> for $name {
            fn from(handle: Handle<$handle>) -> Self {
                Self { handle }
            }
        }

        impl From<$name> for Handle<$handle> {
            fn from(val: $name) -> Self {
                val.handle
            }
        }

        impl crate::script::execution::types::NamedObject for $name {
            fn static_type_name() -> &'static str {
                stringify!($name)
            }
        }

        impl PartialEq for $name {
            fn eq(&self, other: &Self) -> bool {
                self.handle == other.handle
            }
        }
    };
}

pub(crate) use handle_wrapper;

fn unpack_fixed_length_list<'a, S, T, const D: usize>(
    span: &S,
    list: List<'a, S>,
) -> OperatorResult<S, impl Iterator<Item = T> + Clone + 'a>
where
    S: Span,
    T: NamedObject + Clone,
    Value<'a, S>: IntoVariant<T> + AsVariant<T> + TryInto<T>,
{
    if list.len() == D {
        // This cannot exceed length D because we already validated that the list matched that length.
        unpack_dynamic_length_list(span, list)
    } else {
        Err(Failure::ListWrongLength(span.clone(), D, list.len()))
    }
}

fn vector_from_list<'a, S, const D: usize>(
    span: &S,
    convert_to_fornjot_units: fn(&Measurement) -> Option<f64>,
    list: List<'a, S>,
) -> OperatorResult<S, Vector<D>>
where
    S: Span,
{
    let mut measurements = unpack_fixed_length_list::<S, Measurement, D>(span, list)?;

    // Validate that all measurements are lengths.
    for item in measurements.clone() {
        let type_name = Object::<S>::type_name(&item);

        if type_name != "Length" {
            return Err(Failure::ExpectedGot(
                span.clone(),
                "Length".into(),
                type_name,
            ));
        }
    }

    // We've already checked the length and that all of the types are correct, so these unwraps should never fail.
    let array: [Scalar; D] = std::array::from_fn(|_| {
        Scalar::from_f64(convert_to_fornjot_units(&measurements.next().unwrap()).unwrap())
    });

    Ok(Vector::from(array))
}

fn scalar_from_measurement<S: Span>(
    span: &S,
    convert_to_fornjot_units: fn(&Measurement) -> Option<RawNumber>,
    measurement: &Measurement,
) -> OperatorResult<S, Scalar> {
    let length = convert_to_fornjot_units(measurement).ok_or(Failure::ExpectedGot(
        span.clone(),
        "Length".into(),
        Object::<S>::type_name(measurement),
    ))?;

    Ok(Scalar::from_f64(length))
}

fn point_from_list<'a, S, const D: usize>(
    span: &S,
    convert_to_fornjot_units: fn(&Measurement) -> Option<RawNumber>,
    list: List<'a, S>,
) -> OperatorResult<S, Point<D>>
where
    S: Span,
{
    let coords = vector_from_list(span, convert_to_fornjot_units, list)?;

    Ok(Point { coords })
}

#[cfg(test)]
mod test {
    use uom::si::{f64::Length, length::millimeter};

    use crate::script::execution::types::{List, Number};

    use super::*;

    #[test]
    fn test_unpack_lists() {
        assert_eq!(
            unpack_fixed_length_list::<&'static str, Number, 3usize>(
                &"span",
                List::<'_, &'static str>::from([
                    Number::new(1.0).unwrap().into(),
                    Number::new(2.0).unwrap().into(),
                    Number::new(3.0).unwrap().into(),
                ])
            )
            .map(|v| v.collect::<Vec<_>>()),
            Ok(vec![
                Number::new(1.0).unwrap(),
                Number::new(2.0).unwrap(),
                Number::new(3.0).unwrap(),
            ])
        );

        let values = [
            Number::new(1.0).unwrap().into(),
            Number::new(2.0).unwrap().into(),
        ];

        assert_eq!(
            unpack_fixed_length_list::<&'static str, Number, 3usize>(
                &"span",
                List::<'_, &'static str>::from(values)
            )
            .map(|v| v.collect::<Vec<_>>()),
            Err(Failure::ListWrongLength("span", 3, 2))
        );

        let values = [Number::new(1.0).unwrap().into(), true.into()];

        assert_eq!(
            unpack_fixed_length_list::<&'static str, Number, 2usize>(
                &"span",
                List::<'_, &'static str>::from(values)
            )
            .map(|v| v.collect::<Vec<_>>()),
            Err(Failure::ListElementFailure(
                "span",
                1,
                Box::new(Failure::ExpectedGot(
                    "span",
                    "Number".into(),
                    "Boolean".into()
                ))
            ))
        );
    }

    #[test]
    fn test_vector_from_list() {
        assert_eq!(
            vector_from_list::<&'static str, 3usize>(
                &"span",
                |value| {
                    let measurement: Length = value.clone().try_into().ok()?;

                    Some(measurement.get::<millimeter>())
                },
                List::<'_, &'static str>::from([
                    Measurement::try_from(Length::new::<millimeter>(1.0))
                        .unwrap()
                        .into(),
                    Measurement::try_from(Length::new::<millimeter>(2.0))
                        .unwrap()
                        .into(),
                    Measurement::try_from(Length::new::<millimeter>(3.0))
                        .unwrap()
                        .into(),
                ])
            ),
            Ok(Vector::from([1.0, 2.0, 3.0]))
        );
    }
}
