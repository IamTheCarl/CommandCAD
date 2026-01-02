use std::collections::HashMap;

use common_data_types::{Dimension, Float};

use crate::execution::values::{Scalar, ValueNone};

use super::values::{Dictionary, Value, ValueType};

/// Builds standard library.
pub fn build_prelude() -> HashMap<String, Value> {
    let global = HashMap::from([("std".into(), build_std().into())]);

    global
}

fn build_std() -> Dictionary {
    let std = HashMap::from([
        ("types".into(), build_types().into()),
        (
            "scalar".into(),
            build_dimension_types(ValueType::Scalar).into(),
        ),
        (
            "vector2".into(),
            build_dimension_types(ValueType::Vector2).into(),
        ),
        (
            "vector3".into(),
            build_dimension_types(ValueType::Vector3).into(),
        ),
        (
            "vector4".into(),
            build_dimension_types(ValueType::Vector4).into(),
        ),
        ("consts".into(), build_consts().into()),
    ]);
    Dictionary::from(std)
}

/// Adds library for constants.
fn build_consts() -> Dictionary {
    let types: HashMap<String, Value> = HashMap::from_iter([
        ("None".into(), ValueNone.into()),
        (
            "Infinity".into(),
            Scalar {
                dimension: Dimension::zero(),
                value: Float::new(common_data_types::RawFloat::INFINITY).expect("Infinity was NaN"),
            }
            .into(),
        ),
    ]);
    Dictionary::from(types)
}

/// Adds library for type safety.
fn build_types() -> Dictionary {
    let types: HashMap<String, Value> = HashMap::from_iter(
        [
            ("None".into(), ValueType::TypeNone.into()),
            ("Bool".into(), ValueType::Boolean.into()),
            ("SInt".into(), ValueType::SignedInteger.into()),
            ("UInt".into(), ValueType::UnsignedInteger.into()),
            ("ValueType".into(), ValueType::ValueType.into()),
            // TODO we'll need a function to build custom function signature types.
            // ("Function".into(), ValueType::Closure(Arc<ClosureSignature>)),

            // TODO add a function to build custom unit types.
        ]
        .into_iter(),
    );
    Dictionary::from(types)
}

fn build_dimension_types(type_builder: impl Fn(Option<Dimension>) -> ValueType) -> Dictionary {
    let types: HashMap<String, Value> = HashMap::from_iter(
        units::list_named_dimensions()
            .map(|(name, dimension)| (name, Some(dimension)))
            .chain([("Any", Option::None)].into_iter())
            .map(move |(name, dimension)| (name.into(), type_builder(dimension).into())),
    );

    Dictionary::from(types)
}
