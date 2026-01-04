use std::collections::HashMap;

use common_data_types::{Dimension, Float};

use crate::execution::values::{
    BuiltinCallableDatabase, Scalar, SignedInteger, UnsignedInteger, ValueNone,
};

use super::values::{Dictionary, Value, ValueType};

/// Builds standard library.
pub fn build_prelude(database: &BuiltinCallableDatabase) -> HashMap<String, Value> {
    let global = HashMap::from([("std".into(), build_std(database).into())]);

    global
}

fn build_std(database: &BuiltinCallableDatabase) -> Dictionary {
    let std = HashMap::from([
        ("types".into(), build_types(database).into()),
        (
            "scalar".into(),
            build_dimension_types(database, ValueType::Scalar).into(),
        ),
        (
            "vector2".into(),
            build_dimension_types(database, ValueType::Vector2).into(),
        ),
        (
            "vector3".into(),
            build_dimension_types(database, ValueType::Vector3).into(),
        ),
        (
            "vector4".into(),
            build_dimension_types(database, ValueType::Vector4).into(),
        ),
        ("consts".into(), build_consts(database).into()),
    ]);
    Dictionary::new(database, std)
}

/// Adds library for constants.
fn build_consts(database: &BuiltinCallableDatabase) -> Dictionary {
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
        ("UIntMax".into(), UnsignedInteger::from(u64::MAX).into()),
        ("UIntMin".into(), UnsignedInteger::from(u64::MIN).into()),
        (
            "UIntBits".into(),
            UnsignedInteger::from(u64::BITS as u64).into(),
        ),
        ("SIntMax".into(), SignedInteger::from(i64::MAX).into()),
        ("SIntMin".into(), SignedInteger::from(i64::MIN).into()),
        (
            "SIntBits".into(),
            UnsignedInteger::from(i64::BITS as u64).into(),
        ),
    ]);
    Dictionary::new(database, types)
}

/// Adds library for type safety.
fn build_types(database: &BuiltinCallableDatabase) -> Dictionary {
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
    Dictionary::new(database, types)
}

fn build_dimension_types(
    database: &BuiltinCallableDatabase,
    type_builder: impl Fn(Option<Dimension>) -> ValueType,
) -> Dictionary {
    let types: HashMap<String, Value> = HashMap::from_iter(
        units::list_named_dimensions()
            .map(|(name, dimension)| (name, Some(dimension)))
            .chain([("Any", Option::None)].into_iter())
            .map(move |(name, dimension)| (name.into(), type_builder(dimension).into())),
    );

    Dictionary::new(database, types)
}
