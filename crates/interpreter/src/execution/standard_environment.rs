use std::collections::HashMap;

use common_data_types::Dimension;

use crate::execution::values::ValueNone;

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
            build_dimension_types(|dimension| ValueType::Scalar(Some(dimension))).into(),
        ),
        (
            "vector2".into(),
            build_dimension_types(|dimension| ValueType::Vector2(Some(dimension))).into(),
        ),
        (
            "vector3".into(),
            build_dimension_types(|dimension| ValueType::Vector3(Some(dimension))).into(),
        ),
        (
            "vector4".into(),
            build_dimension_types(|dimension| ValueType::Vector4(Some(dimension))).into(),
        ),
        ("consts".into(), build_consts().into()),
    ]);
    Dictionary::from(std)
}

/// Adds library for constants.
fn build_consts() -> Dictionary {
    let types: HashMap<String, Value> = HashMap::from_iter([("None".into(), ValueNone.into())]);
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
            ("Scaler".into(), ValueType::Scalar(None).into()),
            ("Vector2".into(), ValueType::Vector2(None).into()),
            ("Vector3".into(), ValueType::Vector3(None).into()),
            ("Vector4".into(), ValueType::Vector4(None).into()),
            // TODO we'll need a function to build custom function signature types.
            // ("Function".into(), ValueType::Closure(Arc<ClosureSignature>)),

            // TODO add a function to build custom unit types.
        ]
        .into_iter(),
    );
    Dictionary::from(types)
}

fn build_dimension_types(type_builder: impl Fn(Dimension) -> ValueType) -> Dictionary {
    let types: HashMap<String, Value> = HashMap::from_iter(
        units::list_named_dimensions()
            .map(move |(name, dimension)| (name.into(), type_builder(dimension).into())),
    );

    Dictionary::from(types)
}
