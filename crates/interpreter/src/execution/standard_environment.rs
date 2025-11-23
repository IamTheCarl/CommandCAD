use std::collections::HashMap;

use crate::execution::values::ValueNone;

use super::values::{Dictionary, StoredValue, Value, ValueType};

/// Builds standard library.
pub fn build_prelude() -> HashMap<String, StoredValue> {
    let global = HashMap::from([("std".into(), build_std().into())]);

    global
}

fn build_std() -> Dictionary {
    let std = HashMap::from([
        ("types".into(), build_types().into()),
        ("consts".into(), build_consts().into()),
    ]);
    Dictionary::from(std)
}

/// Adds library for constants.
fn build_consts() -> Dictionary {
    let types: HashMap<String, StoredValue> = HashMap::from_iter([(
        "None".into(),
        StoredValue::Value(Value::ValueNone(ValueNone)),
    )]);
    Dictionary::from(types)
}

/// Adds library for type safety.
fn build_types() -> Dictionary {
    let types: HashMap<String, StoredValue> = HashMap::from_iter(
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
        .into_iter()
        .chain(build_dimension_types()),
    );
    Dictionary::from(types)
}

fn build_dimension_types() -> impl Iterator<Item = (String, StoredValue)> {
    units::list_named_dimensions()
        .map(|(name, dimension)| (name.into(), ValueType::Scalar(dimension).into()))
}
