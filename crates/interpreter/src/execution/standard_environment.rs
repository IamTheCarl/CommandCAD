use std::collections::HashMap;

use common_data_types::Dimension;

use super::{
    heap::Heap,
    values::{Dictionary, Value, ValueType},
};

/// Builds standard library.
pub fn build_prelude(heap: &mut Heap) -> HashMap<String, Value> {
    let global = HashMap::from([("std".into(), build_std(heap).into())]);

    global
}

fn build_std(heap: &mut Heap) -> Dictionary {
    let std = HashMap::from([("types".into(), build_types(heap).into())]);
    Dictionary::from_hashmap(heap, std)
}

/// Adds library for type safety.
fn build_types(heap: &mut Heap) -> Dictionary {
    let types = HashMap::from_iter(
        [
            ("Void".into(), ValueType::Void.into()),
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
    Dictionary::from_hashmap(heap, types)
}

fn build_dimension_types() -> impl Iterator<Item = (String, Value)> {
    units::list_named_dimensions()
        .map(|(name, dimension)| (name.into(), ValueType::Scalar(dimension).into()))
}
