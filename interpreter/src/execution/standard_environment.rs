/*
 * Copyright 2026 James Carl
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

use std::{collections::HashMap, path::Path, sync::Mutex};

use common_data_types::{Dimension, Float};
use imstr::ImString;
use tempfile::TempDir;

use crate::{
    execution::{
        functions::Import,
        logging::StackTrace,
        stack::StackScope,
        store::Store,
        values::{BuiltinCallableDatabase, Scalar, SignedInteger, UnsignedInteger, ValueNone},
        ExecutionContext,
    },
    values::BuiltinFunction,
};

use super::values::{Dictionary, Value, ValueType};

/// Builds standard library.
pub fn build_prelude(
    database: &BuiltinCallableDatabase,
) -> std::io::Result<HashMap<ImString, Value>> {
    // Build an incomplete context for bootstrapping.
    let prelude = HashMap::new();
    let store_directory = TempDir::new()?;
    let store = Store::new(store_directory.path());
    let file_cache = Mutex::new(HashMap::new());

    let working_directory = Path::new(".");

    let context = ExecutionContext {
        log: &Mutex::new(Vec::new()),
        stack_trace: &StackTrace::bootstrap(),
        stack: &StackScope::top(&prelude),
        database: &database,
        store: &store,
        file_cache: &file_cache,
        working_directory: &working_directory,
        import_limit: 100,
    };

    let global = HashMap::from([("std".into(), build_std(&context).into())]);

    Ok(global)
}

fn build_std(context: &ExecutionContext) -> Dictionary {
    let std = HashMap::from([
        ("types".into(), build_types(context).into()),
        (
            "scalar".into(),
            build_dimension_types(context, ValueType::Scalar).into(),
        ),
        (
            "vector2".into(),
            build_dimension_types(context, ValueType::Vector2).into(),
        ),
        (
            "vector3".into(),
            build_dimension_types(context, ValueType::Vector3).into(),
        ),
        (
            "vector4".into(),
            build_dimension_types(context, ValueType::Vector4).into(),
        ),
        ("consts".into(), build_consts(context).into()),
        ("mesh3d".into(), build_mesh_3d(context).into()),
        ("import".into(), BuiltinFunction::new::<Import>().into()),
    ]);
    Dictionary::new(context, std)
}

/// Adds library for constants.
fn build_consts(context: &ExecutionContext) -> Dictionary {
    let types: HashMap<ImString, Value> = HashMap::from_iter([
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
    Dictionary::new(context, types)
}

/// Adds library for type safety.
fn build_types(context: &ExecutionContext) -> Dictionary {
    let types: HashMap<ImString, Value> = HashMap::from_iter(
        [
            ("None".into(), ValueType::TypeNone.into()),
            ("Any".into(), ValueType::Any.into()),
            ("Bool".into(), ValueType::Boolean.into()),
            ("SInt".into(), ValueType::SignedInteger.into()),
            ("UInt".into(), ValueType::UnsignedInteger.into()),
            ("String".into(), ValueType::String.into()),
            ("ValueType".into(), ValueType::ValueType.into()),
            ("ManifoldMesh".into(), ValueType::ManifoldMesh3D.into()),
            // TODO we need File types.
            // TODO we'll need a function to build custom function signature types.
            // ("Function".into(), ValueType::Closure(Arc<ClosureSignature>)),

            // TODO add a function to build custom unit types.
        ]
        .into_iter(),
    );
    Dictionary::new(context, types)
}

fn build_dimension_types(
    context: &ExecutionContext,
    type_builder: impl Fn(Option<Dimension>) -> ValueType,
) -> Dictionary {
    let types: HashMap<ImString, Value> = HashMap::from_iter(
        units::list_named_dimensions()
            .map(|(name, dimension)| (name, Some(dimension)))
            .chain([("Any", Option::None)].into_iter())
            .map(move |(name, dimension)| (name.into(), type_builder(dimension).into())),
    );

    Dictionary::new(context, types)
}

fn build_mesh_3d(context: &ExecutionContext) -> Dictionary {
    use crate::values::manifold_mesh::methods::*;

    let types: HashMap<ImString, Value> = HashMap::from_iter(
        [
            ("cone".into(), BuiltinFunction::new::<GenerateCone>().into()),
            ("cube".into(), BuiltinFunction::new::<GenerateCube>().into()),
            (
                "cylinder".into(),
                BuiltinFunction::new::<GenerateCylinder>().into(),
            ),
            (
                "icosphere".into(),
                BuiltinFunction::new::<GenerateIcosphere>().into(),
            ),
            (
                "torus".into(),
                BuiltinFunction::new::<GenerateTorus>().into(),
            ),
            (
                "uv_sphere".into(),
                BuiltinFunction::new::<GenerateUvSphere>().into(),
            ),
        ]
        .into_iter(),
    );
    Dictionary::new(context, types)
}
