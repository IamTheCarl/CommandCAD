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

use std::{
    collections::HashMap,
    path::Path,
    sync::{atomic::AtomicBool, Mutex},
};

use common_data_types::{Dimension, Float};
use imstr::ImString;
use nalgebra::{Matrix3, Matrix4};

use crate::{
    execution::{
        functions::Import,
        logging::StackTrace,
        stack::StackScope,
        store::{DummyStore, Store},
        values::{
            integer::functions::{RangeSInt, RangeUInt},
            BuiltinCallableDatabase, Scalar, SignedInteger, UnsignedInteger, ValueNone,
        },
        ExecutionContext,
    },
    values::{BuiltinFunction, Transform2d, Transform3d},
};

use super::values::{Dictionary, Value, ValueType};

/// Builds standard library.
pub fn build_prelude(database: &BuiltinCallableDatabase) -> HashMap<ImString, Value> {
    // Build an incomplete context for bootstrapping.
    let prelude = HashMap::new();

    // We don't actually use the store for anything during prelude bringup. Use a dummy store for
    // this.
    let store = Store::DummyStore(DummyStore);
    let file_cache = Mutex::new(HashMap::new());

    let working_directory = Path::new(".");
    let shutdown_signal = AtomicBool::new(false);

    let context = ExecutionContext {
        shutdown_singal: &shutdown_signal,
        log: &Mutex::new(Vec::new()),
        stack_trace: &StackTrace::bootstrap(),
        stack: &StackScope::top(&prelude),
        database,
        store: &store,
        file_cache: &file_cache,
        working_directory,
        import_limit: 100,
    };

    HashMap::from([("std".into(), build_std(&context).into())])
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
        ("mesh".into(), build_mesh_3d(context).into()),
        ("line_string".into(), build_line_string(context).into()),
        ("polygon".into(), build_polygon(context).into()),
        ("polygon_set".into(), build_polygon_set(context).into()),
        ("import".into(), BuiltinFunction::new::<Import>().into()),
        ("range".into(), build_range(context).into()),
    ]);
    Dictionary::new(context, std)
}

/// Adds library for range iteration.
fn build_range(context: &ExecutionContext) -> Dictionary {
    let range: HashMap<ImString, Value> = HashMap::from_iter([
        ("UInt".into(), BuiltinFunction::new::<RangeUInt>().into()),
        ("SInt".into(), BuiltinFunction::new::<RangeSInt>().into()),
    ]);
    Dictionary::new(context, range)
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
        (
            "Transform2d".into(),
            Transform2d::new(Matrix3::identity()).into(),
        ),
        (
            "Transform3d".into(),
            Transform3d::new(Matrix4::identity()).into(),
        ),
    ]);
    Dictionary::new(context, types)
}

/// Adds library for type safety.
fn build_types(context: &ExecutionContext) -> Dictionary {
    let types: HashMap<ImString, Value> = HashMap::from_iter([
        ("None".into(), ValueType::TypeNone.into()),
        ("Any".into(), ValueType::Any.into()),
        ("Bool".into(), ValueType::Boolean.into()),
        ("SInt".into(), ValueType::SignedInteger.into()),
        ("UInt".into(), ValueType::UnsignedInteger.into()),
        ("String".into(), ValueType::String.into()),
        ("ValueType".into(), ValueType::ValueType.into()),
        ("ManifoldMesh".into(), ValueType::ManifoldMesh3D.into()),
        ("Transform2d".into(), ValueType::Transform2d.into()),
        ("Transform3d".into(), ValueType::Transform3d.into()),
        ("Transform3d".into(), ValueType::Transform3d.into()),
        ("Iterator".into(), ValueType::Iterator.into()),
        (
            "List".into(),
            BuiltinFunction::new::<crate::values::list::methods_and_functions::BuildType>().into(),
        ),
        ("File".into(), ValueType::File.into()),
        // TODO we'll need a function to build custom function signature types.
        // ("Function".into(), ValueType::Closure(Arc<ClosureSignature>)),

        // TODO add a function to build custom scalar and vector unit types.
    ]);
    Dictionary::new(context, types)
}

fn build_dimension_types(
    context: &ExecutionContext,
    type_builder: impl Fn(Option<Dimension>) -> ValueType,
) -> Dictionary {
    let types: HashMap<ImString, Value> = HashMap::from_iter(
        units::list_named_dimensions()
            .map(|(name, dimension)| (name, Some(dimension)))
            .chain([("Any", None)])
            .map(move |(name, dimension)| (name.into(), type_builder(dimension).into())),
    );

    Dictionary::new(context, types)
}

fn build_line_string(context: &ExecutionContext) -> Dictionary {
    use crate::values::polygon::methods_and_functions::line_string::*;

    let types: HashMap<ImString, Value> = HashMap::from_iter([(
        "from_points".into(),
        BuiltinFunction::new::<FromPoints>().into(),
    )]);
    Dictionary::new(context, types)
}

fn build_polygon(context: &ExecutionContext) -> Dictionary {
    use crate::values::polygon::methods_and_functions::polygon::*;

    let types: HashMap<ImString, Value> = HashMap::from_iter([
        (
            "from_points".into(),
            BuiltinFunction::new::<FromPoints>().into(),
        ),
        (
            "from_line_strings".into(),
            BuiltinFunction::new::<FromLineStrings>().into(),
        ),
        ("circle".into(), BuiltinFunction::new::<Circle>().into()),
        ("box".into(), BuiltinFunction::new::<BuildBox>().into()),
        (
            "box_from_points".into(),
            BuiltinFunction::new::<BuildBoxFromPoints>().into(),
        ),
    ]);
    Dictionary::new(context, types)
}

fn build_polygon_set(context: &ExecutionContext) -> Dictionary {
    use crate::values::polygon::methods_and_functions::polygon_set::*;
    let types: HashMap<ImString, Value> = HashMap::from_iter([(
        "from_polys".into(),
        BuiltinFunction::new::<FromPolys>().into(),
    )]);
    Dictionary::new(context, types)
}

fn build_mesh_3d(context: &ExecutionContext) -> Dictionary {
    use crate::values::manifold_mesh::methods::*;

    let types: HashMap<ImString, Value> = HashMap::from_iter([
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
    ]);
    Dictionary::new(context, types)
}
