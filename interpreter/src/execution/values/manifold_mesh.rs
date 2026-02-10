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

use boolmesh::prelude::*;
use common_data_types::{Dimension, RawFloat};
use std::{borrow::Cow, sync::Arc};

use crate::{
    build_function,
    execution::errors::{ExpressionResult, GenericFailure, Raise},
    values::{
        scalar::Length, vector::Length3, BuiltinCallableDatabase, DowncastForBinaryOpError, Object,
        StaticType, StaticTypeName, Style, UnsignedInteger, Value, ValueType, Vector3,
    },
    ExecutionContext,
};

#[derive(Debug, Clone)]
pub struct ManifoldMesh3D(Arc<Manifold>);

impl Eq for ManifoldMesh3D {}

impl PartialEq for ManifoldMesh3D {
    fn eq(&self, other: &Self) -> bool {
        // FIXME this is skipping a lot of information.
        self.0.ps == other.0.ps
    }
}

impl Object for ManifoldMesh3D {
    fn get_type(&self, _context: &ExecutionContext) -> ValueType {
        ValueType::ManifoldMesh3D
    }

    fn format(
        &self,
        _context: &ExecutionContext,
        f: &mut dyn std::fmt::Write,
        _style: Style,
        _precision: Option<u8>,
    ) -> std::fmt::Result {
        write!(
            f,
            "Manifold Mesh with {} verticies, {} faces, and {} half-edges",
            self.0.nv, self.0.nf, self.0.nh
        )
    }

    fn addition(mut self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let input = Self::unpack_transform_input(context, 0.0, rhs)?;
        let vector = input.raw_value();
        let manifold = Arc::make_mut(&mut self.0);
        manifold.translate(vector.x, vector.y, vector.z);
        Ok(self.into())
    }

    fn subtraction(mut self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let input = Self::unpack_transform_input(context, 0.0, rhs)?;
        let vector = input.raw_value();
        let manifold = Arc::make_mut(&mut self.0);
        manifold.translate(-vector.x, -vector.y, -vector.z);
        Ok(self.into())
    }

    fn multiply(mut self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let input = Self::unpack_transform_input(context, 1.0, rhs)?;
        let vector = input.raw_value();
        let manifold = Arc::make_mut(&mut self.0);
        manifold.scale(vector.x, vector.y, vector.z);
        Ok(self.into())
    }

    fn bit_or(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_for_binary_op_ref(context.stack_trace)?;
        let manifold = compute_boolean(&self.0, &rhs.0, OpType::Add)
            .map_err(|message| GenericFailure(message.into()).to_error(context.stack_trace))?;
        Ok(Self(Arc::new(manifold)).into())
    }

    fn bit_xor(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_for_binary_op_ref(context.stack_trace)?;
        let manifold = compute_boolean(&self.0, &rhs.0, OpType::Subtract)
            .map_err(|message| GenericFailure(message.into()).to_error(context.stack_trace))?;
        Ok(Self(Arc::new(manifold)).into())
    }

    fn bit_and(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_for_binary_op_ref(context.stack_trace)?;
        let manifold = compute_boolean(&self.0, &rhs.0, OpType::Intersect)
            .map_err(|message| GenericFailure(message.into()).to_error(context.stack_trace))?;
        Ok(Self(Arc::new(manifold)).into())
    }
}

impl ManifoldMesh3D {
    fn unpack_transform_input(
        context: &ExecutionContext,
        default: RawFloat,
        input: Value,
    ) -> ExpressionResult<Vector3> {
        let build_type_error = || {
            DowncastForBinaryOpError {
                expected: "Vector2 or Vector3 of lengths".into(),
                got: input.get_type(context).name(),
            }
            .to_error(context.stack_trace)
        };

        let vector = match &input {
            Value::Vector2(v) => {
                let raw = v.raw_value();
                Vector3::new(context, v.dimension(), [raw.x, raw.y, default])
            }
            Value::Vector3(v) => Ok(v.clone()),
            _ => Err(build_type_error()),
        }?;

        if vector.dimension() != Dimension::length() {
            // Wrong dimension type.
            Err(build_type_error())
        } else {
            Ok(vector)
        }
    }
}

impl StaticTypeName for ManifoldMesh3D {
    fn static_type_name() -> Cow<'static, str> {
        "ManifoldMesh3D".into()
    }
}

impl StaticType for ManifoldMesh3D {
    fn static_type() -> ValueType {
        ValueType::ManifoldMesh3D
    }
}

pub mod methods {
    pub struct GenerateCone;
    pub struct GenerateCube;
    pub struct GenerateCylinder;
    pub struct GenerateIcosphere;
    pub struct GenerateTorus;
    pub struct GenerateUvSphere;
}

pub fn register_methods_and_functions(database: &mut BuiltinCallableDatabase) {
    build_function!(
        database,
        methods::GenerateCone, "ManifoldMesh3D::cone", (
            context: &ExecutionContext,
            apex: Length3,
            center: Length3,
            radius: Length,
            divide: UnsignedInteger) -> ManifoldMesh3D
        {
            let manifold = generate_cone(apex.into(), center.into(), radius.into(), divide.0 as usize)
                .map_err(|error| GenericFailure(error.into()).to_error(context.stack_trace))?;
            Ok(ManifoldMesh3D(Arc::new(manifold)).into())
        }
    );
    build_function!(
        database,
        methods::GenerateCube, "ManifoldMesh3D::cube", (context: &ExecutionContext) -> ManifoldMesh3D
        {
            let manifold = generate_cube().map_err(|error| GenericFailure(error.into()).to_error(context.stack_trace))?;
            Ok(ManifoldMesh3D(Arc::new(manifold)).into())
        }
    );
    build_function!(
        database,
        methods::GenerateCylinder, "ManifoldMesh3D::cylinder", (
            context: &ExecutionContext,
            radius: Length,
            height: Length,
            sectors: UnsignedInteger,
            stacks: UnsignedInteger
        ) -> ManifoldMesh3D
        {
            let manifold = generate_cylinder(radius.into(), height.into(), sectors.0 as usize, stacks.0 as usize)
                .map_err(|error| GenericFailure(error.into()).to_error(context.stack_trace))?;
            Ok(ManifoldMesh3D(Arc::new(manifold)).into())
        }
    );
    build_function!(
        database,
        methods::GenerateIcosphere, "ManifoldMesh3D::icosphere", (
            context: &ExecutionContext,
            subdivions: UnsignedInteger) -> ManifoldMesh3D
        {
            let manifold = generate_icosphere(subdivions.0 as u32)
                .map_err(|error| GenericFailure(error.into()).to_error(context.stack_trace))?;
            Ok(ManifoldMesh3D(Arc::new(manifold)).into())
        }
    );
    build_function!(
        database,
        methods::GenerateTorus, "ManifoldMesh3D::torus", (
            context: &ExecutionContext,
            major_radius: Length,
            minor_raidus: Length,
            rings: UnsignedInteger,
            sectors: UnsignedInteger) -> ManifoldMesh3D
        {
            let manifold = generate_torus(major_radius.into(), minor_raidus.into(), rings.0 as usize, sectors.0 as usize)
                .map_err(|error| GenericFailure(error.into()).to_error(context.stack_trace))?;
            Ok(ManifoldMesh3D(Arc::new(manifold)).into())
        }
    );
    build_function!(
        database,
        methods::GenerateUvSphere, "ManifoldMesh3D::uv_sphere", (
            context: &ExecutionContext,
            sectors: UnsignedInteger,
            stacks: UnsignedInteger) -> ManifoldMesh3D
        {
            let manifold = generate_uv_sphere(sectors.0 as usize, stacks.0 as usize)
                .map_err(|error| GenericFailure(error.into()).to_error(context.stack_trace))?;
            Ok(ManifoldMesh3D(Arc::new(manifold)).into())
        }
    );
}
