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
use common_data_types::{Dimension, Float, RawFloat};
use std::{
    borrow::Cow,
    io::{BufWriter, Write as _},
    sync::Arc,
};

use crate::{
    build_function, build_method,
    execution::{
        errors::{ExpressionResult, GenericFailure, Raise},
        store::IoError,
    },
    values::{
        scalar::{Length, UnwrapNotNan},
        vector::{Length3, Zero3},
        Boolean, BuiltinCallableDatabase, BuiltinFunction, DowncastError, File, IString,
        MissingAttributeError, Object, Scalar, StaticType, StaticTypeName, Style, UnsignedInteger,
        Value, ValueNone, ValueType, Vector3,
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

impl std::hash::Hash for ManifoldMesh3D {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // FIXME this is skipping a lot of information.
        self.0.ps.iter().for_each(|v| {
            v.x.to_le_bytes().hash(state);
            v.y.to_le_bytes().hash(state);
            v.z.to_le_bytes().hash(state);
        });
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
        let input = Self::unpack_arithmetic_input(context, 0.0, rhs)?;
        match input {
            ArethmeticInput::Vector(vector) => {
                let vector = vector.raw_value();
                let manifold = Arc::make_mut(&mut self.0);
                manifold.translate(vector.x, vector.y, vector.z);
                Ok(self.into())
            }
            ArethmeticInput::Manifold(rhs) => {
                let manifold =
                    compute_boolean(&self.0, &rhs.0, OpType::Add).map_err(|message| {
                        GenericFailure(message.into()).to_error(context.stack_trace)
                    })?;
                Ok(Self(Arc::new(manifold)).into())
            }
        }
    }

    fn subtraction(mut self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let input = Self::unpack_arithmetic_input(context, 0.0, rhs)?;
        match input {
            ArethmeticInput::Vector(vector) => {
                let vector = vector.raw_value();
                let manifold = Arc::make_mut(&mut self.0);
                manifold.translate(-vector.x, -vector.y, -vector.z);
                Ok(self.into())
            }
            ArethmeticInput::Manifold(rhs) => {
                let manifold =
                    compute_boolean(&self.0, &rhs.0, OpType::Subtract).map_err(|message| {
                        GenericFailure(message.into()).to_error(context.stack_trace)
                    })?;
                Ok(Self(Arc::new(manifold)).into())
            }
        }
    }

    fn multiply(mut self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let input = rhs.downcast::<Zero3>(context.stack_trace)?;
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

        // To compute xor, get the intersectiona and then subtract it from the union of the two
        // shapes.

        let intersection = compute_boolean(&self.0, &rhs.0, OpType::Intersect)
            .map_err(|message| GenericFailure(message.into()).to_error(context.stack_trace))?;

        let union = compute_boolean(&self.0, &rhs.0, OpType::Add)
            .map_err(|message| GenericFailure(message.into()).to_error(context.stack_trace))?;

        let difference = compute_boolean(&union, &intersection, OpType::Subtract)
            .map_err(|message| GenericFailure(message.into()).to_error(context.stack_trace))?;

        Ok(Self(Arc::new(difference)).into())
    }

    fn bit_and(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_for_binary_op_ref(context.stack_trace)?;
        let manifold = compute_boolean(&self.0, &rhs.0, OpType::Intersect)
            .map_err(|message| GenericFailure(message.into()).to_error(context.stack_trace))?;
        Ok(Self(Arc::new(manifold)).into())
    }

    fn get_attribute(
        &self,
        context: &ExecutionContext,
        attribute: &str,
    ) -> ExpressionResult<Value> {
        match attribute {
            "to_stl" => Ok(BuiltinFunction::new::<methods::ToStl>().into()),
            _ => Err(MissingAttributeError {
                name: attribute.into(),
            }
            .to_error(context.stack_trace)),
        }
    }
}

enum ArethmeticInput {
    Vector(Vector3),
    Manifold(ManifoldMesh3D),
}

impl ManifoldMesh3D {
    fn unpack_arithmetic_input(
        context: &ExecutionContext,
        default: RawFloat,
        input: Value,
    ) -> ExpressionResult<ArethmeticInput> {
        let value = match input {
            Value::Vector2(v) => {
                let raw = v.raw_value();
                Ok(ArethmeticInput::Vector(Vector3::new(
                    context,
                    v.dimension(),
                    [raw.x, raw.y, default],
                )?))
            }
            Value::Vector3(v) => Ok(ArethmeticInput::Vector(v.clone())),
            Value::ManifoldMesh3D(manifold) => Ok(ArethmeticInput::Manifold(manifold)),
            value => Err(DowncastError {
                expected: "Vector2 or Vector3 of lengths, or another ManifoldMesh3D".into(),
                got: value.get_type(context).name(),
            }
            .to_error(context.stack_trace)),
        }?;

        match value {
            // We need to validate the value of the dimension.
            ArethmeticInput::Vector(vector) => {
                if vector.dimension() != Dimension::length() {
                    // Wrong dimension type.
                    Err(DowncastError {
                        expected: "Vector2 or Vector3 of lengths, or another ManifoldMesh3D".into(),
                        got: vector.get_type(context).name(),
                    }
                    .to_error(context.stack_trace))
                } else {
                    Ok(ArethmeticInput::Vector(vector))
                }
            }
            // Everything else can be passed as is.
            value => Ok(value),
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

    pub struct ToStl;
}

fn unpack_radius(
    context: &ExecutionContext,
    radius: Option<Length>,
    diameter: Option<Length>,
) -> ExpressionResult<RawFloat> {
    match (radius, diameter) {
        (Some(radius), Option::None) => Ok(radius.into()),
        (Option::None, Some(diameter)) => {
            let diameter: RawFloat = diameter.into();
            Ok(diameter / 2.0)
        }
        (Some(_), Some(_)) => Err(GenericFailure(
            "You must provide the radius or the diameter, not both".into(),
        )
        .to_error(context.stack_trace)),
        (Option::None, Option::None) => Err(GenericFailure(
            "You must provide the radius or the diameter, neither were provided".into(),
        )
        .to_error(context.stack_trace)),
    }
}

pub fn register_methods_and_functions(database: &mut BuiltinCallableDatabase) {
    build_function!(
        database,
        methods::GenerateCone, "ManifoldMesh3D::cone", (
            context: &ExecutionContext,
            apex: Length3,
            center: Length3,
            radius: Option<Length> = ValueNone.into(),
            diameter: Option<Length> = ValueNone.into(),
            divide: UnsignedInteger) -> ManifoldMesh3D
        {
            let radius = unpack_radius(context, radius, diameter)?;

            let manifold = generate_cone(apex.into(), center.into(), radius, divide.0 as usize)
                .map_err(|error| GenericFailure(error.into()).to_error(context.stack_trace))?;
            Ok(ManifoldMesh3D(Arc::new(manifold)).into())
        }
    );
    build_function!(
        database,
        methods::GenerateCube, "ManifoldMesh3D::cube", (
            context: &ExecutionContext,
            size: Length3
        ) -> ManifoldMesh3D {
            let size: Vector3 = size.into();
            let size = size.raw_value();

            let mut manifold = generate_cube().map_err(|error| GenericFailure(error.into()).to_error(context.stack_trace))?;
            manifold.scale(size.x, size.y, size.z);

            Ok(ManifoldMesh3D(Arc::new(manifold)).into())
        }
    );
    build_function!(
        database,
        methods::GenerateCylinder, "ManifoldMesh3D::cylinder", (
            context: &ExecutionContext,
            radius: Option<Length> = ValueNone.into(),
            diameter: Option<Length> = ValueNone.into(),
            height: Length,
            sectors: UnsignedInteger,
            stacks: UnsignedInteger
        ) -> ManifoldMesh3D {
            let radius = unpack_radius(context, radius, diameter)?;

            let manifold = generate_cylinder(radius, height.into(), sectors.0 as usize, stacks.0 as usize)
                .map_err(|error| GenericFailure(error.into()).to_error(context.stack_trace))?;
            Ok(ManifoldMesh3D(Arc::new(manifold)).into())
        }
    );
    build_function!(
        database,
        methods::GenerateIcosphere, "ManifoldMesh3D::icosphere", (
            context: &ExecutionContext,
            subdivions: UnsignedInteger,
            radius: Option<Length> = ValueNone.into(),
            diameter: Option<Length> = ValueNone.into()
        ) -> ManifoldMesh3D {
            let scale = unpack_radius(context, radius, diameter)?;

            let mut manifold = generate_icosphere(subdivions.0 as u32)
                .map_err(|error| GenericFailure(error.into()).to_error(context.stack_trace))?;
            manifold.scale(scale, scale, scale);

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
            sectors: UnsignedInteger
        ) -> ManifoldMesh3D {
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
            stacks: UnsignedInteger,
            radius: Option<Length> = ValueNone.into(),
            diameter: Option<Length> = ValueNone.into()
        ) -> ManifoldMesh3D {
            let scale = unpack_radius(context, radius, diameter)?;

            let mut manifold = generate_uv_sphere(sectors.0 as usize, stacks.0 as usize)
                .map_err(|error| GenericFailure(error.into()).to_error(context.stack_trace))?;
            manifold.scale(scale, scale, scale);

            Ok(ManifoldMesh3D(Arc::new(manifold)).into())
        }
    );

    build_method!(
        database,
        methods::ToStl, "ManifoldMesh3D::to_stl", (
            context: &ExecutionContext,
            this: ManifoldMesh3D,
            name: IString,
            scale: Length = Scalar {
                dimension: Dimension::length(),
                value: Float::new(1.0/1000.0).expect("Default stl scale was NaN")
            }.into(),
            ascii: Boolean = Boolean(false).into()
        ) -> File {
            if !ascii.0 {
                // Produce a binary STL
                let mut mesh = Vec::new();

                use stl_io::{Triangle, Vertex, write_stl};

                for (normal, halfedge) in this.0.face_normals.iter().zip(this.0.hs.chunks(3)) {
                    let p0 = this.0.ps[halfedge[0].tail];
                    let p1 = this.0.ps[halfedge[1].tail];
                    let p2 = this.0.ps[halfedge[2].tail];

                    let scale = 1.0 / *scale.value;

                    let triangle = Triangle {
                        normal: Vertex::new([(normal.x * scale) as f32, (normal.y * scale) as f32, (normal.z * scale) as f32]),
                        vertices: [Vertex::new([(p0.x * scale) as f32, (p0.y * scale) as f32, (p0.z * scale) as f32]),
                                   Vertex::new([(p1.x * scale) as f32, (p1.y * scale) as f32, (p1.z * scale) as f32]),
                                   Vertex::new([(p2.x * scale) as f32, (p2.y * scale) as f32, (p2.z * scale) as f32])]
                    };

                    mesh.push(triangle);
                }

                // This thing doesn't kick back IO errors, so we'll collect to an infaulable
                // structure and then write that ourselves.
                let mut serialized = Vec::new();
                write_stl(&mut serialized, mesh.iter()).map_err(|_| GenericFailure("Failed to serialize STL file".into()).to_error(context.stack_trace))?;

                let path = context.store.get_or_init_file(context, &(&this, &scale, "ascii"), format!("{}.stl", name.0), |file| {
                    file.write_all(&serialized).map_err(|error| IoError(error).to_error(context.stack_trace))?;

                    Ok(())
                })?;

                Ok(File { path: Arc::new(path) })
            } else {
                let path = context.store.get_or_init_file(context, &(&this, &scale, "binary"), format!("{}.stl", name.0), |file| {
                    let mut file = BufWriter::new(file);
                    let scale = *Float::new(1.0 / *scale.value).unwrap_not_nan(context.stack_trace)?;

                    // file.write_all(&serialized).map_err(|error| IoError(error).to_error(context.stack_trace))?;
                    let mut trampoline = || -> std::io::Result<()> {
                        writeln!(file, "solid {}", name.0)?;

                        for (normal, halfedge) in this.0.face_normals.iter().zip(this.0.hs.chunks(3)) {
                            let p0 = this.0.ps[halfedge[0].tail];
                            let p1 = this.0.ps[halfedge[1].tail];
                            let p2 = this.0.ps[halfedge[2].tail];

                            writeln!(file, "\tfacet normal {} {} {}", normal.x, normal.y, normal.z)?;

                            {
                                writeln!(file, "\t\touter loop")?;

                                {
                                    writeln!(file, "\t\t\tvertex {} {} {}", p0.x * scale, p0.y * scale, p0.z * scale)?;
                                    writeln!(file, "\t\t\tvertex {} {} {}", p1.x * scale, p1.y * scale, p1.z * scale)?;
                                    writeln!(file, "\t\t\tvertex {} {} {}", p2.x * scale, p2.y * scale, p2.z * scale)?;
                                }

                                writeln!(file, "\t\tendloop")?;
                            }

                            writeln!(file, "\tendfacet")?;
                        }

                        writeln!(file, "endsolid {}", name.0)?;

                        Ok(())
                    };

                    trampoline().map_err(|error| IoError(error).to_error(context.stack_trace))?;

                    Ok(())
                })?;

                Ok(File { path: Arc::new(path) })
            }
        }
    );
}
