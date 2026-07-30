use std::sync::Arc;

use common_data_types::Dimension;
use fidget::context::Context;
use fidget::context::Tree;
use fidget::mesh::{Octree, Settings};
use fidget::render::CancelToken;
use fidget::render::ThreadPool;
use fidget::shape::Shape;

use crate::execution::errors::{ExecutionResult, Raise, StrError};
use crate::execution::values::{
    closure::{BuiltinCallableDatabase, BuiltinFunction},
    vector::{Length3, Zero3},
    DowncastError, Length, Object, Scalar, StaticType, StaticTypeName, Style, Transform3d,
    UnsignedInteger, Value, ValueNone, ValueType, Vector3,
};
use crate::execution::ExecutionContext;

enum ArithmeticInput {
    Vector(Vector3),
    Surface(Surface3D),
}

fn unpack_arithmetic_input(
    context: &ExecutionContext,
    input: Value,
) -> Result<ArithmeticInput, crate::execution::errors::Error> {
    let value = match input {
        Value::Vector2(v) => {
            let raw = v.raw_value();
            let vec = Vector3::new_raw(context, v.dimension(), [raw.x, raw.y, 0.0].into())?;
            if vec.dimension() != Dimension::length() {
                return Err(DowncastError {
                    expected: "Vector2 or Vector3 of lengths, or another implicit surface".into(),
                    got: vec.get_type(context).name(),
                }
                .to_error(context));
            }
            ArithmeticInput::Vector(vec)
        }
        Value::Vector3(v) => {
            if v.dimension() != Dimension::length() {
                return Err(DowncastError {
                    expected: "Vector2 or Vector3 of lengths, or another implicit surface".into(),
                    got: v.get_type(context).name(),
                }
                .to_error(context));
            }
            ArithmeticInput::Vector(v)
        }
        Value::Surface3D(s) => ArithmeticInput::Surface(s),
        value => {
            return Err(DowncastError {
                expected: "Vector2 or Vector3 of lengths, or another implicit surface".into(),
                got: value.get_type(context).name(),
            }
            .to_error(context));
        }
    };
    Ok(value)
}

fn unpack_radius(
    context: &ExecutionContext,
    radius: Option<Length>,
    diameter: Option<Length>,
) -> Result<common_data_types::RawFloat, crate::execution::errors::Error> {
    match (radius, diameter) {
        (Some(r), None) => Ok(*r.value),
        (None, Some(d)) => Ok(*d.value / 2.0),
        (Some(_), Some(_)) => Err(StrError("Both radius and diameter provided").to_error(context)),
        (None, None) => {
            Err(StrError("Either radius or diameter must be provided").to_error(context))
        }
    }
}

use super::MeshSettings;
use super::MeshingError;
use super::Surface2D;

use super::super::manifold_mesh::ManifoldMesh3D;
use super::super::polygon::PolygonSet;

/// 3D implicit surface wrapping a Fidget Shape.
///
/// Provides `to_manifold()` for mesh generation via Manifold Dual Contouring.
#[derive(Clone, PartialEq, Eq)]
pub struct Surface3D {
    tree: fidget::context::Tree,
    settings: MeshSettings,
}

impl std::fmt::Debug for Surface3D {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Surface3D")
            .field("settings", &self.settings)
            .finish()
    }
}

impl Surface3D {
    pub fn new(tree: fidget::context::Tree) -> Self {
        Self {
            tree,
            settings: MeshSettings::default(),
        }
    }

    /// Create a copy of this surface with a custom meshing depth.
    pub fn with_depth(mut self, depth: u8) -> Self {
        self.settings.depth = depth;
        self
    }

    /// Returns a reference to the underlying fidget Tree expression.
    pub fn tree(&self) -> &fidget::context::Tree {
        &self.tree
    }

    /// Estimate the bounding box by sampling SDF along axes.
    /// Returns (min_x, min_y, min_z, max_x, max_y, max_z).
    ///
    /// Handles shapes that don't contain the origin by scanning for inside
    /// points before binary-searching the boundary.
    pub fn bounding_box_estimate(&self) -> (f64, f64, f64, f64, f64, f64) {
        let mut ctx = Context::new();
        let node = ctx.import(&self.tree);

        let sample = |x: f64, y: f64, z: f64| -> f64 {
            ctx.eval_xyz(node, x, y, z).unwrap_or(f64::INFINITY)
        };

        // Search along one axis for the positive boundary.
        // Scans at exponentially-spaced distances to find inside points,
        // then binary-searches from the furthest inside point outward.
        let search_positive =
            |fn_x: fn(f64) -> f64, fn_y: fn(f64) -> f64, fn_z: fn(f64) -> f64| -> f64 {
                // Exponential scan: find all inside points up to 100 units
                let mut max_inside = None;
                let mut d = 0.001_f64;
                while d <= 100.0 {
                    let x = fn_x(d);
                    let y = fn_y(d);
                    let z = fn_z(d);
                    if sample(x, y, z) < 0.0 {
                        max_inside = Some(d);
                    }
                    d *= 2.0;
                }

                match max_inside {
                    Some(max_d) => {
                        // Binary search between max_d and next power of 2
                        let mut lo = max_d;
                        let mut hi = (max_d * 2.0).min(100.0);
                        // Make sure hi is actually outside
                        while hi < 100.0 && sample(fn_x(hi), fn_y(hi), fn_z(hi)) < 0.0 {
                            lo = hi;
                            hi = (hi * 2.0).min(100.0);
                        }
                        for _ in 0..30 {
                            let mid = (lo + hi) / 2.0;
                            if sample(fn_x(mid), fn_y(mid), fn_z(mid)) < 0.0 {
                                lo = mid;
                            } else {
                                hi = mid;
                            }
                        }
                        hi
                    }
                    None => 10.0, // fallback: origin outside on this axis
                }
            };

        let x_max = search_positive(|v| v, |_| 0.0, |_| 0.0);
        let x_min = -search_positive(|v| -v, |_| 0.0, |_| 0.0);
        let y_max = search_positive(|_| 0.0, |v| v, |_| 0.0);
        let y_min = -search_positive(|_| 0.0, |v| -v, |_| 0.0);
        let z_max = search_positive(|_| 0.0, |_| 0.0, |v| v);
        let z_min = -search_positive(|_| 0.0, |_| 0.0, |v| -v);

        // Ensure minimum extent to avoid degenerate bounding boxes
        let min_extent = 0.01_f64;
        let x_max = x_max.max(x_min + min_extent);
        let y_max = y_max.max(y_min + min_extent);
        let z_max = z_max.max(z_min + min_extent);

        (x_min, y_min, z_min, x_max, y_max, z_max)
    }

    /// Generate a 3D manifold mesh from the implicit surface using Manifold Dual Contouring.
    ///
    /// Returns `ManifoldMesh3D` (wrapping `Arc<Manifold>` from boolmesh).
    pub fn to_manifold(&self) -> Result<ManifoldMesh3D, MeshingError> {
        // Compute bounding box and scale factors to fit in fidget's [-1, 1]³ model space
        let (min_x, min_y, min_z, max_x, max_y, max_z): (f64, f64, f64, f64, f64, f64) = self
            .settings
            .bounding_box
            .unwrap_or((-10.0, -10.0, -10.0, 10.0, 10.0, 10.0));
        let cx = (min_x + max_x) / 2.0;
        let cy = (min_y + max_y) / 2.0;
        let cz = (min_z + max_z) / 2.0;
        // Scale factor: world coordinate = model_coordinate * half_size + center
        let half_x = if (max_x - min_x).abs() > 1e-10 {
            (max_x - min_x) / 2.0
        } else {
            1.0
        };
        let half_y = if (max_y - min_y).abs() > 1e-10 {
            (max_y - min_y) / 2.0
        } else {
            1.0
        };
        let half_z = if (max_z - min_z).abs() > 1e-10 {
            (max_z - min_z) / 2.0
        } else {
            1.0
        };

        // Scale the tree so it fits in fidget's [-1, 1]³ model space
        // world_coord = model_coord * half_size + center
        let scaled_tree = self.tree.remap_xyz(
            Tree::x() * Tree::constant(half_x) + Tree::constant(cx),
            Tree::y() * Tree::constant(half_y) + Tree::constant(cy),
            Tree::z() * Tree::constant(half_z) + Tree::constant(cz),
        );

        let mut ctx = Context::new();
        let node = ctx.import(&scaled_tree);
        let shape: Shape<fidget::jit::JitFunction> = Shape::new(&ctx, node)
            .map_err(|e| MeshingError(format!("Failed to create shape: {e}")))?;

        let settings = Settings {
            depth: self.settings.depth,
            world_to_model: nalgebra::Matrix4::identity(),
            threads: Some(&ThreadPool::Global),
            cancel: CancelToken::new(),
        };

        let octree = Octree::build(&shape, &settings)
            .ok_or_else(|| MeshingError("Octree build cancelled or failed".into()))?;
        let mesh = octree.walk_dual();

        // Transform vertices back to world space
        let model_to_world = nalgebra::Matrix4::<f64>::new(
            half_x, 0.0, 0.0, cx, 0.0, half_y, 0.0, cy, 0.0, 0.0, half_z, cz, 0.0, 0.0, 0.0, 1.0,
        );
        let manifold = convert_fidget_mesh_to_manifold(mesh, &model_to_world)?;
        Ok(ManifoldMesh3D(Arc::new(manifold)))
    }

    /// Slice the 3D implicit surface at a constant x-coordinate, returning a 2D polygon set.
    pub fn slice_x(&self, x: f64) -> Result<PolygonSet, MeshingError> {
        let sliced_tree = self.tree.remap_xyz(Tree::constant(x), Tree::y(), Tree::z());
        let surface_2d = Surface2D::with_settings(sliced_tree, self.settings.clone());
        surface_2d.to_polygon()
    }

    /// Slice the 3D implicit surface at a constant y-coordinate, returning a 2D polygon set.
    pub fn slice_y(&self, y: f64) -> Result<PolygonSet, MeshingError> {
        let sliced_tree = self.tree.remap_xyz(Tree::x(), Tree::constant(y), Tree::z());
        let surface_2d = Surface2D::with_settings(sliced_tree, self.settings.clone());
        surface_2d.to_polygon()
    }

    /// Slice the 3D implicit surface at a constant z-coordinate, returning a 2D polygon set.
    pub fn slice_z(&self, z: f64) -> Result<PolygonSet, MeshingError> {
        let sliced_tree = self.tree.remap_xyz(Tree::x(), Tree::y(), Tree::constant(z));
        let surface_2d = Surface2D::with_settings(sliced_tree, self.settings.clone());
        surface_2d.to_polygon()
    }

    /// Boolean union: points inside either surface.
    /// SDF: min(self, other)
    pub fn union(&self, other: &Surface3D) -> Self {
        Self::new(self.tree.clone().min(other.tree.clone()))
    }

    /// Boolean intersection: points inside both surfaces.
    /// SDF: max(self, other)
    pub fn intersection(&self, other: &Surface3D) -> Self {
        Self::new(self.tree.clone().max(other.tree.clone()))
    }

    /// Boolean difference: points in self but not in other.
    /// SDF: max(self, -other)
    pub fn difference(&self, other: &Surface3D) -> Self {
        Self::new(self.tree.clone().max(-other.tree.clone()))
    }

    /// Boolean symmetric difference (XOR): points in exactly one surface.
    /// SDF: min(max(self, other), 0) + min(-self, -other)
    /// Approximated as: max(min(self, other), min(-self, -other))
    pub fn symmetric_difference(&self, other: &Surface3D) -> Self {
        let a = &self.tree;
        let b = &other.tree;
        // Points in A\B or B\A but not both
        let a_minus_b = a.clone().max(-b.clone());
        let b_minus_a = b.clone().max(-a.clone());
        Self::new(a_minus_b.min(b_minus_a))
    }

    /// Apply an affine transform to the implicit surface.
    /// The transform is applied by remapping SDF coordinates: sdf'(p) = sdf(T^-1 * p).
    pub fn transform(&self, t: &nalgebra::Matrix4<f64>) -> Self {
        let inv = t
            .try_inverse()
            .expect("Transform matrix must be invertible");
        // Express new coordinates as linear combinations of original x, y, z using inverse matrix.
        // new_coord = inv[row][0]*x + inv[row][1]*y + inv[row][2]*z + inv[row][3]
        let (x, y, z) = (Tree::x(), Tree::y(), Tree::z());
        let new_x = x.clone() * Tree::constant(inv[(0, 0)])
            + y.clone() * Tree::constant(inv[(0, 1)])
            + z.clone() * Tree::constant(inv[(0, 2)])
            + Tree::constant(inv[(0, 3)]);
        let new_y = x.clone() * Tree::constant(inv[(1, 0)])
            + y.clone() * Tree::constant(inv[(1, 1)])
            + z.clone() * Tree::constant(inv[(1, 2)])
            + Tree::constant(inv[(1, 3)]);
        let new_z = x * Tree::constant(inv[(2, 0)])
            + y * Tree::constant(inv[(2, 1)])
            + z * Tree::constant(inv[(2, 2)])
            + Tree::constant(inv[(2, 3)]);
        Self::new(self.tree.remap_xyz(new_x, new_y, new_z))
    }
}

/// Converts a Fidget mesh (vertices + triangles) to a boolmesh Manifold.
/// Transforms vertices from model space back to world space using model_to_world matrix.
fn convert_fidget_mesh_to_manifold(
    mesh: fidget::mesh::Mesh,
    model_to_world: &nalgebra::Matrix4<f64>,
) -> Result<boolmesh::prelude::Manifold, MeshingError> {
    use boolmesh::prelude::*;

    let positions: Vec<f64> = mesh
        .vertices
        .iter()
        .flat_map(|v| {
            // Transform vertex from model space to world space
            let x = v.x as f64;
            let y = v.y as f64;
            let z = v.z as f64;
            let wx = model_to_world[(0, 0)] * x
                + model_to_world[(0, 1)] * y
                + model_to_world[(0, 2)] * z
                + model_to_world[(0, 3)];
            let wy = model_to_world[(1, 0)] * x
                + model_to_world[(1, 1)] * y
                + model_to_world[(1, 2)] * z
                + model_to_world[(1, 3)];
            let wz = model_to_world[(2, 0)] * x
                + model_to_world[(2, 1)] * y
                + model_to_world[(2, 2)] * z
                + model_to_world[(2, 3)];
            [wx, wy, wz]
        })
        .collect();

    let triangles: Vec<usize> = mesh
        .triangles
        .iter()
        .flat_map(|t| [t.x, t.y, t.z])
        .collect();

    Manifold::new(&positions, &triangles).map_err(|e| {
        MeshingError(format!(
            "Failed to build boolmesh Manifold from fidget mesh: {e}"
        ))
    })
}

impl StaticTypeName for Surface3D {
    fn static_type_name() -> std::borrow::Cow<'static, str> {
        "ImplicitSurface3D".into()
    }
}

impl StaticType for Surface3D {
    fn static_type() -> ValueType {
        ValueType::ImplicitSurface3D
    }
}

impl Object for Surface3D {
    fn get_type(&self, _context: &ExecutionContext) -> ValueType {
        ValueType::ImplicitSurface3D
    }

    fn addition(self, context: &ExecutionContext, rhs: Value) -> ExecutionResult<Value> {
        match unpack_arithmetic_input(context, rhs)? {
            ArithmeticInput::Vector(vector) => {
                let raw = vector.raw_value();
                let translation = nalgebra::Translation3::from([raw.x, raw.y, raw.z]);
                Ok(self.transform(&translation.to_homogeneous()).into())
            }
            ArithmeticInput::Surface(other) => Ok(self.union(&other).into()),
        }
    }

    fn subtraction(self, context: &ExecutionContext, rhs: Value) -> ExecutionResult<Value> {
        match unpack_arithmetic_input(context, rhs)? {
            ArithmeticInput::Vector(vector) => {
                let raw = vector.raw_value();
                let translation = nalgebra::Translation3::from([-raw.x, -raw.y, -raw.z]);
                Ok(self.transform(&translation.to_homogeneous()).into())
            }
            ArithmeticInput::Surface(other) => Ok(self.difference(&other).into()),
        }
    }

    fn multiply(self, context: &ExecutionContext, rhs: Value) -> ExecutionResult<Value> {
        let input = rhs.downcast::<Zero3>(context)?;
        let vector = input.raw_value();
        let scaling = nalgebra::Matrix4::new_nonuniform_scaling(&vector);
        Ok(self.transform(&scaling).into())
    }

    fn bit_or(self, context: &ExecutionContext, rhs: Value) -> ExecutionResult<Value> {
        let other = rhs.downcast::<Surface3D>(context)?;
        Ok(self.union(&other).into())
    }

    fn bit_and(self, context: &ExecutionContext, rhs: Value) -> ExecutionResult<Value> {
        let other = rhs.downcast::<Surface3D>(context)?;
        Ok(self.intersection(&other).into())
    }

    fn bit_xor(self, context: &ExecutionContext, rhs: Value) -> ExecutionResult<Value> {
        let other = rhs.downcast::<Surface3D>(context)?;
        Ok(self.symmetric_difference(&other).into())
    }

    fn get_attribute(
        &self,
        _context: &ExecutionContext,
        attribute: &str,
    ) -> crate::execution::ExecutionResult<Value> {
        use crate::execution::errors::Raise as _;
        use crate::execution::values::MissingAttributeError;
        match attribute {
            "slice_x" => Ok(BuiltinFunction::new::<methods::SliceX>().into()),
            "slice_y" => Ok(BuiltinFunction::new::<methods::SliceY>().into()),
            "slice_z" => Ok(BuiltinFunction::new::<methods::SliceZ>().into()),
            "to_mesh" => Ok(BuiltinFunction::new::<methods::ToMesh>().into()),
            "union" => Ok(BuiltinFunction::new::<methods::Union>().into()),
            "intersection" => Ok(BuiltinFunction::new::<methods::Intersection>().into()),
            "difference" => Ok(BuiltinFunction::new::<methods::Difference>().into()),
            "symmetric_difference" => {
                Ok(BuiltinFunction::new::<methods::SymmetricDifference>().into())
            }
            "transform" => Ok(BuiltinFunction::new::<methods::Transform>().into()),
            _ => Err(MissingAttributeError {
                name: attribute.into(),
            }
            .to_error(_context)),
        }
    }

    fn format(
        &self,
        _context: &ExecutionContext,
        f: &mut dyn std::fmt::Write,
        _style: Style,
        _precision: Option<u8>,
    ) -> std::fmt::Result {
        write!(f, "Implicit 3D shape")
    }
}

pub mod methods {
    pub struct SliceX;
    pub struct SliceY;
    pub struct SliceZ;
    pub struct ToMesh;
    pub struct Union;
    pub struct Intersection;
    pub struct Difference;
    pub struct SymmetricDifference;
    pub struct Transform;
}

pub fn register_surface3d_methods(database: &mut BuiltinCallableDatabase) {
    use crate::build_method;
    use crate::execution::errors::StringError;

    build_method!(
        database,
        methods::SliceX, "Surface3D::slice_x", (
            context: &ExecutionContext,
            this: Surface3D,
            x: Scalar
        ) -> Value
        {
            let polygon = this.slice_x(x.value.into_inner())
                .map_err(|e| StringError(e.to_string()).to_error(context))?;
            Ok(polygon.into())
        }
    );

    build_method!(
        database,
        methods::SliceY, "Surface3D::slice_y", (
            context: &ExecutionContext,
            this: Surface3D,
            y: Scalar
        ) -> Value
        {
            let polygon = this.slice_y(y.value.into_inner())
                .map_err(|e| StringError(e.to_string()).to_error(context))?;
            Ok(polygon.into())
        }
    );

    build_method!(
        database,
        methods::SliceZ, "Surface3D::slice_z", (
            context: &ExecutionContext,
            this: Surface3D,
            z: Scalar
        ) -> Value
        {
            let polygon = this.slice_z(z.value.into_inner())
                .map_err(|e| StringError(e.to_string()).to_error(context))?;
            Ok(polygon.into())
        }
    );

    build_method!(
        database,
        methods::ToMesh, "Surface3D::to_mesh", (
            context: &ExecutionContext,
            this: Surface3D,
            depth: Option<UnsignedInteger> = ValueNone.into()
        ) -> Value
        {
            let surface = if let Some(d) = depth {
                this.with_depth(d.0 as u8)
            } else {
                this
            };
            let mesh = surface.to_manifold()
                .map_err(|e| StringError(e.to_string()).to_error(context))?;
            Ok(mesh.into())
        }
    );

    build_method!(
        database,
        methods::Union, "Surface3D::union", (
            context: &ExecutionContext,
            this: Surface3D,
            other: Surface3D
        ) -> Value
        {
            let result = this.union(&other);
            Ok(result.into())
        }
    );

    build_method!(
        database,
        methods::Intersection, "Surface3D::intersection", (
            context: &ExecutionContext,
            this: Surface3D,
            other: Surface3D
        ) -> Value
        {
            let result = this.intersection(&other);
            Ok(result.into())
        }
    );

    build_method!(
        database,
        methods::Difference, "Surface3D::difference", (
            context: &ExecutionContext,
            this: Surface3D,
            other: Surface3D
        ) -> Value
        {
            let result = this.difference(&other);
            Ok(result.into())
        }
    );

    build_method!(
        database,
        methods::SymmetricDifference, "Surface3D::symmetric_difference", (
            context: &ExecutionContext,
            this: Surface3D,
            other: Surface3D
        ) -> Value
        {
            let result = this.symmetric_difference(&other);
            Ok(result.into())
        }
    );

    build_method!(
        database,
        methods::Transform, "Surface3D::transform", (
            context: &ExecutionContext,
            this: Surface3D,
            t: Transform3d
        ) -> Value
        {
            let result = this.transform(&t.0);
            Ok(result.into())
        }
    );
}

/// Builtin implicit shape generators.
pub mod implicits {
    pub struct Sphere;
    pub struct Cube;
    pub struct Cylinder;
    pub struct Cone;
    pub struct Torus;
    pub struct RoundedCube;
    pub struct Box;
}

/// Register builtin implicit shape functions.
pub fn register_implicits(database: &mut BuiltinCallableDatabase) {
    use crate::build_function;
    use fidget::context::Tree;
    build_function!(
        database,
        implicits::Sphere, "std.implicits.sphere", (
            context: &ExecutionContext,
            radius: Option<Length> = ValueNone.into(),
            diameter: Option<Length> = ValueNone.into()
        ) -> Value
        {
            let r = unpack_radius(context, radius, diameter)?;
            // SDF: sqrt(x² + y² + z²) - r
            let x = Tree::x();
            let y = Tree::y();
            let z = Tree::z();
            let dist = (x.clone() * x.clone() + y.clone() * y.clone() + z.clone() * z.clone()).sqrt();
            let tree = dist - Tree::constant(r);
            Ok(Surface3D::new(tree).into())
        }
    );

    build_function!(
        database,
        implicits::Cube, "std.implicits.cube", (
            context: &ExecutionContext,
            size: Scalar
        ) -> Value
        {
            let s = size.value.into_inner();
            let half = s / 2.0;
            // SDF: max(|x|, |y|, |z|) - half
            let x = Tree::x().abs();
            let y = Tree::y().abs();
            let z = Tree::z().abs();
            let tree = x.max(y).max(z) - Tree::constant(half);
            Ok(Surface3D::new(tree).into())
        }
    );

    build_function!(
        database,
        implicits::Cylinder, "std.implicits.cylinder", (
            context: &ExecutionContext,
            radius: Option<Length> = ValueNone.into(),
            diameter: Option<Length> = ValueNone.into(),
            height: Length
        ) -> Value
        {
            let r = unpack_radius(context, radius, diameter)?;
            let h = height.value.into_inner();
            // SDF for cylinder along Z axis: max(sqrt(x²+y²)-r, |z|-h/2)
            let x = Tree::x();
            let y = Tree::y();
            let z = Tree::z().abs();
            let radial = (x.clone() * x.clone() + y.clone() * y.clone()).sqrt() - Tree::constant(r);
            let axial = z - Tree::constant(h / 2.0);
            let tree = radial.max(axial);
            Ok(Surface3D::new(tree).into())
        }
    );

    build_function!(
        database,
        implicits::Cone, "std.implicits.cone", (
            context: &ExecutionContext,
            radius: Option<Length> = ValueNone.into(),
            diameter: Option<Length> = ValueNone.into(),
            height: Length
        ) -> Value
        {
            let r = unpack_radius(context, radius, diameter)?;
            let h = height.value.into_inner();
            // Finite cone SDF along Z axis, centered at origin.
            // Apex at (0, 0, -h/2), base at z = h/2 with radius r.
            // Same pattern as cylinder: max of component SDFs.
            let (x, y, z) = (Tree::x(), Tree::y(), Tree::z());
            let half_h = Tree::constant(h / 2.0);
            let r_tree = Tree::constant(r);
            let h_tree = Tree::constant(h);
            // Height from apex: a = z + h/2 (0 at apex, h at base).
            let a = z.clone() + half_h.clone();
            // Perpendicular distance from cone axis.
            let b = (x.clone() * x.clone() + y.clone() * y.clone()).sqrt();
            // Normalized lateral SDF so |grad| = 1.
            let slant = Tree::constant((h * h + r * r).sqrt());
            let d_lat = (b * h_tree - r_tree * a) / slant;
            // Base cap: z - h/2 (negative below base, |grad| = 1).
            let d_base = z.clone() - half_h.clone();
            let tree = d_lat.max(d_base);
            Ok(Surface3D::new(tree).into())
        }
    );

    build_function!(
        database,
        implicits::Torus, "std.implicits.torus", (
            context: &ExecutionContext,
            major_radius: Scalar,
            minor_radius: Scalar
        ) -> Value
        {
            let major = major_radius.value.into_inner();
            let minor = minor_radius.value.into_inner();
            // SDF: sqrt((sqrt(x²+y²)-major)² + z²) - minor
            let x = Tree::x();
            let y = Tree::y();
            let z = Tree::z();
            let radial = (x.clone() * x.clone() + y.clone() * y.clone()).sqrt();
            let diff = radial - Tree::constant(major);
            let tree = (diff.clone() * diff.clone() + z.clone() * z.clone()).sqrt() - Tree::constant(minor);
            Ok(Surface3D::new(tree).into())
        }
    );

    build_function!(
        database,
        implicits::RoundedCube, "std.implicits.rounded_cube", (
            context: &ExecutionContext,
            size: Scalar,
            radius: Length
        ) -> Value
        {
            let s = size.value.into_inner();
            let r = radius.value.into_inner();
            let half = s / 2.0;
            // Rounded box SDF: length(max(abs(p) - b + r, 0)) - r
            // where b is half-size. Normalized: |grad| = 1 everywhere.
            let x = Tree::x().abs();
            let y = Tree::y().abs();
            let z = Tree::z().abs();
            let q_x = (x.clone() - Tree::constant(half) + Tree::constant(r)).max(Tree::constant(0.0));
            let q_y = (y.clone() - Tree::constant(half) + Tree::constant(r)).max(Tree::constant(0.0));
            let q_z = (z.clone() - Tree::constant(half) + Tree::constant(r)).max(Tree::constant(0.0));
            let tree = (q_x.clone() * q_x.clone() + q_y.clone() * q_y.clone() + q_z.clone() * q_z.clone()).sqrt() - Tree::constant(r);
            Ok(Surface3D::new(tree).into())
        }
    );

    build_function!(
        database,
        implicits::Box, "std.implicits.box", (
            context: &ExecutionContext,
            size: Length3
        ) -> Value
        {
            let s = size.0.raw_value();
            // SDF for axis-aligned box centered at origin with per-axis sizes.
            let half_x = Tree::constant(s.x / 2.0);
            let half_y = Tree::constant(s.y / 2.0);
            let half_z = Tree::constant(s.z / 2.0);
            let x = Tree::x().abs();
            let y = Tree::y().abs();
            let z = Tree::z().abs();
            let tree = (x.clone() - half_x).max(y.clone() - half_y.clone()).max(z.clone() - half_z);
            Ok(Surface3D::new(tree).into())
        }
    );
}

#[cfg(test)]
mod surface3d_tests {
    use super::Surface3D;
    use crate::execution::test_run;
    use crate::execution::values::Value;
    use fidget::context::Tree;

    #[test]
    fn integration_slice_z_sphere() {
        // Slice a sphere at z=0 should give a circle
        let result = test_run(
            "let c = (p: std.vector3.Length) -> std.scalar.Length: (p.x * p.x + p.y * p.y + p.z * p.z)::sqrt() - 1.0m; \
             in c::to_implicit()::slice_z(z = 0.0)"
        );
        if let Err(ref e) = result {
            eprintln!("slice_z error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::PolygonSet(_)));
    }

    #[test]
    fn integration_slice_x_sphere() {
        let result = test_run(
            "let c = (p: std.vector3.Length) -> std.scalar.Length: (p.x * p.x + p.y * p.y + p.z * p.z)::sqrt() - 1.0m; \
             in c::to_implicit()::slice_x(x = 0.5)"
        );
        if let Err(ref e) = result {
            eprintln!("slice_x error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::PolygonSet(_)));
    }

    #[test]
    fn integration_slice_y_sphere() {
        let result = test_run(
            "let c = (p: std.vector3.Length) -> std.scalar.Length: (p.x * p.x + p.y * p.y + p.z * p.z)::sqrt() - 1.0m; \
             in c::to_implicit()::slice_y(y = 0.5)"
        );
        if let Err(ref e) = result {
            eprintln!("slice_y error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::PolygonSet(_)));
    }

    #[test]
    fn integration_to_mesh_sphere() {
        // Test to_manifold() directly on a sphere SDF
        let x = Tree::x();
        let y = Tree::y();
        let z = Tree::z();
        let tree = (x.clone() * x + y.clone() * y + z.clone() * z).sqrt() - Tree::constant(5.0);
        let surface = Surface3D::new(tree);

        let result = surface.to_manifold();
        if let Err(ref e) = result {
            eprintln!("to_manifold sphere error: {:?}", e);
        }
        assert!(
            result.is_ok(),
            "sphere to_manifold should succeed: {:?}",
            result
        );
    }

    #[test]
    fn integration_to_mesh_torus() {
        // Test to_manifold() directly on a torus SDF
        // Torus: (x² + y² + z² + R² - r²)² - 4R²(x² + y²) = 0
        // where R = major radius, r = minor radius
        let x = Tree::x();
        let y = Tree::y();
        let z = Tree::z();
        let major = Tree::constant(5.0);
        let minor = Tree::constant(1.5);
        let major_sq = major.clone() * major.clone();
        let minor_sq = minor.clone() * minor;
        let x2 = x.clone() * x;
        let y2 = y.clone() * y;
        let xy_sq = x2.clone() + y2.clone();
        let dist_sq = x2 + y2 + z.clone() * z;
        let inner = dist_sq.clone() + major_sq.clone() - minor_sq;
        let torus = inner.clone() * inner - Tree::constant(4.0) * major_sq * xy_sq;
        let surface = Surface3D::new(torus);

        let result = surface.to_manifold();
        if let Err(ref e) = result {
            eprintln!("to_manifold torus error: {:?}", e);
        }
        assert!(
            result.is_ok(),
            "torus to_manifold should succeed: {:?}",
            result
        );
    }

    #[test]
    fn integration_std_implicits_sphere() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run("std.implicits.sphere(radius = 5.0m)");
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));

        let result = test_run("std.implicits.sphere(diameter = 10.0m)");
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_std_implicits_cube() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run("std.implicits.cube(size = 4.0m)");
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_std_implicits_rounded_cube() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run("std.implicits.rounded_cube(size = 4.0m, radius = 0.5m)");
        if let Err(ref e) = result {
            eprintln!("rounded_cube error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_std_implicits_box() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run("std.implicits.box(size = {4.0m, 3.0m, 2.0m})");
        if let Err(ref e) = result {
            eprintln!("box error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_std_implicits_circle() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run("std.implicits.circle(radius = 2.0m)");
        if let Err(ref e) = result {
            eprintln!("circle error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface2D(_)));

        let result = test_run("std.implicits.circle(diameter = 4.0m)");
        if let Err(ref e) = result {
            eprintln!("circle (diameter) error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface2D(_)));
    }

    #[test]
    fn integration_std_implicits_rectangle() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run("std.implicits.rectangle(size = {4.0m, 3.0m})");
        if let Err(ref e) = result {
            eprintln!("rectangle error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface2D(_)));
    }

    #[test]
    fn integration_std_implicits_square() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run("std.implicits.square(size = 4.0m)");
        if let Err(ref e) = result {
            eprintln!("square error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface2D(_)));
    }

    #[test]
    fn integration_std_implicits_cylinder() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run("std.implicits.cylinder(radius = 2.0m, height = 6.0m)");
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));

        let result = test_run("std.implicits.cylinder(diameter = 4.0m, height = 6.0m)");
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_std_implicits_cone() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run("std.implicits.cone(radius = 2.0m, height = 6.0m)");
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));

        let result = test_run("std.implicits.cone(diameter = 4.0m, height = 6.0m)");
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_std_implicits_torus() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run("std.implicits.torus(major_radius = 4.0m, minor_radius = 1.5m)");
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_std_implicits_sphere_to_mesh() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run("std.implicits.sphere(radius = 5.0m)::to_mesh()");
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::ManifoldMesh3D(_)));

        let result = test_run("std.implicits.sphere(diameter = 10.0m)::to_mesh()");
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::ManifoldMesh3D(_)));

        let result = test_run("std.implicits.sphere(radius = 5.0m)::to_mesh(depth = 4u)");
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::ManifoldMesh3D(_)));
    }

    #[test]
    fn integration_std_implicits_cube_to_mesh() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run("std.implicits.cube(size = 4.0m)::to_mesh()");
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::ManifoldMesh3D(_)));
    }

    #[test]
    fn integration_std_implicits_cone_to_mesh() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run("std.implicits.cone(radius = 3.0m, height = 6.0m)::to_mesh()");
        if let Err(ref e) = result {
            eprintln!("cone to_mesh error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::ManifoldMesh3D(_)));
    }

    #[test]
    fn integration_std_implicits_cylinder_to_mesh() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run("std.implicits.cylinder(radius = 2.0m, height = 6.0m)::to_mesh()");
        if let Err(ref e) = result {
            eprintln!("cylinder to_mesh error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::ManifoldMesh3D(_)));
    }

    #[test]
    fn integration_std_implicits_union() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run(
            "let a = std.implicits.sphere(radius = 3.0m); \
             b = std.implicits.cube(size = 4.0m); \
             in a::union(b)",
        );
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_transform_translate() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run(
            "std.implicits.sphere(radius = 2.0m)::transform(std.consts.Transform3d::translate({3m, 0m, 0m}))",
        );
        if let Err(ref e) = result {
            eprintln!("transform translate error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_transform_scale() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run(
            "std.implicits.sphere(radius = 2.0m)::transform(std.consts.Transform3d::scale({2, 1, 0.5}))",
        );
        if let Err(ref e) = result {
            eprintln!("transform scale error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_transform_rotate() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run(
            "std.implicits.cube(size = 2.0m)::transform(std.consts.Transform3d::rotate({0, 0, 1}, 45deg))",
        );
        if let Err(ref e) = result {
            eprintln!("transform rotate error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_transform_to_mesh() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run(
            "std.implicits.sphere(radius = 2.0m)::transform(std.consts.Transform3d::translate({5m, 0m, 0m}))::to_mesh()",
        );
        if let Err(ref e) = result {
            eprintln!("transform to_mesh error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::ManifoldMesh3D(_)));
    }

    #[test]
    fn integration_arithmetic_add_vector() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run("std.implicits.sphere(radius = 2.0m) + {3m, 0m, 0m}");
        if let Err(ref e) = result {
            eprintln!("arithmetic add vector error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_arithmetic_subtract_vector() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run("std.implicits.sphere(radius = 2.0m) - {1m, 2m, 3m}");
        if let Err(ref e) = result {
            eprintln!("arithmetic subtract vector error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_arithmetic_multiply_scale() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run("std.implicits.sphere(radius = 2.0m) * {2, 1, 0.5}");
        if let Err(ref e) = result {
            eprintln!("arithmetic multiply scale error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_arithmetic_add_vector2() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run("std.implicits.sphere(radius = 2.0m) + {3m, 0m}");
        if let Err(ref e) = result {
            eprintln!("arithmetic add vector2 error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_arithmetic_add_surface() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result =
            test_run("std.implicits.sphere(radius = 2.0m) + std.implicits.cube(size = 3.0m)");
        if let Err(ref e) = result {
            eprintln!("arithmetic add surface error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_arithmetic_subtract_surface() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result =
            test_run("std.implicits.sphere(radius = 2.0m) - std.implicits.cube(size = 3.0m)");
        if let Err(ref e) = result {
            eprintln!("arithmetic subtract surface error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_arithmetic_bit_or() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result =
            test_run("std.implicits.sphere(radius = 2.0m) | std.implicits.cube(size = 3.0m)");
        if let Err(ref e) = result {
            eprintln!("arithmetic bit_or error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_arithmetic_bit_and() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result =
            test_run("std.implicits.sphere(radius = 2.0m) & std.implicits.cube(size = 3.0m)");
        if let Err(ref e) = result {
            eprintln!("arithmetic bit_and error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_arithmetic_bit_xor() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result =
            test_run("std.implicits.sphere(radius = 2.0m) ^ std.implicits.cube(size = 3.0m)");
        if let Err(ref e) = result {
            eprintln!("arithmetic bit_xor error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }
}
