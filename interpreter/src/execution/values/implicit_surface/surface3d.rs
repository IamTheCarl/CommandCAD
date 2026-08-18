use std::sync::Arc;

use common_data_types::Dimension;
use fidget::context::BinaryOpcode;
use fidget::context::Context;
use fidget::context::Tree;
use fidget::context::TreeOp;
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

/// Result of `Surface3D::project()`, indicating which code path was used.
pub enum ProjectResult {
    /// Symbolic min_z distribution succeeded.
    Symbolic(Surface2D),
    /// Symbolic path failed; slicing-based fallback was used.
    Slicing(Surface2D),
}

impl ProjectResult {
    pub fn into_surface(self) -> Surface2D {
        match self {
            ProjectResult::Symbolic(s) | ProjectResult::Slicing(s) => s,
        }
    }

    pub fn used_fallback(&self) -> bool {
        matches!(self, ProjectResult::Slicing(_))
    }

    pub fn is_degenerate(&self) -> bool {
        match self {
            ProjectResult::Symbolic(s) | ProjectResult::Slicing(s) => s.is_degenerate_projection(),
        }
    }
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

    /// Create a copy of this surface with a custom bounding box for meshing.
    pub fn with_bounding_box(mut self, bb: (f64, f64, f64, f64, f64, f64)) -> Self {
        self.settings.bounding_box = Some(bb);
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

    /// Extract critical z-values from mesh feature points via dual contouring.
    ///
    /// Reuses the same meshing pipeline as `to_manifold()` (Octree + walk_dual)
    /// to find sharp feature z-coordinates. Deduplicates within tolerance and
    /// ensures bounding box extremes are included.
    fn critical_z_values(&self, max_slices: usize) -> Result<Vec<f64>, MeshingError> {
        let (min_x, min_y, min_z, max_x, max_y, max_z) = self.bounding_box_estimate();

        let cx = (min_x + max_x) / 2.0;
        let cy = (min_y + max_y) / 2.0;
        let cz = (min_z + max_z) / 2.0;
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

        let scaled_tree = self.tree.remap_xyz(
            Tree::x() * Tree::constant(half_x) + Tree::constant(cx),
            Tree::y() * Tree::constant(half_y) + Tree::constant(cy),
            Tree::z() * Tree::constant(half_z) + Tree::constant(cz),
        );

        let mut ctx = Context::new();
        let node = ctx.import(&scaled_tree);
        let shape: Shape<fidget::jit::JitFunction> = Shape::new(&ctx, node)
            .map_err(|e| MeshingError(format!("Failed to create shape for critical z: {e}")))?;

        let settings = Settings {
            depth: self.settings.depth,
            world_to_model: nalgebra::Matrix4::identity(),
            threads: Some(&ThreadPool::Global),
            cancel: CancelToken::new(),
        };
        let octree = Octree::build(&shape, &settings)
            .ok_or_else(|| MeshingError("Octree build cancelled or failed".into()))?;
        let mesh = octree.walk_dual();

        // Transform vertex z-coordinates back to world space
        let mut z_values: Vec<f64> = mesh
            .vertices
            .iter()
            .map(|v| v.z as f64 * half_z + cz)
            .collect();

        // Deduplicate: sort and merge within tolerance
        z_values.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        let tol = (max_z - min_z) * 0.01;
        let mut deduped: Vec<f64> = Vec::new();
        for &z in &z_values {
            if deduped.is_empty() || (z - *deduped.last().unwrap()).abs() > tol {
                deduped.push(z);
            }
        }

        // Ensure z_min and z_max are included
        if deduped.is_empty() || (*deduped.first().unwrap() - min_z).abs() > tol {
            deduped.insert(0, min_z);
        }
        if (*deduped.last().unwrap() - max_z).abs() > tol {
            deduped.push(max_z);
        }

        // Cap at max_slices: keep extremes, distribute the rest
        if deduped.len() > max_slices {
            let first = deduped[0];
            let last = *deduped.last().unwrap();
            let n_inner = max_slices - 2;
            let mut capped = Vec::with_capacity(max_slices);
            capped.push(first);
            for i in 0..n_inner {
                let t = (i as f64 + 0.5) / n_inner as f64;
                capped.push(first + t * (last - first));
            }
            capped.push(last);
            deduped = capped;
        }

        Ok(deduped)
    }

    /// Project the 3D implicit surface to 2D by slicing at critical z-values.
    ///
    /// Uses dual contouring mesh feature points to find sharp z-coordinates,
    /// then slices the surface at each z and unions the resulting 2D SDFs.
    fn project_by_slicing(&self, max_slices: usize) -> Result<Surface2D, MeshingError> {
        let z_values = self.critical_z_values(max_slices)?;

        if z_values.is_empty() {
            return Err(MeshingError(
                "No critical z-values found for projection".into(),
            ));
        }

        // Create a 2D SDF slice at each z and union them via balanced min-tree.
        let trees: Vec<Tree> = z_values
            .iter()
            .map(|&z| self.tree.remap_xyz(Tree::x(), Tree::y(), Tree::constant(z)))
            .collect();

        let result_tree = reduce_balanced(&trees, |a, b| a.min(b));
        Ok(Surface2D::with_settings(result_tree, self.settings.clone()))
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
        let mut result = Self::new(self.tree.remap_xyz(new_x, new_y, new_z));
        result.settings = self.settings.clone();
        result
    }

    /// Check if the shape is bounded along the z-axis.
    /// Returns false if the SDF remains negative at extreme z values,
    /// indicating the shape extends infinitely in z (projection will be degenerate).
    pub fn is_bounded_along_z(&self) -> bool {
        let mut ctx = Context::new();
        let node = ctx.import(&self.tree);

        // Test at multiple large z values with x=0, y=0 (and a few other xy positions)
        let test_points = [
            (0.0, 0.0, 1e6),
            (0.0, 0.0, -1e6),
            (1.0, 0.0, 1e6),
            (0.0, 1.0, 1e6),
            (1.0, 1.0, 1e6),
        ];

        for &(x, y, z) in &test_points {
            match ctx.eval_xyz(node, x, y, z) {
                Ok(val) if val < -0.1 => return false,
                _ => {}
            }
        }
        true
    }

    /// Check if the shape is closed (finite extent in all directions).
    /// Returns false if the SDF is negative at large distance along any axis,
    /// meaning the shape is unbounded (e.g., a plane or infinite cylinder).
    pub fn is_bounded(&self) -> bool {
        let mut ctx = Context::new();
        let node = ctx.import(&self.tree);

        // Test far along each axis direction
        let d = 1e6_f64;
        let test_points = [
            (d, 0.0, 0.0),
            (-d, 0.0, 0.0),
            (0.0, d, 0.0),
            (0.0, -d, 0.0),
            (0.0, 0.0, d),
            (0.0, 0.0, -d),
        ];

        for &(x, y, z) in &test_points {
            match ctx.eval_xyz(node, x, y, z) {
                Ok(val) if val < -0.1 => return false,
                _ => {}
            }
        }
        true
    }

    /// Project the 3D implicit surface to 2D by computing min_z(f(x,y,z)).
    ///
    /// Uses symbolic min distribution through min/max operations. Falls back
    /// to slicing-based projection for shapes where z is mixed in arithmetic
    /// (cone, torus, rounded_cube).
    pub fn project(&self) -> Result<ProjectResult, MeshingError> {
        match min_z_tree(&self.tree) {
            Ok(result_tree) => Ok(ProjectResult::Symbolic(Surface2D::with_settings(
                result_tree,
                self.settings.clone(),
            ))),
            Err(()) => {
                let surface = self.project_by_slicing(32)?;
                Ok(ProjectResult::Slicing(surface))
            }
        }
    }

    /// Convenience wrapper for `project()` that returns the resulting 2D surface.
    pub fn project_to_2d(&self) -> Result<Surface2D, MeshingError> {
        self.project().map(|r| r.into_surface())
    }
}

/* ── Symbolic min_z projection ── */

fn contains_z_in_op(tree_op: &TreeOp) -> bool {
    match tree_op {
        TreeOp::Input(fidget::var::Var::Z) => true,
        TreeOp::Input(_) | TreeOp::Const(_) => false,
        TreeOp::Binary(_, a, b) => contains_z_in_op(a.as_ref()) || contains_z_in_op(b.as_ref()),
        TreeOp::Unary(_, c) => contains_z_in_op(c.as_ref()),
        TreeOp::RemapAxes { target, x, y, z } => {
            contains_z_in_op(x.as_ref())
                || contains_z_in_op(y.as_ref())
                || contains_z_in_op(z.as_ref())
                || contains_z_in_op(target.as_ref())
        }
        TreeOp::RemapAffine { target, .. } => contains_z_in_op(target.as_ref()),
    }
}

/// Check if a tree contains x or y (but not necessarily z).
/// Used to determine if a z-remap is a pure function of z alone.
#[allow(dead_code)]
fn contains_xy_in_op(tree_op: &TreeOp) -> bool {
    match tree_op {
        TreeOp::Input(fidget::var::Var::X | fidget::var::Var::Y) => true,
        TreeOp::Input(_) | TreeOp::Const(_) => false,
        TreeOp::Binary(_, a, b) => contains_xy_in_op(a.as_ref()) || contains_xy_in_op(b.as_ref()),
        TreeOp::Unary(_, c) => contains_xy_in_op(c.as_ref()),
        TreeOp::RemapAxes { target, x, y, z } => {
            contains_xy_in_op(x.as_ref())
                || contains_xy_in_op(y.as_ref())
                || contains_xy_in_op(z.as_ref())
                || contains_xy_in_op(target.as_ref())
        }
        TreeOp::RemapAffine { target, .. } => contains_xy_in_op(target.as_ref()),
    }
}

/// Extract an expression as `coeff * z + offset` where coeff and offset don't contain z.
/// Returns None if the expression is not linear in z or doesn't contain z at all.
/// Check if an affine expression (built from matrix row) semantically depends on x or y.
/// Handles the case where x*0 + y*0 + z*1 + c still contains Input(X) syntactically
/// but doesn't depend on x/y semantically.
fn affine_depends_on_xy(tree_op: &TreeOp) -> bool {
    let mut depends = false;
    collect_additive_terms(tree_op, &mut |term| {
        if let TreeOp::Binary(BinaryOpcode::Mul, v, c) = term {
            let var = matches!(
                &**v,
                TreeOp::Input(fidget::var::Var::X | fidget::var::Var::Y)
            );
            let coeff_nonzero = match &**c {
                TreeOp::Const(c) => *c != 0.0,
                _ => true,
            };
            if var && coeff_nonzero {
                depends = true;
            }
        }
        if let TreeOp::Binary(BinaryOpcode::Mul, c, v) = term {
            let var = matches!(
                &**v,
                TreeOp::Input(fidget::var::Var::X | fidget::var::Var::Y)
            );
            let coeff_nonzero = match &**c {
                TreeOp::Const(c) => *c != 0.0,
                _ => true,
            };
            if var && coeff_nonzero {
                depends = true;
            }
        }
    });
    depends
}

/// Check if an affine expression semantically depends on z.
/// Handles the case where x*0 + y*0 + z*0 + c still contains Input(Z) syntactically
/// but doesn't depend on z semantically.
fn affine_depends_on_z(tree_op: &TreeOp) -> bool {
    let mut depends = false;
    collect_additive_terms(tree_op, &mut |term| {
        if let TreeOp::Binary(BinaryOpcode::Mul, v, c) = term {
            let var = matches!(&**v, TreeOp::Input(fidget::var::Var::Z));
            let coeff_nonzero = match &**c {
                TreeOp::Const(c) => *c != 0.0,
                _ => true,
            };
            if var && coeff_nonzero {
                depends = true;
            }
        }
        if let TreeOp::Binary(BinaryOpcode::Mul, c, v) = term {
            let var = matches!(&**v, TreeOp::Input(fidget::var::Var::Z));
            let coeff_nonzero = match &**c {
                TreeOp::Const(c) => *c != 0.0,
                _ => true,
            };
            if var && coeff_nonzero {
                depends = true;
            }
        }
    });
    depends
}

/// Collect all additive terms from an expression (walks through Add chains).
fn collect_additive_terms<F>(tree_op: &TreeOp, f: &mut F)
where
    F: FnMut(&TreeOp),
{
    match tree_op {
        TreeOp::Binary(BinaryOpcode::Add, a, b) => {
            collect_additive_terms(a.as_ref(), f);
            collect_additive_terms(b.as_ref(), f);
        }
        _ => f(tree_op),
    }
}

/// Compute min_z(f(x, y, z)) symbolically by distributing min through the tree.
///
/// Returns Ok(Tree) with the resulting 2D SDF, or Err(()) if the tree contains
/// patterns that can't be handled symbolically (z mixed in arithmetic within min/max).
fn min_z_tree(tree: &Tree) -> Result<Tree, ()> {
    min_z_tree_op(tree)
}

/// Convert a TreeOp to a standalone Tree by recursively rebuilding it.
fn treeop_to_tree(tree_op: &TreeOp) -> Result<Tree, ()> {
    use fidget::context::{BinaryOpcode, TreeOp, UnaryOpcode};

    match tree_op {
        TreeOp::Input(var) => Ok(Tree::from(*var)),
        TreeOp::Const(c) => Ok(Tree::constant(*c)),
        TreeOp::Binary(op, a, b) => {
            let a_tree = treeop_to_tree(a.as_ref())?;
            let b_tree = treeop_to_tree(b.as_ref())?;
            match op {
                BinaryOpcode::Add => Ok(a_tree + b_tree),
                BinaryOpcode::Sub => Ok(a_tree - b_tree),
                BinaryOpcode::Mul => Ok(a_tree * b_tree),
                BinaryOpcode::Div => Ok(a_tree.clone() / b_tree.clone()),
                BinaryOpcode::Min => Ok(a_tree.min(b_tree)),
                BinaryOpcode::Max => Ok(a_tree.max(b_tree)),
                BinaryOpcode::Atan => Ok(a_tree.atan2(b_tree)),
                BinaryOpcode::Compare => Ok(a_tree.compare(b_tree)),
                BinaryOpcode::Mod => Ok(a_tree.modulo(b_tree)),
                BinaryOpcode::And => Ok(a_tree.and(b_tree)),
                BinaryOpcode::Or => Ok(a_tree.or(b_tree)),
            }
        }
        TreeOp::Unary(op, c) => {
            let c_tree = treeop_to_tree(c.as_ref())?;
            match op {
                UnaryOpcode::Neg => Ok(-c_tree),
                UnaryOpcode::Abs => Ok(c_tree.abs()),
                UnaryOpcode::Recip => Ok(c_tree.recip()),
                UnaryOpcode::Sqrt => Ok(c_tree.sqrt()),
                UnaryOpcode::Square => Ok(c_tree.square()),
                UnaryOpcode::Floor => Ok(c_tree.floor()),
                UnaryOpcode::Ceil => Ok(c_tree.ceil()),
                UnaryOpcode::Round => Ok(c_tree.round()),
                UnaryOpcode::Sin => Ok(c_tree.sin()),
                UnaryOpcode::Cos => Ok(c_tree.cos()),
                UnaryOpcode::Tan => Ok(c_tree.tan()),
                UnaryOpcode::Asin => Ok(c_tree.asin()),
                UnaryOpcode::Acos => Ok(c_tree.acos()),
                UnaryOpcode::Atan => Ok(c_tree.atan()),
                UnaryOpcode::Exp => Ok(c_tree.exp()),
                UnaryOpcode::Ln => Ok(c_tree.ln()),
                UnaryOpcode::Not => Ok(c_tree.not()),
            }
        }
        // Can't easily rebuild RemapAxes/RemapAffine without Arc<TreeOp> access
        TreeOp::RemapAxes { .. } | TreeOp::RemapAffine { .. } => Err(()),
    }
}

fn min_z_tree_op(tree_op: &TreeOp) -> Result<Tree, ()> {
    use fidget::context::{BinaryOpcode, TreeOp, UnaryOpcode};

    match tree_op {
        // Base cases: X and Y pass through, Z is eliminated (min over all z = -inf)
        TreeOp::Input(fidget::var::Var::X) => Ok(Tree::x()),
        TreeOp::Input(fidget::var::Var::Y) => Ok(Tree::y()),
        TreeOp::Input(fidget::var::Var::Z) => Ok(Tree::constant(f64::NEG_INFINITY)),
        TreeOp::Input(var) => Ok(Tree::from(*var)),

        // Constants pass through unchanged
        TreeOp::Const(c) => Ok(Tree::constant(*c)),

        // min/max distribute: min_z(min(a,b)) = min(min_z(a), min_z(b))
        TreeOp::Binary(BinaryOpcode::Min | BinaryOpcode::Max, a, b) => {
            let opcode = match tree_op {
                TreeOp::Binary(op, ..) => *op,
                _ => unreachable!(),
            };
            let a_min = min_z_tree_op(a.as_ref())?;
            let b_min = min_z_tree_op(b.as_ref())?;
            if opcode == BinaryOpcode::Min {
                Ok(a_min.min(b_min))
            } else {
                Ok(a_min.max(b_min))
            }
        }

        // Arithmetic: only distribute if one side is z-independent
        TreeOp::Binary(BinaryOpcode::Add | BinaryOpcode::Sub | BinaryOpcode::Mul, _, _) => {
            let tree = match tree_op {
                TreeOp::Binary(_, a, b) => (a, b),
                _ => unreachable!(),
            };
            let a = tree.0.as_ref();
            let b = tree.1.as_ref();
            let a_has_z = contains_z_in_op(a);
            let b_has_z = contains_z_in_op(b);
            if !a_has_z && !b_has_z {
                // Neither contains z, just pass through
                let a_tree = treeop_to_tree(a)?;
                let b_tree = treeop_to_tree(b)?;
                let opcode = match tree_op {
                    TreeOp::Binary(op, ..) => *op,
                    _ => unreachable!(),
                };
                apply_binary(opcode, a_tree, b_tree)
            } else if !a_has_z {
                // a is z-independent: min_z(a op b) = a op min_z(b)
                let a_tree = treeop_to_tree(a)?;
                let b_min = min_z_tree_op(b)?;
                let opcode = match tree_op {
                    TreeOp::Binary(op, ..) => *op,
                    _ => unreachable!(),
                };
                apply_binary(opcode, a_tree, b_min)
            } else if !b_has_z {
                // b is z-independent: min_z(a op b) = min_z(a) op b
                let a_min = min_z_tree_op(a)?;
                let b_tree = treeop_to_tree(b)?;
                let opcode = match tree_op {
                    TreeOp::Binary(op, ..) => *op,
                    _ => unreachable!(),
                };
                // For Sub, order matters: a - b -> min_z(a) - b
                // For Mul, order doesn't matter for commutative ops
                apply_binary(opcode, a_min, b_tree)
            } else {
                // Both sides contain z mixed in arithmetic.
                // Special case: f(z) * f(z) = f(z)², min over z is 0 (at f(z)=0).
                if let BinaryOpcode::Mul = match tree_op {
                    TreeOp::Binary(op, ..) => *op,
                    _ => unreachable!(),
                } {
                    // Arc pointer equality means shared sub-expression (deduped by fidget)
                    if Arc::ptr_eq(tree.0, tree.1) {
                        return Ok(Tree::constant(0.0));
                    }
                }
                Err(())
            }
        }

        // Division: distribute when denominator is z-independent and numerator is z-independent
        TreeOp::Binary(BinaryOpcode::Div, a, b) => {
            let a_has_z = contains_z_in_op(a.as_ref());
            let b_has_z = contains_z_in_op(b.as_ref());
            if !a_has_z && !b_has_z {
                let a_tree = treeop_to_tree(a.as_ref())?;
                let b_tree = treeop_to_tree(b.as_ref())?;
                Ok(a_tree / b_tree)
            } else {
                // Can't distribute min_z through division when z is involved.
                // For linear-in-z numerators, the zero-crossing approach gives wrong results
                // when combined with other branches in max/min (e.g., cone silhouette).
                // Sampling fallback handles these cases correctly.
                Err(())
            }
        }

        // Other binary ops (Atan, Compare, Mod, And, Or) -- can't distribute
        TreeOp::Binary(_, _, _) => Err(()),

        // Unary: push through for monotonic non-decreasing operations
        TreeOp::Unary(op, child) => {
            let child_has_z = contains_z_in_op(child.as_ref());
            match op {
                UnaryOpcode::Neg => {
                    let child_min = min_z_tree_op(child.as_ref())?;
                    Ok(-child_min)
                }
                UnaryOpcode::Abs => {
                    // abs is NOT monotonically non-decreasing.
                    // min_z(abs(f(z))) != abs(min_z(f(z))) in general.
                    // If child contains z, min of |child| over all z is 0
                    // (assuming child can reach 0 for some z, which holds for most cases).
                    if child_has_z {
                        Ok(Tree::constant(0.0))
                    } else {
                        let child_tree = treeop_to_tree(child.as_ref())?;
                        Ok(child_tree.abs())
                    }
                }
                UnaryOpcode::Square => {
                    // square is NOT monotonic over all reals.
                    // min_z(f(z)^2) = 0 if f contains z (f can reach 0 for some z).
                    if child_has_z {
                        Ok(Tree::constant(0.0))
                    } else {
                        let child_tree = treeop_to_tree(child.as_ref())?;
                        Ok(child_tree.square())
                    }
                }
                UnaryOpcode::Sqrt => {
                    // sqrt is monotonically non-decreasing for non-negative inputs.
                    // But if child contains z, min_z(sqrt(f(z))) = sqrt(min_z(f(z))).
                    // If min_z(f) is -inf, sqrt(-inf) is NaN. Handle gracefully.
                    let child_min = min_z_tree_op(child.as_ref())?;
                    Ok(child_min.sqrt())
                }
                _ => {
                    let child_min = min_z_tree_op(child.as_ref())?;
                    match op {
                        UnaryOpcode::Recip => Ok(child_min.recip()),
                        UnaryOpcode::Exp => Ok(child_min.exp()),
                        UnaryOpcode::Sin => Ok(child_min.sin()),
                        UnaryOpcode::Cos => Ok(child_min.cos()),
                        UnaryOpcode::Tan => Ok(child_min.tan()),
                        UnaryOpcode::Asin => Ok(child_min.asin()),
                        UnaryOpcode::Acos => Ok(child_min.acos()),
                        UnaryOpcode::Atan => Ok(child_min.atan()),
                        UnaryOpcode::Ln => Ok(child_min.ln()),
                        UnaryOpcode::Floor => Ok(child_min.floor()),
                        UnaryOpcode::Ceil => Ok(child_min.ceil()),
                        UnaryOpcode::Round => Ok(child_min.round()),
                        UnaryOpcode::Not => Ok(child_min.not()),
                        // Already handled: Neg, Abs, Sqrt, Square
                        _ => unreachable!(),
                    }
                }
            }
        }

        // RemapAxes: if z-remap is a pure constant, substitute it
        TreeOp::RemapAxes { target, x, y, z } => {
            // If the z remap is a constant, substitute it into target and recurse
            if let TreeOp::Const(zc) = &**z {
                let substituted = substitute_z(target.as_ref(), *zc);
                return min_z_tree_op(&substituted);
            }
            // If x and y remaps don't contain Z, and z remap is a pure function of z alone
            // (no x or y), we can recurse on target with the remapped axes.
            // min over all z of f(x_rem, y_rem, z_rem(z)) = min over all z' of f(x_rem, y_rem, z')
            // because z_rem(z) is bijective when it only depends on z.
            if !affine_depends_on_z(x.as_ref())
                && !affine_depends_on_z(y.as_ref())
                && !affine_depends_on_xy(z.as_ref())
            {
                let target_min = min_z_tree_op(target.as_ref())?;
                // Rebuild with the same x, y remaps but z eliminated
                let x_tree = treeop_to_tree(x.as_ref())?;
                let y_tree = treeop_to_tree(y.as_ref())?;
                Ok(target_min.remap_xyz(x_tree, y_tree, Tree::constant(0.0)))
            } else {
                Err(())
            }
        }

        // RemapAffine: convert to RemapAxes and process
        TreeOp::RemapAffine { target, mat } => {
            let m = mat.to_homogeneous();
            let x_rem = build_affine_expr(&m, 0);
            let y_rem = build_affine_expr(&m, 1);
            let z_rem = build_affine_expr(&m, 2);

            // If z-remap is constant, substitute and recurse
            if let Some(zc) = extract_constant(&z_rem) {
                let substituted = substitute_z(target.as_ref(), zc);
                return min_z_tree_op(&substituted);
            }

            // If x,y remaps don't contain Z, and z-remap is pure function of z alone, recurse
            if !affine_depends_on_z(&x_rem)
                && !affine_depends_on_z(&y_rem)
                && !affine_depends_on_xy(&z_rem)
            {
                let target_min = min_z_tree_op(target.as_ref())?;
                let x_tree = treeop_to_tree(&x_rem)?;
                let y_tree = treeop_to_tree(&y_rem)?;
                return Ok(target_min.remap_xyz(x_tree, y_tree, Tree::constant(0.0)));
            }

            Err(())
        }
    }
}

/// Check if a tree contains infinity constants (from degenerate min_z results).
/// This catches cases like `max(circle, +inf)` where the projection is empty.
/// Build a TreeOp for one row of an affine matrix: m[row][0]*x + m[row][1]*y + m[row][2]*z + m[row][3]
fn build_affine_expr(m: &nalgebra::Matrix4<f64>, row: usize) -> TreeOp {
    use fidget::context::TreeOp;
    let mut result = TreeOp::Const(0.0);
    for col in 0..3 {
        let coeff = m[(row, col)];
        if (coeff - 1.0).abs() < 1e-12 {
            // coeff == 1: just add the variable
            let var = TreeOp::Input(match col {
                0 => fidget::var::Var::X,
                1 => fidget::var::Var::Y,
                _ => fidget::var::Var::Z,
            });
            result = TreeOp::Binary(
                fidget::context::BinaryOpcode::Add,
                Arc::new(result),
                Arc::new(var),
            );
        } else if (coeff + 1.0).abs() < 1e-12 {
            // coeff == -1: subtract the variable
            let var = TreeOp::Input(match col {
                0 => fidget::var::Var::X,
                1 => fidget::var::Var::Y,
                _ => fidget::var::Var::Z,
            });
            result = TreeOp::Binary(
                fidget::context::BinaryOpcode::Sub,
                Arc::new(result),
                Arc::new(var),
            );
        } else if coeff.abs() > 1e-12 {
            // General case: multiply variable by coefficient
            let var = TreeOp::Input(match col {
                0 => fidget::var::Var::X,
                1 => fidget::var::Var::Y,
                _ => fidget::var::Var::Z,
            });
            let prod = TreeOp::Binary(
                fidget::context::BinaryOpcode::Mul,
                Arc::new(TreeOp::Const(coeff)),
                Arc::new(var),
            );
            result = TreeOp::Binary(
                fidget::context::BinaryOpcode::Add,
                Arc::new(result),
                Arc::new(prod),
            );
        }
    }
    // Add translation component
    let trans = m[(row, 3)];
    if trans.abs() > 1e-12 {
        result = TreeOp::Binary(
            fidget::context::BinaryOpcode::Add,
            Arc::new(result),
            Arc::new(TreeOp::Const(trans)),
        );
    }
    result
}

/// Extract a constant value from a TreeOp if it's a pure constant.
fn extract_constant(tree_op: &TreeOp) -> Option<f64> {
    match tree_op {
        TreeOp::Const(c) => Some(*c),
        _ => None,
    }
}

/// Apply a binary opcode to two trees, returning the result tree.
fn apply_binary(opcode: BinaryOpcode, a: Tree, b: Tree) -> Result<Tree, ()> {
    use fidget::context::BinaryOpcode;
    match opcode {
        BinaryOpcode::Add => Ok(a + b),
        BinaryOpcode::Sub => Ok(a - b),
        BinaryOpcode::Mul => Ok(a * b),
        _ => Err(()),
    }
}

/// Substitute Z with a constant value in a tree, returning a new tree.
fn substitute_z(tree_op: &TreeOp, z_const: f64) -> TreeOp {
    use fidget::context::TreeOp;
    match tree_op {
        TreeOp::Input(fidget::var::Var::Z) => TreeOp::Const(z_const),
        TreeOp::Input(v) => TreeOp::Input(*v),
        TreeOp::Const(c) => TreeOp::Const(*c),
        TreeOp::Binary(op, a, b) => {
            let new_a = substitute_z_op(a.as_ref(), z_const);
            let new_b = substitute_z_op(b.as_ref(), z_const);
            TreeOp::Binary(*op, new_a, new_b)
        }
        TreeOp::Unary(op, c) => {
            let new_c = substitute_z_op(c.as_ref(), z_const);
            TreeOp::Unary(*op, new_c)
        }
        TreeOp::RemapAxes { target, x, y, z } => {
            let new_target = substitute_z_op(target.as_ref(), z_const);
            let new_x = substitute_z_op(x.as_ref(), z_const);
            let new_y = substitute_z_op(y.as_ref(), z_const);
            let new_z = substitute_z_op(z.as_ref(), z_const);
            TreeOp::RemapAxes {
                target: new_target,
                x: new_x,
                y: new_y,
                z: new_z,
            }
        }
        TreeOp::RemapAffine { target, mat } => {
            let new_target = substitute_z_op(target.as_ref(), z_const);
            TreeOp::RemapAffine {
                target: new_target,
                mat: *mat,
            }
        }
    }
}

fn substitute_z_op(tree_op: &TreeOp, z_const: f64) -> std::sync::Arc<TreeOp> {
    std::sync::Arc::new(substitute_z(tree_op, z_const))
}

/// Reduce a list of trees into a single tree using a binary combiner,
/// building a balanced tree (O(log n) depth) instead of left-leaning (O(n)).
fn reduce_balanced(items: &[Tree], combine: impl Fn(Tree, Tree) -> Tree + Copy) -> Tree {
    if items.len() == 1 {
        return items[0].clone();
    }
    let mid = items.len() / 2;
    let left = reduce_balanced(&items[..mid], combine);
    let right = reduce_balanced(&items[mid..], combine);
    combine(left, right)
}

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
            "project" => Ok(BuiltinFunction::new::<methods::Project>().into()),
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
    pub struct Project;
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

    build_method!(
        database,
        methods::Project, "Surface3D::project", (
            context: &ExecutionContext,
            this: Surface3D
        ) -> Value
        {
            // Pre-check: warn if shape is unbounded along z
            if !this.is_bounded_along_z() {
                context.log.push_message(crate::execution::LogMessage {
                    origin: context.stack_trace.bottom().clone(),
                    level: crate::execution::LogLevel::Warning,
                    message: "Shape is unbounded along the projection axis (z); projection result will likely be empty or degenerate".into(),
                });
            }

            let result = this.project()
                .map_err(|e| StringError(e.to_string()).to_error(context))?;
            if result.used_fallback() {
                context.log.push_message(crate::execution::LogMessage {
                    origin: context.stack_trace.bottom().clone(),
                    level: crate::execution::LogLevel::Warning,
                    message: "3D surface projection used slicing fallback (symbolic min_z failed); result is an approximate SDF".into(),
                });
            }
            if result.is_degenerate() {
                context.log.push_message(crate::execution::LogMessage {
                    origin: context.stack_trace.bottom().clone(),
                    level: crate::execution::LogLevel::Warning,
                    message: "Projection contains infinity — the shape is unbounded along the projection axis (z), so the result is empty. Check for terms like `x - z` in max() that diverge as z varies.".into(),
                });
            }
            Ok(result.into_surface().into())
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
    use super::min_z_tree;
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

    #[test]
    fn integration_project_sphere() {
        // Project a sphere to 2D should give a circle
        let result = test_run("std.implicits.sphere(radius = 5.0m)::project()");
        if let Err(ref e) = result {
            eprintln!("project sphere error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface2D(_)));
    }

    #[test]
    fn integration_project_cube() {
        // Project a cube to 2D should give a square
        let result = test_run("std.implicits.cube(size = 4.0m)::project()");
        if let Err(ref e) = result {
            eprintln!("project cube error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface2D(_)));
    }

    #[test]
    fn integration_project_box() {
        // Project a box to 2D should give a rectangle
        let result = test_run("std.implicits.box(size = {3.0m, 5.0m, 7.0m})::project()");
        if let Err(ref e) = result {
            eprintln!("project box error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface2D(_)));
    }

    #[test]
    fn integration_project_cylinder() {
        // Project a cylinder to 2D should give a circle
        let result = test_run("std.implicits.cylinder(radius = 3.0m, height = 10.0m)::project()");
        if let Err(ref e) = result {
            eprintln!("project cylinder error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface2D(_)));
    }

    #[test]
    fn integration_project_cone() {
        // Project a cone to 2D should give a circle (uses sampling fallback)
        let result = test_run("std.implicits.cone(radius = 3.0m, height = 5.0m)::project()");
        if let Err(ref e) = result {
            eprintln!("project cone error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface2D(_)));
    }

    #[test]
    fn integration_project_torus() {
        // Project a torus to 2D should give an annulus (uses sampling fallback)
        let result =
            test_run("std.implicits.torus(major_radius = 5.0m, minor_radius = 2.0m)::project()");
        if let Err(ref e) = result {
            eprintln!("project torus error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface2D(_)));
    }

    #[test]
    fn integration_project_rounded_cube() {
        // Project a rounded cube to 2D (uses sampling fallback)
        let result = test_run("std.implicits.rounded_cube(size = 4.0m, radius = 1.0m)::project()");
        if let Err(ref e) = result {
            eprintln!("project rounded_cube error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface2D(_)));
    }

    #[test]
    fn integration_project_composable() {
        // Verify projected surface can be used in further operations
        let result = test_run(
            "let s = std.implicits.sphere(radius = 3.0m)::project(); \
             in s::to_polygon()",
        );
        if let Err(ref e) = result {
            eprintln!("project composable error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::PolygonSet(_)));
    }

    #[test]
    fn integration_project_translated_union() {
        // Union of sphere and translated cube should use symbolic path
        // (translation creates RemapAxes with affine expressions like x*1+y*0+z*0-1)
        let result = test_run(
            "(std.implicits.sphere(radius = 1m) + (std.implicits.cube(size = 2m) + {1m, 0m, 0m}))::project()",
        );
        if let Err(ref e) = result {
            eprintln!("project translated union error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface2D(_)));

        // Verify symbolic path on the full union tree using actual Surface3D::transform
        let sphere = Surface3D::new({
            let (x, y, z) = Tree::axes();
            (x.clone() * x.clone() + y.clone() * y.clone() + z.clone() * z.clone()).sqrt()
                - Tree::constant(1.0)
        });
        let cube = Surface3D::new({
            let x = Tree::x().abs();
            let y = Tree::y().abs();
            let z = Tree::z().abs();
            x.max(y).max(z) - Tree::constant(1.0)
        });
        let translated_cube =
            cube.transform(&nalgebra::Translation3::new(1.0, 0.0, 0.0).to_homogeneous());
        let union = sphere.union(&translated_cube);
        assert!(
            min_z_tree(&union.tree).is_ok(),
            "Sphere+translated-cube union should use symbolic path (via RemapAxes)"
        );
    }

    #[test]
    fn integration_project_closure_sphere_with_linear_term() {
        // Closure-based sphere with linear z term: max(sqrt(x^2+y^2+z^2) - 1, x - z)
        // pow(2) now produces x*x (not (1*x)*x), so Arc::ptr_eq works for min_z_tree
        let result = test_run(
            "let \
             sphere = (p: std.vector3.Length) -> std.scalar.Length: \
               ((p.x::pow(2) + p.y::pow(2) + p.z::pow(2))::sqrt() - 1.0m)::max(p.x-p.z); \
             in sphere::to_implicit()::project()",
        );
        if let Err(ref e) = result {
            eprintln!("project closure sphere error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface2D(_)));
    }

    #[test]
    fn surface3d_is_bounded_along_z() {
        use fidget::context::Tree;

        // Bounded: sphere
        let sphere = Surface3D::new(
            (Tree::x().clone() * Tree::x().clone()
                + Tree::y().clone() * Tree::y().clone()
                + Tree::z().clone() * Tree::z().clone())
            .sqrt()
                - Tree::constant(1.0),
        );
        assert!(sphere.is_bounded_along_z());

        // Unbounded: min(circle, x - z) - goes to -inf as z -> +inf
        let unbounded = Surface3D::new(
            ((Tree::x().clone() * Tree::x().clone() + Tree::y().clone() * Tree::y().clone())
                .sqrt()
                - Tree::constant(1.0))
            .min(Tree::x().clone() - Tree::z()),
        );
        assert!(!unbounded.is_bounded_along_z());

        // Bounded: cylinder capped at both ends (finite in z via max)
        let cylinder = Surface3D::new(
            ((Tree::x().clone() * Tree::x().clone() + Tree::y().clone() * Tree::y().clone())
                .sqrt()
                - Tree::constant(1.0))
            .max(Tree::z().clone() - Tree::constant(2.0))
            .max(-Tree::z().clone() - Tree::constant(1.0)),
        );
        assert!(cylinder.is_bounded_along_z());
    }

    #[test]
    fn surface3d_is_bounded_general() {
        use fidget::context::Tree;

        // Bounded: sphere
        let sphere = Surface3D::new(
            (Tree::x().clone() * Tree::x().clone()
                + Tree::y().clone() * Tree::y().clone()
                + Tree::z().clone() * Tree::z().clone())
            .sqrt()
                - Tree::constant(1.0),
        );
        assert!(sphere.is_bounded());

        // Unbounded: plane (-y) — infinite in x and z
        let plane = Surface3D::new(-Tree::y());
        assert!(!plane.is_bounded());

        // Unbounded: infinite cylinder along z
        let inf_cylinder = Surface3D::new(
            (Tree::x().clone() * Tree::x().clone() + Tree::y().clone() * Tree::y().clone()).sqrt()
                - Tree::constant(1.0),
        );
        assert!(!inf_cylinder.is_bounded());

        // Bounded: cube
        let cube = Surface3D::new(
            Tree::x().abs().max(Tree::y().abs()).max(Tree::z().abs()) - Tree::constant(1.0),
        );
        assert!(cube.is_bounded());
    }
}
