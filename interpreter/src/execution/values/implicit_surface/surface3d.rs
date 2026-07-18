use std::sync::Arc;

use fidget::context::Context;
use fidget::context::Tree;
use fidget::mesh::{Octree, Settings};
use fidget::render::CancelToken;
use fidget::shape::Shape;

use crate::execution::ExecutionContext;
use crate::execution::values::{
    closure::{BuiltinCallableDatabase, BuiltinFunction},
    Object, Scalar, StaticType, StaticTypeName, Style, Value, ValueType,
};

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

    /// Generate a 3D manifold mesh from the implicit surface using Manifold Dual Contouring.
    ///
    /// Returns `ManifoldMesh3D` (wrapping `Arc<Manifold>` from boolmesh).
    pub fn to_manifold(&self) -> Result<ManifoldMesh3D, MeshingError> {
        // Compute bounding box and scale factors to fit in fidget's [-1, 1]³ model space
        let (min_x, min_y, min_z, max_x, max_y, max_z): (f64, f64, f64, f64, f64, f64) = self.settings.bounding_box.unwrap_or((-10.0, -10.0, -10.0, 10.0, 10.0, 10.0));
        let cx = (min_x + max_x) / 2.0;
        let cy = (min_y + max_y) / 2.0;
        let cz = (min_z + max_z) / 2.0;
        // Scale factor: world coordinate = model_coordinate * half_size + center
        let half_x = if (max_x - min_x).abs() > 1e-10 { (max_x - min_x) / 2.0 } else { 1.0 };
        let half_y = if (max_y - min_y).abs() > 1e-10 { (max_y - min_y) / 2.0 } else { 1.0 };
        let half_z = if (max_z - min_z).abs() > 1e-10 { (max_z - min_z) / 2.0 } else { 1.0 };

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
            threads: None,
            cancel: CancelToken::new(),
        };

        let octree = Octree::build(&shape, &settings)
            .ok_or_else(|| MeshingError("Octree build cancelled or failed".into()))?;
        let mesh = octree.walk_dual();

        // Transform vertices back to world space
        let model_to_world = nalgebra::Matrix4::<f64>::new(
            half_x, 0.0, 0.0, cx,
            0.0, half_y, 0.0, cy,
            0.0, 0.0, half_z, cz,
            0.0, 0.0, 0.0, 1.0,
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
            let wx = model_to_world[(0, 0)] * x + model_to_world[(0, 1)] * y + model_to_world[(0, 2)] * z + model_to_world[(0, 3)];
            let wy = model_to_world[(1, 0)] * x + model_to_world[(1, 1)] * y + model_to_world[(1, 2)] * z + model_to_world[(1, 3)];
            let wz = model_to_world[(2, 0)] * x + model_to_world[(2, 1)] * y + model_to_world[(2, 2)] * z + model_to_world[(2, 3)];
            [wx, wy, wz]
        })
        .collect();

    let triangles: Vec<usize> = mesh
        .triangles
        .iter()
        .flat_map(|t| [t.x, t.y, t.z])
        .collect();

    Manifold::new(&positions, &triangles)
        .map_err(|e| MeshingError(format!("Failed to build boolmesh Manifold from fidget mesh: {e}")))
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

    fn get_attribute(&self, _context: &ExecutionContext, attribute: &str) -> crate::execution::ExecutionResult<Value> {
        use crate::execution::errors::Raise as _;
        use crate::execution::values::MissingAttributeError;
        match attribute {
            "slice_x" => Ok(BuiltinFunction::new::<methods::SliceX>().into()),
            "slice_y" => Ok(BuiltinFunction::new::<methods::SliceY>().into()),
            "slice_z" => Ok(BuiltinFunction::new::<methods::SliceZ>().into()),
            "to_mesh" => Ok(BuiltinFunction::new::<methods::ToMesh>().into()),
            _ => Err(MissingAttributeError {
                name: attribute.into(),
            }.to_error(_context)),
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
            this: Surface3D
        ) -> Value
        {
            let mesh = this.to_manifold()
                .map_err(|e| StringError(e.to_string()).to_error(context))?;
            Ok(mesh.into())
        }
    );
}

#[cfg(test)]
mod surface3d_tests {
    use crate::execution::test_run;
    use crate::execution::values::Value;
    use super::Surface3D;
    use fidget::context::{Context, Tree};
    use fidget::mesh::{Octree, Settings};
    use fidget::shape::Shape;
    use fidget::render::CancelToken;

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
        assert!(result.is_ok(), "sphere to_manifold should succeed: {:?}", result);
    }

    #[test]
    fn integration_to_mesh_torus() {
        // Test to_manifold() directly on a torus SDF
        // Torus: (x² + y² + z² + R² - r²)² - 4R²(x² + y²) = 0
        // where R = major radius, r = minor radius
        let x = Tree::x();
        let y = Tree::y();
        let z = Tree::z();
        let R = Tree::constant(5.0);
        let r = Tree::constant(1.5);
        let R2 = R.clone() * R;
        let r2 = r.clone() * r;
        let x2 = x.clone() * x;
        let y2 = y.clone() * y;
        let xy_sq = x2.clone() + y2.clone();
        let dist_sq = x2 + y2 + z.clone() * z;
        let inner = dist_sq.clone() + R2.clone() - r2;
        let torus = inner.clone() * inner - Tree::constant(4.0) * R2 * xy_sq;
        let surface = Surface3D::new(torus);

        let result = surface.to_manifold();
        if let Err(ref e) = result {
            eprintln!("to_manifold torus error: {:?}", e);
        }
        assert!(result.is_ok(), "torus to_manifold should succeed: {:?}", result);
    }
}
