# Plan: Fidget Integration — Implicit Surface Modeling

## Design Decisions (Confirmed)

- **to_implicit()** on a closure returns `fidget::shape::Shape<fidget::jit::JitFunction>` (raw Fidget Shape, not a mesh)
- **to_manifold()** produces a `ManifoldMesh3D` from the Fidget Shape via Manifold Dual Contouring (3D only)
- **to_polygon()** produces a `PolygonSet` from the Fidget Shape via marching squares contour extraction (2D only)
- **2D closures supported**: single `Vector2` param → 2D implicit curve; maps `p.x`→X, `p.y`→Y
- **3D closures supported**: single `Vector3` param → 3D SDF; maps `p.x`→X, `p.y`→Y, `p.z`→Z
- **Captured values** resolved at `to_implicit()` call time as Fidget constants
- **JIT backend** for fast evaluation (`fidget::jit::JitFunction`)
- **Error reporting**: visual error pointing using AST source locations
- Fidget dependency: `git = "https://github.com/Keeter/fidget"`, features: `jit`, `mesh`, `shapes`

## User-Facing API

```ccm
# 3D implicit surface (sphere)
let sphere = (p: std.vector.Vector3) -> std.scalar.Scalar:
    p.x * p.x + p.y * p.y + p.z * p.z - 1.0
let shape = sphere::to_implicit()
let mesh = shape::to_manifold()  # produces ManifoldMesh3D (3D only)
let polygon = shape::to_polygon()  # produces PolygonSet (2D only)

# 2D implicit curve (circle) with captured values
let radius = 5.0;
let circle = (p: std.vector.Vector2) -> std.scalar.Scalar:
    p.x * p.x + p.y * p.y - radius * radius
let shape = circle::to_implicit()

# CSG via Fidget shapes library
let box = (p: std.vector.Vector3) -> std.scalar.Scalar:
    let dx = std.math.abs(p.x) - 1.0;
    let dy = std.math.abs(p.y) - 1.0;
    let dz = std.math.abs(p.z) - 1.0;
    let max_xyz = std.math.max(dx, std.math.max(dy, dz));
    let len = std.math.sqrt(max_xyz * max_xyz);
    len

# Union of two shapes (min for SDF union)
let union_shape = (p: std.vector.Vector3) -> std.scalar.Scalar:
    let s1 = sphere_closure(p);
    let s2 = box_closure(p);
    std.math.min(s1, s2)
```

## AST-to-Fidget Mapping

| Command CAD AST Node | Fidget Tree | Notes |
|---|---|---|
| `p.x` / `p.y` / `p.z` (Vec3 member access on closure param) | `Tree::x()` / `Tree::y()` / `Tree::z()` | Only when accessing the single closure parameter |
| `p.x` / `p.y` (Vec2 member access on closure param) | `Tree::x()` / `Tree::y()` | For 2D implicit curves |
| captured values (free variables from environment) | `Tree::Const(f64)` | Resolved at `to_implicit()` call time |
| Scalar literals (`1.0`, `-3.14`) | `Tree::Const(f64)` | Direct constant mapping |
| `a + b` | `tree_a + tree_b` | Overloaded `Add` |
| `a - b` | `tree_a - tree_b` | Overloaded `Sub` |
| `a * b` | `tree_a * tree_b` | Overloaded `Mul` |
| `a / b` | `tree_a / tree_b` | Overloaded `Div` |
| `-a` (negation) | `-tree_a` | Overloaded `Neg` |
| `sin(a)` | `tree_a.sin()` | Trig unary op |
| `cos(a)` | `tree_a.cos()` | Trig unary op |
| `sqrt(a)` | `tree_a.sqrt()` | Sqrt unary op |
| `abs(a)` | `tree_a.abs()` | Abs unary op |
| `a.pow(2)` or `a * a` | `tree_a.square()` or `tree_a * tree_a` | |

## Unsupported Expressions (must produce compile-time error)

The following AST node types are NOT supported in implicit surface closures:

- **Variable references** (other than the closure parameter and captured values) — e.g., `some_other_var + p.x`
- **Function calls** — e.g., `std.math.min(a, b)` (builtin functions not supported in body)
- **Method calls** — e.g., `a::some_method()`
- **Struct construction** — e.g., `std.vector.Vector3(x = 1.0, ...)`
- **Dictionary construction** — e.g., `(a = 1.0, b = 2.0)`
- **Closure definitions/calls** — nested closures not supported
- **If/else expressions** — conditional branching not supported
- **Let bindings** — no intermediate variables in body
- **For loops** — iteration not supported
- **Constraint definitions** — not applicable
- **Import statements** — not applicable
- **Struct definitions** — not applicable
- **User-defined function definitions** — not applicable

**Error message format**: `"Implicit surface expression does not support X: <node_type>"` with source location pointing to the offending AST node.

## Dimensional Analysis

Implicit surface SDFs must be **dimensionless** (pure numbers). Captured values with dimensions should produce an error:

```
Error: Captured value 'length' has dimensions [length]; implicit surfaces require dimensionless values
  --> model.ccm:5:16
   |
5  | let shape = my_closure::to_implicit()
   |                ^^^^^^^^^^^^^ captured from here
```

Captured values without dimensions (pure `Scalar` / `UInt` / `Int`) are OK — they become Fidget constants.

## Implementation Steps

### Step 1: Add Fidget Dependency

**File**: `interpreter/Cargo.toml`

Add to `[dependencies]`:

```toml
fidget = { git = "https://github.com/Keeter/fidget", features = ["jit", "mesh", "shapes"] }
```

Features explained:
- `jit` — JIT compilation backend for fast evaluation (x86_64 + aarch64)
- `mesh` — Manifold Dual Contouring mesh generation (`fidget_mesh`)
- `shapes` — Standard shape library (`fidget_shapes`)

**Verification**: `cargo check -p interpreter` should pull in the dependency and compile.

---

### Step 2: Create `interpreter/src/execution/values/fidget_converter.rs`

**NEW FILE** — AST → Fidget Tree converter.

This module handles the recursive descent conversion from Command CAD AST expressions to Fidget `Tree` objects. It is the core translation layer.

#### Module Structure

```rust
use fidget::context::Tree;
use command_cad_common::ast::{AstNode, Expression};
use crate::execution::values::{Value, ArgumentName};
use std::collections::HashMap;

/// Converts a Command CAD AST expression to a Fidget Tree.
///
/// `captured_values` maps variable names (from the closure's captured environment)
/// to their resolved f64 constant values. These become `Tree::Const` nodes.
///
/// `param_var` is the single closure parameter (Vector2 or Vector3).
/// Member accesses on this parameter (p.x, p.y, p.z) map to Fidget axis variables.
pub fn ast_to_fidget(
    node: &AstNode<Expression>,
    captured_values: &HashMap<String, f64>,
    param_dim: ParamDim,  // ParamDim::Vec2 or ParamDim::Vec3
) -> Result<Tree, FidgetConversionError>;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ParamDim {
    Vec2,
    Vec3,
}

#[derive(Debug, thiserror::Error)]
pub enum FidgetConversionError {
    #[error("Implicit surface expression does not support {0}: {1}")]
    UnsupportedExpression(String, /* source location info */),

    #[error("Captured value '{0}' has dimensions; implicit surfaces require dimensionless values")]
    DimensionalMismatch(String),

    #[error("Unknown captured variable '{0}'")]
    UnknownCapturedVariable(String),

    #[error("Parameter member access '{0}' is invalid for {1}D closure")]
    InvalidMemberAccess(String, usize),
}
```

#### Conversion Logic (recursive descent)

The `ast_to_fidget` function pattern-matches on the AST expression node type:

```rust
pub fn ast_to_fidget(
    node: &AstNode<Expression>,
    captured_values: &HashMap<String, f64>,
    param_dim: ParamDim,
) -> Result<Tree, FidgetConversionError> {
    match node.node.as_scalar_expression() {
        // --- Supported binary operations ---
        Some(expr::ScalarExpression::Binary(bin)) => {
            let left = ast_to_fidget(&bin.left, captured_values, param_dim)?;
            let right = ast_to_fidget(&bin.right, captured_values, param_dim)?;
            match bin.operator {
                BinaryOperator::Add => Ok(left + right),
                BinaryOperator::Sub => Ok(left - right),
                BinaryOperator::Mul => Ok(left * right),
                BinaryOperator::Div => Ok(left / right),
                // All other operators are unsupported
                op => Err(FidgetConversionError::UnsupportedExpression(
                    "operator", format!("{}", op)
                )),
            }
        }

        // --- Supported unary operations ---
        Some(expr::ScalarExpression::Unary(unary)) => {
            let operand = ast_to_fidget(&unary.operand, captured_values, param_dim)?;
            match unary.operator {
                UnaryOperator::Neg => Ok(-operand),
                UnaryOperator::Abs => Ok(operand.abs()),
                UnaryOperator::Sin => Ok(operand.sin()),
                UnaryOperator::Cos => Ok(operand.cos()),
                UnaryOperator::Sqrt => Ok(operand.sqrt()),
                // Other unary ops (Not, etc.) unsupported
                op => Err(FidgetConversionError::UnsupportedExpression(
                    "operator", format!("{}", op)
                )),
            }
        }

        // --- Scalar literal ---
        Some(expr::ScalarExpression::Literal(lit)) => {
            // Convert Command CAD scalar literal to f64
            let value = lit.to_f64()?;
            Ok(Tree::constant(value))
        }

        // --- Member access (p.x, p.y, p.z) ---
        Some(expr::ScalarExpression::MemberAccess(member)) => {
            // Check if this is accessing the closure parameter
            if let Some(ident) = member.object.as_identifier() {
                // This is `p.x` where `p` is the closure parameter
                match ident.as_str() {
                    "x" => match param_dim {
                        ParamDim::Vec2 | ParamDim::Vec3 => Ok(Tree::x()),
                        _ => unreachable!(),
                    },
                    "y" => match param_dim {
                        ParamDim::Vec2 | ParamDim::Vec3 => Ok(Tree::y()),
                        _ => Err(FidgetConversionError::InvalidMemberAccess(
                            "y".into(), if param_dim == ParamDim::Vec2 { 2 } else { 3 }
                        )),
                    },
                    "z" => match param_dim {
                        ParamDim::Vec3 => Ok(Tree::z()),
                        ParamDim::Vec2 => Err(FidgetConversionError::InvalidMemberAccess(
                            "z".into(), 2
                        )),
                        _ => unreachable!(),
                    },
                    _ => {
                        // Not the closure parameter — could be a captured value
                        // Check captured_values map
                        if let Some(&val) = captured_values.get(ident.as_str()) {
                            Ok(Tree::constant(val))
                        } else {
                            Err(FidgetConversionError::UnknownCapturedVariable(
                                ident.to_string()
                            ))
                        }
                    }
                }
            } else {
                // Nested member access like `obj.field.x` — unsupported
                Err(FidgetConversionError::UnsupportedExpression(
                    "member access on non-parameter".into(),
                    "complex member chain".into()
                ))
            }
        }

        // --- Variable access (captured values) ---
        Some(expr::ScalarExpression::VariableAccess(var)) => {
            let name = var.name.as_str();
            if let Some(&val) = captured_values.get(name) {
                Ok(Tree::constant(val))
            } else {
                Err(FidgetConversionError::UnknownCapturedVariable(name.into()))
            }
        }

        // --- Unsupported expression types ---
        _ => Err(FidgetConversionError::UnsupportedExpression(
            "expression".into(),
            format!("{:?}", node.node)
        )),
    }
}
```

**Key design decisions for the converter:**

1. **Only `Tree` API, not `Context`** — The `Tree` type has overloaded operators (`+`, `-`, `*`, `/`, `Neg`) that build the expression graph directly. This is simpler than using `Context` with its `ctx.add()`, `ctx.mul()` etc. and avoids the need for node deduplication at this stage.

2. **Captured values become constants** — All captured values are resolved to `f64` at `to_implicit()` call time and inserted as `Tree::Const` nodes. This means changes to captured values after `to_implicit()` won't affect the shape (which is the desired behavior — the shape is a snapshot).

3. **Parameter member access special-cased** — When the AST has `MemberAccess(object=Identifier("p"), member="x")` where `p` is the closure parameter, map to `Tree::x()`. Other member accesses are either captured values (looked up in the map) or errors.

4. **Source location preserved** — Every `FidgetConversionError` variant carries enough info to produce a user-facing error with source location pointing to the offending AST node. The `AstNode` wrapper already contains source location info.

5. **Dimensional check on captured values** — Before converting, verify that all captured values are dimensionless (`Scalar`, `UInt`, `Int`). Values with dimensions (like `Vector3` with `[length]`) produce a `DimensionalMismatch` error.

#### Captured Value Resolution (in `to_implicit()`, not in converter)

The captured values map is built in `to_implicit()` by walking the closure's `captured_values: IndexMap<ArgumentName, Value>` from the `UserClosureInternals`. Each `Value` is checked for dimensions:

```rust
fn resolve_captured_values(
    captured: &IndexMap<ArgumentName, Value>,
) -> Result<HashMap<String, f64>, FidgetConversionError> {
    let mut map = HashMap::new();
    for (name, value) in captured {
        match name {
            ArgumentName::Named(var_name) => {
                // Check dimensions — must be dimensionless
                let dimless = match value {
                    Value::ValueScalar(v) => v.dimensions.is_empty(),
                    Value::ValueUInt(v) => true,  // UInt is dimensionless
                    Value::ValueInt(v) => true,   // Int is dimensionless
                    _ => false,  // Other types not supported as captured
                };
                if !dimless {
                    return Err(FidgetConversionError::DimensionalMismatch(var_name.clone()));
                }
                let f64_val = value.to_f64();
                map.insert(var_name.to_string(), f64_val);
            }
            ArgumentName::Positional(_) => {
                // Shouldn't happen for closures — closures always use named params
                // But handle gracefully
            }
        }
    }
    Ok(map)
}
```

---

### Step 3: Add `to_implicit()` Method to `UserClosure`

**File**: `interpreter/src/execution/values/closure.rs`

Add a new method on `UserClosure`:

```rust
impl UserClosure {
    /// Convert this closure to a Fidget implicit surface shape.
    ///
    /// Validates:
    /// - Closure takes exactly one parameter (Vector2 or Vector3)
    /// - Closure returns Scalar
    /// - Body contains only supported expression types
    /// - All captured values are dimensionless
    ///
    /// Returns a wrapped Fidget Shape ready for meshing or further operations.
    pub fn to_implicit(&self, context: &ExecutionContext<'_>) -> Result<ShapeWrapper, ExecutionError> {
        use super::fidget_converter::{ast_to_fidget, ParamDim};

        // 1. Validate signature
        let (param_dim, param_name) = validate_closure_signature(self.signature())?;

        // 2. Resolve captured values from the current execution context
        let captured_map = resolve_captured_values(&self.data.captured_values)?;

        // 3. Convert AST expression to Fidget Tree
        let tree = ast_to_fidget(
            &self.data.expression,
            &captured_map,
            param_dim,
        ).map_err(|e| {
            // Attach source location from the offending AST node
            ExecutionError::ImplicitSurfaceError {
                message: e.to_string(),
                location: self.data.expression.location,  // or wherever AstNode stores it
            }
        })?;

        // 4. Build Fidget Shape from Tree with JIT backend
        let shape = build_fidget_shape(&tree, param_dim)?;

        // 5. Wrap and return
        Ok(ShapeWrapper::new(shape, param_dim))
    }
}
```

#### Signature Validation Helper

```rust
fn validate_closure_signature(
    signature: &Signature,
) -> Result<(ParamDim, ArgumentName), ExecutionError> {
    let members = &signature.members;
    if members.len() != 1 {
        return Err(ExecutionError::InvalidClosureSignature {
            message: "Implicit surface closure must take exactly one parameter".into(),
            location: /* signature location */,
        });
    }

    let (param_name, param_type) = members.iter().next().unwrap();
    let param_type_id = param_type.type_id();

    // Check if parameter type is Vector2 or Vector3
    let param_dim = if param_type_id == vector2_type_id() {
        ParamDim::Vec2
    } else if param_type_id == vector3_type_id() {
        ParamDim::Vec3
    } else {
        return Err(ExecutionError::InvalidClosureSignature {
            message: format!(
                "Parameter must be std.vector.Vector2 or std.vector.Vector3, got {}",
                param_type
            ),
            location: /* signature location */,
        });
    };

    // Check return type is Scalar
    let return_type_id = signature.return_type.type_id();
    if return_type_id != scalar_type_id() {
        return Err(ExecutionError::InvalidClosureSignature {
            message: "Implicit surface closure must return std.scalar.Scalar".into(),
            location: /* signature location */,
        });
    }

    Ok((param_dim, param_name.clone()))
}
```

#### Fidget Shape Builder Helper

```rust
fn build_fidget_shape(
    tree: &Tree,
    param_dim: ParamDim,
) -> Result<fidget::shape::Shape<fidget::jit::JitFunction>, ExecutionError> {
    use fidget::context::Context;
    use fidget::shape::Shape;

    let mut ctx = Context::new();
    let node = ctx.import(tree).map_err(|e| {
        ExecutionError::ImplicitSurfaceError {
            message: format!("Failed to build Fidget shape: {}", e),
            location: None,
        }
    })?;

    let axes = match param_dim {
        ParamDim::Vec2 => [fidget::var::Var::X, fidget::var::Var::Y],
        ParamDim::Vec3 => [fidget::var::Var::X, fidget::var::Var::Y, fidget::var::Var::Z],
    };

    Shape::new(&ctx, node, axes).map_err(|e| {
        ExecutionError::ImplicitSurfaceError {
            message: format!("Failed to create shape: {}", e),
            location: None,
        }
    })
}
```

---

### Step 4: Create `ShapeWrapper` — New Value Type

**NEW FILE**: `interpreter/src/execution/values/implicit_shape.rs`

This is the Command CAD value type that wraps a Fidget Shape. It provides:
- The `to_manifold()` method
- Integration with the Value enum and standard environment
- Mesh generation settings (depth, world-to-model transform, threads)

#### Module Structure

```rust
use std::sync::Arc;

use fidget::shape::Shape;
use fidget::jit::JitFunction;
use super::fidget_converter::ParamDim;

// Re-export existing types for use in method signatures
use crate::values::{ManifoldMesh3D, PolygonSet};

/// Command CAD value type wrapping a Fidget implicit surface shape.
///
/// Provides `to_manifold()` for mesh generation via Manifold Dual Contouring.
#[derive(Debug, Clone)]
pub struct ShapeWrapper {
    shape: Shape<JitFunction>,
    param_dim: ParamDim,
    /// Mesh generation settings (configurable by user)
    settings: MeshSettings,
}

#[derive(Debug, Clone)]
pub struct MeshSettings {
    /// Octree depth for mesh resolution (higher = more detail, slower)
    pub depth: u8,
    /// World-to-model transform (optional, for positioning/scale)
    pub world_to_model: Option<[[f32; 4]; 4]>,
    /// Number of threads for mesh generation
    pub threads: usize,
}

impl Default for MeshSettings {
    fn default() -> Self {
        Self {
            depth: 6,           // reasonable default
            world_to_model: None,
            threads: num_cpus::get(),
        }
    }
}

/// Meshing error type for implicit surface conversion failures.
#[derive(Debug, Clone)]
pub struct MeshingError(pub String);

impl ShapeWrapper {
    pub fn new(shape: Shape<JitFunction>, param_dim: ParamDim) -> Self {
        Self {
            shape,
            param_dim,
            settings: MeshSettings::default(),
        }
    }

    /// Generate a 3D manifold mesh from the implicit surface using Manifold Dual Contouring.
    ///
    /// Returns `ManifoldMesh3D` (wrapping `Arc<Manifold>` from boolmesh).
    /// For 2D shapes, use `to_polygon()` instead.
    pub fn to_manifold(&self) -> Result<ManifoldMesh3D, MeshingError> {
        if self.param_dim != ParamDim::Vec3 {
            return Err(MeshingError("to_manifold() requires a 3D closure (Vector3 parameter)".into()));
        }
        self.to_manifold_3d()
    }

    /// Generate a 2D polygon set from the implicit curve using marching squares.
    ///
    /// Returns `PolygonSet` (wrapping `Arc<geo::MultiPolygon>`).
    /// For 3D shapes, use `to_manifold()` instead.
    pub fn to_polygon(&self) -> Result<PolygonSet, MeshingError> {
        if self.param_dim != ParamDim::Vec2 {
            return Err(MeshingError("to_polygon() requires a 2D closure (Vector2 parameter)".into()));
        }
        self.to_polygon_impl()
    }

    fn to_manifold_3d(&self) -> Result<ManifoldMesh3D, MeshingError> {
        use fidget::mesh::{Octree, Settings};

        let settings = Settings {
            depth: self.settings.depth as u32,
            world_to_model: self.settings.world_to_model.map(|m| {
                // Convert [[f32; 4]; 4] to fidget's expected transform type
                // fidget uses glam::Mat4 or similar
                todo!("convert transform matrix")
            }),
            threads: self.settings.threads,
            cancel: &fidget::mesh::Cancel::default(),
        };

        let octree = Octree::build(&self.shape, &settings)
            .map_err(|e| MeshingError(e.to_string()))?;

        let mesh = octree.walk_dual();

        // Convert fidget_mesh::Mesh to boolmesh::Manifold:
        // fidget_mesh::Mesh has vertices: Vec<Vector3<f32>> and triangles: Vec<Vector3<usize>>
        // boolmesh::Manifold is constructed from positions/halfedges
        let manifold = convert_fidget_mesh_to_manifold(mesh);

        Ok(ManifoldMesh3D(Arc::new(manifold)))
    }

    fn to_polygon_impl(&self) -> Result<PolygonSet, MeshingError> {
        use fidget::eval::{BulkEvaluator, Function};

        // Determine bounding box from world_to_model transform or use default
        let (origin_x, origin_y, cell_size) = self.compute_grid_params();

        // Grid resolution based on mesh settings depth
        let grid_width = (1.0 / cell_size) as usize;
        let grid_height = (1.0 / cell_size) as usize;

        // Evaluate SDF on the grid using JIT bulk evaluator
        let mut eval = self.shape.f().new_float_slice_eval();
        let mut grid = vec![0.0f32; grid_width * grid_height];

        for iy in 0..grid_height {
            for ix in 0..grid_width {
                let x = origin_x + ix as f32 * cell_size;
                let y = origin_y + iy as f32 * cell_size;
                // JIT evaluates f(x, y) → SDF value
                grid[iy * grid_width + ix] = self.evaluate_at(&mut eval, x, y);
            }
        }

        // Marching squares: extract zero-contour line segments
        let segments = marching_squares(&grid, grid_width, grid_height, origin_x, origin_y, cell_size);

        // Connect segments into closed loops
        let mut loops: Vec<Vec<[f32; 2]>> = connect_segments_into_loops(segments);

        // Classify loops as outer (CCW, positive area) or hole (CW, negative area)
        let mut exterior_coords: Option<Vec<geo::Coord<f32>>> = None;
        let mut interior_coords: Vec<Vec<geo::Coord<f32>>> = Vec::new();

        for loop_vertices in loops {
            let area = signed_area_2d(&loop_vertices);
            if area > 0.0 {
                // Counter-clockwise → outer boundary (first one wins)
                if exterior_coords.is_none() {
                    exterior_coords = Some(loop_vertices.into_iter().map(|[x, y]| geo::Coord { x, y }).collect());
                }
            } else {
                // Clockwise → hole
                interior_coords.push(loop_vertices.into_iter().map(|[x, y]| geo::Coord { x, y }).collect());
            }
        }

        let exterior = exterior_coords.ok_or_else(|| MeshingError("No outer contour found".into()))?;
        let polygon = geo::Polygon::new(geo::LineString(exterior), interior_coords);
        let multi_polygon = geo::MultiPolygon(vec![polygon]);

        Ok(PolygonSet(Arc::new(multi_polygon)))
    }

    fn compute_grid_params(&self) -> (f32, f32, f32) {
        // Use world_to_model transform to determine bounding box and cell size.
        // Default: evaluate over [-10, 10] x [-10, 10] at resolution derived from depth.
        let depth = self.settings.depth as u32;
        // Cell size decreases with octree depth: similar to 3D mesh resolution
        let cell_size = (20.0 / (1u64 << depth) as f32).max(0.001);
        (0.0, 0.0, cell_size)
    }

    fn evaluate_at(&self, eval: &mut impl BulkEvaluator<Data = f32>, x: f32, y: f32) -> f32 {
        // Evaluate the JIT-compiled shape function at (x, y, z=0).
        // For 2D shapes, the Z axis is unused.
        let input = [x, y, 0.0];
        let mut output = [0.0f32; 1];
        eval.eval(&input, &mut output);
        output[0]
    }
}
```

#### Marching Squares Implementation

Helper functions for 2D contour extraction (in `implicit_shape.rs` or a new `marching_squares.rs` module):

```rust
/// A line segment extracted by marching squares.
#[derive(Debug, Clone)]
struct Segment {
    start: [f32; 2],
    end: [f32; 2],
}

/// Run marching squares on a 2D SDF grid to extract zero-contour segments.
///
/// `grid` is row-major (iy * width + ix). Each cell contains the SDF value at that grid point.
/// Marching squares classifies each cell by the sign of its 4 corners and looks up
/// the corresponding edge intersections from a case table (16 cases).
fn marching_squares(
    grid: &[f32],
    width: usize,
    height: usize,
    origin_x: f32,
    origin_y: f32,
    cell_size: f32,
) -> Vec<Segment> {
    const CASE_TABLE: [[(usize, usize); 4]; 16] = [
        // Case 0: all negative → no edges
        [(0, 0), (0, 0), (0, 0), (0, 0)],
        // Case 1: bottom-left negative → edge between bottom and left
        [(0, 1), (0, 0), (0, 0), (0, 0)],
        // Case 2: bottom-right negative → edge between bottom and right
        [(1, 0), (0, 0), (0, 0), (0, 0)],
        // Case 3: bottom + bottom-right negative → edge bottom→right
        [(1, 0), (0, 1), (0, 0), (0, 0)],
        // Case 4: top-left negative → edge between top and left
        [(0, 0), (0, 0), (0, 1), (0, 0)],
        // Case 5: saddle point (diagonal) → two edges (crossing)
        //   Connects (left→top) and (bottom→right) OR (bottom→right) and (left→top)
        //   Ambiguity resolved by checking midpoints or using topology-aware resolution
        [(0, 1), (1, 0), (0, 1), (1, 0)],
        // Case 6: bottom + top-left negative → edge left→right
        [(1, 0), (0, 1), (0, 0), (0, 0)],
        // Case 7: bottom + top-left + top-right negative → edges bottom→left, left→top
        [(0, 1), (0, 1), (0, 0), (0, 0)],
        // Case 8: top-left negative (mirror of case 4)
        [(0, 0), (0, 0), (0, 1), (0, 0)],
        // Case 9: top-left + bottom-right negative → saddle (mirror of case 5)
        [(0, 1), (1, 0), (0, 1), (1, 0)],
        // Case 10: top-left + bottom-left + bottom-right negative
        [(1, 0), (0, 0), (0, 1), (0, 0)],
        // Case 11: top-left + bottom-left negative → edge bottom→top
        [(0, 0), (0, 0), (0, 1), (1, 0)],
        // Case 12: top-right negative (mirror of case 2)
        [(1, 0), (0, 0), (0, 0), (0, 0)],
        // Case 13: top-right + bottom-right negative → edge bottom→top
        [(0, 1), (0, 0), (0, 0), (0, 0)],
        // Case 14: top-right + top-left negative → edge top→right
        [(0, 0), (0, 0), (0, 1), (0, 0)],
        // Case 15: all positive → no edges
        [(0, 0), (0, 0), (0, 0), (0, 0)],
    ];

    // Edge midpoint indices: 0=bottom, 1=right, 2=top, 3=left
    let edge_points = |ix: usize, iy: usize, edge: usize| -> Option<[f32; 2]> {
        let (ex, ey) = match edge {
            0 => (ix + 1, iy),       // bottom edge midpoint
            1 => (ix + 1, iy + 1),   // right edge midpoint
            2 => (ix, iy + 1),       // top edge midpoint
            3 => (ix, iy),           // left edge midpoint
            _ => return None,
        };
        if ex >= width || ey >= height { return None; }

        // Linear interpolation between corner values to find exact zero-crossing
        let (v0, v1) = match edge {
            0 => (grid[iy * width + ix], grid[iy * width + ix + 1]),
            1 => (grid[iy * width + ix + 1], grid[(iy + 1) * width + ix + 1]),
            2 => (grid[(iy) * width + ix], grid[(iy + 1) * width + ix]),
            3 => (grid[iy * width + ix], grid[(iy) * width + ix]), // left = same column
            _ => return None,
        };

        let t = if (v1 - v0).abs() < f32::EPSILON {
            0.5
        } else {
            -v0 / (v1 - v0)
        }.clamp(0.0, 1.0);

        let x = origin_x + (ix as f32 + t * if edge == 0 || edge == 3 { 0.0 } else { 0.5 } + if edge == 1 || edge == 2 { 0.5 } else { 0.0 }) * cell_size;
        // Simpler: interpolate directly
        let x = match edge {
            0 | 3 => origin_x + (ix as f32 + t) * cell_size,
            1 | 2 => origin_x + (ix as f32 + 1.0) * cell_size,
            _ => return None,
        };
        let y = match edge {
            0 | 3 => origin_y + iy as f32 * cell_size,
            1 | 2 => origin_y + (iy as f32 + t) * cell_size,
            _ => return None,
        };

        // Correct interpolation:
        let x = match edge {
            0 => origin_x + (ix as f32 + t) * cell_size,
            1 => origin_x + (ix as f32 + 1.0) * cell_size,
            2 => origin_x + (ix as f32 + t) * cell_size,
            3 => origin_x + ix as f32 * cell_size,
            _ => return None,
        };
        let y = match edge {
            0 => origin_y + iy as f32 * cell_size,
            1 => origin_y + (iy as f32 + t) * cell_size,
            2 => origin_y + (iy as f32 + 1.0) * cell_size,
            3 => origin_y + (iy as f32 + t) * cell_size,
            _ => return None,
        };

        Some([x, y])
    };

    let mut segments = Vec::new();

    for iy in 0..height.saturating_sub(1) {
        for ix in 0..width.saturating_sub(1) {
            let v_bl = grid[iy * width + ix];         // bottom-left
            let v_br = grid[iy * width + ix + 1];     // bottom-right
            let v_tr = grid[(iy + 1) * width + ix + 1]; // top-right
            let v_tl = grid[(iy + 1) * width + ix];   // top-left

            // Build case index: bit 0=BL, 1=BR, 2=TR, 3=TL (negative = 1)
            let case = if v_bl < 0.0 { 1 } else { 0 }
                     | if v_br < 0.0 { 2 } else { 0 }
                     | if v_tr < 0.0 { 4 } else { 0 }
                     | if v_tl < 0.0 { 8 } else { 0 };

            let edges = &CASE_TABLE[case as usize];

            // Extract non-zero edges
            let mut points = Vec::new();
            for &edge in edges.iter() {
                if edge != (0, 0) {
                    if let Some(pt) = edge_points(ix, iy, edge.0) {
                        points.push(pt);
                    }
                }
            }

            // Create segment(s) from intersection points
            match points.len() {
                2 => {
                    segments.push(Segment { start: points[0], end: points[1] });
                }
                4 => {
                    // Saddle point: two separate segments
                    segments.push(Segment { start: points[0], end: points[1] });
                    segments.push(Segment { start: points[2], end: points[3] });
                }
                _ => {}
            }
        }
    }

    segments
}

/// Connect line segments into closed loops.
///
/// Strategy:
/// 1. Build an adjacency graph from segment endpoints (snap nearby points together)
/// 2. Find all Eulerian circuits (each vertex has even degree in a valid contour)
/// 3. Each circuit is one closed loop
fn connect_segments_into_loops(segments: Vec<Segment>) -> Vec<Vec<[f32; 2]>> {
    use std::collections::HashMap;

    // Snap points together (within snap tolerance) to build adjacency
    let snap_tol = 1e-4;
    let mut point_map: HashMap<(i32, i32), [f32; 2]> = HashMap::new();
    let mut snap_key = |pt: &[f32; 2]| -> (i32, i32) {
        ((pt[0] / snap_tol).round() as i32, (pt[1] / snap_tol).round() as i32)
    };

    // Build adjacency list: key → list of keys
    let mut adj: HashMap<(i32, i32), Vec<(i32, i32)>> = HashMap::new();
    let mut stored_points: HashMap<(i32, i32), [f32; 2]> = HashMap::new();

    for seg in &segments {
        let k1 = snap_key(&seg.start);
        let k2 = snap_key(&seg.end);
        stored_points.entry(k1).or_insert(seg.start);
        stored_points.entry(k2).or_insert(seg.end);
        adj.entry(k1).or_default().push(k2);
        adj.entry(k2).or_default().push(k1);
    }

    // Find Eulerian circuits using Hierholzer's algorithm
    let mut loops = Vec::new();
    let mut remaining_edges: HashMap<(i32, i32), Vec<(i32, i32)>> = adj.clone();

    while let Some(start_key) = remaining_edges.keys().next().cloned() {
        let mut circuit = vec![start_key];
        let mut current = start_key;

        loop {
            let neighbors = remaining_edges.get_mut(&current);
            match neighbors {
                Some(list) if !list.is_empty() => {
                    let next = list.pop().unwrap();
                    // Remove reverse edge
                    if let Some(reverse_list) = remaining_edges.get_mut(&next) {
                        reverse_list.retain(|&k| k != current);
                    }
                    circuit.push(next);
                    current = next;
                }
                _ => break,
            }
        }

        // Convert keys back to float points
        let loop_pts: Vec<[f32; 2]> = circuit.iter()
            .filter_map(|k| stored_points.get(k).copied())
            .collect();
        if loop_pts.len() >= 3 {
            loops.push(loop_pts);
        }
    }

    loops
}

/// Compute signed area of a 2D polygon using the shoelace formula.
/// Positive = counter-clockwise (outer boundary), negative = clockwise (hole).
fn signed_area_2d(vertices: &[[f32; 2]]) -> f32 {
    let mut area = 0.0f32;
    let n = vertices.len();
    for i in 0..n {
        let j = (i + 1) % n;
        area += vertices[i][0] * vertices[j][1];
        area -= vertices[j][0] * vertices[i][1];
    }
    area / 2.0
}

/// Convert a Fidget mesh (vertices + triangles) to a boolmesh Manifold.
///
/// Fidget's `ManifoldDualContouring` produces `Mesh { vertices, triangles }` where
/// `triangles` are `Vector3<usize>` indices into `vertices`.
/// boolmesh's `Manifold` is constructed from positions and halfedge topology.
fn convert_fidget_mesh_to_manifold(
    mesh: fidget::mesh::Mesh,
) -> boolmesh::prelude::Manifold {
    use boolmesh::prelude::*;

    // Extract positions
    let positions: Vec<Point3> = mesh
        .vertices
        .iter()
        .map(|v| Point3::new(v.x as f64, v.y as f64, v.z as f64))
        .collect();

    // Extract triangles as halfedge indices
    // boolmesh expects halfedge indexing; convert triangle indices to halfedges
    let triangles: Vec<[usize; 3]> = mesh
        .triangles
        .iter()
        .map(|t| [t.x as usize, t.y as usize, t.z as usize])
        .collect();

    // Build halfedge topology from triangle indices
    // Each triangle vertex i has halfedge at halfedge_index[i]
    let mut halfedges: Vec<Halfedge> = Vec::new();
    let mut face_halfedges: Vec<Halfedge> = Vec::new();

    for tri in &triangles {
        let he0 = Halfedge::new();
        let he1 = Halfedge::new();
        let he2 = Halfedge::new();
        halfedges.push(he0);
        halfedges.push(he1);
        halfedges.push(he2);
        face_halfedges.push(he0);

        // Set next edges (cyclic within triangle)
        halfedges[he0.index()].next = he1;
        halfedges[he1.index()].next = he2;
        halfedges[he2.index()].next = he0;

        // Set opposite edges (to be filled in post-processing)
        halfedges[he0.index()].opposite = Halfedge::null();
        halfedges[he1.index()].opposite = Halfedge::null();
        halfedges[he2.index()].opposite = Halfedge::null();
    }

    // Find opposite edges by matching shared edges
    // This is a simplified approach — production code would use a hash map
    let mut edge_map: std::collections::HashMap<(usize, usize), usize> = std::collections::HashMap::new();
    for (face_idx, tri) in triangles.iter().enumerate() {
        for i in 0..3 {
            let v0 = tri[i];
            let v1 = tri[(i + 1) % 3];
            let he_idx = face_idx * 3 + i;
            edge_map.insert((v0, v1), he_idx);
        }
    }

    for (key, he_idx) in &edge_map {
        let reverse_key = (key.1, key.0);
        if let Some(&reverse_he_idx) = edge_map.get(&reverse_key) {
            if *he_idx < reverse_he_idx {
                halfedges[*he_idx].opposite = halfedges[*reverse_he_idx].opposite;
                halfedges[*reverse_he_idx].opposite = halfedges[*he_idx].opposite;
            }
        }
    }

    // Build the Manifold (simplified — actual implementation would need proper topology)
    // For now, return a placeholder; the real implementation needs careful halfedge setup
    // This is a TODO — the actual conversion requires matching boolmesh's Manifold API
    todo!("Implement full fidget mesh → boolmesh Manifold conversion")
}
```

**Key design decisions for 2D meshing:**

1. **Marching squares over grid evaluation** — Evaluate the SDF on a regular grid, then extract the zero-contour. Grid resolution is derived from `MeshSettings.depth`, matching the 3D octree depth semantics.

2. **Linear interpolation at zero-crossings** — Edge intersection points are interpolated between corner values for sub-cell accuracy, not just snapped to cell edges.

3. **Segment connection via adjacency graph** — Snapped segment endpoints form an adjacency graph. Hierholzer's algorithm finds Eulerian circuits, each representing a closed loop.

4. **Outer/hole classification via signed area** — Counter-clockwise loops (positive shoelace area) are outer boundaries; clockwise loops (negative area) are holes. This is the standard convention for polygon representations with holes.

5. **Saddle point ambiguity** — Cases 5 and 9 (diagonal saddle points) produce crossing edges. The simple approach creates two independent segments. For topologically correct resolution, a midpoint-checking approach could determine which pairing is correct based on the sign at the cell center.

6. **Output as `PolygonSet`** — 2D results use the existing `PolygonSet` type (wrapping `Arc<geo::MultiPolygon>`), consistent with other polygon operations in the codebase.

**Key design decisions:**

1. **`ShapeWrapper` is a new `Value` variant** — It wraps `Shape<JitFunction>` and implements the same traits as other value types (`Object`, `StaticType`, etc.) so it can be used in expressions, stored in dictionaries, passed to functions, etc.

2. **`to_manifold()` returns `ManifoldMesh3D`** — Uses existing `ManifoldMesh3D` type (wrapping `Arc<Manifold>` from boolmesh). 3D shapes run Manifold Dual Contouring; the Fidget mesh is converted to a boolmesh::Manifold.

3. **Mesh settings are configurable** — The `MeshSettings` struct allows the user to control octree depth, world-to-model transform, and thread count. Default values are reasonable for most use cases. Depth controls both 3D octree resolution and 2D grid resolution.

4. **Error handling** — Meshing errors return `Result<T, MeshingError>` (Result type) rather than panicking. This allows the CLI/GUI to display errors gracefully.

#### Integrating `ShapeWrapper` into the Value System

Add `ShapeWrapper` as a new variant in the `Value` enum (or as an `Object` type):

```rust
// In values/mod.rs or values/value.rs
pub enum Value {
    // ... existing variants ...
    ValueImplicitShape(ShapeWrapper),
}
```

Or, if following the existing pattern where complex types use `Object`:

```rust
// Add to the Object enum or trait implementation
pub enum Object {
    // ... existing variants ...
    ImplicitShape(ShapeWrapper),
}
```

The exact integration point depends on how the existing value system is structured. The key is that `ShapeWrapper` must:
- Be constructible from the result of `UserClosure::to_implicit()`
- Be callable with `::to_manifold()` method syntax (i.e., registered in the standard environment)
- Be storable in dictionaries and passable between functions

---

### Step 5: Register `to_implicit()` and `to_manifold()` in Standard Environment

**File**: `interpreter/src/execution/standard_environment.rs` (or equivalent)

Register the methods on the appropriate types:

```rust
// On UserClosure type (or its runtime representation):
register_method("to_implicit", |self_obj, _args| {
    let closure = self_obj.as_closure()?;
    // Need access to ExecutionContext — may require method signature change
    let shape = closure.to_implicit(context)?;
    Ok(Value::ValueImplicitShape(shape))
});

// On ValueImplicitShape / ShapeWrapper:
register_method("to_manifold", |self_obj, _args| {
    let shape = self_obj.as_implicit_shape()?;
    let mesh = shape.to_manifold();
    // Convert to ManifoldMesh3D (existing type)
    let mesh: ManifoldMesh3D = shape.to_manifold().map_err(|e| {
        ExecutionError::MeshingError { message: e.0, location: None }
    })?;
    Ok(mesh.into())
});
```

**Note**: The exact registration mechanism depends on how the standard environment defines methods. Methods on `UserClosure` may need a different registration path than struct methods, since closures are first-class values but not structs.

---

### Step 6: Update README

**File**: `README.md`

Move "Implicit surface modeling" from the **Planned** section to the **Features** section. Update the description:

```markdown
## Features

...

- **Implicit surface modeling** — Define shapes as mathematical closures
  `(p: std.vector.Vector3) -> std.scalar.Scalar` and convert to meshes
  via `to_implicit()` / `to_manifold()`. Supports 3D SDFs with Manifold
  Dual Contouring. Captured values allow parameterized shapes.

...
```

Remove from **Planned** section:
```markdown
- ~~Implicit surface modeling~~ — IMPLEMENTED
```

---

## Files Modified/Created (Complete List)

| File | Changes |
|------|---------|
| `interpreter/Cargo.toml` | Add `fidget` dependency with `jit`, `mesh`, `shapes` features |
| `interpreter/src/execution/values/fidget_converter.rs` | **NEW**: AST → Fidget Tree converter module |
| `interpreter/src/execution/values/implicit_shape.rs` | **NEW**: `ShapeWrapper`, `MeshSettings`, marching squares helpers, mesh conversion functions |
| `interpreter/src/execution/values/closure.rs` | Add `to_implicit()` method on `UserClosure` |
| `interpreter/src/execution/values/mod.rs` | Export new modules (`fidget_converter`, `implicit_shape`) |
| `interpreter/src/execution/values/value.rs` (or `value_type.rs`) | Add `ValueImplicitShape(ShapeWrapper)` variant to `Value` enum |
| `interpreter/src/execution/standard_environment.rs` | Register `to_implicit()` and `to_manifold()` methods |
| `interpreter/src/execution/mod.rs` | Wire up new types in test infrastructure if needed |
| `README.md` | Move "Implicit surface modeling" from Planned to Features |

---

## Error Reporting

All errors include source location pointing to the relevant AST node:

| Error Type | Message Format | Location Source |
|---|---|---|
| Wrong signature | `"Closure must take exactly one std.vector.Vector2 or std.vector.Vector3 parameter and return std.scalar.Scalar"` | Closure signature definition |
| Unsupported expression | `"Implicit surface expression does not support X: <node_type>"` | Offending AST node |
| Dimensional mismatch | `"Captured value 'x' has dimensions [length]; implicit surfaces require dimensionless values"` | Captured value usage site |
| Unknown variable | `"Unknown captured variable 'x'"` | Variable reference AST node |
| Invalid member access | `"Parameter member access 'z' is invalid for 2D closure"` | Member access AST node |
| Meshing failure | `"Meshing failed: <fidget error>"` | N/A (runtime error) |

---

## Testing Strategy

### Unit Tests (in `fidget_converter.rs`)

```rust
#[test]
fn convert_simple_addition() {
    // p.x + p.y → Tree::x() + Tree::y()
}

#[test]
fn convert_sphere_equation() {
    // p.x*p.x + p.y*p.y + p.z*p.z - 1.0 → valid Tree
}

#[test]
fn convert_with_captured_constant() {
    // radius captured as 5.0 → p.x*p.x + p.y*p.y - 25.0
}

#[test]
fn reject_function_call() {
    // std.math.min(a, b) → UnsupportedExpression error
}

#[test]
fn reject_if_expression() {
    // if condition { a } else { b } → UnsupportedExpression error
}

#[test]
fn reject_dimensional_captured_value() {
    // Captured Vector3 with dimensions → DimensionalMismatch error
}

#[test]
fn reject_z_access_on_2d_closure() {
    // (p: Vector2) -> ... p.z → InvalidMemberAccess error
}
```

### Integration Tests (in `execution/mod.rs` or new test file)

```rust
#[test]
fn implicit_surface_sphere_3d() {
    // let s = (p: std.vector.Vector3) -> std.scalar.Scalar: p.x*p.x + p.y*p.y + p.z*p.z - 1.0
    // let shape = s::to_implicit()
    // let mesh = shape::to_manifold()
    // Verify mesh has vertices and triangles
}

#[test]
fn implicit_surface_circle_2d() {
    // let r = 5.0;
    // let c = (p: std.vector.Vector2) -> std.scalar.Scalar: p.x*p.x + p.y*p.y - r*r
    // let shape = c::to_implicit()
    // let polygon = shape::to_polygon()
    // Verify result is PolygonSet with one polygon (the circle)
    // Verify outer loop has >= 3 vertices and positive signed area
}

#[test]
fn implicit_surface_with_captured_values() {
    // let radius = 3.0;
    // let s = (p: std.vector.Vector3) -> std.scalar.Scalar: p.x*p.x + p.y*p.y + p.z*p.z - radius*radius
    // let shape = s::to_implicit()
    // Verify captured value is resolved as constant
}

#[test]
fn implicit_surface_wrong_signature_error() {
    // let bad = (a: UInt, b: UInt) -> UInt: a + b
    // bad::to_implicit() → error
}

#[test]
fn implicit_surface_unsupported_expression_error() {
    // let bad = (p: std.vector.Vector3) -> std.scalar.Scalar: if true { 1.0 } else { 2.0 }
    // bad::to_implicit() → error
}
```

### Verification Commands

```bash
# Compile check
cargo check -p interpreter

# Run interpreter tests (single-threaded to avoid flaky ordering)
cargo test -p interpreter -- --test-threads=1

# Full workspace test
cargo test --all-features

# Clippy
cargo clippy --all-features
```

---

## Execution Order

1. Add `fidget` dependency to `Cargo.toml` ✅ (planned)
2. Create `fidget_converter.rs` — AST → Tree converter with full test coverage ✅ (planned)
3. Create `implicit_shape.rs` — `ShapeWrapper`, `MeshSettings`, marching squares, mesh generation ✅ (planned)
4. Add `to_implicit()` to `UserClosure` in `closure.rs` ✅ (planned)
5. Wire `ShapeWrapper` into `Value` enum and standard environment ✅ (planned)
6. Update README ✅ (planned)
7. Full integration test: `cargo test --all-features` + `cargo clippy` ✅ (planned)

---

## Open Questions / Future Work

1. **2D meshing saddle point ambiguity** — Marching squares cases 5/9 (diagonal saddles) create crossing edges. A midpoint-checking approach would resolve which pairing is topologically correct based on the cell center sign. Currently uses the simple two-segment approach.
2. **Mesh export formats** — Currently only extracts vertices/triangles as raw data. Could add STL/OBJ/PLY export for 3D meshes, and SVG/DXF/WKT export for 2D polygons.
3. **Shape transforms** — Fidget supports `remap_xyz`, `remap_affine` on Trees. Could expose as methods on `ShapeWrapper`.
4. **CSG operations** — Fidget's `shapes` crate provides union/intersection/difference. Could add these as methods on `ShapeWrapper` (for 3D) or on `Polygon2D` (for 2D polygon booleans).
5. **Performance tuning** — Default octree depth of 6 may be too coarse or too fine for some use cases. Could add a `set_resolution(depth)` method. For 2D, grid resolution could be adaptive (refine near zero-contour).
6. **Error recovery** — Currently `to_manifold()` returns `Result<ManifoldMesh3D, MeshingError>`. Could integrate with CLI error display system for better UX.
7. **2D polygon boolean operations** — Once we have `Polygon2D` values, implement union/intersect/difference on polygons (not just implicit surfaces). Useful for 2D CAD workflows before extrusion.
8. **Bounding box estimation** — Currently uses a fixed [-10, 10] x [-10, 10] grid for 2D evaluation. Could auto-detect the bounding box by sampling the SDF and finding where it crosses zero.
