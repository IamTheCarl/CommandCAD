# Fidget 0.4.3 vs Command CAD CCM — Feature Gap Analysis

## Dependency

- **Fidget version**: `0.4.3` (features enabled: `jit`, `mesh`; disabled: `shapes`, `raster`, `solver`, `bytecode`, `rhai`, `gui`)

---

## 1. Built-in Shapes

| Fidget Shape | CCM Exposed | Notes |
|---|---|---|
| `Sphere` (radius) | **Yes** | `std.implicits.sphere(radius/diameter)` |
| `Box` (lower/upper corners) | **Partial** | `std.implicits.cube(size)` — only centered, equal-sized. No arbitrary corner-based box. |
| `Circle` (2D, center + radius) | **No** | No dedicated 2D circle primitive. Must hand-write SDF in a closure. |
| `Rectangle` (lower/upper corners) | **No** | No dedicated 2D rectangle primitive. |
| `Plane` (half-space) | **No** | Not exposed as a named primitive. |
| Custom SDF via closure | **Yes** | `closure::to_implicit()` for both 2D and 3D |

CCM custom shapes not in fidget: `Cylinder`, `Cone`, `Torus` — hand-written SDFs in CCM, which is fine since fidget_shapes is minimal by design.

---

## 2. Boolean / CSG Operations

| Fidget Operation | CCM Exposed | Notes |
|---|---|---|
| `Union` (N-ary) | **Partial** | Binary only (`a::union(b)`). No N-ary variant. |
| `Intersection` (N-ary) | **Partial** | Binary only (`a::intersection(b)`). No N-ary variant. |
| `Difference` (binary) | **Yes** | `a::difference(b)` on both Surface2D and Surface3D |
| `Inverse` (negation) | **No** | Not exposed. Could be `surface::inverse()` or `surface::negate()`. |
| `Blend` (smooth quadratic blend) | **No** | Smooth/organic transition between two shapes. Significant feature for organic modeling. |
| Symmetric difference (XOR) | **Yes** | `a::symmetric_difference(b)` — CCM implements this but it's not a dedicated fidget type |

---

## 3. Transforms

| Fidget Transform | CCM Exposed | Notes |
|---|---|---|
| `Move` (translation dx, dy, dz) | **No** | |
| `Rotate` (arbitrary axis + center + angle) | **No** | |
| `RotateX` / `RotateY` / `RotateZ` | **No** | |
| `Scale` (non-uniform sx, sy, sz) | **No** | |
| `ScaleUniform` (single factor) | **No** | |
| `Reflect` / `ReflectX` / `ReflectY` / `ReflectZ` | **No** | |
| `Tree::remap_affine` (4x4 matrix) | **Internal only** | Used internally in `to_manifold()` for bbox scaling, but not exposed to CCM. |
| `Tree::remap_xyz` (arbitrary axis remap) | **Internal only** | Used internally for slicing, but not exposed to CCM. |

**This is the biggest gap.** No way to translate, rotate, scale, or reflect an implicit surface from CCM code.

---

## 4. 2D-to-3D Operations

| Fidget Operation | CCM Exposed | Notes |
|---|---|---|
| `ExtrudeZ` (extrude XY shape along Z) | **No** | |
| `LoftZ` (loft between two XY shapes along Z) | **No** | |
| `RevolveY` (revolve 2D profile about Y axis) | **No** | |

These are powerful operations for turning 2D sketches into 3D solids.

---

## 5. Other Fidget Features

| Feature | CCM Exposed | Notes |
|---|---|---|
| `Tree::deriv(var)` — symbolic differentiation | **No** | Useful for normals, sensitivity analysis. |
| `Shape::simplify(trace)` — interval-based simplification | **No** | Optimizes expression trees. |
| `fidget::raster` — 2D image rendering | **No** | Feature not even enabled in Cargo.toml. |
| `fidget::raster` — 3D voxel rendering | **No** | Feature not even enabled. |
| `fidget::solver` — constraint solving / least-squares | **No** | Feature not even enabled. |
| `fidget::bytecode` — serialization | **No** | Feature not even enabled. |
| N-ary `Union`/`Intersection` (variadic) | **No** | Fidget supports `Union { shapes: [Tree] }` with any number of inputs. CCM is binary-only. |

---

## Summary: What's Missing from CCM (Prioritized)

### High Impact (core modeling operations)

1. **Transforms** — translate, rotate (X/Y/Z/arbitrary), scale (uniform/non-uniform), reflect. These are essential for positioning primitives and composing complex models.
2. **Blend** — smooth organic transitions between shapes. Unique to fidget, very useful for rounded/organic design.
3. **Inverse** — flip inside/outside of a shape (`-tree` works at the Tree level but no CCM method).
4. **ExtrudeZ / RevolveY** — turn 2D profiles into 3D solids. Fundamental CAD operations.
5. **LoftZ** — blend between two 2D profiles along Z.

### Medium Impact (convenience & power)

6. **N-ary union/intersection** — `union([a, b, c, d])` instead of nested binary calls.
7. **2D primitives** — `circle(radius)`, `rectangle(x1, y1, x2, y2)` as named functions.
8. **Plane** — half-space primitive for clipping/bounding.
9. **Arbitrary Box** — `box(min, max)` with independent corner coordinates (current `cube` is size-only, centered).

### Lower Impact (advanced/specialized)

10. **Symbolic differentiation** (`deriv`) — for analytical normals.
11. **Shape simplification** — interval-based optimization.
12. **Raster rendering** — 2D bitmap output from implicit surfaces.
13. **Constraint solving** — inverse modeling / parameter fitting.
