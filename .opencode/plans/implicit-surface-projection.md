# Implicit Surface Projection — Future Work

## Goal
Compute the silhouette (orthographic projection) of a 3D implicit surface onto a 2D plane, producing a `PolygonSet` that outlines the visible boundary of the projected shape.

## Approaches Explored

### 1. Exact Symbolic Projection (Rejected)
Eliminate one coordinate via polynomial resultant.

- For `f(x,y,z) = 0`, silhouette is `Res_z(f, ∂f/∂z) = 0`
- Computed as determinant of Sylvester matrix `(2d-1) × (2d-1)` where `d` = degree in z

**Why rejected**: coefficient sizes grow factorially with degree `d`. Practical only for degree ≤ 4. Fidget uses JIT computational graphs, not symbolic polynomials, so no symbolic form is available.

**References**:
- mkeeter/fidget — no projection/silhouette functionality, no plans to add it
- Macaulay2, Singular — computer algebra systems that can compute resultants, but not suitable for runtime CAD

### 2. Sampling-Based Contour Extraction (Recommended)
Sample the implicit function on a grid, then extract silhouette edges from the sampled data.

**Algorithm outline**:
1. Choose projection plane (e.g., xy-plane for top-down view)
2. For each pixel `(x,y)` in the target resolution:
   - Solve `f(x,y,z) = 0` for z using root-finding (e.g., bisection, Newton)
   - Check if `∂f/∂z ≈ 0` at the root (silhouette condition: surface normal is parallel to view direction)
   - Alternatively: find the extremal z value where `f(x,y,z) = 0` (max or min z along the ray)
3. Extract contour from the resulting 2D scalar field using marching squares

**Key insight**: the silhouette boundary occurs where the ray `z → f(x₀,y₀,z)` is tangent to the surface, i.e., where the equation has a double root (`f = 0` and `∂f/∂z = 0` simultaneously).

**Implementation options**:
- **Option A**: For each `(x,y)`, find the max z such that `f(x,y,z) = 0`. The silhouette is the boundary of the region where real roots exist.
- **Option B**: Compute discriminant-like quantity: for each `(x,y)`, evaluate whether `f(x,y,z)` has real roots in z. The silhouette is the zero-level set of the discriminant.
- **Option C**: Ray march along z-axis, find all intersections. Silhouette = boundary of the union of intersection intervals.

**Pros**: works with any fidget tree (no symbolic form needed), numerically stable, resolution-controllable
**Cons**: approximation, resolution-dependent, slower than exact method for low-degree surfaces

### 3. Analytic Silhouette via Gradient (Partial)
Use `f(x,y,z) = 0` and `∂f/∂z(x,y,z) = 0` together as a 2D implicit curve.

- Fidget supports `Tree::deriv(Var)` for symbolic differentiation
- Could evaluate both `f` and `∂f/∂z` on a 3D grid, then find their common zero set
- The intersection of two surfaces is a curve; project that curve onto the target plane

**Challenge**: finding the intersection curve of two implicit surfaces is itself non-trivial. Would need a 3D contouring algorithm that extracts curves (not just surfaces).

## Recommended Path: Option A (Sampling + Extremal Z)

### Steps
1. Add `Surface3D::project_xy(resolution, bounding_box)` method
2. For each `(x,y)` grid point:
   - Use `fidget::context::Context::eval_xyz()` to evaluate `f(x,y,z)` along z-axis
   - Binary search for the maximum z where `f(x,y,z) = 0` (or detect no real root)
   - Store result in a 2D grid: `grid[y][x] = max_z` or `NaN` if no intersection
3. The silhouette is the boundary of the valid region (where `max_z` is finite)
4. Extract contour using existing marching squares code from `surface2d.rs`

### API Design
```rust
impl Surface3D {
    /// Project the silhouette onto the xy-plane.
    fn project_xy(&self, resolution: u32) -> Result<PolygonSet, MeshingError> { ... }

    /// Project the silhouette onto the xz-plane.
    fn project_xz(&self, resolution: u32) -> Result<PolygonSet, MeshingError> { ... }

    /// Project the silhouette onto the yz-plane.
    fn project_yz(&self, resolution: u32) -> Result<PolygonSet, MeshingError> { ... }
}
```

### Dependencies
- Reuse `marching_squares` and `connect_segments_into_loops` from `surface2d.rs`
- Use `Tree::eval_xyz()` for fast JIT evaluation along z-axis
- Binary search bounds from `MeshSettings` bounding box

## Open Questions
- How to handle self-intersections in the projected silhouette?
- What resolution is needed for acceptable accuracy? (trade-off with performance)
- Should we expose this as a language-level method or keep it internal?
- For non-convex surfaces, the silhouette may have multiple disconnected components — does marching squares handle this correctly? (yes, based on existing tests)

## Related
- Slicing (`slice_x`, `slice_y`, `slice_z`) is already implemented and uses similar grid evaluation patterns
- The `Tree::remap_xyz()` method used for slicing could be useful here too
