use std::collections::HashMap;
use std::sync::Arc;

use fidget::context::Context;
use fidget::jit::JitFunction;
use fidget::shape::{EzShape, Shape};
use fidget::types::Interval;
use geo::{Coord, LineString, MultiPolygon, Polygon};
use nalgebra;

use crate::execution::errors::Raise;
use crate::execution::values::{
    closure::BuiltinCallableDatabase, scalar::Angle, vector::Zero2, DowncastError, Length, Object,
    Scalar, StaticType, StaticTypeName, Style, Transform2d, Value, ValueNone, ValueType, Vector2,
};
use crate::execution::ExecutionContext;

use super::MeshSettings;
use super::MeshingError;
use super::Surface3D;

use super::super::polygon::PolygonSet;

enum ArithmeticInput {
    Vector(Vector2),
    Surface(Surface2D),
}

fn unpack_arithmetic_input(
    context: &ExecutionContext,
    input: Value,
) -> Result<ArithmeticInput, crate::execution::errors::Error> {
    let value = match input {
        Value::Vector2(v) => {
            if v.dimension() != common_data_types::Dimension::length() {
                return Err(DowncastError {
                    expected: "Vector2 of lengths or another implicit surface".into(),
                    got: v.get_type(context).name(),
                }
                .to_error(context));
            }
            ArithmeticInput::Vector(v)
        }
        Value::Surface2D(s) => ArithmeticInput::Surface(s),
        value => {
            return Err(DowncastError {
                expected: "Vector2 of lengths or another implicit surface".into(),
                got: value.get_type(context).name(),
            }
            .to_error(context));
        }
    };
    Ok(value)
}

/// 2D implicit surface wrapping a Fidget Shape.
///
/// Provides `to_polygon()` for contour extraction via marching squares.
#[derive(Clone, PartialEq, Eq)]
pub struct Surface2D {
    tree: fidget::context::Tree,
    settings: MeshSettings,
}

impl std::fmt::Debug for Surface2D {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Surface2D")
            .field("settings", &self.settings)
            .finish()
    }
}

impl Surface2D {
    pub fn new(tree: fidget::context::Tree) -> Self {
        Self {
            tree,
            settings: MeshSettings::default(),
        }
    }

    /// Create a surface with custom mesh settings.
    pub fn with_settings(tree: fidget::context::Tree, settings: MeshSettings) -> Self {
        Self { tree, settings }
    }

    /// Access the underlying fidget Tree.
    pub fn tree(&self) -> &fidget::context::Tree {
        &self.tree
    }

    /// Check if this surface contains infinity constants (from degenerate projections).
    pub fn is_degenerate_projection(&self) -> bool {
        super::tree_contains_infinity(&self.tree)
    }

    /// Check if the shape is closed (finite extent in all directions).
    /// Returns false if the SDF is negative at large distance along any axis,
    /// meaning the shape is unbounded (e.g., a half-plane or infinite strip).
    pub fn is_bounded(&self) -> bool {
        use fidget::context::Context;
        let mut ctx = Context::new();
        let node = ctx.import(&self.tree);

        // Test far along each axis direction (Z not used for 2D, but passed as 0)
        let d = 1e6_f64;
        let test_points = [(d, 0.0), (-d, 0.0), (0.0, d), (0.0, -d)];

        for &(x, y) in &test_points {
            match ctx.eval_xyz(node, x, y, 0.0) {
                Ok(val) if val < -0.1 => return false,
                _ => {}
            }
        }
        true
    }

    /// Estimate the bounding box using interval arithmetic.
    /// Returns (min_x, min_y, max_x, max_y) in the shape's coordinate system.
    ///
    /// Uses fidget's interval evaluator to conservatively determine whether
    /// a region contains the zero contour. If the interval result's lower bound
    /// is positive, the region is provably outside the shape.
    pub fn bounding_box_estimate(&self) -> (f64, f64, f64, f64) {
        let shape = Shape::<JitFunction>::from(self.tree.clone());
        let mut eval = Shape::<JitFunction>::new_interval_eval();
        let tape = shape.ez_interval_tape();

        let mut eval_region = |x_lo: f32, x_hi: f32, y_lo: f32, y_hi: f32| -> Interval {
            eval.eval(&tape, [x_lo, x_hi], [y_lo, y_hi], [0.0_f32, 0.0_f32])
                .map(|(result, _)| result)
                .unwrap_or_else(|_| Interval::new(f32::NAN, f32::NAN))
        };

        // Large initial search region
        let search_limit = 100.0_f32;

        // Check if the shape exists at all in the search region
        let full = eval_region(-search_limit, search_limit, -search_limit, search_limit);
        if full.lower() > 0.0 || full.has_nan() {
            return (0.0, 0.0, 0.01, 0.01);
        }

        // Binary search for x_min: find smallest x where shape exists.
        // Evaluate [x_lo, mid] × [-R, R]. If no shape in left half, x_min > mid.
        let mut x_lo = -search_limit;
        let mut x_hi = search_limit;
        for _ in 0..20 {
            let mid = (x_lo + x_hi) / 2.0;
            let result = eval_region(x_lo, mid, -search_limit, search_limit);
            if result.lower() > 0.0 || result.has_nan() {
                x_lo = mid;
            } else {
                x_hi = mid;
            }
        }
        let x_min = x_hi;

        // Binary search for x_max: find largest x where shape exists.
        // Evaluate [mid, x_hi] × [-R, R]. If no shape in right half, x_max < mid.
        let mut x_lo = x_min;
        let mut x_hi = search_limit;
        for _ in 0..20 {
            let mid = (x_lo + x_hi) / 2.0;
            let result = eval_region(mid, x_hi, -search_limit, search_limit);
            if result.lower() > 0.0 || result.has_nan() {
                x_hi = mid;
            } else {
                x_lo = mid;
            }
        }
        let x_max = x_lo;

        // Binary search for y_min
        let mut y_lo = -search_limit;
        let mut y_hi = search_limit;
        for _ in 0..20 {
            let mid = (y_lo + y_hi) / 2.0;
            let result = eval_region(x_min, x_max, y_lo, mid);
            if result.lower() > 0.0 || result.has_nan() {
                y_lo = mid;
            } else {
                y_hi = mid;
            }
        }
        let y_min = y_hi;

        // Binary search for y_max
        let mut y_lo = y_min;
        let mut y_hi = search_limit;
        for _ in 0..20 {
            let mid = (y_lo + y_hi) / 2.0;
            let result = eval_region(x_min, x_max, mid, y_hi);
            if result.lower() > 0.0 || result.has_nan() {
                y_hi = mid;
            } else {
                y_lo = mid;
            }
        }
        let y_max = y_lo;

        let x_min = x_min as f64;
        let y_min = y_min as f64;
        let x_max = x_max as f64;
        let y_max = y_max as f64;

        // Ensure minimum extent to avoid degenerate bounding boxes
        let min_extent = 0.01_f64;
        let x_max = x_max.max(x_min + min_extent);
        let y_max = y_max.max(y_min + min_extent);

        (x_min, y_min, x_max, y_max)
    }

    /// Boolean union: min(a, b) — inside either shape.
    pub fn union(&self, other: &Surface2D) -> Self {
        Self::new(self.tree.clone().min(other.tree.clone()))
    }

    /// Boolean intersection: max(a, b) — inside both shapes.
    pub fn intersection(&self, other: &Surface2D) -> Self {
        Self::new(self.tree.clone().max(other.tree.clone()))
    }

    /// Boolean difference: max(a, -b) — inside a but not b.
    pub fn difference(&self, other: &Surface2D) -> Self {
        Self::new(self.tree.clone().max(-other.tree.clone()))
    }

    /// Boolean symmetric difference: min(max(a, -b), max(b, -a)) — XOR.
    pub fn symmetric_difference(&self, other: &Surface2D) -> Self {
        let a = &self.tree;
        let b = &other.tree;
        let a_minus_b = a.clone().max(-b.clone());
        let b_minus_a = b.clone().max(-a.clone());
        Self::new(a_minus_b.min(b_minus_a))
    }

    /// Apply an affine transform to the implicit surface.
    /// The transform is applied by remapping SDF coordinates: sdf'(p) = sdf(T^-1 * p).
    pub fn transform(&self, t: &nalgebra::Matrix3<f64>) -> Self {
        let inv = t
            .try_inverse()
            .expect("Transform matrix must be invertible");
        // Express new coordinates as linear combinations of original x, y using inverse matrix.
        // new_x = inv[0][0]*x + inv[0][1]*y + inv[0][2]
        // new_y = inv[1][0]*x + inv[1][1]*y + inv[1][2]
        let (x, y) = (fidget::context::Tree::x(), fidget::context::Tree::y());
        let new_x = x.clone() * fidget::context::Tree::constant(inv[(0, 0)])
            + y.clone() * fidget::context::Tree::constant(inv[(0, 1)])
            + fidget::context::Tree::constant(inv[(0, 2)]);
        let new_y = x * fidget::context::Tree::constant(inv[(1, 0)])
            + y * fidget::context::Tree::constant(inv[(1, 1)])
            + fidget::context::Tree::constant(inv[(1, 2)]);
        Self::new(
            self.tree
                .remap_xyz(new_x, new_y, fidget::context::Tree::z()),
        )
    }

    /// Extrude the 2D profile along the Z axis to create a 3D solid.
    ///
    /// SDF: max(sdf_2d(x, y), -z, z - height)
    /// The solid extends from z=0 to z=height.
    pub fn extrude_z(&self, height: f64) -> Surface3D {
        use fidget::context::Tree;
        let z = Tree::z();
        let sdf_2d = self.tree.clone();
        let bottom_cap = -z.clone();
        let top_cap = z - Tree::constant(height);
        let tree = sdf_2d.clone().max(bottom_cap).max(top_cap);
        Surface3D::new(tree)
    }

    /// Revolve the 2D profile about the Y axis to create a 3D solid.
    ///
    /// The profile is interpreted in (r, y) coordinates where r = sqrt(x^2 + z^2).
    /// For full revolution: sdf_3d(x, y, z) = sdf_2d(r, y).
    /// For partial angle: additionally clamped by angular sweep from x>0 toward +z.
    pub fn revolve_y(&self, angle: f64) -> Surface3D {
        use fidget::context::Tree;
        let (x, y, z) = (Tree::x(), Tree::y(), Tree::z());
        let r = (x.clone() * x.clone() + z.clone() * z.clone()).sqrt();
        let tree = self.tree.remap_xyz(r.clone(), y, Tree::z());
        if (angle - 2.0 * std::f64::consts::PI).abs() < 1e-10 {
            return Surface3D::new(tree);
        }
        // Partial angle: clamp by angular sweep using Euclidean distance to cap planes.
        // The sweep is from -ha to +ha, centered at theta=0 (positive X axis).
        // Upper cap (theta = ha): distance = -sin(ha)*x + cos(ha)*z
        // Lower cap (theta = -ha): distance = -sin(ha)*x - cos(ha)*z
        // Both must be negative (inside) for the point to be within the sweep.
        let half_angle = angle / 2.0;
        let sin_ha = Tree::constant(half_angle.sin());
        let cos_ha = Tree::constant(half_angle.cos());
        let angular_sdf_upper = -x.clone() * sin_ha.clone() + z.clone() * cos_ha.clone();
        let angular_sdf_lower = -x * sin_ha - z * cos_ha;
        Surface3D::new(tree.max(angular_sdf_upper).max(angular_sdf_lower))
    }

    /// Generate a 2D polygon set from the implicit curve using dual contouring.
    ///
    /// Each crossing cell produces a single vertex via QEF optimization,
    /// which snaps to sharp features (corners, edges) instead of rounding them.
    /// Returns `PolygonSet` (wrapping `Arc<geo::MultiPolygon>`).
    pub fn to_polygon(&self) -> Result<PolygonSet, MeshingError> {
        let (origin_x, origin_y, cell_size, grid_width, grid_height) = self.compute_grid_params();

        let mut eval_ctx = Context::new();
        let node = eval_ctx.import(&self.tree);

        // Evaluate SDF at all grid points (for cell occupancy)
        let mut grid = vec![0.0f32; grid_width * grid_height];
        for iy in 0..grid_height {
            for ix in 0..grid_width {
                let x = origin_x + ix as f32 * cell_size;
                let y = origin_y + iy as f32 * cell_size;
                let val = eval_ctx
                    .eval_xyz(node, x as f64, y as f64, 0.0_f64)
                    .map_err(|e| MeshingError(format!("SDF evaluation failed: {e}")))?;
                grid[iy * grid_width + ix] = val as f32;
            }
        }

        // Dual contouring: compute QEF vertex for each crossing cell, then assemble segments.
        // Pass the JIT evaluator for accurate gradient computation at crossing points.
        let segments = dual_contour_segments_jit(
            &grid,
            grid_width,
            grid_height,
            origin_x,
            origin_y,
            cell_size,
            &eval_ctx,
            node,
        );
        let loops = connect_segments_into_loops(segments, cell_size);

        // Find the loop with the largest absolute area as the exterior contour.
        // The marching squares algorithm may produce CW or CCW loops depending on
        // the case table ordering. We normalize: exterior must be CCW (positive area),
        // holes must be CW (negative area).
        let mut best_exterior: Option<(Vec<Coord<f32>>, f32)> = None;
        let mut interior_coords: Vec<Vec<Coord<f32>>> = Vec::new();

        for loop_vertices in loops {
            if loop_vertices.len() < 3 {
                continue;
            }
            let area = signed_area_2d(&loop_vertices);
            let abs_area = area.abs();

            if best_exterior.is_none() || abs_area > best_exterior.as_ref().unwrap().1.abs() {
                // Promote old best exterior to a hole (it's already normalized to CCW)
                if let Some((old_coords, _)) = best_exterior.take() {
                    // Reverse from CCW back to CW for hole
                    let mut hole = old_coords;
                    hole.reverse();
                    interior_coords.push(hole);
                }

                // This loop is the new best exterior — normalize to CCW
                let mut coords: Vec<Coord<f32>> = loop_vertices
                    .into_iter()
                    .map(|[x, y]| Coord { x, y })
                    .collect();
                if area < 0.0 {
                    coords.reverse();
                }
                best_exterior = Some((coords, abs_area));
            } else {
                // Smaller loop — treat as hole, normalize to CW
                let mut coords: Vec<Coord<f32>> = loop_vertices
                    .into_iter()
                    .map(|[x, y]| Coord { x, y })
                    .collect();
                if area > 0.0 {
                    coords.reverse();
                }
                interior_coords.push(coords);
            }
        }

        let exterior = best_exterior
            .map(|(coords, _)| coords)
            .ok_or_else(|| MeshingError("No outer contour found".into()))?;
        let exterior_f64: Vec<geo::Coord<f64>> = exterior
            .into_iter()
            .map(|c| geo::Coord {
                x: c.x as f64,
                y: c.y as f64,
            })
            .collect();
        let interiors_f64: Vec<LineString<f64>> = interior_coords
            .into_iter()
            .map(|coords| {
                let coords_f64: Vec<geo::Coord<f64>> = coords
                    .into_iter()
                    .map(|c| geo::Coord {
                        x: c.x as f64,
                        y: c.y as f64,
                    })
                    .collect();
                LineString(coords_f64)
            })
            .collect();
        let polygon = Polygon::new(LineString(exterior_f64), interiors_f64);
        let multi_polygon: geo::MultiPolygon<f64> = MultiPolygon(vec![polygon]);

        Ok(PolygonSet(Arc::new(multi_polygon)))
    }

    fn compute_grid_params(&self) -> (f32, f32, f32, usize, usize) {
        // Estimate shape bounds by sampling SDF along axes.
        // This ensures the grid adapts to the shape size (e.g., 1cm circle).
        let mut eval_ctx = Context::new();
        let node = eval_ctx.import(&self.tree);

        let sample_xy =
            |x: f64, y: f64| -> f32 { eval_ctx.eval_xyz(node, x, y, 0.0).unwrap_or(0.0) as f32 };
        let sample_x = |x: f32| sample_xy(x as f64, 0.0);
        let sample_y = |y: f32| sample_xy(0.0, y as f64);

        // Binary search for zero-crossing along an axis direction.
        let find_boundary = |mut lo: f32, mut hi: f32, sample: &dyn Fn(f32) -> f32| -> f32 {
            while sample(hi) < 0.0 && hi < 100.0 {
                hi *= 2.0;
            }
            if hi >= 100.0 {
                return 10.0;
            }
            for _ in 0..20 {
                let mid = (lo + hi) / 2.0;
                if sample(mid) < 0.0 {
                    lo = mid;
                } else {
                    hi = mid;
                }
            }
            hi
        };

        let origin_inside = sample_xy(0.0, 0.0) < 0.0;

        let extent = if origin_inside {
            let sx = find_boundary(0.0, 1.0, &sample_x);
            let sy = find_boundary(0.0, 1.0, &sample_y);
            sx.max(sy).max(0.001) * 1.5
        } else {
            10.0
        };

        let extent = extent.min(20.0);
        let depth = (self.settings.depth as u32).max(10);
        let cell_size = (extent * 2.0 / (1u64 << depth) as f32).max(0.0001);
        let grid_size = (extent * 2.0 / cell_size) as usize;
        (-extent, -extent, cell_size, grid_size, grid_size)
    }
}

/// A line segment extracted by marching squares.
#[derive(Debug, Clone)]
struct Segment {
    start: [f32; 2],
    end: [f32; 2],
}

/// Run marching squares on a 2D SDF grid to extract zero-contour segments.
#[allow(dead_code)]
fn marching_squares(
    grid: &[f32],
    width: usize,
    height: usize,
    origin_x: f32,
    origin_y: f32,
    cell_size: f32,
) -> Vec<Segment> {
    // Edge indices: 0=bottom, 1=right, 2=top, 3=left
    // Each entry is (edge_index, _). (0,0) means no crossing.
    // Case bits: bit0=BL, bit1=BR, bit2=TR, bit3=TL (negative = 1)
    const CASE_TABLE: [[(usize, usize); 4]; 16] = [
        // case 0 (0000): no crossings
        [(0, 0), (0, 0), (0, 0), (0, 0)],
        // case 1 (0001): BL neg → edges 0(bottom), 3(left)
        [(0, 1), (3, 1), (0, 0), (0, 0)],
        // case 2 (0010): BR neg → edges 0(bottom), 1(right)
        [(0, 1), (1, 1), (0, 0), (0, 0)],
        // case 3 (0011): BL+BR neg → edges 1(right), 3(left)
        [(1, 1), (3, 1), (0, 0), (0, 0)],
        // case 4 (0100): TR neg → edges 1(right), 2(top)
        [(1, 1), (2, 1), (0, 0), (0, 0)],
        // case 5 (0101): BL+TR neg → saddle: handled separately
        [(0, 0), (0, 0), (0, 0), (0, 0)],
        // case 6 (0110): BR+TR neg → edges 0(bottom), 2(top)
        [(0, 1), (2, 1), (0, 0), (0, 0)],
        // case 7 (0111): BL+BR+TR neg → edges 2(top), 3(left)
        [(2, 1), (3, 1), (0, 0), (0, 0)],
        // case 8 (1000): TL neg → edges 2(top), 3(left)
        [(2, 1), (3, 1), (0, 0), (0, 0)],
        // case 9 (1001): BL+TL neg → edges 0(bottom), 2(top)
        [(0, 1), (2, 1), (0, 0), (0, 0)],
        // case 10 (1010): BR+TL neg → saddle: handled separately
        [(0, 0), (0, 0), (0, 0), (0, 0)],
        // case 11 (1011): BL+BR+TL neg → edges 1(right), 2(top)
        [(1, 1), (2, 1), (0, 0), (0, 0)],
        // case 12 (1100): TR+TL neg → edges 1(right), 3(left)
        [(1, 1), (3, 1), (0, 0), (0, 0)],
        // case 13 (1101): BL+TR+TL neg → edges 0(bottom), 1(right)
        [(0, 1), (1, 1), (0, 0), (0, 0)],
        // case 14 (1110): BR+TR+TL neg → edges 0(bottom), 3(left)
        [(0, 1), (3, 1), (0, 0), (0, 0)],
        // case 15 (1111): all neg → no crossings
        [(0, 0), (0, 0), (0, 0), (0, 0)],
    ];

    let mut segments = Vec::new();

    for iy in 0..height.saturating_sub(1) {
        for ix in 0..width.saturating_sub(1) {
            let v_bl = grid[iy * width + ix];
            let v_br = grid[iy * width + ix + 1];
            let v_tr = grid[(iy + 1) * width + ix + 1];
            let v_tl = grid[(iy + 1) * width + ix];

            let case = if v_bl < 0.0 { 1 } else { 0 }
                | if v_br < 0.0 { 2 } else { 0 }
                | if v_tr < 0.0 { 4 } else { 0 }
                | if v_tl < 0.0 { 8 } else { 0 };

            // Handle saddle cases separately with correct segment pairing
            if case == 5 || case == 10 {
                let pts: Vec<[f32; 2]> = (0..4)
                    .filter_map(|e| {
                        interpolate_edge(ix, iy, e, grid, width, origin_x, origin_y, cell_size)
                    })
                    .collect();
                if pts.len() == 4 {
                    // Case 5 (BL+TR neg): pair (bottom, left) and (right, top)
                    // Case 10 (BR+TL neg): pair (bottom, right) and (top, left)
                    if case == 5 {
                        segments.push(Segment {
                            start: pts[0],
                            end: pts[3],
                        });
                        segments.push(Segment {
                            start: pts[1],
                            end: pts[2],
                        });
                    } else {
                        segments.push(Segment {
                            start: pts[0],
                            end: pts[1],
                        });
                        segments.push(Segment {
                            start: pts[2],
                            end: pts[3],
                        });
                    }
                }
                continue;
            }

            let edges = &CASE_TABLE[case as usize];

            let mut points = Vec::new();
            for &edge in edges.iter() {
                if edge != (0, 0) {
                    if let Some(pt) =
                        interpolate_edge(ix, iy, edge.0, grid, width, origin_x, origin_y, cell_size)
                    {
                        points.push(pt);
                    }
                }
            }

            if points.len() == 2 {
                segments.push(Segment {
                    start: points[0],
                    end: points[1],
                });
            }
        }
    }

    segments
}

// ─── 2D Dual Contouring with QEF ─────────────────────────────────────

/// 2D Quadratic Error Function solver (mirrors fidget's QuadraticErrorSolver).
///
/// Collects edge crossings with their gradients, then solves for a vertex
/// position that minimizes weighted distance to all crossings. Rank detection
/// via eigenvalue cutoff handles sharp features: rank-2 for corners,
/// rank-1 for planar edges.
#[derive(Copy, Clone, Debug, Default)]
struct Qef2D {
    ata: nalgebra::Matrix2<f32>,
    atb: nalgebra::Vector2<f32>,
    btb: f32,
    mass_point: nalgebra::Vector3<f32>, // (x, y, w) for averaging
}

impl Qef2D {
    fn add_intersection(&mut self, pos: [f32; 2], grad: [f32; 2]) {
        self.mass_point.x += pos[0];
        self.mass_point.y += pos[1];
        self.mass_point.z += 1.0;
        let norm = (grad[0] * grad[0] + grad[1] * grad[1]).sqrt();
        if norm < 1e-10 {
            return;
        }
        let nx = grad[0] / norm;
        let ny = grad[1] / norm;
        self.ata[(0, 0)] += nx * nx;
        self.ata[(0, 1)] += nx * ny;
        self.ata[(1, 0)] += nx * ny;
        self.ata[(1, 1)] += ny * ny;
        let d = nx * pos[0] + ny * pos[1];
        self.atb.x += nx * d;
        self.atb.y += ny * d;
        self.btb += d * d;
    }

    fn solve(&self) -> [f32; 2] {
        // Mean crossing position (mass point): good default for underconstrained directions
        let mean = [
            self.mass_point.x / self.mass_point.z,
            self.mass_point.y / self.mass_point.z,
        ];
        let atb_shifted = self.atb - self.ata * nalgebra::Vector2::new(mean[0], mean[1]);
        let det = self.ata.determinant();
        // Eigenvalue cutoff: if determinant is tiny relative to trace, use rank-1
        let trace = self.ata[(0, 0)] + self.ata[(1, 1)];
        let cutoff = trace.abs() * 1e-3;
        if det.abs() > cutoff {
            // Rank-2: solve via Cramer's rule
            let x = (atb_shifted.x * self.ata[(1, 1)] - atb_shifted.y * self.ata[(1, 0)]) / det;
            let y = (self.ata[(0, 0)] * atb_shifted.y - self.ata[(0, 1)] * atb_shifted.x) / det;
            [x + mean[0], y + mean[1]]
        } else {
            // Rank-1: solve along dominant axis, use mean crossing for unconstrained direction.
            // This preserves sharp corners (QEF snaps constrained direction) while keeping
            // smooth curves (mean crossing position for the free direction).
            if self.ata[(0, 0)].abs() > self.ata[(1, 1)].abs() {
                let x = if self.ata[(0, 0)].abs() > 1e-10 {
                    atb_shifted.x / self.ata[(0, 0)]
                } else {
                    0.0
                };
                [x + mean[0], mean[1]]
            } else {
                let y = if self.ata[(1, 1)].abs() > 1e-10 {
                    atb_shifted.y / self.ata[(1, 1)]
                } else {
                    0.0
                };
                [mean[0], y + mean[1]]
            }
        }
    }
}

/// Run dual contouring on a 2D SDF grid to extract zero-contour segments.
///
/// Each crossing cell produces one vertex via QEF optimization. Segments connect
/// vertices of adjacent crossing cells that share an edge with a sign change.
/// Uses JIT-evaluated gradients for accurate corner detection.
#[allow(clippy::too_many_arguments)]
fn dual_contour_segments_jit(
    grid: &[f32],
    width: usize,
    height: usize,
    origin_x: f32,
    origin_y: f32,
    cell_size: f32,
    ctx: &Context,
    node: fidget::context::Node,
) -> Vec<Segment> {
    // Per-cell vertex: None if cell is uniform (all inside or all outside)
    let mut cell_verts: Vec<Option<[f32; 2]>> = vec![None; width * height];

    // Process each cell
    for iy in 0..height.saturating_sub(1) {
        for ix in 0..width.saturating_sub(1) {
            let idx = iy * width + ix;
            let v_bl = grid[idx];
            let v_br = grid[idx + 1];
            let v_tr = grid[(iy + 1) * width + ix + 1];
            let v_tl = grid[(iy + 1) * width + ix];

            // Check if cell has sign changes (not all same sign)
            let neg = [v_bl < 0.0, v_br < 0.0, v_tr < 0.0, v_tl < 0.0];
            if neg.iter().all(|&b| b) || neg.iter().all(|&b| !b) {
                continue; // uniform cell, no crossing
            }

            // Find edge crossings and their gradients (JIT-evaluated)
            let mut qef = Qef2D::default();
            let cell_origin_x = origin_x + ix as f32 * cell_size;
            let cell_origin_y = origin_y + iy as f32 * cell_size;

            // Edge 0: bottom (BL→BR)
            if v_bl * v_br < 0.0 {
                let crossing = find_edge_crossing_2d(
                    cell_origin_x,
                    cell_origin_y,
                    cell_size,
                    0.0,
                    0.0,
                    1.0,
                    0.0,
                    v_bl,
                    v_br,
                );
                let grad = compute_gradient_jit(ctx, node, crossing[0] as f64, crossing[1] as f64);
                qef.add_intersection(crossing, grad);
            }
            // Edge 1: right (BR→TR)
            if v_br * v_tr < 0.0 {
                let crossing = find_edge_crossing_2d(
                    cell_origin_x,
                    cell_origin_y,
                    cell_size,
                    1.0,
                    0.0,
                    1.0,
                    1.0,
                    v_br,
                    v_tr,
                );
                let grad = compute_gradient_jit(ctx, node, crossing[0] as f64, crossing[1] as f64);
                qef.add_intersection(crossing, grad);
            }
            // Edge 2: top (TL→TR)
            if v_tl * v_tr < 0.0 {
                let crossing = find_edge_crossing_2d(
                    cell_origin_x,
                    cell_origin_y,
                    cell_size,
                    0.0,
                    1.0,
                    1.0,
                    1.0,
                    v_tl,
                    v_tr,
                );
                let grad = compute_gradient_jit(ctx, node, crossing[0] as f64, crossing[1] as f64);
                qef.add_intersection(crossing, grad);
            }
            // Edge 3: left (BL→TL)
            if v_bl * v_tl < 0.0 {
                let crossing = find_edge_crossing_2d(
                    cell_origin_x,
                    cell_origin_y,
                    cell_size,
                    0.0,
                    0.0,
                    0.0,
                    1.0,
                    v_bl,
                    v_tl,
                );
                let grad = compute_gradient_jit(ctx, node, crossing[0] as f64, crossing[1] as f64);
                qef.add_intersection(crossing, grad);
            }

            // Solve QEF for vertex position
            cell_verts[idx] = Some(qef.solve());
        }
    }

    // Assemble segments: connect adjacent crossing cells when their shared edge
    // has a sign change (the two corners of the shared edge have different signs).
    let mut segments = Vec::new();

    // Vertical shared edges (between col ix and ix+1, same row iy)
    for iy in 0..height.saturating_sub(1) {
        for ix in 0..width.saturating_sub(1) {
            let left_idx = iy * width + ix;
            let right_idx = iy * width + ix + 1;
            if cell_verts[left_idx].is_some() && cell_verts[right_idx].is_some() {
                // Shared edge is vertical, corners at top and bottom
                let top = grid[iy * width + ix + 1];
                let bottom = grid[(iy + 1) * width + ix + 1];
                if (top < 0.0) != (bottom < 0.0) {
                    segments.push(Segment {
                        start: cell_verts[left_idx].unwrap(),
                        end: cell_verts[right_idx].unwrap(),
                    });
                }
            }
        }
    }

    // Horizontal shared edges (between row iy and iy+1, same col ix)
    for iy in 0..height.saturating_sub(1) {
        for ix in 0..width.saturating_sub(1) {
            let top_idx = iy * width + ix;
            let bot_idx = (iy + 1) * width + ix;
            if cell_verts[top_idx].is_some() && cell_verts[bot_idx].is_some() {
                // Shared edge is horizontal, corners at left and right
                let left = grid[(iy + 1) * width + ix];
                let right = grid[(iy + 1) * width + ix + 1];
                if (left < 0.0) != (right < 0.0) {
                    segments.push(Segment {
                        start: cell_verts[top_idx].unwrap(),
                        end: cell_verts[bot_idx].unwrap(),
                    });
                }
            }
        }
    }

    segments
}

/// Find the zero-crossing position along a parametric edge within a cell.
/// `p0` and `p1` are local cell coordinates (0..1), `v0` and `v1` are SDF values.
#[allow(clippy::too_many_arguments)]
fn find_edge_crossing_2d(
    cell_origin_x: f32,
    cell_origin_y: f32,
    cell_size: f32,
    px0: f32,
    py0: f32,
    px1: f32,
    py1: f32,
    v0: f32,
    v1: f32,
) -> [f32; 2] {
    let t = if (v1 - v0).abs() < f32::EPSILON {
        0.5
    } else {
        (-v0 / (v1 - v0)).clamp(0.0, 1.0)
    };
    let lx = px0 + t * (px1 - px0);
    let ly = py0 + t * (py1 - py0);
    [
        cell_origin_x + lx * cell_size,
        cell_origin_y + ly * cell_size,
    ]
}

/// Compute gradient at a world-space position using central finite differences
/// on bilinear-interpolated grid values (kept for reference, not used by JIT path).
#[allow(dead_code)]
fn compute_gradient_2d(
    grid: &[f32],
    width: usize,
    wx: f32,
    wy: f32,
    origin_x: f32,
    origin_y: f32,
    cell_size: f32,
) -> [f32; 2] {
    // Convert world coords to grid coords (floating point)
    let gx = (wx - origin_x) / cell_size;
    let gy = (wy - origin_y) / cell_size;
    // Bilinear interpolation of gradient: compute gradient at each of 4 surrounding
    // grid cells and interpolate. But for simplicity, use central differences on
    // the interpolated grid values.
    let eps = 0.5 * cell_size; // step size for finite difference
    let interp = |gx: f32, gy: f32| -> f32 {
        let ix = gx.floor() as i32;
        let iy = gy.floor() as i32;
        let fx = gx - ix as f32;
        let fy = gy - iy as f32;
        // Clamp to grid bounds
        let ix = ix.max(0).min((width - 2) as i32) as usize;
        let iy = iy.max(0).min((width - 2) as i32) as usize;
        let v00 = grid[iy * width + ix];
        let v10 = grid[iy * width + ix + 1];
        let v01 = grid[(iy + 1) * width + ix];
        let v11 = grid[(iy + 1) * width + ix + 1];
        v00 * (1.0 - fx) * (1.0 - fy)
            + v10 * fx * (1.0 - fy)
            + v01 * (1.0 - fx) * fy
            + v11 * fx * fy
    };
    let dx = (interp(gx + eps / cell_size, gy) - interp(gx - eps / cell_size, gy)) / (2.0 * eps);
    let dy = (interp(gx, gy + eps / cell_size) - interp(gx, gy - eps / cell_size)) / (2.0 * eps);
    [dx, dy]
}

/// Compute gradient at a world-space position using JIT-evaluated SDF.
///
/// Uses central finite differences with a tiny step size (1e-5) for accurate
/// gradients. Unlike grid-based gradients, this evaluates the true SDF (not
/// a bilinear approximation), giving exact normals even at sharp features.
fn compute_gradient_jit(ctx: &Context, node: fidget::context::Node, x: f64, y: f64) -> [f32; 2] {
    let eps = 1e-5;
    let dx = (ctx.eval_xyz(node, x + eps, y, 0.0).unwrap_or(0.0)
        - ctx.eval_xyz(node, x - eps, y, 0.0).unwrap_or(0.0))
        / (2.0 * eps);
    let dy = (ctx.eval_xyz(node, x, y + eps, 0.0).unwrap_or(0.0)
        - ctx.eval_xyz(node, x, y - eps, 0.0).unwrap_or(0.0))
        / (2.0 * eps);
    [dx as f32, dy as f32]
}

/// Interpolate the zero-crossing point on a cell edge.
/// Edge indices: 0=bottom, 1=right, 2=top, 3=left
#[allow(dead_code, clippy::too_many_arguments)]
fn interpolate_edge(
    ix: usize,
    iy: usize,
    edge: usize,
    grid: &[f32],
    width: usize,
    origin_x: f32,
    origin_y: f32,
    cell_size: f32,
) -> Option<[f32; 2]> {
    let (v0, v1, t_x0, t_x1, t_y0, t_y1) = match edge {
        0 => {
            let v0 = grid[iy * width + ix];
            let v1 = grid[iy * width + ix + 1];
            (v0, v1, ix as f32, (ix + 1) as f32, iy as f32, iy as f32)
        }
        1 => {
            let v0 = grid[iy * width + ix + 1];
            let v1 = grid[(iy + 1) * width + ix + 1];
            (
                v0,
                v1,
                (ix + 1) as f32,
                (ix + 1) as f32,
                iy as f32,
                (iy + 1) as f32,
            )
        }
        2 => {
            let v0 = grid[(iy + 1) * width + ix];
            let v1 = grid[(iy + 1) * width + ix + 1];
            (
                v0,
                v1,
                ix as f32,
                (ix + 1) as f32,
                (iy + 1) as f32,
                (iy + 1) as f32,
            )
        }
        3 => {
            let v0 = grid[iy * width + ix];
            let v1 = grid[(iy + 1) * width + ix];
            (v0, v1, ix as f32, ix as f32, iy as f32, (iy + 1) as f32)
        }
        _ => return None,
    };

    let t = if (v1 - v0).abs() < f32::EPSILON {
        0.5
    } else {
        (-v0 / (v1 - v0)).clamp(0.0, 1.0)
    };

    let x = origin_x + (t_x0 + t * (t_x1 - t_x0)) * cell_size;
    let y = origin_y + (t_y0 + t * (t_y1 - t_y0)) * cell_size;

    Some([x, y])
}

/// Connect line segments into closed loops.
fn connect_segments_into_loops(segments: Vec<Segment>, cell_size: f32) -> Vec<Vec<[f32; 2]>> {
    // Snap tolerance for merging nearby vertices. Dual contouring QEF places
    // vertices near sharp features (corners) that should be merged into a single
    // point. Use ~0.25 cell_size to snap corner vertices while preserving smooth curves.
    let snap_tol = cell_size * 0.25;
    let snap_key = |pt: &[f32; 2]| -> (i32, i32) {
        (
            (pt[0] / snap_tol).round() as i32,
            (pt[1] / snap_tol).round() as i32,
        )
    };

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

    let mut loops = Vec::new();
    let mut remaining_edges: HashMap<(i32, i32), Vec<(i32, i32)>> = adj.clone();

    // Collect all keys with non-empty adjacency lists, sorted for determinism
    loop {
        let start_key = remaining_edges
            .keys()
            .filter(|k| remaining_edges.get(*k).is_some_and(|v| !v.is_empty()))
            .min() // deterministic: pick smallest key
            .cloned();

        let start_key = match start_key {
            Some(k) => k,
            None => break,
        };

        let mut circuit = vec![start_key];
        let mut current = start_key;

        loop {
            let neighbors = remaining_edges.get_mut(&current);
            match neighbors {
                Some(list) if !list.is_empty() => {
                    let next = list.pop().unwrap();
                    if let Some(reverse_list) = remaining_edges.get_mut(&next) {
                        reverse_list.retain(|&k| k != current);
                    }
                    circuit.push(next);
                    current = next;
                }
                _ => break,
            }
        }

        // Remove empty entries to prevent infinite loops
        remaining_edges.retain(|_, v| !v.is_empty());

        let loop_pts: Vec<[f32; 2]> = circuit
            .iter()
            .filter_map(|k| stored_points.get(k).copied())
            .collect();
        if loop_pts.len() >= 3 {
            loops.push(loop_pts);
        }
    }

    loops
}

/// Compute signed area of a 2D polygon using the shoelace formula.
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

impl StaticTypeName for Surface2D {
    fn static_type_name() -> std::borrow::Cow<'static, str> {
        "ImplicitSurface2D".into()
    }
}

impl StaticType for Surface2D {
    fn static_type() -> ValueType {
        ValueType::ImplicitSurface2D
    }
}

impl Object for Surface2D {
    fn get_type(&self, _context: &ExecutionContext) -> ValueType {
        ValueType::ImplicitSurface2D
    }

    fn addition(
        self,
        context: &ExecutionContext,
        rhs: Value,
    ) -> crate::execution::ExecutionResult<Value> {
        match unpack_arithmetic_input(context, rhs)? {
            ArithmeticInput::Vector(vector) => {
                let raw = vector.raw_value();
                let translation = nalgebra::Translation2::from([raw.x, raw.y]);
                Ok(self.transform(&translation.to_homogeneous()).into())
            }
            ArithmeticInput::Surface(other) => Ok(self.union(&other).into()),
        }
    }

    fn subtraction(
        self,
        context: &ExecutionContext,
        rhs: Value,
    ) -> crate::execution::ExecutionResult<Value> {
        match unpack_arithmetic_input(context, rhs)? {
            ArithmeticInput::Vector(vector) => {
                let raw = vector.raw_value();
                let translation = nalgebra::Translation2::from([-raw.x, -raw.y]);
                Ok(self.transform(&translation.to_homogeneous()).into())
            }
            ArithmeticInput::Surface(other) => Ok(self.difference(&other).into()),
        }
    }

    fn multiply(
        self,
        context: &ExecutionContext,
        rhs: Value,
    ) -> crate::execution::ExecutionResult<Value> {
        let input = rhs.downcast::<Zero2>(context)?;
        let vector = input.raw_value();
        let scaling =
            nalgebra::Matrix3::new_nonuniform_scaling(&nalgebra::Vector2::new(vector.x, vector.y));
        Ok(self.transform(&scaling).into())
    }

    fn bit_or(
        self,
        context: &ExecutionContext,
        rhs: Value,
    ) -> crate::execution::ExecutionResult<Value> {
        let other = rhs.downcast::<Surface2D>(context)?;
        Ok(self.union(&other).into())
    }

    fn bit_and(
        self,
        context: &ExecutionContext,
        rhs: Value,
    ) -> crate::execution::ExecutionResult<Value> {
        let other = rhs.downcast::<Surface2D>(context)?;
        Ok(self.intersection(&other).into())
    }

    fn bit_xor(
        self,
        context: &ExecutionContext,
        rhs: Value,
    ) -> crate::execution::ExecutionResult<Value> {
        let other = rhs.downcast::<Surface2D>(context)?;
        Ok(self.symmetric_difference(&other).into())
    }

    fn get_attribute(
        &self,
        _context: &ExecutionContext,
        attribute: &str,
    ) -> crate::execution::ExecutionResult<Value> {
        use crate::execution::errors::Raise as _;
        use crate::execution::values::{BuiltinFunction, MissingAttributeError};
        match attribute {
            "to_polygon" => Ok(BuiltinFunction::new::<methods::ToPolygon>().into()),
            "union" => Ok(BuiltinFunction::new::<methods::Union>().into()),
            "intersection" => Ok(BuiltinFunction::new::<methods::Intersection>().into()),
            "difference" => Ok(BuiltinFunction::new::<methods::Difference>().into()),
            "symmetric_difference" => {
                Ok(BuiltinFunction::new::<methods::SymmetricDifference>().into())
            }
            "transform" => Ok(BuiltinFunction::new::<methods::Transform>().into()),
            "extrude_z" => Ok(BuiltinFunction::new::<methods::ExtrudeZ>().into()),
            "revolve_y" => Ok(BuiltinFunction::new::<methods::RevolveY>().into()),
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
        write!(f, "Implicit 2D shape")
    }
}

/// Builtin 2D implicit shape generators.
pub mod implicits {
    pub struct Circle;
    pub struct Rectangle;
    pub struct Square;
}

/// Register builtin 2D implicit shape functions.
pub fn register_implicits(database: &mut BuiltinCallableDatabase) {
    use crate::build_function;
    use fidget::context::Tree;

    build_function!(
        database,
        implicits::Circle, "std.implicits.circle", (
            context: &ExecutionContext,
            radius: Option<Length> = ValueNone.into(),
            diameter: Option<Length> = ValueNone.into()
        ) -> Value
        {
            let r = match (radius, diameter) {
                (Some(r), None) => *r.value,
                (None, Some(d)) => *d.value / 2.0,
                (Some(_), Some(_)) => {
                    return Err(crate::execution::errors::StrError("Both radius and diameter provided").to_error(context));
                }
                (None, None) => {
                    return Err(crate::execution::errors::StrError("Either radius or diameter must be provided").to_error(context));
                }
            };
            // SDF: sqrt(x² + y²) - r
            let x = Tree::x();
            let y = Tree::y();
            let dist = (x.clone() * x.clone() + y.clone() * y.clone()).sqrt();
            let tree = dist - Tree::constant(r);
            Ok(Surface2D::new(tree).into())
        }
    );

    build_function!(
        database,
        implicits::Rectangle, "std.implicits.rectangle", (
            context: &ExecutionContext,
            size: crate::execution::values::vector::Length2
        ) -> Value
        {
            let s = size.0.raw_value();
            let half_x = s.x / 2.0;
            let half_y = s.y / 2.0;
            // SDF: max(|x| - half_x, |y| - half_y)
            let x = Tree::x().abs();
            let y = Tree::y().abs();
            let tree = x.clone() - Tree::constant(half_x);
            let tree = tree.max(y.clone() - Tree::constant(half_y));
            Ok(Surface2D::new(tree).into())
        }
    );

    build_function!(
        database,
        implicits::Square, "std.implicits.square", (
            context: &ExecutionContext,
            size: Scalar
        ) -> Value
        {
            let s = size.value.into_inner();
            let half = s / 2.0;
            // SDF: max(|x|, |y|) - half
            let x = Tree::x().abs();
            let y = Tree::y().abs();
            let tree = x.max(y) - Tree::constant(half);
            Ok(Surface2D::new(tree).into())
        }
    );
}

pub mod methods {
    pub struct ToPolygon;
    pub struct Union;
    pub struct Intersection;
    pub struct Difference;
    pub struct SymmetricDifference;
    pub struct Transform;
    pub struct ExtrudeZ;
    pub struct RevolveY;
}

pub fn register_surface2d_methods(database: &mut BuiltinCallableDatabase) {
    use crate::build_method;
    use crate::execution::errors::StringError;

    build_method!(
        database,
        methods::ToPolygon, "Surface2D::to_polygon", (
            context: &ExecutionContext,
            this: Surface2D
        ) -> Value
        {
            let polygon = this.to_polygon()
                .map_err(|e| StringError(e.to_string()).to_error(context))?;
            Ok(polygon.into())
        }
    );

    build_method!(
        database,
        methods::Union, "Surface2D::union", (
            _context: &ExecutionContext,
            this: Surface2D,
            other: Surface2D
        ) -> Value
        {
            Ok(this.union(&other).into())
        }
    );

    build_method!(
        database,
        methods::Intersection, "Surface2D::intersection", (
            _context: &ExecutionContext,
            this: Surface2D,
            other: Surface2D
        ) -> Value
        {
            Ok(this.intersection(&other).into())
        }
    );

    build_method!(
        database,
        methods::Difference, "Surface2D::difference", (
            _context: &ExecutionContext,
            this: Surface2D,
            other: Surface2D
        ) -> Value
        {
            Ok(this.difference(&other).into())
        }
    );

    build_method!(
        database,
        methods::SymmetricDifference, "Surface2D::symmetric_difference", (
            _context: &ExecutionContext,
            this: Surface2D,
            other: Surface2D
        ) -> Value
        {
            Ok(this.symmetric_difference(&other).into())
        }
    );

    build_method!(
        database,
        methods::Transform, "Surface2D::transform", (
            context: &ExecutionContext,
            this: Surface2D,
            t: Transform2d
        ) -> Value
        {
            let result = this.transform(&t.0);
            Ok(result.into())
        }
    );

    build_method!(
        database,
        methods::ExtrudeZ, "Surface2D::extrude_z", (
            _context: &ExecutionContext,
            this: Surface2D,
            height: Length
        ) -> Value
        {
            let result = this.extrude_z(*height.value);
            Ok(result.into())
        }
    );

    build_method!(
        database,
        methods::RevolveY, "Surface2D::revolve_y", (
            _context: &ExecutionContext,
            this: Surface2D,
            angle: Angle = Scalar { dimension: common_data_types::Dimension::angle(), value: common_data_types::Float::new(std::f64::consts::PI * 2.0).unwrap() }.into()
        ) -> Value
        {
            let result = this.revolve_y(*angle.value);
            Ok(result.into())
        }
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use fidget::context::Tree;

    // --- signed_area_2d ---

    #[test]
    fn signed_area_unit_square_ccw() {
        let vertices = [[0.0, 0.0], [1.0, 0.0], [1.0, 1.0], [0.0, 1.0]];
        assert!((signed_area_2d(&vertices) - 1.0).abs() < 1e-6);
    }

    #[test]
    fn signed_area_unit_square_cw() {
        let vertices = [[0.0, 0.0], [0.0, 1.0], [1.0, 1.0], [1.0, 0.0]];
        assert!((signed_area_2d(&vertices) - (-1.0)).abs() < 1e-6);
    }

    #[test]
    fn signed_area_triangle() {
        let vertices = [[0.0, 0.0], [1.0, 0.0], [0.0, 1.0]];
        assert!((signed_area_2d(&vertices) - 0.5).abs() < 1e-6);
    }

    #[test]
    fn signed_area_degenerate() {
        let vertices = [[0.0, 0.0], [1.0, 0.0], [2.0, 0.0]];
        assert!((signed_area_2d(&vertices) - 0.0).abs() < 1e-6);
    }

    // --- marching_squares ---

    fn sdf_circle_grid(
        cx: f32,
        cy: f32,
        r: f32,
        size: usize,
        origin: f32,
        cell_size: f32,
    ) -> Vec<f32> {
        let mut grid = vec![0.0f32; size * size];
        for iy in 0..size {
            for ix in 0..size {
                let x = origin + ix as f32 * cell_size;
                let y = origin + iy as f32 * cell_size;
                grid[iy * size + ix] = ((x - cx).powi(2) + (y - cy).powi(2)).sqrt() - r;
            }
        }
        grid
    }

    #[test]
    fn marching_squares_circle_basic() {
        let size = 32;
        let origin = -2.0;
        let cell_size = 4.0 / size as f32;
        let grid = sdf_circle_grid(0.0, 0.0, 1.0, size, origin, cell_size);
        let segments = marching_squares(&grid, size, size, origin, origin, cell_size);
        assert!(!segments.is_empty(), "circle should produce segments");
    }

    #[test]
    fn marching_squares_all_positive() {
        let grid = vec![1.0f32; 16 * 16];
        let segments = marching_squares(&grid, 16, 16, 0.0, 0.0, 1.0);
        assert!(
            segments.is_empty(),
            "all positive should produce no segments"
        );
    }

    #[test]
    fn marching_squares_all_negative() {
        let grid = vec![-1.0f32; 16 * 16];
        let segments = marching_squares(&grid, 16, 16, 0.0, 0.0, 1.0);
        assert!(
            segments.is_empty(),
            "all negative should produce no segments"
        );
    }

    #[test]
    fn marching_squares_single_cell_crossing() {
        let grid = vec![-1.0, -1.0, 1.0, 1.0];
        let segments = marching_squares(&grid, 2, 2, 0.0, 0.0, 1.0);
        assert_eq!(
            segments.len(),
            1,
            "single crossing should produce one segment"
        );
    }

    // --- connect_segments_into_loops ---

    #[test]
    fn connect_segments_square_loop() {
        let segments = vec![
            Segment {
                start: [0.0, 0.0],
                end: [1.0, 0.0],
            },
            Segment {
                start: [1.0, 0.0],
                end: [1.0, 1.0],
            },
            Segment {
                start: [1.0, 1.0],
                end: [0.0, 1.0],
            },
            Segment {
                start: [0.0, 1.0],
                end: [0.0, 0.0],
            },
        ];
        let loops = connect_segments_into_loops(segments, 1.0);
        assert_eq!(loops.len(), 1, "should form one loop");
        assert!(loops[0].len() >= 3);
    }

    #[test]
    fn connect_segments_disconnected() {
        let segments = vec![
            Segment {
                start: [0.0, 0.0],
                end: [1.0, 0.0],
            },
            Segment {
                start: [1.0, 0.0],
                end: [1.0, 1.0],
            },
            Segment {
                start: [1.0, 1.0],
                end: [0.0, 1.0],
            },
            Segment {
                start: [0.0, 1.0],
                end: [0.0, 0.0],
            },
            Segment {
                start: [5.0, 5.0],
                end: [6.0, 5.0],
            },
            Segment {
                start: [6.0, 5.0],
                end: [6.0, 6.0],
            },
            Segment {
                start: [6.0, 6.0],
                end: [5.0, 6.0],
            },
            Segment {
                start: [5.0, 6.0],
                end: [5.0, 5.0],
            },
        ];
        let loops = connect_segments_into_loops(segments, 1.0);
        assert_eq!(loops.len(), 2, "should form two separate loops");
    }

    #[test]
    fn connect_segments_empty() {
        let segments: Vec<Segment> = vec![];
        let loops = connect_segments_into_loops(segments, 1.0);
        assert!(loops.is_empty());
    }

    // --- Surface2D::to_polygon ---

    fn make_circle_surface(radius: f64) -> Surface2D {
        let x = Tree::x();
        let y = Tree::y();
        let tree = (x.clone() * x + y.clone() * y).sqrt() - Tree::constant(radius);
        Surface2D::new(tree)
    }

    fn make_circle_surface_with_depth(radius: f64, depth: u8) -> Surface2D {
        let x = Tree::x();
        let y = Tree::y();
        let tree = (x.clone() * x + y.clone() * y).sqrt() - Tree::constant(radius);
        Surface2D::with_settings(
            tree,
            super::super::MeshSettings {
                depth,
                ..Default::default()
            },
        )
    }

    fn polygon_area(poly: &geo::Polygon<f64>) -> f64 {
        let exterior = poly.exterior();
        let coords: Vec<_> = exterior.coords().collect();
        let mut area = 0.0f64;
        let n = coords.len();
        for i in 0..n {
            let j = (i + 1) % n;
            area += coords[i].x * coords[j].y;
            area -= coords[j].x * coords[i].y;
        }
        area.abs() / 2.0
    }

    #[test]
    fn to_polygon_circle_basic() {
        let surface = make_circle_surface(1.0);
        let result = surface.to_polygon();
        assert!(
            result.is_ok(),
            "circle should produce a polygon: {:?}",
            result
        );
        let poly = result.unwrap();
        let mp = &*poly.0;
        assert_eq!(mp.0.len(), 1, "should have one polygon");
    }

    #[test]
    fn to_polygon_circle_approximate_area() {
        // Use depth=10 for finer grid (1024x1024 over [-10,10]², cell_size ~ 0.02)
        let surface = make_circle_surface_with_depth(1.0, 10);
        let result = surface.to_polygon();
        assert!(result.is_ok());
        let poly = result.unwrap();
        let mp = &*poly.0;
        let area = polygon_area(&mp.0[0]);
        let expected = std::f64::consts::PI;
        // Debug: print some coordinates
        let exterior = mp.0[0].exterior();
        let coords: Vec<_> = exterior.coords().collect();
        eprintln!(
            "polygon area: {}, num vertices: {}, first few coords: {:?}",
            area,
            coords.len(),
            &coords[..4.min(coords.len())]
        );
        if coords.len() > 4 {
            eprintln!("last few coords: {:?}", &coords[coords.len() - 4..]);
        }
        assert!(
            (area - expected).abs() / expected < 0.15,
            "circle area {:?} should be close to PI ({:?})",
            area,
            expected
        );
    }

    #[test]
    fn to_polygon_no_contour() {
        let surface = Surface2D::new(Tree::constant(100.0));
        let result = surface.to_polygon();
        assert!(
            result.is_err(),
            "positive constant should produce no contour"
        );
    }

    #[test]
    fn to_polygon_small_circle() {
        let surface = make_circle_surface(0.5);
        let result = surface.to_polygon();
        assert!(
            result.is_ok(),
            "small circle should produce a polygon: {:?}",
            result
        );
    }

    #[test]
    fn to_polygon_large_circle() {
        // Use depth=10 for finer grid (1024x1024 over [-10,10]², cell_size ~ 0.02)
        let surface = make_circle_surface_with_depth(5.0, 10);
        let result = surface.to_polygon();
        assert!(
            result.is_ok(),
            "large circle should produce a polygon: {:?}",
            result
        );
        let poly = result.unwrap();
        let mp = &*poly.0;
        let area = polygon_area(&mp.0[0]);
        let expected = 25.0 * std::f64::consts::PI;
        assert!(
            (area - expected).abs() / expected < 0.15,
            "large circle area {:?} should be close to 25*PI ({:?})",
            area,
            expected
        );
    }

    // --- Debug test for JIT gradients and QEF vertices ---

    #[test]
    fn debug_circle_final_polygon() {
        let surface = make_circle_surface(1.0);
        let result = surface.to_polygon().unwrap();
        let mp = &*result.0;
        let exterior = mp.0[0].exterior();
        let coords: Vec<_> = exterior.coords().collect();
        let area = polygon_area(&mp.0[0]);
        eprintln!("Circle: {} vertices, area={:.4}", coords.len(), area);
        // Print every Nth vertex to see distribution
        let step = coords.len() / 16;
        for i in (0..coords.len()).step_by(step.max(1)) {
            let c = &coords[i];
            let r = (c.x * c.x + c.y * c.y).sqrt();
            eprintln!("  V{:3}: ({:+.4}, {:+.4}) r={:.4}", i, c.x, c.y, r);
        }
        assert!(coords.len() > 100);
    }

    #[test]
    fn debug_circle_jit_gradients() {
        use fidget::context::Context;

        let surface = make_circle_surface(1.0);
        let (origin_x, origin_y, cell_size, grid_width, grid_height) =
            surface.compute_grid_params();

        let mut eval_ctx = Context::new();
        let node = eval_ctx.import(&surface.tree);

        // Evaluate grid
        let mut grid = vec![0.0f32; grid_width * grid_height];
        for iy in 0..grid_height {
            for ix in 0..grid_width {
                let x = origin_x + ix as f32 * cell_size;
                let y = origin_y + iy as f32 * cell_size;
                let val = eval_ctx
                    .eval_xyz(node, x as f64, y as f64, 0.0_f64)
                    .unwrap();
                grid[iy * grid_width + ix] = val as f32;
            }
        }

        // Print some QEF vertices
        eprintln!(
            "Debug: cell_size={:.4}, grid={}x{}",
            cell_size, grid_width, grid_height
        );
        let mut count = 0usize;
        for iy in 0..grid_height.saturating_sub(1) {
            for ix in 0..grid_width.saturating_sub(1) {
                let idx = iy * grid_width + ix;
                let v_bl = grid[idx];
                let v_br = grid[idx + 1];
                let v_tr = grid[(iy + 1) * grid_width + ix + 1];
                let v_tl = grid[(iy + 1) * grid_width + ix];
                let neg = [v_bl < 0.0, v_br < 0.0, v_tr < 0.0, v_tl < 0.0];
                if neg.iter().all(|&b| b) || neg.iter().all(|&b| !b) {
                    continue;
                }

                let cell_x = origin_x + (ix as f32 + 0.5) * cell_size;
                let cell_y = origin_y + (iy as f32 + 0.5) * cell_size;
                let dist_from_origin = (cell_x * cell_x + cell_y * cell_y).sqrt();
                if (dist_from_origin - 1.0).abs() > cell_size * 3.0 {
                    continue;
                }

                // Compute QEF vertex for this cell
                let mut qef = Qef2D::default();
                let co_x = origin_x + ix as f32 * cell_size;
                let co_y = origin_y + iy as f32 * cell_size;

                if v_bl * v_br < 0.0 {
                    let c = find_edge_crossing_2d(
                        co_x, co_y, cell_size, 0.0, 0.0, 1.0, 0.0, v_bl, v_br,
                    );
                    let g = compute_gradient_jit(&eval_ctx, node, c[0] as f64, c[1] as f64);
                    qef.add_intersection(c, g);
                }
                if v_br * v_tr < 0.0 {
                    let c = find_edge_crossing_2d(
                        co_x, co_y, cell_size, 1.0, 0.0, 1.0, 1.0, v_br, v_tr,
                    );
                    let g = compute_gradient_jit(&eval_ctx, node, c[0] as f64, c[1] as f64);
                    qef.add_intersection(c, g);
                }
                if v_tl * v_tr < 0.0 {
                    let c = find_edge_crossing_2d(
                        co_x, co_y, cell_size, 0.0, 1.0, 1.0, 1.0, v_tl, v_tr,
                    );
                    let g = compute_gradient_jit(&eval_ctx, node, c[0] as f64, c[1] as f64);
                    qef.add_intersection(c, g);
                }
                if v_bl * v_tl < 0.0 {
                    let c = find_edge_crossing_2d(
                        co_x, co_y, cell_size, 0.0, 0.0, 0.0, 1.0, v_bl, v_tl,
                    );
                    let g = compute_gradient_jit(&eval_ctx, node, c[0] as f64, c[1] as f64);
                    qef.add_intersection(c, g);
                }

                let vert = qef.solve();
                let vert_dist = (vert[0] * vert[0] + vert[1] * vert[1]).sqrt();

                if count < 30 {
                    eprintln!(
                        "  [{:4},{:4}] cell=({:+.3},{:+.3}) QEF=({:+.4},{:+.4}) |v|={:.4}",
                        ix, iy, cell_x, cell_y, vert[0], vert[1], vert_dist
                    );
                }
                count += 1;
            }
        }
        eprintln!("  Total crossing cells sampled: {}", count);
        assert!(count > 0);
    }

    // --- Sharp corner tests (dual contouring vs marching squares) ---

    fn make_square_surface(size: f64, depth: u8) -> Surface2D {
        let half = size / 2.0;
        let x = Tree::x().abs();
        let y = Tree::y().abs();
        let tree = x.max(y) - Tree::constant(half);
        Surface2D::with_settings(
            tree,
            super::super::MeshSettings {
                depth,
                ..Default::default()
            },
        )
    }

    #[test]
    fn to_polygon_square_sharp_corners() {
        // Use depth=10 for fine grid to test corner sharpness
        let surface = make_square_surface(2.0, 10);
        let result = surface.to_polygon();
        assert!(
            result.is_ok(),
            "square should produce a polygon: {:?}",
            result
        );
        let poly = result.unwrap();
        let mp = &*poly.0;
        let exterior = mp.0[0].exterior();
        let coords: Vec<_> = exterior.coords().collect();

        // Expected corners of a 2x2 square centered at origin: (-1,-1), (1,-1), (1,1), (-1,1)
        let expected_corners = [[-1.0, -1.0], [1.0, -1.0], [1.0, 1.0], [-1.0, 1.0]];

        // Find the closest polygon vertex to each expected corner
        for &exp in &expected_corners {
            let min_dist = coords
                .iter()
                .map(|c| {
                    let dx = c.x - exp[0];
                    let dy = c.y - exp[1];
                    (dx * dx + dy * dy).sqrt()
                })
                .fold(f64::INFINITY, f64::min);
            // With dual contouring QEF, corners should be sharp (within a few cell sizes)
            // cell_size at depth=10 is ~0.02, so corners should be within ~0.05
            assert!(
                min_dist < 0.05,
                "Expected corner ({}, {}) not found: closest vertex is {:.4} away. \
                 Dual contouring should produce sharp corners.",
                exp[0],
                exp[1],
                min_dist
            );
        }
    }

    #[test]
    fn to_polygon_rectangle_sharp_corners() {
        // Rectangle with different x/y sizes to test non-square corners
        let half_x = 1.5;
        let half_y = 0.5;
        let x = Tree::x().abs();
        let y = Tree::y().abs();
        let tree = x.clone() - Tree::constant(half_x);
        let tree = tree.max(y.clone() - Tree::constant(half_y));
        let surface = Surface2D::with_settings(
            tree,
            super::super::MeshSettings {
                depth: 10,
                ..Default::default()
            },
        );
        let result = surface.to_polygon();
        assert!(result.is_ok(), "rectangle should produce a polygon");
        let poly = result.unwrap();
        let mp = &*poly.0;
        let exterior = mp.0[0].exterior();
        let coords: Vec<_> = exterior.coords().collect();

        let expected_corners = [
            [-half_x, -half_y],
            [half_x, -half_y],
            [half_x, half_y],
            [-half_x, half_y],
        ];

        for &exp in &expected_corners {
            let min_dist = coords
                .iter()
                .map(|c| {
                    let dx = c.x - exp[0];
                    let dy = c.y - exp[1];
                    (dx * dx + dy * dy).sqrt()
                })
                .fold(f64::INFINITY, f64::min);
            assert!(
                min_dist < 0.05,
                "Expected corner ({}, {}) not found: closest is {:.4} away",
                exp[0],
                exp[1],
                min_dist
            );
        }
    }

    // --- compute_grid_params ---

    #[test]
    fn compute_grid_params_default_depth() {
        // Tree::constant(0.0) has origin on boundary (not inside), falls back to extent=10
        let surface = Surface2D::new(Tree::constant(0.0));
        let (ox, oy, cs, w, h) = surface.compute_grid_params();
        assert_eq!(ox, -10.0);
        assert_eq!(oy, -10.0);
        // Dual contouring uses min depth 10: cell_size = 20/1024 ≈ 0.0195
        assert!(cs > 0.0 && cs <= 0.02);
        assert_eq!(w, h);
    }

    #[test]
    fn compute_grid_params_adaptive_small_circle() {
        // Small circle (r=0.01 = 1cm) should get adaptive bounds
        let surface = make_circle_surface(0.01);
        let (ox, oy, cs, w, h) = surface.compute_grid_params();
        // Bounds should be much smaller than default [-10, 10]
        assert!(
            ox.abs() < 1.0,
            "adaptive extent should be small for 1cm circle, got {:.2}",
            ox.abs()
        );
        assert!(
            cs > 0.0 && cs < 0.001,
            "cell_size should be tiny: {:.6}",
            cs
        );
        assert_eq!(w, h);
    }

    #[test]
    fn compute_grid_params_min_cell_size() {
        let mut surface = Surface2D::new(Tree::constant(0.0));
        surface.settings.depth = 20;
        let (_ox, _oy, cs, _w, _h) = surface.compute_grid_params();
        assert!(
            cs >= 0.0001,
            "cell size should not go below 0.0001, got {:.6}",
            cs
        );
    }

    // --- Debug test for full pipeline ---

    #[test]
    fn debug_circle_pipeline() {
        use fidget::context::Context;

        let surface = make_circle_surface(1.0);
        let (origin_x, origin_y, cell_size, grid_width, grid_height) =
            surface.compute_grid_params();

        let mut eval_ctx = Context::new();
        let node = eval_ctx.import(&surface.tree);

        let mut grid = vec![0.0f32; grid_width * grid_height];
        for iy in 0..grid_height {
            for ix in 0..grid_width {
                let x = origin_x + ix as f32 * cell_size;
                let y = origin_y + iy as f32 * cell_size;
                let val = eval_ctx
                    .eval_xyz(node, x as f64, y as f64, 0.0_f64)
                    .unwrap();
                grid[iy * grid_width + ix] = val as f32;
            }
        }

        // Check that we have some negative values (inside circle) and positive values (outside)
        let neg_count = grid.iter().filter(|&&v| v < 0.0).count();
        let pos_count = grid.iter().filter(|&&v| v > 0.0).count();
        eprintln!(
            "Grid: {}x{}, cell_size={:.4}, neg={}, pos={}",
            grid_width, grid_height, cell_size, neg_count, pos_count
        );

        // Check a few cells near center
        let mid = (grid_width / 2) as usize;
        let mid_y = (grid_height / 2) as usize;
        eprintln!(
            "Center cell [{}][{}] = {:.4}",
            mid_y,
            mid,
            grid[mid_y * grid_width + mid]
        );
        eprintln!(
            "Cell [{}][{}] = {:.4}",
            mid_y,
            mid + 1,
            grid[mid_y * grid_width + mid + 1]
        );

        let segments = marching_squares(
            &grid,
            grid_width,
            grid_height,
            origin_x,
            origin_y,
            cell_size,
        );
        eprintln!("Segments: {}", segments.len());
        for (i, seg) in segments.iter().enumerate() {
            eprintln!(
                "  Seg {}: [{:.3}, {:.3}] -> [{:.3}, {:.3}]",
                i, seg.start[0], seg.start[1], seg.end[0], seg.end[1]
            );
        }

        let loops = connect_segments_into_loops(segments.clone(), cell_size);
        eprintln!("Loops: {}", loops.len());
        for (i, loop_v) in loops.iter().enumerate() {
            let area = signed_area_2d(loop_v);
            eprintln!(
                "  Loop {}: {} vertices, signed_area={:.4}",
                i,
                loop_v.len(),
                area
            );
            for (j, pt) in loop_v.iter().enumerate() {
                eprintln!("    Vertex {}: [{:.3}, {:.3}]", j, pt[0], pt[1]);
            }
        }

        assert!(neg_count > 0, "should have negative values inside circle");
        assert!(pos_count > 0, "should have positive values outside circle");
        assert!(!segments.is_empty(), "should have segments");
        assert!(!loops.is_empty(), "should have loops");
    }

    // --- bounding_box_estimate tests ---

    #[test]
    fn bounding_box_origin_circle() {
        let surface = make_circle_surface(1.0);
        let (xmin, ymin, xmax, ymax) = surface.bounding_box_estimate();
        let tol = 0.1;
        assert!(
            (xmin - (-1.0)).abs() < tol,
            "xmin: expected ~-1.0, got {:.4}",
            xmin
        );
        assert!(
            (ymin - (-1.0)).abs() < tol,
            "ymin: expected ~-1.0, got {:.4}",
            ymin
        );
        assert!(
            (xmax - 1.0).abs() < tol,
            "xmax: expected ~1.0, got {:.4}",
            xmax
        );
        assert!(
            (ymax - 1.0).abs() < tol,
            "ymax: expected ~1.0, got {:.4}",
            ymax
        );
    }

    #[test]
    fn bounding_box_off_axis_circle() {
        // Circle centered at (5, 3) with radius 1
        let x = Tree::x();
        let y = Tree::y();
        let tree = ((x.clone() - Tree::constant(5.0_f64)).clone()
            * (x.clone() - Tree::constant(5.0_f64))
            + (y.clone() - Tree::constant(3.0_f64)) * (y.clone() - Tree::constant(3.0_f64)))
        .sqrt()
            - Tree::constant(1.0_f64);
        let surface = Surface2D::new(tree);
        let (xmin, ymin, xmax, ymax) = surface.bounding_box_estimate();
        let tol = 0.15;
        assert!(
            (xmin - 4.0).abs() < tol,
            "xmin: expected ~4.0, got {:.4}",
            xmin
        );
        assert!(
            (ymin - 2.0).abs() < tol,
            "ymin: expected ~2.0, got {:.4}",
            ymin
        );
        assert!(
            (xmax - 6.0).abs() < tol,
            "xmax: expected ~6.0, got {:.4}",
            xmax
        );
        assert!(
            (ymax - 4.0).abs() < tol,
            "ymax: expected ~4.0, got {:.4}",
            ymax
        );
    }

    #[test]
    fn bounding_box_union_distant_circles() {
        // Two circles at (-10, 0) and (10, 0), radius 1 each
        let x = Tree::x();
        let y = Tree::y();
        let c1 = ((x.clone() + Tree::constant(10.0_f64)) * (x.clone() + Tree::constant(10.0_f64))
            + y.clone() * y.clone())
        .sqrt()
            - Tree::constant(1.0_f64);
        let c2 = ((x.clone() - Tree::constant(10.0_f64)) * (x.clone() - Tree::constant(10.0_f64))
            + y.clone() * y.clone())
        .sqrt()
            - Tree::constant(1.0_f64);
        let tree = c1.min(c2);
        let surface = Surface2D::new(tree);
        let (xmin, ymin, xmax, ymax) = surface.bounding_box_estimate();
        let tol = 0.15;
        assert!(
            (xmin - (-11.0)).abs() < tol,
            "xmin: expected ~-11.0, got {:.4}",
            xmin
        );
        assert!(
            (ymin - (-1.0)).abs() < tol,
            "ymin: expected ~-1.0, got {:.4}",
            ymin
        );
        assert!(
            (xmax - 11.0).abs() < tol,
            "xmax: expected ~11.0, got {:.4}",
            xmax
        );
        assert!(
            (ymax - 1.0).abs() < tol,
            "ymax: expected ~1.0, got {:.4}",
            ymax
        );
    }

    #[test]
    fn bounding_box_empty_shape() {
        // Constant positive SDF — no zero contour anywhere
        let surface = Surface2D::new(Tree::constant(5.0_f64));
        let (xmin, ymin, xmax, ymax) = surface.bounding_box_estimate();
        assert_eq!(xmin, 0.0);
        assert_eq!(ymin, 0.0);
        assert_eq!(xmax, 0.01);
        assert_eq!(ymax, 0.01);
    }

    #[test]
    fn bounding_box_transformed_circle() {
        // Circle at origin translated to (5, 3) via transform matrix
        let surface = make_circle_surface(1.0);
        let t = nalgebra::Matrix3::<f64>::new_translation(&nalgebra::Vector2::new(5.0, 3.0));
        let translated = surface.transform(&t);
        let (xmin, ymin, xmax, ymax) = translated.bounding_box_estimate();
        let tol = 0.15;
        assert!(
            (xmin - 4.0).abs() < tol,
            "xmin: expected ~4.0, got {:.4}",
            xmin
        );
        assert!(
            (ymin - 2.0).abs() < tol,
            "ymin: expected ~2.0, got {:.4}",
            ymin
        );
        assert!(
            (xmax - 6.0).abs() < tol,
            "xmax: expected ~6.0, got {:.4}",
            xmax
        );
        assert!(
            (ymax - 4.0).abs() < tol,
            "ymax: expected ~4.0, got {:.4}",
            ymax
        );
    }

    #[test]
    fn surface2d_is_bounded() {
        // Bounded: circle
        let circle = Surface2D::new(
            (Tree::x().clone() * Tree::x().clone() + Tree::y().clone() * Tree::y().clone())
                .sqrt()
                - Tree::constant(1.0),
        );
        assert!(circle.is_bounded());

        // Unbounded: half-plane (-y) — infinite in x direction
        let halfplane = Surface2D::new(-Tree::y());
        assert!(!halfplane.is_bounded());

        // Bounded: square
        let square = Surface2D::new(
            Tree::x().abs().max(Tree::y().abs()) - Tree::constant(1.0),
        );
        assert!(square.is_bounded());

        // Unbounded: infinite strip (|y| < 1, infinite in x)
        let strip = Surface2D::new(Tree::y().abs() - Tree::constant(1.0));
        assert!(!strip.is_bounded());
    }
}

#[cfg(test)]
mod integration_tests {
    use crate::execution::test_run;
    use crate::execution::values::Value;

    #[test]
    fn integration_to_polygon_circle() {
        let result = test_run(
            "let c = (p: std.vector2.Length) -> std.scalar.Length: (p.x * p.x + p.y * p.y)::sqrt() - 1.0m; \
             in c::to_implicit()::to_polygon()"
        );
        if let Err(ref e) = result {
            eprintln!("to_polygon error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::PolygonSet(_)));
    }

    #[test]
    fn integration_boolean_union() {
        let result = test_run(
            "let a = (p: std.vector2.Length) -> std.scalar.Length: (p.x * p.x + p.y * p.y)::sqrt() - 1.0m; \
             b = (p: std.vector2.Length) -> std.scalar.Length: ((p.x - 1.0m) * (p.x - 1.0m) + p.y * p.y)::sqrt() - 0.5m; \
             combined = a::to_implicit()::union(b::to_implicit()); \
             in combined::to_polygon()"
        );
        if let Err(ref e) = result {
            eprintln!("boolean union error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::PolygonSet(_)));
    }

    #[test]
    fn integration_boolean_intersection() {
        let result = test_run(
            "let a = (p: std.vector2.Length) -> std.scalar.Length: (p.x * p.x + p.y * p.y)::sqrt() - 1.0m; \
             b = (p: std.vector2.Length) -> std.scalar.Length: ((p.x - 1.0m) * (p.x - 1.0m) + p.y * p.y)::sqrt() - 0.5m; \
             combined = a::to_implicit()::intersection(b::to_implicit()); \
             in combined::to_polygon()"
        );
        if let Err(ref e) = result {
            eprintln!("boolean intersection error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::PolygonSet(_)));
    }

    #[test]
    fn integration_boolean_difference() {
        let result = test_run(
            "let a = (p: std.vector2.Length) -> std.scalar.Length: (p.x * p.x + p.y * p.y)::sqrt() - 1.0m; \
             b = (p: std.vector2.Length) -> std.scalar.Length: ((p.x - 1.0m) * (p.x - 1.0m) + p.y * p.y)::sqrt() - 0.5m; \
             combined = a::to_implicit()::difference(b::to_implicit()); \
             in combined::to_polygon()"
        );
        if let Err(ref e) = result {
            eprintln!("boolean difference error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::PolygonSet(_)));
    }

    #[test]
    fn integration_boolean_symmetric_difference() {
        let result = test_run(
            "let a = (p: std.vector2.Length) -> std.scalar.Length: (p.x * p.x + p.y * p.y)::sqrt() - 1.0m; \
             b = (p: std.vector2.Length) -> std.scalar.Length: ((p.x - 1.0m) * (p.x - 1.0m) + p.y * p.y)::sqrt() - 0.5m; \
             combined = a::to_implicit()::symmetric_difference(b::to_implicit()); \
             in combined::to_polygon()"
        );
        if let Err(ref e) = result {
            eprintln!("boolean symmetric_difference error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::PolygonSet(_)));
    }

    #[test]
    fn integration_transform_translate() {
        let result = test_run(
            "std.implicits.circle(radius = 2.0m)::transform(std.consts.Transform2d::translate(offset = {3m, 0m}))",
        );
        if let Err(ref e) = result {
            eprintln!("transform translate error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface2D(_)));
    }

    #[test]
    fn integration_transform_scale() {
        let result = test_run(
            "std.implicits.circle(radius = 2.0m)::transform(std.consts.Transform2d::scale(scale = {2, 1}))",
        );
        if let Err(ref e) = result {
            eprintln!("transform scale error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface2D(_)));
    }

    #[test]
    fn integration_transform_rotate() {
        let result = test_run(
            "std.implicits.square(size = 2.0m)::transform(std.consts.Transform2d::rotate(angle = 45deg))",
        );
        if let Err(ref e) = result {
            eprintln!("transform rotate error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface2D(_)));
    }

    #[test]
    fn integration_transform_to_polygon() {
        let result = test_run(
            "std.implicits.circle(radius = 2.0m)::transform(std.consts.Transform2d::translate(offset = {5m, 0m}))::to_polygon()",
        );
        if let Err(ref e) = result {
            eprintln!("transform to_polygon error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::PolygonSet(_)));
    }

    #[test]
    fn integration_arithmetic_add_vector() {
        let result = test_run("std.implicits.circle(radius = 2.0m) + {3m, 0m}");
        if let Err(ref e) = result {
            eprintln!("arithmetic add vector error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface2D(_)));
    }

    #[test]
    fn integration_arithmetic_subtract_vector() {
        let result = test_run("std.implicits.circle(radius = 2.0m) - {1m, 2m}");
        if let Err(ref e) = result {
            eprintln!("arithmetic subtract vector error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface2D(_)));
    }

    #[test]
    fn integration_arithmetic_multiply_scale() {
        let result = test_run("std.implicits.circle(radius = 2.0m) * {2, 1}");
        if let Err(ref e) = result {
            eprintln!("arithmetic multiply scale error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface2D(_)));
    }

    #[test]
    fn integration_arithmetic_add_surface() {
        let result =
            test_run("std.implicits.circle(radius = 2.0m) + std.implicits.square(size = 3.0m)");
        if let Err(ref e) = result {
            eprintln!("arithmetic add surface error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface2D(_)));
    }

    #[test]
    fn integration_arithmetic_subtract_surface() {
        let result =
            test_run("std.implicits.circle(radius = 2.0m) - std.implicits.square(size = 3.0m)");
        if let Err(ref e) = result {
            eprintln!("arithmetic subtract surface error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface2D(_)));
    }

    #[test]
    fn integration_arithmetic_bit_or() {
        let result =
            test_run("std.implicits.circle(radius = 2.0m) | std.implicits.square(size = 3.0m)");
        if let Err(ref e) = result {
            eprintln!("arithmetic bit_or error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface2D(_)));
    }

    #[test]
    fn integration_arithmetic_bit_and() {
        let result =
            test_run("std.implicits.circle(radius = 2.0m) & std.implicits.square(size = 3.0m)");
        if let Err(ref e) = result {
            eprintln!("arithmetic bit_and error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface2D(_)));
    }

    #[test]
    fn integration_arithmetic_bit_xor() {
        let result =
            test_run("std.implicits.circle(radius = 2.0m) ^ std.implicits.square(size = 3.0m)");
        if let Err(ref e) = result {
            eprintln!("arithmetic bit_xor error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface2D(_)));
    }

    #[test]
    fn integration_extrude_z() {
        let result = test_run("std.implicits.circle(radius = 2.0m)::extrude_z(height = 5.0m)");
        if let Err(ref e) = result {
            eprintln!("extrude_z error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_extrude_z_to_mesh() {
        let result =
            test_run("std.implicits.circle(radius = 2.0m)::extrude_z(height = 5.0m)::to_mesh()");
        if let Err(ref e) = result {
            eprintln!("extrude_z to_mesh error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::ManifoldMesh3D(_)));
    }

    #[test]
    fn integration_revolve_y_full() {
        let result = test_run("std.implicits.circle(radius = 1.0m)::revolve_y()");
        if let Err(ref e) = result {
            eprintln!("revolve_y full error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_revolve_y_partial() {
        let result = test_run("std.implicits.circle(radius = 1.0m)::revolve_y(angle = 90deg)");
        if let Err(ref e) = result {
            eprintln!("revolve_y partial error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_revolve_y_full_to_mesh() {
        let result = test_run("std.implicits.circle(radius = 1.0m)::revolve_y()::to_mesh()");
        if let Err(ref e) = result {
            eprintln!("revolve_y full to_mesh error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::ManifoldMesh3D(_)));
    }
}
