use std::collections::HashMap;
use std::sync::Arc;

use fidget::context::Context;
use geo::{Coord, LineString, MultiPolygon, Polygon};

use crate::execution::values::{
    BuiltinCallableDatabase, Object, StaticType, StaticTypeName, Style, Value, ValueType,
};
use crate::execution::ExecutionContext;

use super::MeshSettings;
use super::MeshingError;

use super::super::polygon::PolygonSet;

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

    /// Generate a 2D polygon set from the implicit curve using marching squares.
    ///
    /// Returns `PolygonSet` (wrapping `Arc<geo::MultiPolygon>`).
    pub fn to_polygon(&self) -> Result<PolygonSet, MeshingError> {
        let (origin_x, origin_y, cell_size, grid_width, grid_height) = self.compute_grid_params();

        let mut eval_ctx = Context::new();
        let node = eval_ctx.import(&self.tree);

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

        let segments = marching_squares(
            &grid,
            grid_width,
            grid_height,
            origin_x,
            origin_y,
            cell_size,
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
        let extent = 10.0;
        let depth = self.settings.depth as u32;
        let cell_size = (extent * 2.0 / (1u64 << depth) as f32).max(0.001);
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

/// Interpolate the zero-crossing point on a cell edge.
/// Edge indices: 0=bottom, 1=right, 2=top, 3=left
#[allow(clippy::too_many_arguments)]
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
    // Use a small snap tolerance to only merge points that are numerically close
    // (from shared edges between adjacent cells), not geometrically distinct points.
    let snap_tol = cell_size * 0.01;
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

pub mod methods {
    pub struct ToPolygon;
    pub struct Union;
    pub struct Intersection;
    pub struct Difference;
    pub struct SymmetricDifference;
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

    // --- compute_grid_params ---

    #[test]
    fn compute_grid_params_default_depth() {
        let surface = Surface2D::new(Tree::constant(0.0));
        let (ox, oy, cs, w, h) = surface.compute_grid_params();
        assert_eq!(ox, -10.0);
        assert_eq!(oy, -10.0);
        assert!(cs > 0.0 && cs <= 0.315);
        assert_eq!(w, h);
    }

    #[test]
    fn compute_grid_params_min_cell_size() {
        let mut surface = Surface2D::new(Tree::constant(0.0));
        surface.settings.depth = 20;
        let (_ox, _oy, cs, _w, _h) = surface.compute_grid_params();
        assert!(cs >= 0.001, "cell size should not go below 0.001");
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
}
