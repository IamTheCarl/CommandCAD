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

use std::{hash::Hash, sync::Arc};

use common_data_types::{Dimension, Float};
use sha2::Digest;

use crate::{
    build_function,
    execution::{
        errors::{ExecutionResult, Raise as _},
        store::StoreHasher,
        values::{
            scalar::{Length, Scalar},
            BuiltinCallableDatabase, File, IString, List, Value,
        },
        ExecutionContext, StoreTrait,
    },
    values::Object,
};

/// Shape styling properties.
struct ShapeStyle {
    fill: String,
    stroke: String,
    stroke_width: f64,
}

impl std::hash::Hash for ShapeStyle {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.fill.hash(state);
        self.stroke.hash(state);
        // Hash the bits of f64 since f64 doesn't implement Hash
        state.write_u64(self.stroke_width.to_bits());
    }
}

impl Default for ShapeStyle {
    fn default() -> Self {
        Self {
            fill: "gray".to_string(),
            stroke: "white".to_string(),
            stroke_width: 2.0,
        }
    }
}

/// A single exported shape with its geometry and styling.
struct ExportedShape {
    paths: Vec<svg::node::element::Path>,
    style: ShapeStyle,
    /// Bounding box of the raw geometry (in CAD meters).
    bbox: (f64, f64, f64, f64),
    /// SHA-256 hash of the raw geometry coordinates.
    geometry_hash: [u8; 32],
}

/// Hashable representation of exported shapes for store caching.
struct ExportCacheKey {
    shape_hashes: Vec<[u8; 32]>,
    units: Length,
}

impl std::hash::Hash for ExportCacheKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.shape_hashes.hash(state);
        // Hash the raw float value and dimension of units
        self.units.value.hash(state);
    }
}

/// Feeds coordinate bytes into a SHA-256 hasher.
fn hash_coords(coords: &[geo::Coord], state: &mut StoreHasher) {
    coords.len().hash(state);
    for coord in coords {
        coord.x.to_le_bytes().hash(state);
        coord.y.to_le_bytes().hash(state);
    }
}

/// Hashes a LineString's coordinates using SHA-256.
fn hash_linestring(line_string: &geo::LineString, is_closed: bool) -> [u8; 32] {
    let mut hasher = StoreHasher::new();
    let coords: Vec<_> = line_string.coords().cloned().collect();
    hash_coords(&coords, &mut hasher);
    is_closed.hash(&mut hasher);
    hasher.0.finalize().into()
}

/// Hashes a Polygon's exterior and interior ring coordinates using SHA-256.
fn hash_polygon(polygon: &geo::Polygon) -> [u8; 32] {
    let mut hasher = StoreHasher::new();
    // Hash exterior ring
    let exterior_coords: Vec<_> = polygon.exterior().coords().cloned().collect();
    hash_coords(&exterior_coords, &mut hasher);
    // Hash number of interior rings
    polygon.interiors().len().hash(&mut hasher);
    // Hash each interior ring
    for interior in polygon.interiors() {
        let interior_coords: Vec<_> = interior.coords().cloned().collect();
        hash_coords(&interior_coords, &mut hasher);
    }
    hasher.0.finalize().into()
}

/// Hashes a PolygonSet's constituent polygons using SHA-256.
fn hash_polygon_set(polygon_set: &geo::MultiPolygon) -> [u8; 32] {
    let mut hasher = StoreHasher::new();
    polygon_set.0.len().hash(&mut hasher);
    for polygon in polygon_set.0.iter() {
        hash_polygon(polygon).hash(&mut hasher);
    }
    hasher.0.finalize().into()
}

/// Parses a single item from the shapes list.
fn parse_shape_item(
    context: &ExecutionContext,
    value: Value,
    units: &Length,
) -> ExecutionResult<ExportedShape> {
    let style = if let Value::Dictionary(dict) = &value {
        let fill = match dict.get("fill") {
            Some(v) => {
                let cloned = v.clone();
                cloned.downcast::<IString>(context)?.0.as_str().to_string()
            }
            None => "gray".to_string(),
        };
        let stroke = match dict.get("stroke") {
            Some(v) => {
                let cloned = v.clone();
                cloned.downcast::<IString>(context)?.0.as_str().to_string()
            }
            None => "white".to_string(),
        };
        let stroke_width = match dict.get("stroke_width") {
            Some(v) => {
                let cloned = v.clone();
                *cloned.downcast::<Scalar>(context)?.value
            }
            None => 2.0,
        };
        ShapeStyle {
            fill,
            stroke,
            stroke_width,
        }
    } else {
        ShapeStyle::default()
    };

    let shape_value = if let Value::Dictionary(dict) = &value {
        dict.get("shape")
            .ok_or_else(|| {
                crate::values::MissingAttributeError {
                    name: "shape".to_string(),
                }
                .to_error(context)
            })?
            .clone()
    } else {
        value.clone()
    };

    let multiplier = *units.value;
    let (paths, bbox, geometry_hash) =
        geometry_to_paths_with_hash(&shape_value, context, multiplier)?;

    Ok(ExportedShape {
        paths,
        style,
        bbox,
        geometry_hash,
    })
}

/// Converts geometry to SVG path elements, computes bounding box, and hashes the raw geometry.
#[allow(clippy::type_complexity)]
fn geometry_to_paths_with_hash(
    value: &Value,
    context: &ExecutionContext,
    multiplier: f64,
) -> ExecutionResult<(
    Vec<svg::node::element::Path>,
    (f64, f64, f64, f64),
    [u8; 32],
)> {
    match value {
        Value::LineString(line_string) => {
            Ok(linestring_to_paths_with_hash(&line_string.0, multiplier))
        }
        Value::Polygon(polygon) => Ok(polygon_to_paths_with_hash(&polygon.0, multiplier)),
        Value::PolygonSet(polygon_set) => {
            let mut all_paths = Vec::new();
            let mut bbox = (
                f64::INFINITY,
                f64::INFINITY,
                f64::NEG_INFINITY,
                f64::NEG_INFINITY,
            );
            for polygon in polygon_set.0.iter() {
                let (paths, poly_bbox, _) = polygon_to_paths_with_hash(polygon, multiplier);
                all_paths.extend(paths);
                bbox = merge_bbox(bbox, poly_bbox);
            }
            Ok((all_paths, bbox, hash_polygon_set(&polygon_set.0)))
        }
        value => Err(crate::values::DowncastError {
            expected: "Polygon, PolygonSet, or LineString".into(),
            got: value.get_type(context).name(),
        }
        .to_error(context)),
    }
}

fn merge_bbox(a: (f64, f64, f64, f64), b: (f64, f64, f64, f64)) -> (f64, f64, f64, f64) {
    (a.0.min(b.0), a.1.min(b.1), a.2.max(b.2), a.3.max(b.3))
}

/// Converts a single LineString to SVG path elements, bounding box, and geometry hash.
fn linestring_to_paths_with_hash(
    line_string: &geo::LineString,
    multiplier: f64,
) -> (
    Vec<svg::node::element::Path>,
    (f64, f64, f64, f64),
    [u8; 32],
) {
    let mut data = svg::node::element::path::Data::new();

    let coords: Vec<_> = line_string.coords().collect();
    if coords.is_empty() {
        return (
            vec![],
            (0.0, 0.0, 100.0, 100.0),
            hash_linestring(line_string, false),
        );
    }

    let mut min_x = f64::INFINITY;
    let mut min_y = f64::INFINITY;
    let mut max_x = f64::NEG_INFINITY;
    let mut max_y = f64::NEG_INFINITY;

    let first = coords[0];
    let sx = first.x * multiplier;
    let sy = -first.y * multiplier;
    min_x = min_x.min(sx);
    min_y = min_y.min(sy);
    max_x = max_x.max(sx);
    max_y = max_y.max(sy);
    data = data.move_to((sx, sy));

    for coord in &coords[1..] {
        let sx = coord.x * multiplier;
        let sy = -coord.y * multiplier;
        min_x = min_x.min(sx);
        min_y = min_y.min(sy);
        max_x = max_x.max(sx);
        max_y = max_y.max(sy);
        data = data.line_to((sx, sy));
    }

    if line_string.is_closed() {
        data = data.close();
    }

    (
        vec![svg::node::element::Path::new().set("d", data)],
        (min_x, min_y, max_x - min_x, max_y - min_y),
        hash_linestring(line_string, line_string.is_closed()),
    )
}

/// Converts a single Polygon to SVG path elements, bounding box, and geometry hash.
fn polygon_to_paths_with_hash(
    polygon: &geo::Polygon,
    multiplier: f64,
) -> (
    Vec<svg::node::element::Path>,
    (f64, f64, f64, f64),
    [u8; 32],
) {
    let exterior = polygon.exterior();
    let mut data = svg::node::element::path::Data::new();

    let coords: Vec<_> = exterior.coords().collect();
    let mut min_x = f64::INFINITY;
    let mut min_y = f64::INFINITY;
    let mut max_x = f64::NEG_INFINITY;
    let mut max_y = f64::NEG_INFINITY;

    if !coords.is_empty() {
        let first = coords[0];
        let sx = first.x * multiplier;
        let sy = -first.y * multiplier;
        min_x = min_x.min(sx);
        min_y = min_y.min(sy);
        max_x = max_x.max(sx);
        max_y = max_y.max(sy);
        data = data.move_to((sx, sy));

        for coord in &coords[1..] {
            let sx = coord.x * multiplier;
            let sy = -coord.y * multiplier;
            min_x = min_x.min(sx);
            min_y = min_y.min(sy);
            max_x = max_x.max(sx);
            max_y = max_y.max(sy);
            data = data.line_to((sx, sy));
        }

        for interior in polygon.interiors() {
            let interior_coords: Vec<_> = interior.coords().collect();
            if !interior_coords.is_empty() {
                let first = interior_coords[0];
                let sx = first.x * multiplier;
                let sy = -first.y * multiplier;
                min_x = min_x.min(sx);
                min_y = min_y.min(sy);
                max_x = max_x.max(sx);
                max_y = max_y.max(sy);
                data = data.line_to((sx, sy));
                for coord in &interior_coords[1..] {
                    let sx = coord.x * multiplier;
                    let sy = -coord.y * multiplier;
                    min_x = min_x.min(sx);
                    min_y = min_y.min(sy);
                    max_x = max_x.max(sx);
                    max_y = max_y.max(sy);
                    data = data.line_to((sx, sy));
                }
            }
        }

        data = data.close();
    }

    (
        vec![svg::node::element::Path::new().set("d", data)],
        (min_x, min_y, max_x - min_x, max_y - min_y),
        hash_polygon(polygon),
    )
}

/// Builds the SVG document from exported shapes with optional size overrides.
fn build_document(
    shapes: &[ExportedShape],
    width_override: Option<f64>,
    height_override: Option<f64>,
) -> svg::Document {
    // Compute overall bounding box from all shapes.
    let mut min_x = f64::INFINITY;
    let mut min_y = f64::INFINITY;
    let mut max_x = f64::NEG_INFINITY;
    let mut max_y = f64::NEG_INFINITY;

    for shape in shapes {
        min_x = min_x.min(shape.bbox.0);
        min_y = min_y.min(shape.bbox.1);
        max_x = max_x.max(shape.bbox.0 + shape.bbox.2);
        max_y = max_y.max(shape.bbox.1 + shape.bbox.3);
    }

    let (bbox_width, bbox_height) = if min_x == f64::INFINITY {
        (100.0, 100.0)
    } else {
        (max_x - min_x, max_y - min_y)
    };

    // Determine final width and height with aspect ratio preservation.
    let (final_width, final_height) = match (width_override, height_override) {
        (Some(w), Some(h)) => (w, h),
        (Some(w), None) => {
            let h = bbox_height * (w / bbox_width);
            (w, h)
        }
        (None, Some(h)) => {
            let w = bbox_width * (h / bbox_height);
            (w, h)
        }
        (None, None) => (bbox_width, bbox_height),
    };

    let mut document = svg::Document::new()
        .set("xmlns", "http://www.w3.org/2000/svg")
        .set("version", "1.1")
        .set(
            "viewBox",
            format!("{min_x} {min_y} {final_width} {final_height}"),
        )
        .set("width", format!("{final_width}"))
        .set("height", format!("{final_height}"));

    for shape in shapes {
        for path in &shape.paths {
            let styled = path
                .clone()
                .set("fill", svg::node::Value::from(shape.style.fill.as_str()))
                .set(
                    "stroke",
                    svg::node::Value::from(shape.style.stroke.as_str()),
                )
                .set(
                    "stroke-width",
                    svg::node::Value::from(format!("{}", shape.style.stroke_width).as_str()),
                );

            document = document.add(styled);
        }
    }

    document
}

/// `std.export.svg` function.
pub struct ExportSvg;

/// Registers all export functions with the callable database.
pub fn register_methods_and_functions(database: &mut BuiltinCallableDatabase) {
    build_function!(
        database,
        ExportSvg, "std.export.svg", (
            context: &ExecutionContext,
            shapes: List,
            name: IString,
            units: Length = Scalar {
                dimension: Dimension::length(),
                value: Float::new(1000.0).expect("Default svg units was NaN")
            }.into(),
            width: Length = Scalar {
                dimension: Dimension::length(),
                value: Float::new(0.0).expect("Default svg width was NaN")
            }.into(),
            height: Length = Scalar {
                dimension: Dimension::length(),
                value: Float::new(0.0).expect("Default svg height was NaN")
            }.into()
        ) -> File {
            let mut exported_shapes = Vec::with_capacity(shapes.len());

            for value in shapes {
                let shape = parse_shape_item(context, value, &units)?;
                exported_shapes.push(shape);
            }

            let width_override = if *width.value > 0.0 {
                Some(*width.value)
            } else {
                None
            };

            let height_override = if *height.value > 0.0 {
                Some(*height.value)
            } else {
                None
            };

            let document = build_document(&exported_shapes, width_override, height_override);

            let cache_key = ExportCacheKey {
                shape_hashes: exported_shapes.iter().map(|s| s.geometry_hash).collect(),
                units: units.clone(),
            };
            let path = context.store.get_or_init_file(
                context,
                &(&cache_key,),
                format!("{}.svg", name.0),
                |file| {
                    svg::write(file, &document).map_err(|error| {
                        let msg = format!("Failed to write SVG: {error}");
                        crate::execution::errors::StringError(msg).to_error(context)
                    })?;
                    Ok(())
                },
            )?;

            Ok(File { path: Arc::new(path) })
        }
    );
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::execution::standard_environment::build_prelude;
    use crate::execution::store::FsStore;
    use crate::execution::test_run;
    use std::collections::HashMap;
    use std::path::Path;
    use std::sync::Mutex;
    use tempfile::TempDir;

    fn test_run_with_content(input: &str) -> (ExecutionResult<Value>, TempDir) {
        let database = crate::execution::values::BuiltinCallableDatabase::new();
        let prelude = build_prelude(&database);
        let store_directory = TempDir::new().unwrap();
        let store = crate::execution::Store::FsStore(FsStore::new(store_directory.path()));
        let file_cache = Mutex::new(HashMap::new());
        let working_directory = Path::new(".");
        let shutdown_signal = std::sync::atomic::AtomicBool::new(false);

        let context = crate::execution::ExecutionContext {
            shutdown_singal: &shutdown_signal,
            log: &Mutex::new(Vec::new()),
            stack_trace: &crate::execution::StackTrace::test(),
            stack: &crate::execution::StackScope::top(&prelude),
            database: &database,
            store: &store,
            file_cache: &file_cache,
            working_directory,
            import_limit: 100,
        };

        let root = crate::compile::full_compile(input);
        let result = crate::execution::execute_expression(&context, &root);
        (result, store_directory)
    }

    #[test]
    fn export_svg_basic_polygon() {
        let result = test_run(
            "std.export.svg(shapes = [std.polygon.box(size = {1m, 1m})], name = \"test_box\")",
        );
        assert!(
            result.is_ok(),
            "Basic polygon export failed: {:?}",
            result.err()
        );
    }

    #[test]
    fn export_svg_line_string() {
        let result = test_run(
            "std.export.svg(shapes = [std.line_string.from_points(points = [{0m, 0m}, {1m, 1m}])], name = \"test_line\")"
        );
        assert!(
            result.is_ok(),
            "LineString export failed: {:?}",
            result.err()
        );
    }

    #[test]
    fn export_svg_polygon_set() {
        let result = test_run(
            "std.export.svg(shapes = [std.polygon_set.from_polys(polys = [std.polygon.box(size = {1m, 1m}), std.polygon.box(size = {2m, 2m})])], name = \"test_set\")"
        );
        assert!(
            result.is_ok(),
            "PolygonSet export failed: {:?}",
            result.err()
        );
    }

    #[test]
    fn export_svg_dictionary_styling() {
        let result = test_run(
            "std.export.svg(shapes = [(shape = std.polygon.box(size = {1m, 1m}), fill = \"blue\", stroke = \"red\", stroke_width = 3)], name = \"test_styled\")"
        );
        assert!(result.is_ok(), "Styled export failed: {:?}", result.err());
    }

    #[test]
    fn export_svg_mixed_shapes() {
        let result = test_run(
            r#"std.export.svg(shapes = [
                std.polygon.box(size = {1m, 1m}),
                (shape = std.polygon.circle(radius = 1m, number_of_points = 24u), fill = "none", stroke = "green"),
                std.line_string.from_points(points = [{0m, 0m}, {2m, 2m}])
            ], name = "test_mixed")"#,
        );
        assert!(
            result.is_ok(),
            "Mixed shapes export failed: {:?}",
            result.err()
        );
    }

    #[test]
    fn export_svg_no_shapes() {
        let result = test_run("std.export.svg(shapes = [], name = \"test_empty\")");
        assert!(
            result.is_ok(),
            "Empty shapes export failed: {:?}",
            result.err()
        );
    }

    #[test]
    fn hash_linestring_different_coords() {
        let mut hasher1 = StoreHasher::new();
        let mut hasher2 = StoreHasher::new();
        // Hash using bit representation since f64 doesn't implement Hash
        1.0f64.to_bits().hash(&mut hasher1);
        2.0f64.to_bits().hash(&mut hasher1);
        1.0f64.to_bits().hash(&mut hasher2);
        3.0f64.to_bits().hash(&mut hasher2);
        assert_ne!(hasher1.0.finalize(), hasher2.0.finalize());
    }

    #[test]
    fn hash_polygon_different_exterior() {
        let mut hasher1 = StoreHasher::new();
        let mut hasher2 = StoreHasher::new();
        // Hash using bit representation since Coord doesn't implement Hash
        0.0f64.to_bits().hash(&mut hasher1);
        0.0f64.to_bits().hash(&mut hasher1);
        1.0f64.to_bits().hash(&mut hasher1);
        0.0f64.to_bits().hash(&mut hasher1);
        0.0f64.to_bits().hash(&mut hasher2);
        0.0f64.to_bits().hash(&mut hasher2);
        2.0f64.to_bits().hash(&mut hasher2);
        0.0f64.to_bits().hash(&mut hasher2);
        assert_ne!(hasher1.0.finalize(), hasher2.0.finalize());
    }

    #[test]
    fn export_svg_auto_size_tight_fit() {
        let (result, _temp_dir) = test_run_with_content(
            "std.export.svg(shapes = [std.polygon.box(size = {1m, 2m})], name = \"test_tight\")",
        );
        assert!(
            result.is_ok(),
            "Auto size export failed: {:?}",
            result.err()
        );
        let content = match result.unwrap() {
            Value::File(file) => std::fs::read_to_string(file.path.as_path()).unwrap(),
            _ => panic!("Expected File"),
        };
        assert!(
            content.contains("viewBox=\"0 -2000 1000 2000\""),
            "Expected viewBox=\"0 -2000 1000 2000\", got: {}",
            content
        );
    }

    #[test]
    fn export_svg_auto_size_with_offset() {
        let (result, _temp_dir) = test_run_with_content(
            "std.export.svg(shapes = [std.polygon.box_from_points(a = {1m, 1m}, b = {3m, 4m})], name = \"test_offset\")",
        );
        assert!(
            result.is_ok(),
            "Auto size with offset export failed: {:?}",
            result.err()
        );
        let content = match result.unwrap() {
            Value::File(file) => std::fs::read_to_string(file.path.as_path()).unwrap(),
            _ => panic!("Expected File"),
        };
        assert!(
            content.contains("viewBox=\"1000 -4000 2000 3000\""),
            "Expected viewBox=\"1000 -4000 2000 3000\", got: {}",
            content
        );
    }

    #[test]
    fn export_svg_explicit_width_and_height() {
        let (result, _temp_dir) = test_run_with_content(
            "std.export.svg(shapes = [std.polygon.box(size = {1m, 1m})], name = \"test_explicit\", width = 500mm, height = 500mm)",
        );
        assert!(
            result.is_ok(),
            "Explicit size export failed: {:?}",
            result.err()
        );
        let content = match result.unwrap() {
            Value::File(file) => std::fs::read_to_string(file.path.as_path()).unwrap(),
            _ => panic!("Expected File"),
        };
        assert!(
            content.contains("viewBox=\"0 -1000 0.5 0.5\""),
            "Expected viewBox=\"0 -1000 0.5 0.5\", got: {}",
            content
        );
        assert!(
            content.contains("width=\"0.5\""),
            "Expected width=\"0.5\", got: {}",
            content
        );
        assert!(
            content.contains("height=\"0.5\""),
            "Expected height=\"0.5\", got: {}",
            content
        );
    }

    #[test]
    fn export_svg_width_only_derives_height() {
        let (result, _temp_dir) = test_run_with_content(
            "std.export.svg(shapes = [std.polygon.box(size = {1m, 2m})], name = \"test_width_only\", width = 200mm)",
        );
        assert!(
            result.is_ok(),
            "Width-only size export failed: {:?}",
            result.err()
        );
        let content = match result.unwrap() {
            Value::File(file) => std::fs::read_to_string(file.path.as_path()).unwrap(),
            _ => panic!("Expected File"),
        };
        assert!(
            content.contains("viewBox=\"0 -2000 0.2 0.4\""),
            "Expected viewBox=\"0 -2000 0.2 0.4\", got: {}",
            content
        );
        assert!(
            content.contains("width=\"0.2\""),
            "Expected width=\"0.2\", got: {}",
            content
        );
        assert!(
            content.contains("height=\"0.4\""),
            "Expected height=\"0.4\", got: {}",
            content
        );
    }

    #[test]
    fn export_svg_height_only_derives_width() {
        let (result, _temp_dir) = test_run_with_content(
            "std.export.svg(shapes = [std.polygon.box(size = {1m, 1m})], name = \"test_height_only\", height = 300mm)",
        );
        assert!(
            result.is_ok(),
            "Height-only size export failed: {:?}",
            result.err()
        );
        let content = match result.unwrap() {
            Value::File(file) => std::fs::read_to_string(file.path.as_path()).unwrap(),
            _ => panic!("Expected File"),
        };
        assert!(
            content.contains("viewBox=\"0 -1000 0.3 0.3\""),
            "Expected viewBox=\"0 -1000 0.3 0.3\", got: {}",
            content
        );
        assert!(
            content.contains("width=\"0.3\""),
            "Expected width=\"0.3\", got: {}",
            content
        );
        assert!(
            content.contains("height=\"0.3\""),
            "Expected height=\"0.3\", got: {}",
            content
        );
    }
}
