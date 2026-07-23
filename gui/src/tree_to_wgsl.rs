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

//! Converts fidget Tree expressions into WGSL shader source strings.
//!
//! Generates two shader modules:
//! - **Main pass**: ray-marches SDF to intermediate color + depth textures
//! - **Writeback pass**: composites intermediate results onto main render target with depth testing

use std::collections::HashMap;

use fidget::context::{BinaryOpcode, Tree, UnaryOpcode};
use fidget::var::Var;

/// Extracts the SDF expression body from a fidget `Tree` as WGSL.
pub fn emit_sdf_body(tree: &Tree) -> String {
    emit_expr(tree, &mut HashMap::new())
}

/// Converts a fidget `Tree` into the main-pass WGSL shader.
///
/// This fragment shader ray-marches the SDF and writes color to the color
/// attachment and depth to `@builtin(frag_depth)`.
#[cfg(test)]
pub fn tree_to_wgsl_main(tree: &Tree) -> String {
    let sdf_body = emit_sdf_body(tree);
    tree_to_wgsl_main_from_sdf(&sdf_body)
}

pub fn tree_to_wgsl_main_from_sdf(sdf_body: &str) -> String {
    format!(
        r#"{header}

fn sdf(pos: vec3<f32>) -> f32 {{
    return {sdf_body};
}}

struct MainOutput {{
    @location(0) color: vec4<f32>,
    @builtin(frag_depth) depth: f32,
}}

@fragment
fn fs(@location(0) uv: vec2<f32>) -> MainOutput {{
    var out: MainOutput;

    // NDC from UV: (-1,-1) at bottom-left, (1,1) at top-right
    let ndc_x = uv.x * 2.0 - 1.0;
    let ndc_y = -(uv.y * 2.0 - 1.0);

    // Orthographic ray: parallel rays from camera through each pixel
    let ray_origin = u.camera_position.xyz
        + ndc_x * u.ortho_half_width * u.camera_right.xyz
        + ndc_y * u.ortho_half_height * u.camera_up.xyz;
    let ray_dir = u.camera_forward.xyz;

    var p = ray_origin;
    var dist: f32 = 0.0;
    var nearest: f32 = 100.0;
    var nearest_t: f32 = 0.0;
    var t: f32 = 0.0;
    var i: u32 = 0u;
    let max_dist: f32 = 100.0;
    // Epsilon scales with zoom: 0.5 pixel precision at any zoom level
    let world_units_per_pixel = (u.ortho_half_width * 2.0) / u.viewport_size.x;
    let epsilon = 0.5 * world_units_per_pixel;

    while (true) {{
        dist = sdf(p);
        if (dist < nearest) {{
            nearest = dist;
            nearest_t = t;
        }}
        if (dist < epsilon) {{ break; }}
        if (dist > max_dist) {{ break; }}
        p += ray_dir * dist;
        t += dist;
        i += 1u;
        if (i >= 128u) {{ break; }}
    }}

    // No hit — check for near-miss outline
    if (i >= 128u || dist > max_dist) {{
        // SDF Near-Miss outline: ~2-pixel width in screen space
        let threshold = 2.0 * world_units_per_pixel;
        let t_norm = clamp(nearest / threshold, 0.0, 1.0);
        let outline = 1.0 - pow(t_norm, 8.0);
        if (outline > 0.001) {{
            out.color = vec4<f32>(1.0, 1.0, 1.0, outline);
            // Depth at estimated surface position (where near-miss occurred + offset to surface)
            out.depth = -(nearest_t + nearest);
        }} else {{
            out.color = vec4<f32>(0.0, 0.0, 0.0, 0.0);
            out.depth = 0.0;
        }}
        return out;
    }}

    // Hit distance from camera (for depth)
    let hit_dist = length(p - ray_origin);

    // Finite-difference normal
    let e = 0.002;
    let nx = sdf(vec3<f32>(p.x + e, p.y, p.z)) - sdf(vec3<f32>(p.x - e, p.y, p.z));
    let ny = sdf(vec3<f32>(p.x, p.y + e, p.z)) - sdf(vec3<f32>(p.x, p.y - e, p.z));
    let nz = sdf(vec3<f32>(p.x, p.y, p.z + e)) - sdf(vec3<f32>(p.x, p.y, p.z - e));
    let normal = normalize(vec3<f32>(nx, ny, nz) / (2.0 * e));

    // Lighting matching Bevy PBR: directional light orbits camera and looks
    // at origin, so light rays travel opposite to camera view direction.
    let light_dir = -u.camera_forward.xyz;
    let diff = max(dot(normal, light_dir), 0.0);
    // Base color matches mesh StandardMaterial: gray (0.502) with PBR lighting.
    // Ambient from GlobalAmbientLight (50 lux) + directional (1000 lux).
    let base_color: f32 = 0.502;
    let color = base_color * (0.05 + 0.95 * diff);

    out.color = vec4<f32>(color, color, color, 1.0);
    // Negate depth so that Greater compare means "SDF is closer"
    // (bevy_march convention: smaller hit_dist = more negative = passes Greater test)
    out.depth = -hit_dist;
    return out;
}}
"#,
        header = MAIN_UNIFORM_HEADER,
        sdf_body = sdf_body,
    )
}

/// Generates the writeback WGSL shader (static, no SDF dependency).
///
/// Samples the intermediate color/depth textures and composites onto the
/// main render target with depth testing. Only writes where the SDF hit
/// is closer than existing scene geometry (`depth_compare: Greater`).
pub fn tree_to_wgsl_writeback() -> String {
    String::from(
        r#"@group(0) @binding(0)
var implicit_color_texture: texture_2d<f32>;

@group(0) @binding(1)
var implicit_sampler: sampler;

@fragment
fn fs(@location(0) uv: vec2<f32>) -> @location(0) vec4<f32> {
    let color = textureSample(implicit_color_texture, implicit_sampler, uv);

    // No SDF hit — discard entirely
    if (color.a == 0.0) {
        discard;
    }

    // Un-premultiply and write
    return vec4<f32>(color.rgb / color.a, 1.0);
}
"#,
    )
}

const MAIN_UNIFORM_HEADER: &str = r#"@group(0) @binding(0)
var<uniform> u: ImplicitUniform;

struct ImplicitUniform {
    camera_position: vec4<f32>,
    camera_right: vec4<f32>,
    camera_up: vec4<f32>,
    camera_forward: vec4<f32>,
    ortho_half_width: f32,
    ortho_half_height: f32,
    viewport_size: vec2<f32>,
}
"#;


/// Recursively converts a fidget TreeOp into a WGSL expression string.
fn emit_expr(tree_op: &fidget::context::TreeOp, var_map: &mut HashMap<Var, String>) -> String {
    match tree_op {
        fidget::context::TreeOp::Input(var) => {
            if let Some(expr) = var_map.get(var) {
                expr.clone()
            } else {
                match var {
                    Var::X => "pos.x".to_string(),
                    Var::Y => "pos.y".to_string(),
                    Var::Z => "pos.z".to_string(),
                    Var::V(_) => "0.0".to_string(),
                }
            }
        }
        fidget::context::TreeOp::Const(v) => f32_from(*v),
        fidget::context::TreeOp::Binary(op, lhs, rhs) => {
            let lhs_str = emit_expr(lhs, var_map);
            let rhs_str = emit_expr(rhs, var_map);
            emit_binary(op, &lhs_str, &rhs_str)
        }
        fidget::context::TreeOp::Unary(op, child) => {
            let child_str = emit_expr(child, var_map);
            emit_unary(op, &child_str)
        }
        fidget::context::TreeOp::RemapAxes { target, x, y, z } => {
            let x_expr = emit_expr(x, var_map);
            let y_expr = emit_expr(y, var_map);
            let z_expr = emit_expr(z, var_map);

            let old_x = var_map.insert(Var::X, x_expr);
            let old_y = var_map.insert(Var::Y, y_expr);
            let old_z = var_map.insert(Var::Z, z_expr);

            let result = emit_expr(target, var_map);

            restore_var(var_map, &Var::X, old_x);
            restore_var(var_map, &Var::Y, old_y);
            restore_var(var_map, &Var::Z, old_z);

            result
        }
        fidget::context::TreeOp::RemapAffine { target, mat } => {
            let m = mat.to_homogeneous();
            let new_x = affine_component(&m, 0, var_map);
            let new_y = affine_component(&m, 1, var_map);
            let new_z = affine_component(&m, 2, var_map);

            let old_x = var_map.insert(Var::X, new_x);
            let old_y = var_map.insert(Var::Y, new_y);
            let old_z = var_map.insert(Var::Z, new_z);

            let result = emit_expr(target, var_map);

            restore_var(var_map, &Var::X, old_x);
            restore_var(var_map, &Var::Y, old_y);
            restore_var(var_map, &Var::Z, old_z);

            result
        }
    }
}

fn restore_var(var_map: &mut HashMap<Var, String>, var: &Var, old: Option<String>) {
    if let Some(v) = old {
        var_map.insert(*var, v);
    } else {
        var_map.remove(var);
    }
}

fn emit_binary(op: &BinaryOpcode, lhs: &str, rhs: &str) -> String {
    match op {
        BinaryOpcode::Add => format!("({lhs} + {rhs})"),
        BinaryOpcode::Sub => format!("({lhs} - {rhs})"),
        BinaryOpcode::Mul => format!("({lhs} * {rhs})"),
        BinaryOpcode::Div => format!("({lhs} / {rhs})"),
        BinaryOpcode::Mod => format!("mod({lhs}, {rhs})"),
        BinaryOpcode::Min | BinaryOpcode::And => format!("min({lhs}, {rhs})"),
        BinaryOpcode::Max | BinaryOpcode::Or => format!("max({lhs}, {rhs})"),
        BinaryOpcode::Atan => format!("atan({lhs}, {rhs})"),
        BinaryOpcode::Compare => {
            format!("select(-1.0, 1.0, {lhs} > {rhs})")
        }
    }
}

fn emit_unary(op: &UnaryOpcode, expr: &str) -> String {
    match op {
        UnaryOpcode::Neg => format!("(-{expr})"),
        UnaryOpcode::Not => format!("(bool({expr}) == false) as f32"),
        UnaryOpcode::Abs => format!("abs({expr})"),
        UnaryOpcode::Recip => format!("(1.0 / {expr})"),
        UnaryOpcode::Sqrt => format!("sqrt({expr})"),
        UnaryOpcode::Square => format!("({expr} * {expr})"),
        UnaryOpcode::Floor => format!("floor({expr})"),
        UnaryOpcode::Ceil => format!("ceil({expr})"),
        UnaryOpcode::Round => format!("round({expr})"),
        UnaryOpcode::Sin => format!("sin({expr})"),
        UnaryOpcode::Cos => format!("cos({expr})"),
        UnaryOpcode::Tan => format!("tan({expr})"),
        UnaryOpcode::Asin => format!("asin({expr})"),
        UnaryOpcode::Acos => format!("acos({expr})"),
        UnaryOpcode::Atan => format!("atan({expr})"),
        UnaryOpcode::Exp => format!("exp({expr})"),
        UnaryOpcode::Ln => format!("log2({expr}) * 1.44269504089"),
    }
}

/// Builds a single affine-transformed coordinate from a matrix row.
fn affine_component(
    m: &nalgebra::Matrix4<f64>,
    row: usize,
    _var_map: &mut HashMap<Var, String>,
) -> String {
    let vars = ["pos.x", "pos.y", "pos.z", "1.0"];
    let mut terms: Vec<String> = Vec::new();

    for col in 0..4 {
        let coeff = m[(row, col)];
        if coeff.abs() <= 1e-10 {
            continue;
        }
        let scaled = if (coeff - 1.0).abs() < 1e-10 {
            vars[col].to_string()
        } else if (coeff + 1.0).abs() < 1e-10 {
            format!("-{}", vars[col])
        } else {
            format!("{} * {}", f32_from(coeff), vars[col])
        };
        terms.push(scaled);
    }

    if terms.is_empty() {
        "0.0".to_string()
    } else if terms.len() == 1 {
        terms.into_iter().next().unwrap()
    } else {
        format!("({})", terms.join(" + "))
    }
}

fn f32_from(v: f64) -> String {
    let f = v as f32;
    if f.fract() == 0.0 && f.abs() < 1000.0 {
        format!("{:.1}", f)
    } else {
        format!("{}", f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sphere_expr() {
        let x = Tree::x();
        let y = Tree::y();
        let z = Tree::z();
        let tree = (x.clone() * x.clone() + y.clone() * y.clone() + z.clone() * z.clone()).sqrt()
            - Tree::constant(5.0);
        let wgsl = tree_to_wgsl_main(&tree);
        assert!(wgsl.contains("pos"));
        assert!(wgsl.contains("sqrt"));
    }

    #[test]
    fn test_cube_expr() {
        let x = Tree::x().abs();
        let y = Tree::y().abs();
        let z = Tree::z().abs();
        let tree = x.max(y).max(z) - Tree::constant(2.0);
        let wgsl = tree_to_wgsl_main(&tree);
        assert!(wgsl.contains("abs"));
    }

    #[test]
    fn test_cylinder_expr() {
        let x = Tree::x();
        let y = Tree::y();
        let z = Tree::z().abs();
        let radial = (x.clone() * x.clone() + y.clone() * y.clone()).sqrt() - Tree::constant(3.0);
        let axial = z - Tree::constant(4.0);
        let tree = radial.max(axial);
        let wgsl = tree_to_wgsl_main(&tree);
        assert!(wgsl.contains("sqrt"));
    }

    #[test]
    fn test_has_sdf_function() {
        let tree = Tree::x();
        let wgsl = tree_to_wgsl_main(&tree);
        assert!(wgsl.contains("fn sdf"));
        assert!(wgsl.contains("@fragment"));
    }

    #[test]
    fn test_writeback_shader() {
        let wgsl = tree_to_wgsl_writeback();
        assert!(wgsl.contains("implicit_color_texture"));
        assert!(wgsl.contains("discard"));
    }

    // ─── Naga validation tests ──────────────────────────────────────────────

    fn validate_wgsl(wgsl: &str, label: &str) {
        let result = naga::front::wgsl::parse_str(wgsl);
        let module = match result {
            Ok(m) => m,
            Err(e) => panic!("{}: WGSL parse failed: {:?}", label, e),
        };
        let info = match naga::valid::Validator::new(
            naga::valid::ValidationFlags::all(),
            naga::valid::Capabilities::all(),
        )
        .validate(&module)
        {
            Ok(i) => i,
            Err(e) => panic!("{}: WGSL validation failed: {:?}", label, e),
        };
        assert!(
            !naga::back::wgsl::write_string(&module, &info, naga::back::wgsl::WriterFlags::empty())
                .unwrap()
                .is_empty(),
            "{}: WGSL round-trip failed",
            label,
        );
    }

    #[test]
    fn test_validate_main_sphere() {
        let x = Tree::x();
        let y = Tree::y();
        let z = Tree::z();
        let tree = (x.clone() * x.clone() + y.clone() * y.clone() + z.clone() * z.clone()).sqrt()
            - Tree::constant(5.0);
        let wgsl = tree_to_wgsl_main(&tree);
        validate_wgsl(&wgsl, "sphere main");
    }

    #[test]
    fn test_validate_main_cube() {
        let x = Tree::x().abs();
        let y = Tree::y().abs();
        let z = Tree::z().abs();
        let tree = x.max(y).max(z) - Tree::constant(2.0);
        let wgsl = tree_to_wgsl_main(&tree);
        validate_wgsl(&wgsl, "cube main");
    }

    #[test]
    fn test_validate_main_cylinder() {
        let x = Tree::x();
        let y = Tree::y();
        let z = Tree::z().abs();
        let radial = (x.clone() * x.clone() + y.clone() * y.clone()).sqrt() - Tree::constant(3.0);
        let axial = z - Tree::constant(4.0);
        let tree = radial.max(axial);
        let wgsl = tree_to_wgsl_main(&tree);
        validate_wgsl(&wgsl, "cylinder main");
    }

    #[test]
    fn test_validate_main_cone() {
        let x = Tree::x();
        let y = Tree::y();
        let z = Tree::z();
        let r = (x.clone() * x.clone() + y.clone() * y.clone()).sqrt();
        let tree = r * Tree::constant(1.0) - z * Tree::constant(0.5);
        let wgsl = tree_to_wgsl_main(&tree);
        validate_wgsl(&wgsl, "cone main");
    }

    #[test]
    fn test_validate_main_torus() {
        let x = Tree::x();
        let y = Tree::y();
        let z = Tree::z();
        let r = (x.clone() * x.clone() + y.clone() * y.clone()).sqrt();
        let tree = ((r - Tree::constant(3.0)).clone().square() + z.clone() * z.clone()).sqrt()
            - Tree::constant(1.0);
        let wgsl = tree_to_wgsl_main(&tree);
        validate_wgsl(&wgsl, "torus main");
    }

    #[test]
    fn test_validate_main_union() {
        let sphere1 = (Tree::x().clone() * Tree::x().clone()
            + Tree::y().clone() * Tree::y().clone()
            + Tree::z().clone() * Tree::z().clone())
        .sqrt()
            - Tree::constant(1.0);
        let sphere2 = ((Tree::x().clone() - Tree::constant(3.0)).clone().square()
            + Tree::y().clone() * Tree::y().clone()
            + Tree::z().clone() * Tree::z().clone())
        .sqrt()
            - Tree::constant(1.0);
        let tree = sphere1.min(sphere2);
        let wgsl = tree_to_wgsl_main(&tree);
        validate_wgsl(&wgsl, "union main");
    }

    #[test]
    fn test_validate_main_intersection() {
        let sphere1 = (Tree::x().clone() * Tree::x().clone()
            + Tree::y().clone() * Tree::y().clone()
            + Tree::z().clone() * Tree::z().clone())
        .sqrt()
            - Tree::constant(2.0);
        let sphere2 = ((Tree::x().clone() - Tree::constant(1.0)).clone().square()
            + Tree::y().clone() * Tree::y().clone()
            + Tree::z().clone() * Tree::z().clone())
        .sqrt()
            - Tree::constant(2.0);
        let tree = sphere1.max(sphere2);
        let wgsl = tree_to_wgsl_main(&tree);
        validate_wgsl(&wgsl, "intersection main");
    }

    #[test]
    fn test_validate_main_difference() {
        let sphere1 = (Tree::x().clone() * Tree::x().clone()
            + Tree::y().clone() * Tree::y().clone()
            + Tree::z().clone() * Tree::z().clone())
        .sqrt()
            - Tree::constant(2.0);
        let sphere2 = ((Tree::x().clone() - Tree::constant(1.0)).clone().square()
            + Tree::y().clone() * Tree::y().clone()
            + Tree::z().clone() * Tree::z().clone())
        .sqrt()
            - Tree::constant(2.0);
        let tree = sphere1.max(-sphere2);
        let wgsl = tree_to_wgsl_main(&tree);
        validate_wgsl(&wgsl, "difference main");
    }

    #[test]
    fn test_validate_writeback() {
        let wgsl = tree_to_wgsl_writeback();
        validate_wgsl(&wgsl, "writeback");
    }

    #[test]
    fn test_main_has_frag_depth_output() {
        // Validates the main shader's output struct has @builtin(frag_depth).
        // This catches issues where depth output is missing or malformed,
        // which would cause wgpu validation errors at runtime
        // (e.g., clear value out of [0,1] range for Depth32Float).
        let tree = Tree::x();
        let wgsl = tree_to_wgsl_main(&tree);
        assert!(
            wgsl.contains("@builtin(frag_depth)"),
            "Main shader must output @builtin(frag_depth)"
        );
        assert!(
            wgsl.contains("struct MainOutput"),
            "Main shader must have MainOutput struct"
        );
        // Validate the full module parses and validates with naga
        validate_wgsl(&wgsl, "frag_depth main");
    }
}
