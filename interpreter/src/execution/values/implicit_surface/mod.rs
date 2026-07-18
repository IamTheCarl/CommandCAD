use std::collections::HashMap;

use common_data_types::Dimension;
use thiserror::Error;

use crate::compile::{AstNode, Expression};

mod fidget_conversion;
pub use fidget_conversion::{ast_to_fidget, resolve_captured_values, CapturedClosure, ParamDim};

pub mod surface2d;
pub use surface2d::Surface2D;

pub mod surface3d;
pub use surface3d::Surface3D;

/// Mesh generation settings for implicit surface conversion.
#[derive(Debug, Clone)]
pub struct MeshSettings {
    /// Octree depth for 3D mesh resolution (higher = more detail, slower)
    pub depth: u8,
    /// World-to-model transform (optional, for positioning/scale)
    pub world_to_model: Option<[[f32; 4]; 4]>,
    /// Number of threads for mesh generation
    pub threads: usize,
    /// Bounding box for meshing: (min_x, min_y, min_z, max_x, max_y, max_z)
    pub bounding_box: Option<(f64, f64, f64, f64, f64, f64)>,
}

impl PartialEq for MeshSettings {
    fn eq(&self, other: &Self) -> bool {
        self.depth == other.depth
            && self.threads == other.threads
            && match (&self.world_to_model, &other.world_to_model) {
                (None, None) => true,
                (Some(a), Some(b)) => a == b,
                _ => false,
            }
            && self.bounding_box == other.bounding_box
    }
}

impl Eq for MeshSettings {}

impl Default for MeshSettings {
    fn default() -> Self {
        Self {
            depth: 6,
            world_to_model: None,
            threads: std::thread::available_parallelism()
                .map(|n| n.get())
                .unwrap_or(1),
            bounding_box: Some((-10.0, -10.0, -10.0, 10.0, 10.0, 10.0)),
        }
    }
}

/// Errors that can occur during mesh generation.
#[derive(Debug, Error, Clone)]
#[error("Meshing failed: {0}")]
pub struct MeshingError(pub String);

/// Converts a Command CAD AST expression to the appropriate surface type.
///
/// This is the main entry point for `closure::to_implicit()`.
pub fn ast_to_shape(
    expression: &AstNode<Expression>,
    captured_values: &HashMap<String, f64>,
    closures: &HashMap<String, CapturedClosure>,
    param_dim: ParamDim,
) -> Result<crate::execution::values::Value, MeshingError> {
    let (tree, result_dim) = ast_to_fidget(expression, captured_values, closures, param_dim)
        .map_err(|e| MeshingError(e.to_string()))?;

    // Check that the SDF expression has length dimension
    if result_dim != Dimension::length() {
        return Err(MeshingError(format!(
            "SDF expression must have length dimension, got {:?}",
            result_dim
        )));
    }

    match param_dim {
        ParamDim::Vec2 => {
            let surface = Surface2D::new(tree);
            Ok(surface.into())
        }
        ParamDim::Vec3 => {
            let surface = Surface3D::new(tree);
            Ok(surface.into())
        }
    }
}
