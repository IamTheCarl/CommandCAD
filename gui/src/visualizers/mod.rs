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

mod common2d;
mod common3d;
mod implicit2d;
mod implicit3d;
mod mesh;

pub use common2d::{
    ViewState2d, build_fill_mesh_from_polygon, draw_grid, paint_linestring, paint_polygon,
};
pub use common3d::{
    ViewState3d, orbit_camera, orbit_light, setup_3d, sync_wireframe_visibility, update_3d_camera,
    update_grid,
};
pub use implicit2d::Implicit2dPlugin;
pub use implicit3d::Implicit3dPlugin;
pub use mesh::spawn_meshes;
