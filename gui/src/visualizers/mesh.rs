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
use bevy::{
    asset::RenderAssetUsages,
    color::palettes::css,
    pbr::wireframe::{Wireframe, WireframeColor},
    prelude::*,
    {ecs::system::Query, mesh::PrimitiveTopology},
};
use bevy_mod_outline::{OutlineMode, OutlineVolume};

use crate::visualizers::common3d::MeshModel;
use crate::{JobBridge, JobOutput};

pub fn spawn_meshes(
    mut commands: Commands,
    mut command_cad: ResMut<JobBridge>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mesh_models: Query<(Entity, &Mesh3d), With<MeshModel>>,
) {
    let is_mesh = matches!(
        &command_cad.last_result,
        Some(Ok(JobOutput::ManifoldMesh(_)))
    );

    // Remove old meshes when switching away from mesh results
    if !is_mesh && !mesh_models.is_empty() {
        for (entity, mesh) in mesh_models.iter() {
            meshes.remove(mesh.id());
            commands.entity(entity).try_despawn();
        }
        return;
    }

    if let Some(Ok(JobOutput::ManifoldMesh(manifold_state))) = &mut command_cad.last_result
        && !manifold_state.uploaded_to_gpu
    {
        manifold_state.uploaded_to_gpu = true;

        for (entity, mesh) in mesh_models.iter() {
            meshes.remove(mesh.id());
            commands.entity(entity).try_despawn();
        }

        let mut m = Mesh::new(
            PrimitiveTopology::TriangleList,
            RenderAssetUsages::default(),
        );
        let mut pos = vec![];
        let mut vns = vec![];
        for tri in manifold_state.manifold.0.triangles() {
            let [p0, p1, p2] = tri.positions;
            pos.push([p0.x as f32, p0.y as f32, p0.z as f32]);
            pos.push([p1.x as f32, p1.y as f32, p1.z as f32]);
            pos.push([p2.x as f32, p2.y as f32, p2.z as f32]);
            vns.push([
                tri.normal.x as f32,
                tri.normal.y as f32,
                tri.normal.z as f32,
            ]);
            vns.push([
                tri.normal.x as f32,
                tri.normal.y as f32,
                tri.normal.z as f32,
            ]);
            vns.push([
                tri.normal.x as f32,
                tri.normal.y as f32,
                tri.normal.z as f32,
            ]);
        }
        m.insert_attribute(Mesh::ATTRIBUTE_POSITION, pos);
        m.insert_attribute(Mesh::ATTRIBUTE_NORMAL, vns);

        let fill_color = egui::Color32::GRAY;
        let wireframe_color = egui::Color32::WHITE;

        commands.spawn((
            Mesh3d(meshes.add(m).clone()),
            MeshMaterial3d(materials.add(StandardMaterial {
                base_color: Color::Srgba(Srgba::rgb(
                    fill_color.r() as f32 / 255.0,
                    fill_color.g() as f32 / 255.0,
                    fill_color.b() as f32 / 255.0,
                )),
                ..default()
            })),
            Transform::default(),
            OutlineVolume {
                visible: true,
                width: 2.0,
                colour: Color::Srgba(Srgba::rgb(
                    wireframe_color.r() as f32 / 255.0,
                    wireframe_color.g() as f32 / 255.0,
                    wireframe_color.b() as f32 / 255.0,
                )),
            },
            OutlineMode::FloodFlatDoubleSided,
            Wireframe,
            WireframeColor {
                color: css::BLACK.into(),
            },
            MeshModel,
        ));
    }
}
