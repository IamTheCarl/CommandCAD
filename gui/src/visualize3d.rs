use bevy::asset::RenderAssetUsages;
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
use bevy::camera::ScalingMode;
use bevy::{
    color::palettes::css::*,
    pbr::wireframe::{Wireframe, WireframeColor},
    prelude::*,
};
use bevy::{ecs::system::Query, mesh::PrimitiveTopology};

use crate::{JobBridge, JobOutput, ViewState};

#[derive(Debug, Default, Resource)]
pub struct ViewState3d;

pub fn setup_3d(mut commands: Commands) {
    commands.spawn((
        // Transform::from_translation(Vec3::new(0.0, 1.5, 5.0)),
        // PanOrbitCamera {
        //     zoom_upper_limit: Some(1.0),
        //     zoom_lower_limit: 1.0,
        //     ..default()
        // },
        Camera3d::default(),
        Projection::from(OrthographicProjection {
            scaling_mode: ScalingMode::WindowSize,
            // scaling_mode: ScalingMode::FixedVertical {
            //     viewport_height: 1.0,
            // },
            ..OrthographicProjection::default_3d()
        }),
        Transform::from_xyz(0.0, 0.0, -5.0).looking_at(Vec3::ZERO, Vec3::Y),
    ));
}

pub fn update_projection(
    view_state: Res<ViewState>,
    mut cameras: Query<&mut Projection, With<Camera3d>>,
) {
    for mut projection in &mut cameras {
        if let Projection::Orthographic(projection) = &mut *projection {
            projection.scale = 1.0 / view_state.pixels_per_meter();
        }
    }
}

#[derive(Component)]
pub struct MeshModel;

pub fn spawn_meshes(
    mut commands: Commands,
    mut command_cad: ResMut<JobBridge>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mesh_models: Query<Entity, With<MeshModel>>,
) {
    if let Some(Ok(JobOutput::ManifoldMesh(manifold_state))) = &mut command_cad.last_result
        && !manifold_state.uploaded_to_gpu
    {
        // commands.spawn((
        //     Mesh3d(meshes.add(Cuboid::new(1.0, 1.0, 1.0))),
        //     MeshMaterial3d(materials.add(Color::srgb(0.8, 0.7, 0.6))),
        //     Transform::from_xyz(0.0, 0.5, 0.0),
        // ));

        manifold_state.uploaded_to_gpu = true;

        // Start by removing the old model.
        for entity in mesh_models.iter() {
            commands.entity(entity).despawn();
        }

        // Now build our  mesh.
        let mut m = Mesh::new(
            PrimitiveTopology::TriangleList,
            RenderAssetUsages::default(),
        );
        let mut pos = vec![];
        let mut vns = vec![];
        for (fid, hs) in manifold_state.manifold.0.hs.chunks(3).enumerate() {
            let p0 = manifold_state.manifold.0.ps[hs[0].tail];
            let p1 = manifold_state.manifold.0.ps[hs[1].tail];
            let p2 = manifold_state.manifold.0.ps[hs[2].tail];
            let n = manifold_state.manifold.0.face_normals[fid];
            pos.push([p0.x as f32, p0.y as f32, p0.z as f32]);
            pos.push([p1.x as f32, p1.y as f32, p1.z as f32]);
            pos.push([p2.x as f32, p2.y as f32, p2.z as f32]);
            vns.push([n.x as f32, n.y as f32, n.z as f32]);
            vns.push([n.x as f32, n.y as f32, n.z as f32]);
            vns.push([n.x as f32, n.y as f32, n.z as f32]);
        }
        m.insert_attribute(Mesh::ATTRIBUTE_POSITION, pos);
        m.insert_attribute(Mesh::ATTRIBUTE_NORMAL, vns);

        commands.spawn((
            Mesh3d(meshes.add(m).clone()),
            MeshMaterial3d(materials.add(StandardMaterial { ..default() })),
            Transform::default(),
            Wireframe,
            WireframeColor {
                color: BLACK.into(),
            },
            MeshModel,
        ));
    }
}
