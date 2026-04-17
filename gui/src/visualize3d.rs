use bevy::anti_alias::smaa::Smaa;
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
use bevy::pbr::wireframe::{Wireframe, WireframeColor};
use bevy::prelude::*;
use bevy::{ecs::system::Query, mesh::PrimitiveTopology};
use bevy_mod_outline::{AsyncSceneInheritOutline, OutlineMode, OutlineVolume};

use crate::{JobBridge, JobOutput, ViewState};

const VIEW_Z_OFFSET: f32 = -10.0;

#[derive(Debug, Resource, Default)]
pub struct ViewState3d {
    rotation_x: f32,
    rotation_y: f32,
}

impl ViewState3d {
    const POINTER_SCALE: f32 = 0.007;

    pub fn track_movement(&mut self, input_state: &egui::InputState) {
        if input_state.pointer.secondary_down() {
            let drag_delta = input_state.pointer.delta();

            // TODO These probably need to be scaled differently on a 4k display.
            // It would probably be best to base the rotation factor based off the viewport size.
            self.rotation_x -= drag_delta.y * Self::POINTER_SCALE;
            self.rotation_y += drag_delta.x * Self::POINTER_SCALE;

            self.rotation_x = self
                .rotation_x
                .clamp(-std::f32::consts::PI, std::f32::consts::PI);
            self.rotation_y = self
                .rotation_y
                .clamp(-std::f32::consts::PI, std::f32::consts::PI);
        }
    }
}

pub fn setup_3d(mut commands: Commands) {
    commands.spawn((
        Camera3d::default(),
        Projection::from(OrthographicProjection {
            scaling_mode: ScalingMode::WindowSize,
            ..OrthographicProjection::default_3d()
        }),
        Transform::from_xyz(0.0, 0.0, VIEW_Z_OFFSET).looking_at(Vec3::ZERO, Vec3::Y),
        Smaa::default(),
    ));

    commands.spawn((
        DirectionalLight {
            illuminance: light_consts::lux::AMBIENT_DAYLIGHT,
            ..default()
        },
        Transform::from_xyz(2.0, 4.0, -2.0).looking_at(Vec3::ZERO, Vec3::Y),
    ));
    commands.insert_resource(GlobalAmbientLight {
        brightness: light_consts::lux::HALLWAY,
        ..default()
    });

    commands.insert_resource(ViewState3d::default());
}

pub fn update_3d_camera(
    view_state: Res<ViewState>,
    mut cameras: Query<&mut Projection, With<Camera3d>>,
) {
    for mut projection in &mut cameras {
        if let Projection::Orthographic(projection) = &mut *projection {
            projection.scale = 1.0 / view_state.pixels_per_meter();
        }
    }
}

pub fn update_model_transforms(
    view_state: Res<ViewState>,
    view_state_3d: Res<ViewState3d>,
    mut models: Query<&mut Transform, With<MeshModel>>,
) {
    for mut transform in &mut models {
        let mut new_transform = Transform::default();

        new_transform.rotate(-Quat::from_rotation_y(view_state_3d.rotation_y));
        new_transform.rotate(Quat::from_rotation_x(view_state_3d.rotation_x));

        new_transform.translation = Vec3::new(-view_state.offset.x, -view_state.offset.y, 0.0);

        *transform = new_transform;
    }
}

#[derive(Component)]
pub struct MeshModel;

pub fn spawn_meshes(
    mut commands: Commands,
    mut command_cad: ResMut<JobBridge>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mesh_models: Query<(Entity, &Mesh3d), With<MeshModel>>,
) {
    if let Some(Ok(JobOutput::ManifoldMesh(manifold_state))) = &mut command_cad.last_result
        && !manifold_state.uploaded_to_gpu
    {
        manifold_state.uploaded_to_gpu = true;

        // Start by removing the old model.
        for (entity, mesh) in mesh_models.iter() {
            meshes.remove(mesh.id());
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

        let fill_color = egui::Color32::GRAY;
        let wireframe_color = egui::Color32::WHITE;

        commands.spawn((
            Mesh3d(meshes.add(m).clone()),
            MeshMaterial3d(materials.add(StandardMaterial {
                base_color: Color::Srgba(Srgba::rgb(fill_color.r() as f32 / 255.0, fill_color.g() as f32 / 255.0, fill_color.b() as f32 / 255.0)),
                ..default()
            })),
            Transform::default(),
            OutlineVolume {
                visible: true,
                width: 2.0,
                colour: Color::Srgba(Srgba::rgb(wireframe_color.r() as f32 / 255.0, wireframe_color.g() as f32 / 255.0, wireframe_color.b() as f32 / 255.0)),
            },
            AsyncSceneInheritOutline::default(),
            OutlineMode::FloodFlatDoubleSided,
            // TODO this should only be enabled with a checkbox in the UI.
            Wireframe,
            WireframeColor {
                color: bevy::color::palettes::css::BLACK.into(),
            },
            MeshModel,
        ));
    }
}
