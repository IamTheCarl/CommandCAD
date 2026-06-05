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
    anti_alias::smaa::Smaa,
    asset::RenderAssetUsages,
    camera::ScalingMode,
    color::palettes::css,
    pbr::wireframe::{Wireframe, WireframeColor},
    prelude::*,
    {ecs::system::Query, mesh::PrimitiveTopology},
};
use bevy_mod_outline::{OutlineMode, OutlineVolume};

use crate::grid::GridSettings;
use crate::{JobBridge, JobOutput};
use interpreter::values::manifold_mesh::ManifoldMesh3D;

const GRID_MAX_EXTENT: f32 = 10.0;
const GRID_LINE_SCREEN_WIDTH: f32 = 1.0; // target line width in screen pixels

#[derive(Component)]
pub struct GridEntity;

const VIEW_Z_OFFSET: f32 = -10.0;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AxisView {
    XPlus,
    XMinus,
    YPlus,
    YMinus,
    ZPlus,
    ZMinus,
}

#[derive(Debug, Resource)]
pub struct ViewState3d {
    offset: bevy::prelude::Vec3,
    zoom: f32,
    rotation_x: f32,
    rotation_y: f32,
    show_wireframe: bool,
}

impl Default for ViewState3d {
    fn default() -> Self {
        let mut view_state = ViewState3d {
            offset: bevy::prelude::Vec3::ZERO,
            zoom: 0.0,
            rotation_x: 0.0,
            rotation_y: 0.0,
            show_wireframe: false,
        };
        view_state.set_pixels_per_meter(10.0);
        view_state
    }
}

impl ViewState3d {
    pub fn offset(&self) -> bevy::prelude::Vec3 {
        self.offset
    }
}

impl ViewState3d {
    // Percentage of scale per scale factor unit.
    const SCALE_FACTOR: f32 = 1.01;
    const POINTER_SCALE: f32 = 0.007;

    pub fn pixels_per_meter(&self) -> f32 {
        Self::SCALE_FACTOR.powf(self.zoom)
    }

    pub fn set_pixels_per_meter(&mut self, pixels_per_meter: f32) {
        self.zoom = pixels_per_meter.log(Self::SCALE_FACTOR);
    }

    pub fn fit_to_screen(
        &mut self,
        draw_area: egui::Rect,
        camera_transform: &Transform,
        toolbar_offset: f32,
        manifold: &ManifoldMesh3D,
    ) {
        let camera_rotation = camera_transform.rotation;
        let camera_inverse = camera_rotation.inverse();

        let mut min = Vec3::MAX;
        let mut max = Vec3::MIN;

        for p in manifold
            .0
            .triangles()
            .flat_map(|triangle| triangle.positions)
        {
            let v = Vec3::new(p.x as f32, p.y as f32, p.z as f32);
            let p_local = camera_inverse * v;

            min = min.min(p_local);
            max = max.max(p_local);
        }

        let size = max - min;
        let dx = draw_area.x_range().span() / size.x;
        let dy = draw_area.y_range().span() / size.y;
        let pixels_per_meter = dx.min(dy);
        self.set_pixels_per_meter(pixels_per_meter);

        self.offset = camera_rotation * ((min + max) / 2.0)
            + camera_rotation * Vec3::new(0.0, toolbar_offset / pixels_per_meter / 2.0, 0.0);
    }

    pub fn snap_to_axis_view(&mut self, axis: AxisView) {
        match axis {
            AxisView::XPlus => {
                self.rotation_y = 0.0;
                self.rotation_x = 0.0;
            }
            AxisView::XMinus => {
                self.rotation_y = std::f32::consts::PI;
                self.rotation_x = 0.0;
            }
            AxisView::YPlus => {
                self.rotation_y = std::f32::consts::FRAC_PI_2;
                self.rotation_x = 0.0;
            }
            AxisView::YMinus => {
                self.rotation_y = -std::f32::consts::FRAC_PI_2;
                self.rotation_x = 0.0;
            }
            AxisView::ZPlus => {
                self.rotation_y = 0.0;
                self.rotation_x = -std::f32::consts::FRAC_PI_2;
            }
            AxisView::ZMinus => {
                self.rotation_y = 0.0;
                self.rotation_x = std::f32::consts::PI / 2.0;
            }
        }
    }

    pub fn draw_interface(
        &mut self,
        ui: &mut egui::Ui,
        last_result: &Option<Result<JobOutput, crate::JobError>>,
    ) {
        if let Some(Ok(JobOutput::ManifoldMesh(_state))) = last_result {
            ui.checkbox(&mut self.show_wireframe, "Show Wireframe");

            ui.separator();

            ui.horizontal(|ui| {
                if ui.button("X+").clicked() {
                    self.snap_to_axis_view(AxisView::XPlus);
                }
                if ui.button("X-").clicked() {
                    self.snap_to_axis_view(AxisView::XMinus);
                }
                if ui.button("Y+").clicked() {
                    self.snap_to_axis_view(AxisView::YPlus);
                }
                if ui.button("Y-").clicked() {
                    self.snap_to_axis_view(AxisView::YMinus);
                }
                if ui.button("Z+").clicked() {
                    self.snap_to_axis_view(AxisView::ZPlus);
                }
                if ui.button("Z-").clicked() {
                    self.snap_to_axis_view(AxisView::ZMinus);
                }
            });
        }
    }

    pub fn track_movement(&mut self, camera_transform: &Transform, input_state: &egui::InputState) {
        self.zoom += input_state.smooth_scroll_delta.y;
        self.zoom = self.zoom.max(0.0);

        if input_state.pointer.primary_down() {
            let drag_delta = input_state.pointer.delta();
            let delta = drag_delta / self.pixels_per_meter();
            let right = camera_transform.right();
            let up = camera_transform.up();

            self.offset += right * -delta.x + up * delta.y;
        } else if input_state.pointer.secondary_down() {
            let drag_delta = input_state.pointer.delta();

            // TODO These probably need to be scaled differently on a 4k display.
            // It would probably be best to base the rotation factor based off the viewport size.
            self.rotation_x += drag_delta.y * Self::POINTER_SCALE;
            self.rotation_y += drag_delta.x * Self::POINTER_SCALE;

            self.rotation_x = self
                .rotation_x
                .clamp(-89_f32.to_radians(), 89_f32.to_radians());
            self.rotation_y = self
                .rotation_y
                .clamp(-std::f32::consts::PI, std::f32::consts::PI);
        }
    }
}

fn build_grid_mesh(world_step: f32, line_half_thickness: f32, grid_extent: f32) -> Mesh {
    let mut m = Mesh::new(
        PrimitiveTopology::TriangleList,
        RenderAssetUsages::default(),
    );
    let mut positions = vec![];
    let mut normals = vec![];

    let first_line_idx = (-grid_extent / world_step).floor() as i32;
    let last_line_idx = (grid_extent / world_step).ceil() as i32;

    for i in first_line_idx..=last_line_idx {
        let pos = (i as f32) * world_step;

        // Vertical lines (along local Y axis) — thin quad centered at x = pos
        let t = line_half_thickness;
        let e = grid_extent;

        // Front face (CCW from +Z, normals +Z)
        positions.push([pos - t, -e, 0.0]);
        positions.push([pos + t, -e, 0.0]);
        positions.push([pos - t, e, 0.0]);
        positions.push([pos - t, e, 0.0]);
        positions.push([pos + t, -e, 0.0]);
        positions.push([pos + t, e, 0.0]);

        // Back face (CCW from -Z, normals -Z)
        positions.push([pos - t, -e, 0.0]);
        positions.push([pos - t, e, 0.0]);
        positions.push([pos + t, -e, 0.0]);
        positions.push([pos + t, -e, 0.0]);
        positions.push([pos - t, e, 0.0]);
        positions.push([pos + t, e, 0.0]);

        // Horizontal lines (along local X axis) — thin quad centered at y = pos
        // Front face (CCW from +Z, normals +Z)
        positions.push([-e, pos - t, 0.0]);
        positions.push([e, pos - t, 0.0]);
        positions.push([-e, pos + t, 0.0]);
        positions.push([-e, pos + t, 0.0]);
        positions.push([e, pos - t, 0.0]);
        positions.push([e, pos + t, 0.0]);

        // Back face (CCW from -Z, normals -Z)
        positions.push([-e, pos - t, 0.0]);
        positions.push([-e, pos + t, 0.0]);
        positions.push([e, pos - t, 0.0]);
        positions.push([e, pos - t, 0.0]);
        positions.push([-e, pos + t, 0.0]);
        positions.push([e, pos + t, 0.0]);

        for _ in 0..12 {
            normals.push([0.0, 0.0, 1.0]);
        }
        for _ in 0..12 {
            normals.push([0.0, 0.0, -1.0]);
        }
    }

    m.insert_attribute(Mesh::ATTRIBUTE_POSITION, positions);
    m.insert_attribute(Mesh::ATTRIBUTE_NORMAL, normals);
    m
}

#[allow(clippy::type_complexity)]
pub fn update_grid(
    mut grid: Query<(&mut Transform, &mut Visibility, &GridEntity, &Mesh3d)>,
    cameras: Query<(&Camera, &Transform), (With<Camera3d>, Without<GridEntity>)>,
    view_state_3d: Res<ViewState3d>,
    grid_settings: Res<GridSettings>,
    mut meshes: ResMut<Assets<Mesh>>,
) {
    let Some((camera, camera_transform)) = cameras.iter().next() else {
        return;
    };

    let cam_back = camera_transform.rotation * Vec3::Z;
    let cam_right = camera_transform.rotation * Vec3::X;
    let cam_up = camera_transform.rotation * Vec3::Y;

    let grid_rot = Mat4::from_cols(
        Vec4::new(cam_right.x, cam_right.y, cam_right.z, 0.0),
        Vec4::new(cam_up.x, cam_up.y, cam_up.z, 0.0),
        Vec4::new(cam_back.x, cam_back.y, cam_back.z, 0.0),
        Vec4::new(0.0, 0.0, 0.0, 1.0),
    );

    let viewport = match camera.physical_viewport_size() {
        Some(size) => size,
        None => return,
    };

    let pixels_per_meter = view_state_3d.pixels_per_meter();
    let visible_x = viewport.x as f32 / pixels_per_meter;
    let visible_y = viewport.y as f32 / pixels_per_meter;
    let grid_extent = (visible_x.max(visible_y) * 1.25).max(GRID_MAX_EXTENT);

    for (mut transform, mut visibility, _grid, mesh_handle) in &mut grid {
        transform.translation = Vec3::ZERO;
        transform.rotation = Quat::from_mat4(&grid_rot);

        if grid_settings.world_step().is_some() {
            let world_step = grid_settings.world_step().unwrap_or(0.01);
            let pixels_per_cell = world_step * pixels_per_meter;

            if pixels_per_cell < 3.0 {
                *visibility = Visibility::Hidden;
            } else {
                *visibility = Visibility::Visible;
                let line_half_thickness = GRID_LINE_SCREEN_WIDTH / pixels_per_meter / 2.0;

                if let Some(mesh) = meshes.get_mut(&mesh_handle.0) {
                    *mesh = build_grid_mesh(world_step, line_half_thickness, grid_extent);
                }
            }
        } else {
            *visibility = Visibility::Hidden;
        }
    }
}

pub fn setup_3d(
    mut commands: Commands,
    grid_settings: Option<Res<GridSettings>>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
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

    if let Some(grid_settings) = grid_settings {
        let world_step = grid_settings.world_step().unwrap_or(0.01);
        let initial_thickness = GRID_LINE_SCREEN_WIDTH / 10.0;
        let grid_mesh = meshes.add(build_grid_mesh(
            world_step,
            initial_thickness,
            GRID_MAX_EXTENT,
        ));
        commands.spawn((
            Mesh3d(grid_mesh),
            MeshMaterial3d(materials.add(StandardMaterial {
                base_color: Color::Srgba(css::DARK_GRAY),
                unlit: true,
                depth_bias: 1_000_000.0,
                ..default()
            })),
            Transform::default(),
            Visibility::Visible,
            GridEntity,
        ));
    }

    commands.insert_resource(ViewState3d::default());
}

pub fn update_3d_camera(
    view_state_3d: Res<ViewState3d>,
    mut cameras: Query<&mut Projection, With<Camera3d>>,
) {
    for mut projection in &mut cameras {
        if let Projection::Orthographic(projection) = &mut *projection {
            projection.scale = 1.0 / view_state_3d.pixels_per_meter();
        }
    }
}

pub fn orbit_camera(
    view_state_3d: Res<ViewState3d>,
    mut cameras: Query<(&mut Transform, &Camera3d), With<Camera3d>>,
) {
    let radius = VIEW_Z_OFFSET.abs();
    let (yaw, pitch) = (view_state_3d.rotation_y, view_state_3d.rotation_x);

    let x = view_state_3d.offset().x + yaw.sin() * pitch.cos() * radius;
    let z = view_state_3d.offset().z - yaw.cos() * pitch.cos() * radius;
    let y = view_state_3d.offset().y + pitch.sin() * radius;

    let camera_pos = Vec3::new(x, y, z);
    let forward = (view_state_3d.offset() - camera_pos).normalize();
    let back = -forward;

    let right = Vec3::Y.cross(back).normalize();
    let up = back.cross(right);

    let cam_rot = Mat4::from_cols(
        Vec4::new(right.x, right.y, right.z, 0.0),
        Vec4::new(up.x, up.y, up.z, 0.0),
        Vec4::new(back.x, back.y, back.z, 0.0),
        Vec4::new(0.0, 0.0, 0.0, 1.0),
    );

    for (mut transform, _camera) in &mut cameras {
        transform.translation = camera_pos;
        transform.rotation = Quat::from_mat4(&cam_rot);
    }
}

#[allow(clippy::type_complexity)]
pub fn orbit_light(
    cameras: Query<&Transform, (With<Camera3d>, Without<DirectionalLight>)>,
    mut lights: Query<
        &mut Transform,
        (
            With<DirectionalLight>,
            Without<Camera3d>,
            Without<GridEntity>,
        ),
    >,
) {
    let camera_transform = cameras.single().unwrap();
    for mut light in &mut lights {
        light.translation = camera_transform.translation;
        light.rotation = camera_transform.rotation;
    }
}

#[derive(Component)]
pub struct MeshModel;

pub fn sync_wireframe_visibility(
    view_state_3d: Res<ViewState3d>,
    mut commands: Commands,
    wireframe_entities: Query<Entity, (With<MeshModel>, With<Wireframe>)>,
    non_wireframe_entities: Query<Entity, (With<MeshModel>, Without<Wireframe>)>,
) {
    if !view_state_3d.show_wireframe {
        for entity in &wireframe_entities {
            commands.entity(entity).remove::<Wireframe>();
        }
    } else {
        for entity in &non_wireframe_entities {
            commands.entity(entity).insert((
                Wireframe,
                WireframeColor {
                    color: bevy::color::palettes::css::BLACK.into(),
                },
            ));
        }
    }
}

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
            commands.entity(entity).try_despawn();
        }

        // Now build our  mesh.
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
                color: bevy::color::palettes::css::BLACK.into(),
            },
            MeshModel,
        ));
    }
}
