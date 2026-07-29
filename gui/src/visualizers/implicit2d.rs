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

//! GPU-based renderer for implicit 2D surfaces.
//!
//! Renders to a texture via fullscreen fragment shader that evaluates the SDF
//! per-pixel at world coordinates. The texture is displayed by egui (not Bevy
//! render graph writeback), so only the main pass is needed here.

use std::borrow::Cow;
use std::num::NonZeroU64;
use std::sync::Mutex;

use bevy::core_pipeline::FullscreenShader;
use bevy::ecs::{prelude::*, query::QueryItem};
use bevy::prelude::*;
use bevy::render::{
    MainWorld, Render, RenderApp, RenderStartup, RenderSystems,
    render_asset::RenderAssets,
    render_graph::{
        NodeRunError, RenderGraph, RenderGraphContext, RenderGraphExt, RenderLabel, ViewNode,
        ViewNodeRunner,
    },
    render_resource::{
        BindGroupEntries, BindGroupLayoutDescriptor, BindGroupLayoutEntry, BindingResource,
        BindingType, BufferBinding, BufferBindingType, BufferInitDescriptor, BufferUsages,
        CachedRenderPipelineId, ColorTargetState, ColorWrites, FragmentState, LoadOp,
        Operations, PipelineCache, PrimitiveState, RenderPassColorAttachment,
        RenderPassDescriptor, RenderPipelineDescriptor, ShaderStages, SpecializedRenderPipeline,
        SpecializedRenderPipelines, StoreOp, TextureFormat,
    },
    renderer::RenderContext,
    texture::GpuImage,
};
use bevy_core_pipeline::core_2d::graph::{Core2d, Node2d};
use bevy_encase_derive::ShaderType;
use bevy_shader::Shader;
use bytemuck::{Pod, Zeroable};

use crate::{
    JobBridge, JobOutput, ViewState2d,
    tree_to_wgsl::tree_to_wgsl_2d_main_from_sdf,
};

// ─── Uniform buffer ─────────────────────────────────────────────────────────────────

/// Per-frame view data pushed to the 2D implicit shader.
/// Layout must match WGSL `Implicit2dUniform` struct in tree_to_wgsl.rs.
#[derive(Clone, Copy, Default, ShaderType, Pod, Zeroable)]
#[repr(C)]
pub struct Implicit2dUniform {
    /// xy = world origin, z = world_units_per_pixel, w = unused
    pub origin_and_scale: Vec4,
    /// xy = viewport size in pixels, zw = unused
    pub viewport_size_and_pad: Vec4,
}

// ─── Resources ──────────────────────────────────────────────────────────────

/// Tracks the active main-pass shader handle (main world -> render world).
#[derive(Resource, Default, Clone)]
struct Implicit2dFragmentShader(Option<Handle<Shader>>);

/// Pre-computed uniform from main world, extracted to render world.
#[derive(Clone, Copy, Resource)]
pub struct Implicit2dUniformExtracted {
    pub uniform: Implicit2dUniform,
}

impl FromWorld for Implicit2dUniformExtracted {
    fn from_world(_world: &mut World) -> Self {
        Self {
            uniform: Implicit2dUniform::default(),
        }
    }
}

/// Extracted uniform for the render node.
#[derive(Resource)]
struct Implicit2dUniformRender {
    uniform: Implicit2dUniform,
}

impl FromWorld for Implicit2dUniformRender {
    fn from_world(_world: &mut World) -> Self {
        Self {
            uniform: Implicit2dUniform::default(),
        }
    }
}

/// Incremented whenever the shader is regenerated.
#[derive(Resource, Default, Clone, Copy)]
struct Implicit2dShaderVersion(u64);

/// Extracted shader version (render world).
#[derive(Resource, Default, Clone, Copy)]
struct Implicit2dShaderVersionRender(u64);

/// Dynamic pipeline for the main 2D implicit pass.
#[derive(Resource)]
struct Implicit2dMainPipeline {
    fullscreen_shader: FullscreenShader,
    bind_group_layout_descriptor: BindGroupLayoutDescriptor,
    fragment_shader: Mutex<Option<Handle<Shader>>>,
}

/// Per-view main-pass pipeline ID.
#[derive(Component, Deref, DerefMut)]
struct Implicit2dMainPipelineId(CachedRenderPipelineId);

/// Marker component to gate render nodes.
#[derive(Component)]
struct Implicit2dRenderActive;

/// Intermediate texture for the main pass (rendered to GPU, displayed by egui).
#[derive(Component, Clone, bevy::render::extract_component::ExtractComponent)]
struct Implicit2dIntermediateTexture {
    color: Handle<Image>,
}

#[derive(Clone, Hash, PartialEq, Eq)]
struct Implicit2dMainPipelineKey {
    format: TextureFormat,
    shader: Handle<Shader>,
}

impl SpecializedRenderPipeline for Implicit2dMainPipeline {
    type Key = Implicit2dMainPipelineKey;

    fn specialize(&self, key: Self::Key) -> RenderPipelineDescriptor {
        let fragment = self.fragment_shader.lock().unwrap().clone();
        let Some(fragment) = fragment else {
            return RenderPipelineDescriptor {
                label: Some("implicit_2d_dummy".into()),
                layout: vec![],
                vertex: self.fullscreen_shader.to_vertex_state(),
                fragment: None,
                ..Default::default()
            };
        };

        RenderPipelineDescriptor {
            label: Some("implicit_2d_main".into()),
            layout: vec![self.bind_group_layout_descriptor.clone()],
            vertex: self.fullscreen_shader.to_vertex_state(),
            fragment: Some(FragmentState {
                shader: fragment,
                shader_defs: Vec::new(),
                entry_point: Some("fs".into()),
                targets: vec![Some(ColorTargetState {
                    format: key.format,
                    blend: None,
                    write_mask: ColorWrites::ALL,
                })],
            }),
            primitive: PrimitiveState::default(),
            depth_stencil: None,
            ..Default::default()
        }
    }
}

// ─── Render graph nodes ──────────────────────────────────────────────────────

#[derive(RenderLabel, Clone, Copy, Default, PartialEq, Eq, Hash, Debug)]
struct Implicit2dMainPassLabel;

/// Main pass: evaluates SDF per-pixel to intermediate color texture.
#[derive(Default)]
struct Implicit2dMainPassNode;

impl ViewNode for Implicit2dMainPassNode {
    type ViewQuery = (
        &'static Implicit2dMainPipelineId,
        &'static Implicit2dIntermediateTexture,
        &'static Implicit2dRenderActive,
    );

    fn run<'w>(
        &self,
        _graph: &mut RenderGraphContext,
        render_context: &mut RenderContext<'w>,
        (pipeline_id, texture, _active): QueryItem<'w, '_, Self::ViewQuery>,
        world: &'w World,
    ) -> Result<(), NodeRunError> {
        let pipeline_cache = world.resource::<PipelineCache>();
        let Some(render_pipeline) = pipeline_cache.get_render_pipeline(**pipeline_id) else {
            return Ok(());
        };

        let gpu_images = world.resource::<RenderAssets<GpuImage>>();
        let Some(color_gpu) = gpu_images.get(texture.color.id()) else {
            return Ok(());
        };

        let uniform = world
            .get_resource::<Implicit2dUniformRender>()
            .map(|u| u.uniform)
            .unwrap_or_default();

        let uniform_buffer = render_context.render_device().create_buffer_with_data(
            &BufferInitDescriptor {
                label: Some("implicit_2d_uniform"),
                contents: bytemuck::cast_slice(&[uniform]),
                usage: BufferUsages::UNIFORM | BufferUsages::COPY_DST,
            },
        );

        let bind_group = render_context.render_device().create_bind_group(
            "implicit_2d_main_bind_group",
            &pipeline_cache.get_bind_group_layout(
                &world
                    .resource::<Implicit2dMainPipeline>()
                    .bind_group_layout_descriptor,
            ),
            &BindGroupEntries::sequential((BindingResource::Buffer(BufferBinding {
                buffer: &uniform_buffer,
                offset: 0,
                size: NonZeroU64::new(std::mem::size_of::<Implicit2dUniform>() as u64),
            }),)),
        );

        let clear_color = Color::srgba(0.0, 0.0, 0.0, 0.0).to_linear();
        let mut render_pass = render_context.begin_tracked_render_pass(RenderPassDescriptor {
            label: Some("implicit_2d_main"),
            color_attachments: &[Some(RenderPassColorAttachment {
                view: &color_gpu.texture_view,
                depth_slice: None,
                resolve_target: None,
                ops: Operations {
                    load: LoadOp::Clear(clear_color.into()),
                    store: StoreOp::Store,
                },
            })],
            depth_stencil_attachment: None,
            timestamp_writes: None,
            occlusion_query_set: None,
        });

        render_pass.set_render_pipeline(render_pipeline);
        render_pass.set_bind_group(0, &bind_group, &[]);
        render_pass.draw(0..3, 0..1);

        Ok(())
    }
}

// ─── Plugin ────────────────────────────────────────────────────────────────

/// Plugin that registers the implicit 2D surface render-to-texture pass.
pub struct Implicit2dPlugin;

impl Plugin for Implicit2dPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<Implicit2dFragmentShader>()
            .init_resource::<Implicit2dShaderVersion>()
            .init_resource::<Implicit2dUniformExtracted>()
            .add_plugins(bevy::render::extract_component::ExtractComponentPlugin::<
                Implicit2dIntermediateTexture,
            >::default())
            .add_systems(Startup, setup_implicit2d_camera)
            .add_systems(
                Update,
                (
                    update_implicit2d_shader,
                    update_implicit2d_uniform,
                    prepare_implicit2d_textures,
                )
                .after(crate::check_job),
            );
    }

    fn finish(&self, app: &mut App) {
        let Some(render_app) = app.get_sub_app_mut(RenderApp) else {
            return;
        };

        render_app
            .init_resource::<SpecializedRenderPipelines<Implicit2dMainPipeline>>()
            .init_resource::<Implicit2dLastShaderVersion>()
            .init_resource::<Implicit2dUniformRender>()

            .add_systems(RenderStartup, setup_implicit2d_main_pipeline)
            .add_systems(bevy::render::ExtractSchedule, extract_implicit2d_shader)
            .add_systems(bevy::render::ExtractSchedule, extract_implicit2d_shader_version)
            .add_systems(bevy::render::ExtractSchedule, extract_implicit2d_uniform)
            .add_systems(
                Render,
                prepare_implicit2d_pipelines.in_set(RenderSystems::Prepare),
            )
            .add_render_graph_node::<ViewNodeRunner<Implicit2dMainPassNode>>(
                Core2d,
                Implicit2dMainPassLabel,
            )
            .add_systems(RenderStartup, add_implicit2d_edges);
    }
}

// ─── Setup systems ─────────────────────────────────────────────────────────

/// Marker for the dedicated 2D implicit camera (creates a Core2d view for our render node).
#[derive(Component)]
struct Implicit2dCamera;

fn setup_implicit2d_camera(mut commands: Commands) {
    commands.spawn((
        Camera {
            order: -1,
            ..Default::default()
        },
        Camera2d::default(),
        Implicit2dCamera,
    ));
}

fn setup_implicit2d_main_pipeline(mut commands: Commands, fullscreen_shader: Res<FullscreenShader>) {
    let bind_group_layout_descriptor = BindGroupLayoutDescriptor {
        label: Cow::Borrowed("implicit_2d_main_bind_group_layout"),
        entries: vec![BindGroupLayoutEntry {
            binding: 0,
            visibility: ShaderStages::FRAGMENT,
            ty: BindingType::Buffer {
                ty: BufferBindingType::Uniform,
                has_dynamic_offset: false,
                min_binding_size: NonZeroU64::new(std::mem::size_of::<Implicit2dUniform>() as u64),
            },
            count: None,
        }],
    };

    commands.insert_resource(Implicit2dMainPipeline {
        fullscreen_shader: fullscreen_shader.clone(),
        bind_group_layout_descriptor,
        fragment_shader: Mutex::new(None),
    });
}

/// Extract the current shader handle from the main world to the render world.
fn extract_implicit2d_shader(main_world: Res<MainWorld>, main_pipeline: Res<Implicit2dMainPipeline>) {
    let shader_handle = main_world
        .get_resource::<Implicit2dFragmentShader>()
        .and_then(|s| s.0.clone());
    *main_pipeline.fragment_shader.lock().unwrap() = shader_handle;
}

/// Extract the shader version from the main world to the render world.
fn extract_implicit2d_shader_version(main_world: Res<MainWorld>, mut commands: Commands) {
    if let Some(version) = main_world.get_resource::<Implicit2dShaderVersion>() {
        commands.insert_resource(Implicit2dShaderVersionRender(version.0));
    }
}

/// Extract the pre-computed uniform from main world to render world.
fn extract_implicit2d_uniform(
    main_world: Res<MainWorld>,
    mut uniform_render: ResMut<Implicit2dUniformRender>,
) {
    if let Some(uniform) = main_world.get_resource::<Implicit2dUniformExtracted>() {
        uniform_render.uniform = uniform.uniform;
    }
}

/// Tracks the last shader version seen by the prepare system.
#[derive(Resource, Default, Clone, Copy)]
struct Implicit2dLastShaderVersion(u64);

/// Prepare per-view pipelines and gate rendering on active Surface2D.
#[allow(clippy::too_many_arguments)]
fn prepare_implicit2d_pipelines(
    mut commands: Commands,
    pipeline_cache: Res<PipelineCache>,
    mut main_pipelines: ResMut<SpecializedRenderPipelines<Implicit2dMainPipeline>>,
    views_without: Query<
        Entity,
        (
            Without<Implicit2dMainPipelineId>,
            Without<Implicit2dRenderActive>,
        ),
    >,
    main_pipeline: Res<Implicit2dMainPipeline>,
    views_with: Query<(Entity, &Implicit2dMainPipelineId)>,
    _textures: Query<&Implicit2dIntermediateTexture>,
    version: Option<Res<Implicit2dShaderVersionRender>>,
    mut last_version: Option<ResMut<Implicit2dLastShaderVersion>>,
) {
    let has_shader = main_pipeline.fragment_shader.lock().unwrap().is_some();

    // If shader version changed, clear all pipeline IDs so they re-specialize
    if let Some(v) = version {
        if let Some(lv) = last_version.as_mut() {
            if v.0 > lv.0 {
                for (entity, _) in &views_with {
                    commands
                        .entity(entity)
                        .remove::<(Implicit2dMainPipelineId, Implicit2dRenderActive)>();
                }
                lv.0 = v.0;
            }
        } else if let Some(mut lv) = last_version {
            lv.0 = v.0;
        }
    }

    if has_shader {
        let shader = main_pipeline.fragment_shader.lock().unwrap().clone().unwrap();
        for entity in &views_without {
            let main_key = Implicit2dMainPipelineKey {
                format: TextureFormat::Rgba16Float,
                shader: shader.clone(),
            };
            let main_id = main_pipelines.specialize(&pipeline_cache, &main_pipeline, main_key);
            commands.entity(entity).insert((Implicit2dMainPipelineId(main_id), Implicit2dRenderActive));
        }
    } else {
        for (entity, _) in &views_with {
            commands.entity(entity).remove::<(
                Implicit2dMainPipelineId,
                Implicit2dRenderActive,
                Implicit2dIntermediateTexture,
            )>();
        }
    }
}

/// Create intermediate texture in the main world when Surface2D is active.
fn prepare_implicit2d_textures(
    mut commands: Commands,
    mut command_cad: ResMut<JobBridge>,
    cameras: Query<Entity, With<Implicit2dCamera>>,
    mut images: ResMut<Assets<Image>>,
) {
    let has_surface = matches!(&command_cad.last_result, Some(Ok(JobOutput::Surface2D(_))));

    if has_surface {
        let Ok(camera_entity) = cameras.single() else {
            return;
        };
        // Use draw area size from egui pass (one-frame lag)
        let Some(draw_size) = command_cad.implicit2d_draw_area_size else {
            return;
        };
        let size = UVec2::new(draw_size.x.max(1), draw_size.y.max(1));

        let needs_recreate = match command_cad.implicit2d_texture_size {
            Some(old_size) => old_size != size || command_cad.implicit2d_texture.is_none(),
            None => true,
        };

        if needs_recreate {
            if let Some(old) = command_cad.implicit2d_texture.take() {
                images.remove(&old);
            }

            let mut color_img = Image::default_uninit();
            color_img.texture_descriptor.size = bevy::render::render_resource::Extent3d {
                width: size.x,
                height: size.y,
                depth_or_array_layers: 1,
            };
            color_img.texture_descriptor.dimension =
                bevy::render::render_resource::TextureDimension::D2;
            color_img.texture_descriptor.format = TextureFormat::Rgba16Float;
            color_img.texture_descriptor.usage =
                bevy::render::render_resource::TextureUsages::TEXTURE_BINDING
                    | bevy::render::render_resource::TextureUsages::RENDER_ATTACHMENT;
            let color_handle = images.add(color_img);

            command_cad.implicit2d_texture = Some(color_handle.clone());
            command_cad.implicit2d_texture_size = Some(size);
            command_cad.implicit2d_egui_texture = None; // force re-register with egui

            commands.entity(camera_entity).remove::<(Implicit2dMainPipelineId, Implicit2dRenderActive)>();
        }

        if let Some(color) = &command_cad.implicit2d_texture {
            commands.entity(camera_entity).insert(Implicit2dIntermediateTexture {
                color: color.clone(),
            });
        }
    } else {
        for entity in &cameras {
            commands.entity(entity).remove::<Implicit2dIntermediateTexture>();
        }
        if let Some(old) = command_cad.implicit2d_texture.take() {
            images.remove(&old);
        }
        command_cad.implicit2d_egui_texture = None;
    }
}

fn add_implicit2d_edges(mut render_graph: ResMut<RenderGraph>) {
    let subgraph = render_graph.sub_graph_mut(Core2d);
    subgraph.add_node_edge(Node2d::MainTransparentPass, Implicit2dMainPassLabel);
}

/// Compute the uniform from ViewState2d and stored draw area size.
fn update_implicit2d_uniform(
    command_cad: Res<JobBridge>,
    view_state: Res<ViewState2d>,
    mut uniform_extracted: ResMut<Implicit2dUniformExtracted>,
) {
    let Some(size) = command_cad.implicit2d_draw_area_size else {
        return;
    };

    let ppm = view_state.pixels_per_meter();
    let wup = 1.0 / ppm;
    let width = size.x as f32;
    let height = size.y as f32;
    let offset = view_state.offset();

    // origin is world position at pixel (0, 0) in shader coords.
    // With negated pixel_y: texel (0,0) [egui top-left] maps to pixel_y=H,
    // so world_pos = origin + (0, -H)*wup.
    // We need this to match grid's world coords at egui center:
    //   center of draw area -> world (-offset.x, -offset.y)
    uniform_extracted.uniform = Implicit2dUniform {
        origin_and_scale: Vec4::new(
            -width * wup / 2.0 - offset.x,
            height * wup / 2.0 - offset.y,
            wup,
            0.0,
        ),
        viewport_size_and_pad: Vec4::new(width, height, 0.0, 0.0),
    };
}

// ─── Main-world shader generation ──────────────────────────────────────────

/// Generate WGSL shader from Surface2D result and register it with Bevy.
fn update_implicit2d_shader(
    mut command_cad: ResMut<JobBridge>,
    mut shaders: ResMut<Assets<Shader>>,
    mut implicit_shader: ResMut<Implicit2dFragmentShader>,
    mut version: ResMut<Implicit2dShaderVersion>,
) {
    let Some(Ok(JobOutput::Surface2D(surface))) = &command_cad.last_result else {
        if let Some(main) = command_cad.implicit2d_shader.take() {
            shaders.remove(&main);
        }
        implicit_shader.0 = None;
        return;
    };

    if command_cad.implicit2d_shader.is_none() {
        let sdf_body = crate::tree_to_wgsl::emit_sdf_body(surface.tree());
        let main_wgsl = tree_to_wgsl_2d_main_from_sdf(&sdf_body);
        let main_shader = Shader::from_wgsl(Cow::Owned(main_wgsl), "implicit_2d_main.wgsl");
        let main_handle = shaders.add(main_shader);

        command_cad.implicit2d_shader = Some(main_handle.clone());
        implicit_shader.0 = Some(main_handle);
        version.0 += 1;
    }
}
