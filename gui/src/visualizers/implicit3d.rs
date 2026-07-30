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

//! Ray-marching renderer for implicit 3D surfaces.
//!
//! Two-pass architecture (inspired by bevy_march):
//! 1. **Main pass**: ray-marches SDF to intermediate color + depth textures,
//!    with SDF Near-Miss outline detection for dark silhouette edges
//! 2. **Writeback pass**: composites onto main render target with depth testing,
//!    inserted between MainOpaquePass and MainTransmissivePass for proper scene integration

use std::borrow::Cow;
use std::num::NonZeroU64;
use std::sync::Mutex;

use bevy::asset::RenderAssetUsages;
use bevy::core_pipeline::FullscreenShader;
use bevy::ecs::{prelude::*, query::QueryItem};
use bevy::prelude::*;
use bevy::render::{
    ExtractSchedule, MainWorld, Render, RenderApp, RenderStartup, RenderSystems,
    render_asset::RenderAssets,
    render_graph::{
        NodeRunError, RenderGraph, RenderGraphContext, RenderGraphExt, RenderLabel, ViewNode,
        ViewNodeRunner,
    },
    render_resource::{
        BindGroupEntries, BindGroupLayoutDescriptor, BindGroupLayoutEntry, BindingResource,
        BindingType, BufferBinding, BufferBindingType, BufferInitDescriptor, BufferUsages,
        CachedRenderPipelineId, ColorTargetState, ColorWrites, CompareFunction, DepthStencilState,
        FragmentState, LoadOp, MultisampleState, Operations, PipelineCache, PrimitiveState,
        RenderPassColorAttachment, RenderPassDescriptor, RenderPipelineDescriptor, Sampler,
        SamplerBindingType, SamplerDescriptor, ShaderStages, SpecializedRenderPipeline,
        SpecializedRenderPipelines, StoreOp, TextureFormat, TextureSampleType,
        TextureViewDimension,
    },
    renderer::{RenderContext, RenderDevice},
    texture::{DepthAttachment, GpuImage},
    view::{ExtractedView, ViewTarget},
};
use bevy_core_pipeline::core_3d::graph::{Core3d, Node3d};
use bevy_encase_derive::ShaderType;
use bevy_shader::Shader;
use bytemuck::{Pod, Zeroable};

use crate::{
    JobBridge, JobOutput,
    tree_to_wgsl::{tree_to_wgsl_main_from_sdf, tree_to_wgsl_writeback},
};

// ─── Uniform buffer ─────────────────────────────────────────────────────────────────

/// Per-frame camera data pushed to the ray-marching shader.
/// Layout must match WGSL `ImplicitUniform` struct in tree_to_wgsl.rs.
#[derive(Clone, Copy, ShaderType, Pod, Zeroable)]
#[repr(C)]
struct ImplicitUniform {
    /// Camera position in world space.
    camera_position: Vec4,
    /// Camera right axis in world space.
    camera_right: Vec4,
    /// Camera up axis in world space.
    camera_up: Vec4,
    /// Camera forward axis in world space (direction rays travel).
    camera_forward: Vec4,
    /// Half-width of the orthographic view in world units.
    ortho_half_width: f32,
    /// Half-height of the orthographic view in world units.
    ortho_half_height: f32,
    /// Viewport size in pixels (width, height).
    viewport_size: Vec2,
}

impl ImplicitUniform {
    fn from_extracted_view(extracted: &ExtractedView, viewport_size: UVec2) -> Self {
        let view_transform = extracted.world_from_view.to_matrix();

        let right = Vec3::new(
            view_transform.x_axis.x,
            view_transform.x_axis.y,
            view_transform.x_axis.z,
        )
        .normalize();
        let up = Vec3::new(
            view_transform.y_axis.x,
            view_transform.y_axis.y,
            view_transform.y_axis.z,
        )
        .normalize();
        let back = Vec3::new(
            view_transform.z_axis.x,
            view_transform.z_axis.y,
            view_transform.z_axis.z,
        )
        .normalize();
        let forward = -back;

        let position = extracted.world_from_view.compute_transform().translation;

        let proj = extracted.clip_from_view;
        let scale_x = proj.x_axis.x.abs().max(1.0 / 10000.0);
        let scale_y = proj.y_axis.y.abs().max(1.0 / 10000.0);
        let half_width = 1.0 / scale_x;
        let half_height = 1.0 / scale_y;

        Self {
            camera_position: Vec4::new(position.x, position.y, position.z, 0.0),
            camera_right: Vec4::new(right.x, right.y, right.z, 0.0),
            camera_up: Vec4::new(up.x, up.y, up.z, 0.0),
            camera_forward: Vec4::new(forward.x, forward.y, forward.z, 0.0),
            ortho_half_width: half_width,
            ortho_half_height: half_height,
            viewport_size: Vec2::new(viewport_size.x as f32, viewport_size.y as f32),
        }
    }
}

// ─── Resources ──────────────────────────────────────────────────────

/// Tracks the active main-pass shader handle (main world -> render world).
#[derive(Resource, Default, Clone)]
struct ImplicitFragmentShader(Option<Handle<Shader>>);

/// Incremented whenever the shader is regenerated, so the render world can
/// detect changes and re-specialize pipelines.
#[derive(Resource, Default, Clone, Copy)]
struct ImplicitShaderVersion(u64);

/// Extracted shader version (render world).
#[derive(Resource, Default, Clone, Copy)]
struct ImplicitShaderVersionRender(u64);

/// Writeback shader handle (main world, extracted to render world).
#[derive(Resource, Clone)]
struct ImplicitWritebackShader(Handle<Shader>);

/// Extracted writeback shader handle (render world).
#[derive(Resource, Clone)]
struct ImplicitWritebackShaderRender(Handle<Shader>);

/// Dynamic pipeline for the main ray-marching pass.
#[derive(Resource)]
struct ImplicitMainPipeline {
    fullscreen_shader: FullscreenShader,
    bind_group_layout_descriptor: BindGroupLayoutDescriptor,
    fragment_shader: Mutex<Option<Handle<Shader>>>,
}

/// Dynamic writeback pipeline resource.
#[derive(Resource)]
struct ImplicitWritebackPipeline {
    fullscreen_shader: FullscreenShader,
    layout: BindGroupLayoutDescriptor,
    sampler: Sampler,
    shader: Handle<Shader>,
}

#[derive(Clone, Hash, PartialEq, Eq)]
struct ImplicitWritebackPipelineKey {
    format: TextureFormat,
    sample_count: u32,
}

impl SpecializedRenderPipeline for ImplicitWritebackPipeline {
    type Key = ImplicitWritebackPipelineKey;

    fn specialize(&self, key: Self::Key) -> RenderPipelineDescriptor {
        let multisample = MultisampleState {
            count: key.sample_count,
            mask: !0,
            alpha_to_coverage_enabled: false,
        };
        RenderPipelineDescriptor {
            label: Some("implicit_writeback_pipeline".into()),
            layout: vec![self.layout.clone()],
            vertex: self.fullscreen_shader.to_vertex_state(),
            fragment: Some(FragmentState {
                shader: self.shader.clone(),
                shader_defs: Vec::new(),
                entry_point: Some("fs".into()),
                targets: vec![Some(ColorTargetState {
                    format: key.format,
                    blend: Some(bevy::render::render_resource::BlendState {
                        color: bevy::render::render_resource::BlendComponent {
                            src_factor: bevy::render::render_resource::BlendFactor::One,
                            dst_factor:
                                bevy::render::render_resource::BlendFactor::OneMinusSrcAlpha,
                            operation: bevy::render::render_resource::BlendOperation::Add,
                        },
                        alpha: bevy::render::render_resource::BlendComponent {
                            src_factor: bevy::render::render_resource::BlendFactor::One,
                            dst_factor:
                                bevy::render::render_resource::BlendFactor::OneMinusSrcAlpha,
                            operation: bevy::render::render_resource::BlendOperation::Add,
                        },
                    }),
                    write_mask: ColorWrites::ALL,
                })],
            }),
            primitive: PrimitiveState::default(),
            depth_stencil: None,
            multisample,
            push_constant_ranges: vec![],
            zero_initialize_workgroup_memory: false,
        }
    }
}

/// Per-view main-pass pipeline ID (only present when implicit rendering is active).
#[derive(Component, Deref, DerefMut)]
struct ImplicitMainPipelineId(CachedRenderPipelineId);

/// Per-view writeback pipeline ID (only present when implicit rendering is active).
#[derive(Component, Deref, DerefMut)]
struct ImplicitWritebackPipelineId(CachedRenderPipelineId);

/// Marker component to gate render nodes.
#[derive(Component)]
struct ImplicitRenderActive;

/// Intermediate textures for the main pass (color + depth).
#[derive(Component, Clone, bevy::render::extract_component::ExtractComponent)]
struct ImplicitIntermediateTextures {
    color: Handle<Image>,
    depth: Handle<Image>,
}

#[derive(Clone, Hash, PartialEq, Eq)]
struct ImplicitMainPipelineKey {
    format: TextureFormat,
    shader: Handle<Shader>,
}

impl SpecializedRenderPipeline for ImplicitMainPipeline {
    type Key = ImplicitMainPipelineKey;

    fn specialize(&self, key: Self::Key) -> RenderPipelineDescriptor {
        let fragment = self.fragment_shader.lock().unwrap().clone();
        let Some(fragment) = fragment else {
            return RenderPipelineDescriptor {
                label: Some("implicit_main_dummy".into()),
                layout: vec![],
                vertex: self.fullscreen_shader.to_vertex_state(),
                fragment: None,
                ..Default::default()
            };
        };

        RenderPipelineDescriptor {
            label: Some("implicit_main_ray_march".into()),
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
            depth_stencil: Some(DepthStencilState {
                format: TextureFormat::Depth32Float,
                depth_write_enabled: true,
                depth_compare: CompareFunction::Always,
                bias: Default::default(),
                stencil: Default::default(),
            }),
            ..Default::default()
        }
    }
}

// ─── Render graph nodes ──────────────────────────────────────────────

#[derive(RenderLabel, Clone, Copy, Default, PartialEq, Eq, Hash, Debug)]
struct ImplicitMainPassLabel;

#[derive(RenderLabel, Clone, Copy, Default, PartialEq, Eq, Hash, Debug)]
struct ImplicitWritebackLabel;

/// Main pass: ray-marches SDF to intermediate color + depth textures.
#[derive(Default)]
struct ImplicitMainPassNode;

impl ViewNode for ImplicitMainPassNode {
    type ViewQuery = (
        &'static ExtractedView,
        &'static ImplicitMainPipelineId,
        &'static ImplicitIntermediateTextures,
        &'static ImplicitRenderActive,
    );

    fn run<'w>(
        &self,
        _graph: &mut RenderGraphContext,
        render_context: &mut RenderContext<'w>,
        (extracted_view, pipeline_id, textures, _active): QueryItem<'w, '_, Self::ViewQuery>,
        world: &'w World,
    ) -> Result<(), NodeRunError> {
        let pipeline_cache = world.resource::<PipelineCache>();
        let Some(render_pipeline) = pipeline_cache.get_render_pipeline(**pipeline_id) else {
            trace!("Implicit3D main pass: pipeline not ready");
            return Ok(());
        };

        let gpu_images = world.resource::<RenderAssets<GpuImage>>();
        let Some(color_gpu) = gpu_images.get(textures.color.id()) else {
            trace!("Implicit3D main pass: color GPU image not ready");
            return Ok(());
        };
        let Some(depth_gpu) = gpu_images.get(textures.depth.id()) else {
            trace!("Implicit3D main pass: depth GPU image not ready");
            return Ok(());
        };

        let viewport_size = UVec2::new(color_gpu.size.width, color_gpu.size.height);
        let uniform = ImplicitUniform::from_extracted_view(extracted_view, viewport_size);
        let uniform_buffer =
            render_context
                .render_device()
                .create_buffer_with_data(&BufferInitDescriptor {
                    label: Some("implicit_uniform"),
                    contents: bytemuck::cast_slice(&[uniform]),
                    usage: BufferUsages::UNIFORM | BufferUsages::COPY_DST,
                });

        let bind_group = render_context.render_device().create_bind_group(
            "implicit_main_bind_group",
            &pipeline_cache.get_bind_group_layout(
                &world
                    .resource::<ImplicitMainPipeline>()
                    .bind_group_layout_descriptor,
            ),
            &BindGroupEntries::sequential((BindingResource::Buffer(BufferBinding {
                buffer: &uniform_buffer,
                offset: 0,
                size: NonZeroU64::new(std::mem::size_of::<ImplicitUniform>() as u64),
            }),)),
        );

        // Render to color + depth attachments
        let clear_color = Color::srgba(0.0, 0.0, 0.0, 0.0).to_linear();
        let depth_attachment = DepthAttachment::new(depth_gpu.texture_view.clone(), Some(1.0));
        let mut render_pass = render_context.begin_tracked_render_pass(RenderPassDescriptor {
            label: Some("implicit_main_ray_march"),
            color_attachments: &[Some(RenderPassColorAttachment {
                view: &color_gpu.texture_view,
                depth_slice: None,
                resolve_target: None,
                ops: Operations {
                    load: LoadOp::Clear(clear_color.into()),
                    store: StoreOp::Store,
                },
            })],
            depth_stencil_attachment: Some(depth_attachment.get_attachment(StoreOp::Store)),
            timestamp_writes: None,
            occlusion_query_set: None,
        });

        render_pass.set_render_pipeline(render_pipeline);
        render_pass.set_bind_group(0, &bind_group, &[]);
        render_pass.draw(0..3, 0..1);

        Ok(())
    }
}

/// Writeback pass: composites intermediate results onto main target with depth testing.
#[derive(Default)]
struct ImplicitWritebackNode;

impl ViewNode for ImplicitWritebackNode {
    type ViewQuery = (
        &'static ViewTarget,
        &'static ImplicitIntermediateTextures,
        &'static ImplicitWritebackPipelineId,
        &'static ImplicitRenderActive,
    );

    fn run<'w>(
        &self,
        _graph: &mut RenderGraphContext,
        render_context: &mut RenderContext<'w>,
        (view_target, textures, pipeline_id, _active): QueryItem<'w, '_, Self::ViewQuery>,
        world: &'w World,
    ) -> Result<(), NodeRunError> {
        let pipeline_cache = world.resource::<PipelineCache>();
        let pipelines = world.resource::<ImplicitWritebackPipeline>();

        let Some(pipeline) = pipeline_cache.get_render_pipeline(**pipeline_id) else {
            trace!("Implicit3D writeback pass: pipeline not ready");
            return Ok(());
        };

        // Re-create bind group each frame with current GPU images
        let gpu_images = world.resource::<RenderAssets<GpuImage>>();
        let Some(color_gpu) = gpu_images.get(textures.color.id()) else {
            return Ok(());
        };

        let fresh_bind_group = render_context.render_device().create_bind_group(
            "implicit_writeback_bind_group",
            &pipeline_cache.get_bind_group_layout(&pipelines.layout),
            &BindGroupEntries::sequential((&color_gpu.texture_view, &pipelines.sampler)),
        );

        let mut render_pass = render_context.begin_tracked_render_pass(RenderPassDescriptor {
            label: Some("implicit_writeback"),
            color_attachments: &[Some(view_target.get_color_attachment())],
            depth_stencil_attachment: None,
            timestamp_writes: None,
            occlusion_query_set: None,
        });

        render_pass.set_render_pipeline(pipeline);
        render_pass.set_bind_group(0, &fresh_bind_group, &[]);
        render_pass.draw(0..3, 0..1);

        Ok(())
    }
}

// ─── Plugin ────────────────────────────────────────────────────────────────

/// Plugin that registers the implicit surface ray-marching render pass.
pub struct Implicit3dPlugin;

impl Plugin for Implicit3dPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<ImplicitFragmentShader>()
            .init_resource::<ImplicitShaderVersion>()
            .add_plugins(bevy::render::extract_component::ExtractComponentPlugin::<
                ImplicitIntermediateTextures,
            >::default())
            .add_systems(
                Update,
                (update_implicit_shader, prepare_implicit_textures).after(crate::check_job),
            );

        // Create static writeback shader in main world
        let wgsl = tree_to_wgsl_writeback();
        let shader = Shader::from_wgsl(Cow::Owned(wgsl), "implicit_writeback.wgsl");
        let handle = app.world_mut().resource_mut::<Assets<Shader>>().add(shader);
        app.insert_resource(ImplicitWritebackShader(handle));
    }

    fn finish(&self, app: &mut App) {
        let Some(render_app) = app.get_sub_app_mut(RenderApp) else {
            return;
        };

        render_app
            .init_resource::<SpecializedRenderPipelines<ImplicitMainPipeline>>()
            .init_resource::<SpecializedRenderPipelines<ImplicitWritebackPipeline>>()
            .init_resource::<ImplicitLastShaderVersion>()
            .add_systems(RenderStartup, setup_implicit_main_pipeline)
            .add_systems(
                ExtractSchedule,
                (
                    extract_implicit_shader,
                    extract_implicit_writeback_shader,
                    extract_implicit_shader_version,
                ),
            )
            .add_systems(Render, setup_implicit_writeback_pipeline)
            .add_systems(
                Render,
                prepare_implicit_pipelines.in_set(RenderSystems::Prepare),
            )
            .add_render_graph_node::<ViewNodeRunner<ImplicitMainPassNode>>(
                Core3d,
                ImplicitMainPassLabel,
            )
            .add_render_graph_node::<ViewNodeRunner<ImplicitWritebackNode>>(
                Core3d,
                ImplicitWritebackLabel,
            )
            .add_systems(RenderStartup, add_implicit_edges);
    }
}

// ─── Setup systems ─────────────────────────────────────────────────────────

fn setup_implicit_main_pipeline(mut commands: Commands, fullscreen_shader: Res<FullscreenShader>) {
    let bind_group_layout_descriptor = BindGroupLayoutDescriptor {
        label: Cow::Borrowed("implicit_main_bind_group_layout"),
        entries: vec![BindGroupLayoutEntry {
            binding: 0,
            visibility: ShaderStages::FRAGMENT,
            ty: BindingType::Buffer {
                ty: BufferBindingType::Uniform,
                has_dynamic_offset: false,
                min_binding_size: NonZeroU64::new(std::mem::size_of::<ImplicitUniform>() as u64),
            },
            count: None,
        }],
    };

    commands.insert_resource(ImplicitMainPipeline {
        fullscreen_shader: fullscreen_shader.clone(),
        bind_group_layout_descriptor,
        fragment_shader: Mutex::new(None),
    });
}

fn setup_implicit_writeback_pipeline(
    mut commands: Commands,
    render_device: Res<RenderDevice>,
    fullscreen_shader: Res<FullscreenShader>,
    writeback_shader: Option<Res<ImplicitWritebackShaderRender>>,
    existing: Option<Res<ImplicitWritebackPipeline>>,
) {
    let Some(writeback_shader) = writeback_shader else {
        return;
    };
    if existing.is_some() {
        return;
    }
    let layout = BindGroupLayoutDescriptor {
        label: Cow::Borrowed("implicit_writeback_layout"),
        entries: vec![
            BindGroupLayoutEntry {
                binding: 0,
                visibility: ShaderStages::FRAGMENT,
                ty: BindingType::Texture {
                    sample_type: TextureSampleType::Float { filterable: true },
                    view_dimension: TextureViewDimension::D2,
                    multisampled: false,
                },
                count: None,
            },
            BindGroupLayoutEntry {
                binding: 1,
                visibility: ShaderStages::FRAGMENT,
                ty: BindingType::Sampler(SamplerBindingType::Filtering),
                count: None,
            },
        ],
    };

    let sampler = render_device.create_sampler(&SamplerDescriptor {
        label: Some("implicit_writeback_sampler"),
        ..Default::default()
    });

    commands.insert_resource(ImplicitWritebackPipeline {
        fullscreen_shader: fullscreen_shader.clone(),
        layout,
        sampler,
        shader: writeback_shader.0.clone(),
    });
}

/// Extract the current shader handle from the main world to the render world.
fn extract_implicit_shader(main_world: Res<MainWorld>, main_pipeline: Res<ImplicitMainPipeline>) {
    let shader_handle = main_world
        .get_resource::<ImplicitFragmentShader>()
        .and_then(|s| s.0.clone());
    *main_pipeline.fragment_shader.lock().unwrap() = shader_handle;
}

/// Extract the writeback shader handle from the main world to the render world.
fn extract_implicit_writeback_shader(main_world: Res<MainWorld>, mut commands: Commands) {
    if let Some(shader) = main_world.get_resource::<ImplicitWritebackShader>() {
        commands.insert_resource(ImplicitWritebackShaderRender(shader.0.clone()));
    }
}

/// Extract the shader version from the main world to the render world.
fn extract_implicit_shader_version(main_world: Res<MainWorld>, mut commands: Commands) {
    if let Some(version) = main_world.get_resource::<ImplicitShaderVersion>() {
        commands.insert_resource(ImplicitShaderVersionRender(version.0));
    }
}

/// Tracks the last shader version seen by the prepare system.
#[derive(Resource, Default, Clone, Copy)]
struct ImplicitLastShaderVersion(u64);

/// Prepare per-view pipelines and gate rendering on active Surface3D.
#[allow(clippy::too_many_arguments)]
fn prepare_implicit_pipelines(
    mut commands: Commands,
    pipeline_cache: Res<PipelineCache>,
    mut main_pipelines: ResMut<SpecializedRenderPipelines<ImplicitMainPipeline>>,
    mut writeback_pipelines: ResMut<SpecializedRenderPipelines<ImplicitWritebackPipeline>>,
    views_without: Query<
        Entity,
        (
            Without<ImplicitMainPipelineId>,
            Without<ImplicitRenderActive>,
        ),
    >,
    main_pipeline: Res<ImplicitMainPipeline>,
    writeback_pipeline: Option<Res<ImplicitWritebackPipeline>>,
    views_with: Query<(Entity, &ImplicitMainPipelineId)>,
    _textures: Query<&ImplicitIntermediateTextures>,
    version: Option<Res<ImplicitShaderVersionRender>>,
    mut last_version: Option<ResMut<ImplicitLastShaderVersion>>,
) {
    let has_shader = main_pipeline.fragment_shader.lock().unwrap().is_some();
    let Some(writeback_pipeline) = writeback_pipeline else {
        return;
    };

    // If shader version changed, clear all pipeline IDs so they re-specialize
    if let Some(v) = version {
        if let Some(lv) = last_version.as_mut() {
            if v.0 > lv.0 {
                trace!(
                    "Implicit3D: shader version changed {}, clearing pipelines",
                    v.0
                );
                for (entity, _) in &views_with {
                    commands.entity(entity).remove::<(
                        ImplicitMainPipelineId,
                        ImplicitWritebackPipelineId,
                        ImplicitRenderActive,
                    )>();
                }
                lv.0 = v.0;
            }
        } else if let Some(mut lv_commands) = last_version {
            lv_commands.0 = v.0;
        }
    }

    if has_shader {
        let shader = main_pipeline
            .fragment_shader
            .lock()
            .unwrap()
            .clone()
            .unwrap();
        for entity in &views_without {
            trace!("Implicit3D: preparing pipeline for view {entity:?}");
            let main_key = ImplicitMainPipelineKey {
                format: TextureFormat::Rgba16Float,
                shader: shader.clone(),
            };
            let main_id = main_pipelines.specialize(&pipeline_cache, &main_pipeline, main_key);

            let wb_key = ImplicitWritebackPipelineKey {
                format: TextureFormat::bevy_default(),
                sample_count: 4,
            };
            let wb_id =
                writeback_pipelines.specialize(&pipeline_cache, &writeback_pipeline, wb_key);

            commands.entity(entity).insert((
                ImplicitMainPipelineId(main_id),
                ImplicitWritebackPipelineId(wb_id),
                ImplicitRenderActive,
            ));
        }
    } else {
        for (entity, _) in &views_with {
            commands.entity(entity).remove::<(
                ImplicitMainPipelineId,
                ImplicitWritebackPipelineId,
                ImplicitRenderActive,
                ImplicitIntermediateTextures,
            )>();
        }
    }
}

/// Create intermediate textures in the main world when Surface3D is active.
/// Textures are cached and recreated on resize or when the Surface3D changes.
fn prepare_implicit_textures(
    mut commands: Commands,
    mut command_cad: ResMut<JobBridge>,
    cameras: Query<(Entity, &Camera)>,
    mut images: ResMut<Assets<Image>>,
) {
    let has_surface = matches!(&command_cad.last_result, Some(Ok(JobOutput::Surface3D(_))));

    if has_surface {
        // Get viewport size from the first camera
        let Some((camera_entity, camera)) = cameras.iter().next() else {
            return;
        };
        let Some(target_size) = camera.logical_target_size() else {
            return;
        };
        let size = UVec2::new(target_size.x.max(1.0) as u32, target_size.y.max(1.0) as u32);

        // Recreate textures if size changed, not yet created, or cache was invalidated
        let needs_recreate = match command_cad.implicit_texture_size {
            Some(old_size) => old_size != size || command_cad.implicit_textures.is_none(),
            None => true,
        };

        if needs_recreate {
            // Remove old textures
            if let Some((color, depth)) = command_cad.implicit_textures.take() {
                images.remove(&color);
                images.remove(&depth);
            }

            info!("Implicit3D: creating intermediate textures at {size:?}");
            let mut color_img = Image::new_fill(
                bevy::render::render_resource::Extent3d {
                    width: size.x,
                    height: size.y,
                    depth_or_array_layers: 1,
                },
                bevy::render::render_resource::TextureDimension::D2,
                &[0u8; 8],
                TextureFormat::Rgba16Float,
                RenderAssetUsages::RENDER_WORLD,
            );
            color_img.texture_descriptor.usage =
                bevy::render::render_resource::TextureUsages::TEXTURE_BINDING
                    | bevy::render::render_resource::TextureUsages::RENDER_ATTACHMENT;
            let color_handle = images.add(color_img);

            // Depth textures can't be initialized with data (no Queue::write_texture support)
            let mut depth_img = Image::default_uninit();
            depth_img.texture_descriptor.size = bevy::render::render_resource::Extent3d {
                width: size.x,
                height: size.y,
                depth_or_array_layers: 1,
            };
            depth_img.texture_descriptor.dimension =
                bevy::render::render_resource::TextureDimension::D2;
            depth_img.texture_descriptor.format = TextureFormat::Depth32Float;
            depth_img.texture_descriptor.usage =
                bevy::render::render_resource::TextureUsages::TEXTURE_BINDING
                    | bevy::render::render_resource::TextureUsages::RENDER_ATTACHMENT;
            let depth_handle = images.add(depth_img);

            command_cad.implicit_textures = Some((color_handle.clone(), depth_handle.clone()));
            command_cad.implicit_texture_size = Some(size);

            // Clear old pipeline IDs so they re-specialize with new textures
            commands.entity(camera_entity).remove::<(
                ImplicitMainPipelineId,
                ImplicitWritebackPipelineId,
                ImplicitRenderActive,
            )>();
        }

        // Insert cached textures on the camera entity
        if let Some((color, depth)) = &command_cad.implicit_textures {
            commands
                .entity(camera_entity)
                .insert(ImplicitIntermediateTextures {
                    color: color.clone(),
                    depth: depth.clone(),
                });
        }
    } else {
        for (entity, _) in cameras.iter() {
            commands
                .entity(entity)
                .remove::<ImplicitIntermediateTextures>();
        }
        // Clear cached textures when no Surface3D
        if let Some((color, depth)) = command_cad.implicit_textures.take() {
            images.remove(&color);
            images.remove(&depth);
        }
    }
}

fn add_implicit_edges(mut render_graph: ResMut<RenderGraph>) {
    let subgraph = render_graph.sub_graph_mut(Core3d);
    // Main pass: after prepasses, before main rendering
    subgraph.add_node_edge(Node3d::EndPrepasses, ImplicitMainPassLabel);
    // Writeback: composites object onto main render target
    subgraph.add_node_edge(Node3d::MainOpaquePass, ImplicitWritebackLabel);
    subgraph.add_node_edge(ImplicitWritebackLabel, Node3d::MainTransmissivePass);
}

// ─── Main-world shader generation ──────────────────────────────────────────

/// Generate WGSL shader from Surface3D result and register it with Bevy.
/// Shader is cached and only regenerated when the Surface3D changes.
fn update_implicit_shader(
    mut command_cad: ResMut<JobBridge>,
    mut shaders: ResMut<Assets<Shader>>,
    mut implicit_shader: ResMut<ImplicitFragmentShader>,
    mut version: ResMut<ImplicitShaderVersion>,
) {
    let Some(Ok(JobOutput::Surface3D(surface))) = &command_cad.last_result else {
        // No Surface3D — clear cached main shader
        if let Some(main) = command_cad.implicit_shader.take() {
            shaders.remove(&main);
        }
        implicit_shader.0 = None;
        return;
    };

    // Generate shader only once (or when Surface3D changes)
    if command_cad.implicit_shader.is_none() {
        info!("Implicit3D: generating shader for Surface3D");
        let sdf_body = crate::tree_to_wgsl::emit_sdf_body(surface.tree());
        let main_wgsl = tree_to_wgsl_main_from_sdf(&sdf_body);
        let main_shader = Shader::from_wgsl(Cow::Owned(main_wgsl), "implicit_main.wgsl");
        let main_handle = shaders.add(main_shader);

        command_cad.implicit_shader = Some(main_handle.clone());
        implicit_shader.0 = Some(main_handle);
        // Increment version so render world knows to re-specialize pipelines
        version.0 += 1;
    }
}
