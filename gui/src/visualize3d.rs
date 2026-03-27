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
use eframe::{
    egui_wgpu,
    wgpu::{self, util::DeviceExt as _},
};
use egui::{Painter, Rect};
use interpreter::values::manifold_mesh::ManifoldMesh3D;
use nalgebra::{Matrix4, Vector3};

use crate::{ManifoldMeshState, ViewState};

const VIEW_NEAR: f32 = -1000.0;
const VIEW_FAR: f32 = 1000.0;

#[repr(C)]
#[derive(Default, Copy, Clone, Debug, bytemuck::Pod, bytemuck::Zeroable)]
struct VertexUniform {
    view_projection: Matrix4<f32>,
    pixels_per_meter: f32,
}

#[derive(Debug)]
pub struct ViewState3D {}

impl ViewState3D {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        let wgpu_render_state = cc
            .wgpu_render_state
            .as_ref()
            .expect("Project built without WGPU support");

        let device = &wgpu_render_state.device;

        let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("mesh_shader"),
            source: wgpu::ShaderSource::Wgsl(include_str!("./shaders/mesh.wgsl").into()),
        });

        let vertex_bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: Some("mesh_vertex_bind_group_layout"),
                entries: &[wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::VERTEX,
                    ty: wgpu::BindingType::Buffer {
                        ty: wgpu::BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                }],
            });
        let fragment_bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: Some("fragment_vertex_bind_group_layout"),
                entries: &[wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Buffer {
                        ty: wgpu::BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                }],
            });

        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("mesh_pipeline_layout"),
            bind_group_layouts: &[&vertex_bind_group_layout, &fragment_bind_group_layout],
            push_constant_ranges: &[],
        });

        let pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("mesh_pipeline"),
            layout: Some(&pipeline_layout),
            vertex: wgpu::VertexState {
                module: &shader,
                entry_point: Some("vs_main"),
                buffers: &[wgpu::VertexBufferLayout {
                    array_stride: std::mem::size_of::<Vector3<f32>>() as wgpu::BufferAddress,
                    step_mode: wgpu::VertexStepMode::Vertex,
                    attributes: &wgpu::vertex_attr_array![0 => Float32x3],
                }],
                compilation_options: wgpu::PipelineCompilationOptions::default(),
            },
            fragment: Some(wgpu::FragmentState {
                module: &shader,
                entry_point: Some("fs_main"),
                targets: &[Some(wgpu_render_state.target_format.into())],
                compilation_options: wgpu::PipelineCompilationOptions::default(),
            }),
            primitive: wgpu::PrimitiveState::default(),
            depth_stencil: None,
            multisample: wgpu::MultisampleState::default(),
            multiview: None,
            cache: None,
        });

        let vertex_uniform_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("vertex_uniform_buffer"),
            contents: bytemuck::cast_slice(&[VertexUniform::default()]),
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
        });

        let vertex_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("vertex_bind_group"),
            layout: &vertex_bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: vertex_uniform_buffer.as_entire_binding(),
            }],
        });

        let vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("vertex_buffer"),
            contents: bytemuck::cast_slice::<Vector3<f32>, _>(&[]),
            usage: wgpu::BufferUsages::VERTEX,
        });
        let index_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("index_buffer"),
            contents: bytemuck::cast_slice::<u16, _>(&[]),
            usage: wgpu::BufferUsages::INDEX,
        });

        let fragment_uniform_buffer =
            device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: Some("fragment_uniform_buffer"),
                contents: bytemuck::cast_slice(&[Matrix4::<f32>::zeros()]),
                usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
            });
        let fragment_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("fragment_bind_group"),
            layout: &fragment_bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: fragment_uniform_buffer.as_entire_binding(),
            }],
        });

        wgpu_render_state
            .renderer
            .write()
            .callback_resources
            .insert(MeshRenderResources {
                pipeline,
                vertex_bind_group,
                vertex_uniform_buffer,
                vertex_buffer,
                index_buffer,
                index_buffer_length: 0,
                fragment_bind_group,
            });

        Self {}
    }

    pub fn update(&mut self, _ui: &mut egui::Ui, _draw_area: Rect) {
        // TODO we need to take mouse input to update our projection matrix with.
    }

    pub fn paint(
        &self,
        view_state: &ViewState,
        manifold_state: &mut ManifoldMeshState,
        painter: Painter,
    ) {
        let area = painter.clip_rect();
        painter.add(egui_wgpu::Callback::new_paint_callback(
            area,
            MeshRenderCallback {
                vertex_uniform: VertexUniform {
                    view_projection: Matrix4::new_orthographic(
                        area.left(),
                        area.right(),
                        area.bottom(),
                        area.top(),
                        VIEW_NEAR,
                        VIEW_FAR,
                    ),
                    pixels_per_meter: view_state.pixels_per_meter,
                },
                manifold_to_upload: if manifold_state.uploaded_to_gpu {
                    None
                } else {
                    manifold_state.uploaded_to_gpu = true;
                    Some(manifold_state.manifold.clone())
                },
            },
        ));
    }
}

struct MeshRenderResources {
    pipeline: wgpu::RenderPipeline,
    vertex_bind_group: wgpu::BindGroup,
    vertex_uniform_buffer: wgpu::Buffer,
    vertex_buffer: wgpu::Buffer,
    index_buffer: wgpu::Buffer,
    index_buffer_length: u32,
    fragment_bind_group: wgpu::BindGroup,
}

struct MeshRenderCallback {
    vertex_uniform: VertexUniform,
    manifold_to_upload: Option<ManifoldMesh3D>,
}

impl eframe::egui_wgpu::CallbackTrait for MeshRenderCallback {
    fn prepare(
        &self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        _screen_descriptor: &egui_wgpu::ScreenDescriptor,
        _egui_encoder: &mut wgpu::CommandEncoder,
        callback_resources: &mut egui_wgpu::CallbackResources,
    ) -> Vec<wgpu::CommandBuffer> {
        let resources: &mut MeshRenderResources = callback_resources
            .get_mut()
            .expect("Mesh render resources were never inserted");

        queue.write_buffer(
            &resources.vertex_uniform_buffer,
            0,
            bytemuck::cast_slice(&[self.vertex_uniform]),
        );

        if let Some(manifold) = &self.manifold_to_upload {
            resources.vertex_buffer = device.create_buffer(&wgpu::BufferDescriptor {
                label: Some("vertex_buffer"),
                size: (manifold.0.ps.len() * std::mem::size_of::<Vector3<f32>>()) as u64,
                usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
                mapped_at_creation: false,
            });
            for (index, vector) in manifold.0.ps.iter().enumerate() {
                queue.write_buffer(
                    &resources.vertex_buffer,
                    (index * std::mem::size_of::<Vector3<f32>>()) as u64,
                    bytemuck::cast_slice(&[vector.cast::<f32>()]),
                );
            }

            resources.index_buffer = device.create_buffer(&wgpu::BufferDescriptor {
                label: Some("index_buffer"),
                size: (manifold.0.hs.len() * std::mem::size_of::<u32>()) as u64,
                usage: wgpu::BufferUsages::INDEX | wgpu::BufferUsages::COPY_DST,
                mapped_at_creation: false,
            });

            for (index, half_edge) in manifold.0.hs.iter().enumerate() {
                queue.write_buffer(
                    &resources.index_buffer,
                    (index * std::mem::size_of::<u32>()) as u64,
                    bytemuck::cast_slice(&[half_edge.tail as u32]),
                );
            }
            resources.index_buffer_length = manifold.0.hs.len() as u32;
        }

        Vec::new()
    }

    fn paint(
        &self,
        _info: egui::PaintCallbackInfo,
        render_pass: &mut wgpu::RenderPass<'static>,
        callback_resources: &egui_wgpu::CallbackResources,
    ) {
        let resources: &MeshRenderResources = callback_resources
            .get()
            .expect("Mesh render resources were never inserted");

        render_pass.set_pipeline(&resources.pipeline);
        render_pass.set_bind_group(0, &resources.vertex_bind_group, &[]);
        render_pass.set_bind_group(1, &resources.fragment_bind_group, &[]);
        render_pass.set_vertex_buffer(0, resources.vertex_buffer.slice(..));
        render_pass.set_index_buffer(resources.index_buffer.slice(..), wgpu::IndexFormat::Uint32);
        render_pass.draw_indexed(0..resources.index_buffer_length, 0, 0..1);
    }
}
