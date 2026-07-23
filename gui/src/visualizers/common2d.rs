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
use bevy::ecs::resource::Resource;
use egui::{
    Color32, Mesh, Painter, Pos2, Rect, Shape, StrokeKind, Ui, Vec2,
    emath::TSTransform,
    epaint::{ColorMode, PathShape, PathStroke},
};
use interpreter::geo::{BoundingRect, TriangulateEarcut};
use std::sync::Arc;

use crate::grid::GridSettings;
use crate::{JobError, JobOutput};

pub fn draw_grid(
    painter: &Painter,
    draw_area: Rect,
    view_state: &ViewState2d,
    grid_settings: &GridSettings,
) {
    let world_step = match grid_settings.world_step() {
        Some(ws) => ws,
        None => return,
    };

    let pixels_per_meter = view_state.pixels_per_meter();
    let pixels_per_cell = world_step * pixels_per_meter;

    if pixels_per_cell < 3.0 {
        return;
    }

    let visuals = painter.ctx().style().visuals.clone();
    let grid_color = visuals.weak_text_color();

    let center_offset = draw_area.center().to_vec2();
    let view_offset = Vec2::new(view_state.offset().x, view_state.offset().y);

    let left_world = (draw_area.left() - center_offset.x) / pixels_per_meter - view_offset.x;
    let right_world = (draw_area.right() - center_offset.x) / pixels_per_meter - view_offset.x;
    let top_world = (draw_area.top() - center_offset.y) / pixels_per_meter - view_offset.y;
    let bottom_world = (draw_area.bottom() - center_offset.y) / pixels_per_meter - view_offset.y;

    let screen_top = draw_area.top();
    let screen_bottom = draw_area.bottom();

    let first_line_idx = (left_world / world_step).floor();
    let last_line_idx = (right_world / world_step).floor();
    for i in (first_line_idx as i32)..=(last_line_idx as i32) {
        let world_x = (i as f32) * world_step;
        let x = (world_x + view_offset.x) * pixels_per_meter + center_offset.x;
        painter.line_segment(
            [Pos2::new(x, screen_top), Pos2::new(x, screen_bottom)],
            (1.0, grid_color),
        );
    }

    let first_line_idx = (top_world / world_step).floor();
    let last_line_idx = (bottom_world / world_step).floor();
    for i in (first_line_idx as i32)..=(last_line_idx as i32) {
        let world_y = (i as f32) * world_step;
        let y = (world_y + view_offset.y) * pixels_per_meter + center_offset.y;
        painter.line_segment(
            [
                Pos2::new(draw_area.left(), y),
                Pos2::new(draw_area.right(), y),
            ],
            (1.0, grid_color),
        );
    }
}

#[derive(Debug, Resource)]
pub struct ViewState2d {
    offset: egui::Vec2,
    zoom: f32,
    pub fit_to_screen_requested: bool,
}

impl Default for ViewState2d {
    fn default() -> Self {
        let mut view_state = ViewState2d {
            offset: egui::Vec2::ZERO,
            zoom: 0.0,
            fit_to_screen_requested: false,
        };
        view_state.set_pixels_per_meter(10.0);
        view_state
    }
}

impl ViewState2d {
    const SCALE_FACTOR: f32 = 1.01;

    pub fn track_movement(&mut self, input_state: &egui::InputState, draw_area: egui::Rect) {
        if let Some(pos) = input_state.pointer.interact_pos() {
            if !draw_area.contains(pos) {
                return;
            }
        } else {
            return;
        }

        self.zoom += input_state.smooth_scroll_delta.y;
        self.zoom = self.zoom.max(0.0);

        if input_state.pointer.primary_down() {
            let drag_delta = input_state.pointer.delta();
            let delta = drag_delta / self.pixels_per_meter();
            self.offset += egui::Vec2::new(delta.x, delta.y);
        }
    }

    pub fn prep_for_painting(&mut self, ui: &mut Ui) -> Painter {
        let draw_area = ui.available_rect_before_wrap();
        Painter::new(ui.ctx().clone(), ui.layer_id(), draw_area)
    }

    pub fn pixels_per_meter(&self) -> f32 {
        Self::SCALE_FACTOR.powf(self.zoom)
    }

    pub fn set_pixels_per_meter(&mut self, pixels_per_meter: f32) {
        self.zoom = pixels_per_meter.log(Self::SCALE_FACTOR);
    }

    pub fn offset(&self) -> Vec2 {
        self.offset
    }

    pub fn fit_to_screen(&mut self, value: &JobOutput, draw_area: Rect) {
        let bounds = match value {
            JobOutput::LineString(line_string) => line_string.0.bounding_rect(),
            JobOutput::Polygon { polygon, .. } => polygon.0.bounding_rect(),
            JobOutput::PolygonSet { polygon_set, .. } => polygon_set.0.bounding_rect(),
            _ => None,
        };

        if let Some(bounds) = bounds {
            let size = bounds.max() - bounds.min();
            let dx = draw_area.x_range().span() / size.x as f32;
            let dy = draw_area.y_range().span() / size.y as f32;
            let pixels_per_meter = dx.min(dy);
            self.set_pixels_per_meter(pixels_per_meter);

            let center = bounds.center();
            self.offset = egui::Vec2::new(-center.x as f32, center.y as f32);
        } else {
            *self = ViewState2d::default();
        }
    }

    pub fn draw_interface(
        &mut self,
        ui: &mut Ui,
        last_result: &Option<Result<JobOutput, JobError>>,
    ) {
        if let Some(Ok(value)) = last_result
            && matches!(
                value,
                JobOutput::LineString(_) | JobOutput::Polygon { .. } | JobOutput::PolygonSet { .. }
            )
            && ui.button("Fit to screen").clicked()
        {
            self.fit_to_screen_requested = true;
        }
    }
}

pub fn paint_linestring(
    transform: &TSTransform,
    stroke_kind: StrokeKind,
    line_string: &interpreter::geo::LineString,
) -> Shape {
    let path = PathShape {
        points: line_string
            .coords()
            .map(|coord| transform.mul_pos(Pos2::new(coord.x as f32, -coord.y as f32)))
            .collect(),
        closed: line_string.is_closed(),
        fill: Color32::TRANSPARENT,
        stroke: PathStroke {
            width: 2.0,
            color: ColorMode::Solid(Color32::WHITE),
            kind: stroke_kind,
        },
    };
    Shape::Path(path)
}

pub fn build_fill_mesh_from_polygon(polygon: &interpreter::geo::Polygon) -> Arc<Mesh> {
    let mut mesh = Mesh::default();
    let triangulation = polygon.earcut_triangles_raw();
    for vert in triangulation.vertices.chunks(2) {
        let x = vert[0];
        let y = vert[1];

        mesh.colored_vertex(Pos2::new(x as f32, -y as f32), Color32::GRAY);
    }

    for triangle in triangulation.triangle_indices.chunks(3) {
        let a = triangle[0];
        let b = triangle[1];
        let c = triangle[2];

        mesh.add_triangle(a as u32, b as u32, c as u32);
    }

    Arc::new(mesh)
}

pub fn paint_polygon(
    painter: &Painter,
    draw_area: Rect,
    view_state: &ViewState2d,
    polygon: &interpreter::geo::Polygon,
    mesh: Arc<Mesh>,
) {
    let pixels_per_meter = view_state.pixels_per_meter();
    let center_offset = draw_area.center().to_vec2();
    let view_offset = Vec2::new(view_state.offset.x, view_state.offset.y);

    let transform = TSTransform {
        scaling: pixels_per_meter,
        translation: center_offset + view_offset * pixels_per_meter,
    };

    let mut shape = Shape::Mesh(mesh);
    shape.transform(transform);
    painter.add(shape);

    painter.add(paint_linestring(
        &transform,
        StrokeKind::Inside,
        polygon.exterior(),
    ));

    for interior in polygon.interiors() {
        painter.add(paint_linestring(&transform, StrokeKind::Outside, interior));
    }
}
