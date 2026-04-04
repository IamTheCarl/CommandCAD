use std::sync::Arc;

use bevy::ecs::resource::Resource;
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
use egui::{
    Color32, Mesh, Painter, Pos2, Rect, Shape, StrokeKind, Ui, Vec2,
    emath::TSTransform,
    epaint::{ColorMode, PathShape, PathStroke},
};
use interpreter::geo::{BoundingRect, TriangulateEarcut};

use crate::{JobError, ViewState};

use super::JobOutput;

#[derive(Debug, Default, Resource)]
pub struct ViewState2d;

impl ViewState2d {
    fn fit_to_screen(&mut self, view_state: &mut ViewState, value: &JobOutput, draw_area: Rect) {
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
            view_state.set_pixels_per_meter(pixels_per_meter);

            let center = bounds.center();
            view_state.offset = bevy::prelude::Vec2::new(-center.x as f32, center.y as f32);
        } else {
            // We don't know how to fit this. Just assume the default.
            *self = Self::default();
        }
    }

    pub fn draw_interface(
        &mut self,
        view_state: &mut ViewState,
        ui: &mut Ui,
        last_result: &Option<Result<JobOutput, JobError>>,
        draw_area: Rect,
    ) {
        if let Some(Ok(value)) = last_result
            && matches!(
                value,
                JobOutput::LineString(_) | JobOutput::Polygon { .. } | JobOutput::PolygonSet { .. }
            )
            && ui.button("Fit to screen").clicked()
        {
            self.fit_to_screen(view_state, value, draw_area);
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
            .map(|coord| {
                transform.mul_pos(Pos2::new(coord.x as f32, -coord.y as f32))
            })
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
    view_state: &ViewState,
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

    // Render fill
    let mut shape = Shape::Mesh(mesh);
    shape.transform(transform);
    painter.add(shape);

    // Render exterior
    painter.add(paint_linestring(
            &transform,
        StrokeKind::Inside,
        polygon.exterior(),
    ));

    // Render interior.
    for interior in polygon.interiors() {
        painter.add(paint_linestring(
            &transform,
            StrokeKind::Outside,
            interior,
        ));
    }
}
