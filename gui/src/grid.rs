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

#[derive(Debug, Resource, Default)]
pub struct GridSettings {
    pub unit_string: String,
    unit_meters: Option<f32>,
    subdivisions: i32,
}

impl GridSettings {
    pub fn world_step(&self) -> Option<f32> {
        let unit_meters = self.unit_meters?;
        Some(unit_meters / self.subdivisions as f32)
    }

    pub fn parse(&mut self) {
        let s = self.unit_string.trim();
        if s.is_empty() {
            self.unit_meters = None;
            self.subdivisions = 1;
            return;
        }

        // Try to split off a unit abbreviation from the end.
        // Find the first digit from the right to separate number from unit.
        let (num_str, unit_str) = match s.rfind(|c: char| c.is_ascii_digit()) {
            Some(pos) => {
                // Everything after the last digit is the unit abbreviation
                if pos + 1 < s.len() {
                    (&s[..pos + 1], &s[pos + 1..])
                } else {
                    (s, "")
                }
            }
            None => (s, ""),
        };

        let number: f32 = match num_str.trim().parse() {
            Ok(n) if n > 0.0 => n,
            _ => {
                self.unit_meters = None;
                self.subdivisions = 1;
                return;
            }
        };

        let coefficient = match units::get_conversion_factor(unit_str) {
            Some(cf) => {
                // Only allow length dimensions
                if cf.dimension.length != 1 {
                    self.unit_meters = None;
                    self.subdivisions = 1;
                    return;
                }
                cf.coefficient as f32
            }
            None => {
                // Unknown unit abbreviation — no grid
                self.unit_meters = None;
                self.subdivisions = 1;
                return;
            }
        };

        self.unit_meters = Some(number * coefficient);

        // Compute subdivisions once based on a target cell size of ~100px.
        // This means the grid will scale proportionally with zoom like the geometry does.
        // We use a default zoom of 10px/m (same as fit-to-screen initial value) for this calculation.
        let pixels_per_meter = 10.0;
        let pixels_per_cell = self.unit_meters.unwrap() * pixels_per_meter;
        if pixels_per_cell > 500.0 {
            self.subdivisions = ((pixels_per_cell / 100.0).ceil()).max(2.0) as i32;
        } else {
            self.subdivisions = 1;
        }
    }
}
