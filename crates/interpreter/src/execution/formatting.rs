/*
 * Copyright 2025 James Carl
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

use std::fmt::Display;

/// Presents iterators in a comma separated format.
pub struct IteratorFormatter<I, D>(pub I)
where
    I: Iterator<Item = D> + Clone,
    D: Display;

impl<I, D> Display for IteratorFormatter<I, D>
where
    I: Iterator<Item = D> + Clone,
    D: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.clone().peekable();

        loop {
            match (iter.next(), iter.peek().is_some()) {
                (Some(next), true) => write!(f, "{}, ", next)?,
                (Some(next), false) => write!(f, "{}", next)?,
                (None, _) => break Ok(()),
            }
        }
    }
}
