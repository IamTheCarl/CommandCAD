/*
 * Copyright 2024 James Carl
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

use fj_core::{objects::ObjectSet, storage::Handle};

use crate::script::{
    execution::{
        types::{List, OperatorResult, Value},
        Failure,
    },
    Span,
};

impl<'a, S, T> From<&ObjectSet<T>> for Value<'a, S>
where
    S: Span,
    Value<'a, S>: From<Handle<T>>,
{
    fn from(object_set: &ObjectSet<T>) -> Self {
        List::from(object_set.iter().map(|handle| Value::from(handle.clone()))).into()
    }
}

pub fn check_for_duplicates<'a, S, T>(
    span: &S,
    expected_items: usize,
    items: impl Iterator<Item = T>,
) -> OperatorResult<S, Vec<T>>
where
    S: Span,
    T: Eq,
{
    let mut set = Vec::with_capacity(expected_items);

    for (index, item) in items.enumerate() {
        if !set.contains(&item) {
            set.push(item);
        } else {
            return Err(Failure::ListContainsDuplicate(span.clone(), index));
        }
    }

    Ok(set)
}
