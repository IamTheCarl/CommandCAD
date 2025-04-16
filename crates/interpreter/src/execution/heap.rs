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

use std::{
    collections::HashMap,
    num::NonZeroUsize,
    sync::atomic::{AtomicUsize, Ordering},
};

use super::values::{DictionaryStorage, StructMemberStorage};

#[derive(Debug, Default)]
/// A collection of objects that are stored on the heap.
pub struct Heap {
    pub dictionaries: DictionaryStorage,
    pub struct_members: StructMemberStorage,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct HeapKey(NonZeroUsize);

#[derive(Debug)]
struct HeapData<D> {
    reference_count: AtomicUsize,
    data: D,
}

#[derive(Debug)]
pub struct HeapStorage<D: std::fmt::Debug> {
    allocations: HashMap<NonZeroUsize, HeapData<D>>,
    unused_keys: Vec<NonZeroUsize>,
    next_key: usize,
}

impl<D: std::fmt::Debug> HeapStorage<D> {
    fn next_free_key(&mut self) -> HeapKey {
        if let Some(key) = self.unused_keys.pop() {
            HeapKey(key)
        } else {
            let key = self.next_key;
            self.next_key += 1;
            HeapKey(NonZeroUsize::new(key).expect("Key was zero"))
        }
    }

    pub fn get(&self, key: &HeapKey) -> &D {
        &self
            .allocations
            .get(&key.0)
            .expect("Zombie key used for reference")
            .data
    }

    pub fn get_mut(&mut self, key: &HeapKey) -> &mut D {
        &mut self
            .allocations
            .get_mut(&key.0)
            .expect("Zombie key used for reference")
            .data
    }

    /// Creates a new allocation from the given data and returns a key
    /// for the first reference.
    #[must_use]
    pub fn new_allocation(&mut self, data: D) -> HeapKey {
        let key = self.next_free_key();

        let data = HeapData {
            // Reference count starts as one since a key already exists.
            reference_count: AtomicUsize::new(1),
            data,
        };
        self.allocations.insert(key.0, data);

        key
    }

    /// Increments the reference count of a dictionary by one.
    #[must_use]
    pub fn reference_allocation(&self, key: &HeapKey) -> HeapKey {
        let data = self.allocations.get(&key.0).expect("Zombie key referenced");

        data.reference_count.fetch_add(1, Ordering::AcqRel);

        // The user can't clone keys themselves, so we'll do that for them.
        HeapKey(key.0)
    }

    /// Decrements the reference count of a dictionary by one.
    /// Returns the allocation if the reference count reaches zero.
    pub fn dereference_allocation(&mut self, key: HeapKey) -> Option<D> {
        let data = self
            .allocations
            .get_mut(&key.0)
            .expect("Zombie key dropped");

        let previous_count = data.reference_count.fetch_sub(1, Ordering::AcqRel);
        let raw_key = key.0;

        // Make sure the key doesn't run its drop method, otherwise it'll panic.
        std::mem::forget(key);

        if previous_count - 1 == 0 {
            // No more references. Drop the data.
            let data = self
                .allocations
                .remove(&raw_key)
                .expect("Data to delete vanished");
            self.unused_keys.push(raw_key);

            Some(data.data)
        } else {
            None
        }
    }
}

impl<D: std::fmt::Debug> Default for HeapStorage<D> {
    fn default() -> Self {
        Self {
            allocations: HashMap::new(),
            unused_keys: Vec::new(),
            next_key: 1,
        }
    }
}

#[cfg(debug_assertions)]
impl<D: std::fmt::Debug> Drop for HeapStorage<D> {
    fn drop(&mut self) {
        assert!(
            self.allocations.is_empty(),
            "Heap storage was not empty: {self:?}",
        );
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn free_key() {
        let mut heap = HeapStorage::default();
        let key = heap.new_allocation(24);
        let key2 = heap.reference_allocation(&key);

        let freed_data = heap.dereference_allocation(key);
        assert!(freed_data.is_none());

        let freed_data = heap.dereference_allocation(key2);
        assert_eq!(freed_data, Some(24));
    }
}
