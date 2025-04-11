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

use std::{collections::HashMap, fmt::Display, sync::Arc};

use hashable_map::HashableMap;

use crate::{
    compile::{AstNode, DictionaryConstruction, SourceReference},
    execute_expression,
    execution::{
        errors::{ErrorType, ExpressionResult, Raise as _},
        heap::{HeapKey, HeapStorage},
        logging::RuntimeLog,
        stack::Stack,
        Heap,
    },
};

use super::{
    MissingAttributeError, Object, ObjectClone, StaticTypeName, StructDefinition, StructMember,
    Value, ValueType,
};

pub type DictionaryStorage = HeapStorage<HashableMap<String, Value>>;

#[derive(Debug, Eq, PartialEq)]
pub struct Dictionary {
    /// The actual storage to the dictionary lives in the heap, and we store a reference to it.
    key: HeapKey,
}

impl Object for Dictionary {
    fn get_type(&self) -> ValueType {
        static MEMBERS: std::sync::OnceLock<Arc<Vec<StructMember>>> = std::sync::OnceLock::new();

        ValueType::Dictionary(StructDefinition {
            members: MEMBERS.get_or_init(|| Arc::new(vec![])).clone(),
            variadic: true,
        })
    }

    fn get_attribute_ref<'h>(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        heap: &'h Heap,
        attribute: &str,
    ) -> ExpressionResult<&'h Value> {
        let members = heap.dictionaries.get(&self.key);
        if let Some(member) = members.get(attribute) {
            Ok(member)
        } else {
            Err(MissingAttributeError {
                name: attribute.into(),
            }
            .to_error(stack_trace))
        }
    }
    fn get_attribute_mut<'h>(
        &mut self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        heap: &'h mut Heap,
        attribute: &str,
    ) -> ExpressionResult<&'h mut Value> {
        let members = heap.dictionaries.get_mut(&self.key);
        if let Some(member) = members.get_mut(attribute) {
            Ok(member)
        } else {
            Err(MissingAttributeError {
                name: attribute.into(),
            }
            .to_error(stack_trace))
        }
    }

    fn drop(self, heap: &mut Heap) {
        if let Some(mut internal_storage) = heap.dictionaries.dereference_allocation(self.key) {
            // That was the last reference to this dictionary.
            // We need to drop all the content.
            for (_name, value) in internal_storage.drain() {
                value.drop(heap);
            }
        }
    }
}

impl ObjectClone for Dictionary {
    fn object_clone(&self, heap: &Heap) -> Value {
        // Get another reference to the allocation and return that.
        let key = heap.dictionaries.reference_allocation(&self.key);
        Self { key }.into()
    }
}

impl StaticTypeName for Dictionary {
    fn static_type_name() -> &'static str {
        "Dictionary"
    }
}

impl Dictionary {
    pub fn from_hashmap(heap: &mut Heap, map: HashMap<String, Value>) -> Self {
        // HashableMap is just a wrapper around HashMap, so this has no additional cost.
        let content = HashableMap::from(map);

        let key = heap.dictionaries.new_allocation(content);

        Self { key }
    }

    pub fn from_ast(
        log: &mut dyn RuntimeLog,
        stack_trace: &mut Vec<SourceReference>,
        stack: &mut Stack,
        heap: &mut Heap,
        ast_node: &AstNode<DictionaryConstruction>,
    ) -> ExpressionResult<Self> {
        let mut members = HashMap::with_capacity(ast_node.node.assignments.len());

        for assignment in ast_node.node.assignments.iter() {
            let name = assignment.node.name.node.clone();
            let value =
                execute_expression(log, stack_trace, stack, heap, &assignment.node.assignment)?;

            if let Some(already_occupied) = members.insert(name, value) {
                // That's an error. We need to drop all that data and then report an error.
                already_occupied.drop(heap);

                for (_name, value) in members {
                    value.drop(heap);
                }

                return Err(DuplicateMemberError {
                    name: assignment.node.name.node.clone(),
                }
                .to_error(stack_trace.iter().chain([&assignment.reference])));
            }
        }

        Ok(Self::from_hashmap(heap, members))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct DuplicateMemberError {
    pub name: String,
}

impl ErrorType for DuplicateMemberError {}

impl Display for DuplicateMemberError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Dictionary was created with duplicate member: {}",
            self.name
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::execution::{test_run, values::Void};

    #[test]
    fn build_dictionary() {
        let (product, mut heap) = test_run("(void = ~)").unwrap();
        let product_heap = heap.dictionaries.get(&product.as_dictionary().unwrap().key);
        let expected = HashableMap::from(HashMap::from_iter([("void".to_string(), Void.into())]));

        assert_eq!(product_heap, &expected);

        product.drop(&mut heap);
    }

    #[test]
    fn duplicate_entries() {
        // Two values of the same name is not allowed.
        test_run("(void = (), void = ())").unwrap_err();
    }

    #[test]
    fn non_existant_member() {
        test_run("(void = ()).does_not_exist").unwrap_err();
    }
}
