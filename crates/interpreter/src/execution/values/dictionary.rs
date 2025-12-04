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
        logging::RuntimeLog,
        stack::{ScopeType, Stack},
    },
};

use super::{
    MissingAttributeError, Object, StaticTypeName, StructDefinition, StructMember, Value, ValueType,
};

#[derive(Clone, Debug, Eq)]
pub(crate) struct DictionaryData {
    pub members: HashableMap<String, Value>,
    pub struct_def: StructDefinition,
}

impl PartialEq for DictionaryData {
    fn eq(&self, other: &Self) -> bool {
        self.members == other.members
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Dictionary {
    pub(crate) data: Arc<DictionaryData>,
}

impl Object for Dictionary {
    fn get_type(&self) -> ValueType {
        self.data.struct_def.clone().into()
    }

    fn get_attribute(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        attribute: &str,
    ) -> ExpressionResult<&Value> {
        if let Some(member) = self.data.members.get(attribute) {
            Ok(member)
        } else {
            Err(MissingAttributeError {
                name: attribute.into(),
            }
            .to_error(stack_trace))
        }
    }
}

impl StaticTypeName for Dictionary {
    fn static_type_name() -> &'static str {
        "Dictionary"
    }
}

impl From<HashMap<String, Value>> for Dictionary {
    fn from(map: HashMap<String, Value>) -> Self {
        let mut struct_members = HashMap::with_capacity(map.len());

        for (name, value) in map.iter() {
            let member = StructMember {
                ty: value.get_type(),
                default: None,
            };

            struct_members.insert(name.clone(), member);
        }

        // HashableMap is just a wrapper around HashMap, so this has no additional cost.
        let data = Arc::new(DictionaryData {
            members: HashableMap::from(map),
            struct_def: StructDefinition {
                members: Arc::new(HashableMap::from(struct_members)),
                variadic: false,
            },
        });

        Self { data }
    }
}

impl Dictionary {
    pub fn struct_def(&self) -> &StructDefinition {
        &self.data.struct_def
    }

    pub fn from_ast(
        log: &mut dyn RuntimeLog,
        stack_trace: &mut Vec<SourceReference>,
        stack: &mut Stack,
        ast_node: &AstNode<DictionaryConstruction>,
    ) -> ExpressionResult<Self> {
        let mut members = HashMap::with_capacity(ast_node.node.assignments.len());

        stack.scope(stack_trace, ScopeType::Inherited, |stack, stack_trace| {
            for assignment in ast_node.node.assignments.iter() {
                let name = assignment.node.name.node.clone();
                let value =
                    execute_expression(log, stack_trace, stack, &assignment.node.assignment)?;

                if members.insert(name.clone(), value.clone()).is_some() {
                    // That's a duplicate member.
                    return Err(DuplicateMemberError {
                        name: assignment.node.name.node.clone(),
                    }
                    .to_error(stack_trace.iter().chain([&assignment.reference])));
                }

                stack.insert_value(name, value);
            }

            Ok(())
        })??;

        Ok(Self::from(members))
    }

    pub fn iter(&self) -> impl Iterator<Item = (&String, &Value)> {
        self.data.members.iter()
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
    use crate::execution::{test_run, values};

    #[test]
    fn build_dictionary() {
        let product = test_run("(none = std.consts.None)").unwrap();
        let expected = Arc::new(DictionaryData {
            members: HashableMap::from(HashMap::from_iter([(
                "none".to_string(),
                values::ValueNone.into(),
            )])),
            struct_def: StructDefinition {
                members: Arc::new(HashableMap::from(HashMap::from([(
                    "none".into(),
                    StructMember {
                        ty: ValueType::TypeNone,
                        default: None,
                    },
                )]))),
                variadic: false,
            },
        });

        assert_eq!(product.as_dictionary().unwrap().data, expected);
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

    #[test]
    fn reference_own_member() {
        let product = test_run("let d = (one = 1u, two = one + 1u); in d.two").unwrap();
        assert_eq!(product, values::UnsignedInteger::from(2).into());
    }
}
