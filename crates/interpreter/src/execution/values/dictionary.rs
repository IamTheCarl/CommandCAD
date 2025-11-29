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
        stack::Stack,
    },
};

use super::{
    MissingAttributeError, Object, StaticTypeName, StructDefinition, StructMember, Value, ValueType,
};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Dictionary {
    members: Arc<HashableMap<String, Value>>,
}

impl Object for Dictionary {
    fn get_type(&self) -> ValueType {
        static MEMBERS: std::sync::OnceLock<Arc<Vec<StructMember>>> = std::sync::OnceLock::new();

        ValueType::Dictionary(StructDefinition {
            members: MEMBERS.get_or_init(|| Arc::new(vec![])).clone(),
            variadic: true,
        })
    }

    fn get_attribute(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        attribute: &str,
    ) -> ExpressionResult<&Value> {
        if let Some(member) = self.members.get(attribute) {
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
        // HashableMap is just a wrapper around HashMap, so this has no additional cost.
        let members = Arc::new(HashableMap::from(map));

        Self { members }
    }
}

impl Dictionary {
    pub fn from_ast(
        log: &mut dyn RuntimeLog,
        stack_trace: &mut Vec<SourceReference>,
        stack: &mut Stack,
        ast_node: &AstNode<DictionaryConstruction>,
    ) -> ExpressionResult<Self> {
        let mut members = HashMap::with_capacity(ast_node.node.assignments.len());

        for assignment in ast_node.node.assignments.iter() {
            let name = assignment.node.name.node.clone();
            let value = execute_expression(log, stack_trace, stack, &assignment.node.assignment)?;

            if members.insert(name, value).is_some() {
                // That's a duplicate member.
                return Err(DuplicateMemberError {
                    name: assignment.node.name.node.clone(),
                }
                .to_error(stack_trace.iter().chain([&assignment.reference])));
            }
        }

        Ok(Self::from(members))
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
    use crate::execution::{test_run, values::ValueNone};

    #[test]
    fn build_dictionary() {
        let product = test_run("(none = std.consts.None)").unwrap();
        let expected = Arc::new(HashableMap::from(HashMap::from_iter([(
            "none".to_string(),
            ValueNone.into(),
        )])));

        assert_eq!(product.as_dictionary().unwrap().members, expected);
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
