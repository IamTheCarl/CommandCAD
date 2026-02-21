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

use std::{borrow::Cow, collections::HashMap, fmt::Display, sync::Arc};

use hashable_map::HashableMap;
use imstr::ImString;
use rayon::prelude::*;

use crate::{
    compile::{AstNode, DictionaryConstruction},
    execute_expression,
    execution::{
        errors::{ExecutionResult, Raise as _},
        find_all_variable_accesses_in_expression,
        stack::ScopeType,
        values::string::formatting::Style,
        ExecutionContext,
    },
    values::StaticType,
};

use super::{
    MissingAttributeError, Object, StaticTypeName, StructDefinition, StructMember, Value, ValueType,
};

#[derive(Clone, Debug, Eq)]
pub(crate) struct DictionaryData {
    pub members: HashableMap<ImString, Value>,
    pub struct_def: StructDefinition,
}

impl PartialEq for DictionaryData {
    fn eq(&self, other: &Self) -> bool {
        self.members == other.members
    }
}

pub fn find_all_variable_accesses_in_dictionary_construction(
    dictionary_construction: &crate::compile::DictionaryConstruction,
    access_collector: &mut dyn FnMut(&AstNode<ImString>) -> ExecutionResult<()>,
) -> ExecutionResult<()> {
    for assignment in dictionary_construction.assignments.iter() {
        find_all_variable_accesses_in_expression(
            &assignment.node.assignment.node,
            access_collector,
        )?;
    }

    Ok(())
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Dictionary {
    pub(crate) data: Arc<DictionaryData>,
}

impl Object for Dictionary {
    fn get_type(&self, _context: &ExecutionContext) -> ValueType {
        self.data.struct_def.clone().into()
    }

    fn format(
        &self,
        context: &ExecutionContext,
        f: &mut dyn std::fmt::Write,
        style: Style,
        precision: Option<u8>,
    ) -> std::fmt::Result {
        write!(f, "(")?;

        let mut items = self.data.members.iter().peekable();

        while let Some((name, value)) = items.next() {
            write!(f, "{name} = ")?;
            value.format(context, f, style, precision)?;
            if items.peek().is_some() {
                write!(f, ", ")?;
            }
        }

        write!(f, ")")?;

        Ok(())
    }

    fn get_attribute(&self, context: &ExecutionContext, attribute: &str) -> ExecutionResult<Value> {
        if let Some(member) = self.data.members.get(attribute) {
            Ok(member.clone())
        } else {
            Err(MissingAttributeError {
                name: attribute.into(),
            }
            .to_error(context.stack_trace))
        }
    }
}

impl StaticTypeName for Dictionary {
    fn static_type_name() -> Cow<'static, str> {
        "Dictionary".into()
    }
}

impl StaticType for Dictionary {
    fn static_type() -> ValueType {
        static TYPE: std::sync::OnceLock<std::sync::Arc<HashableMap<ImString, StructMember>>> =
            std::sync::OnceLock::new();
        let signature = TYPE.get_or_init(|| Arc::new(HashableMap::new()));
        ValueType::Dictionary(StructDefinition {
            members: signature.clone(),
            variadic: true,
        })
    }
}

impl Dictionary {
    pub fn struct_def(&self) -> &StructDefinition {
        &self.data.struct_def
    }

    pub fn from_ast(
        context: &ExecutionContext,
        ast_node: &AstNode<DictionaryConstruction>,
    ) -> ExecutionResult<Self> {
        let mut members = HashMap::with_capacity(ast_node.node.assignments.len());

        context.stack.scope_mut(
            context.stack_trace,
            ScopeType::Inherited,
            HashMap::new(),
            |stack, stack_trace| {
                let mut buffer = Vec::new();
                for group in ast_node.node.compute_groups() {
                    {
                        let context = ExecutionContext {
                            stack_trace,
                            stack,
                            ..context.clone()
                        };

                        buffer.par_extend(group.par_iter().map(|assignment| {
                            (
                                assignment.node.name.node.clone(),
                                execute_expression(&context, &assignment.node.assignment),
                            )
                        }));
                    }

                    for (name, result) in buffer.drain(..) {
                        let value = result?;

                        if members.insert(name.clone(), value.clone()).is_some() {
                            // That's a duplicate member.
                            return Err(DuplicateMemberError { name }.to_error(
                                context.stack_trace.iter().chain([&ast_node.reference]),
                            ));
                        }

                        stack.insert_value(name, value);
                    }
                }

                Ok(())
            },
        )??;

        Ok(Self::new(context, members))
    }

    pub fn new(context: &ExecutionContext, map: HashMap<ImString, Value>) -> Self {
        let mut struct_members = HashMap::with_capacity(map.len());

        for (name, value) in map.iter() {
            let member = StructMember {
                ty: value.get_type(context),
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

    pub fn iter(&self) -> impl Iterator<Item = (&ImString, &Value)> {
        self.data.members.iter()
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.data.members.get(name)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct DuplicateMemberError {
    pub name: ImString,
}

impl std::error::Error for DuplicateMemberError {}

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
                "none".into(),
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

    #[test]
    fn format() {
        let product = test_run("\"{value}\"::format(value = (a = 1u)) == \"(a = 1)\"").unwrap();
        assert_eq!(product, values::Boolean(true).into());

        let product =
            test_run("let result = \"{value}\"::format(value = (a = 1u, b = 2u)); in result == \"(a = 1, b = 2)\" || result == \"(b = 2, a = 1)\"")
                .unwrap();
        assert_eq!(product, values::Boolean(true).into());

        let product =
            test_run("\"{value:X}\"::format(value = (a = 0xDEADBEEFu)) == \"(a = DEADBEEF)\"")
                .unwrap();
        assert_eq!(product, values::Boolean(true).into());
    }
}
