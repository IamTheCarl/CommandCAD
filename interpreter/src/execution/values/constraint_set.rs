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

use common_data_types::Float;
use hashable_map::{HashableMap, HashableSet};
use imstr::ImString;

use crate::{
    compile::{constraint_set::ConstraintSet as AstConstraintSet, AstNode},
    execution::{
        errors::{ErrorType, ExpressionResult, GenericFailure, Raise},
        find_all_variable_accesses_in_expression,
        logging::LocatedStr,
    },
    values::{
        closure::{BuiltinCallable, Signature},
        BuiltinCallableDatabase, BuiltinFunction, Dictionary, MissingAttributeError, Object,
        Scalar, StaticTypeName, StructDefinition, Style, Value, ValueType,
    },
    ExecutionContext,
};
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    sync::Arc,
};

pub fn find_all_captured_variables_in_constraint_set(
    constraint_set: &AstConstraintSet,
    access_collector: &mut dyn FnMut(&AstNode<ImString>) -> ExpressionResult<()>,
) -> ExpressionResult<()> {
    let mut access_collector = |name: &AstNode<ImString>| {
        if !constraint_set
            .variables
            .iter()
            .any(|variable| variable.node == name.node)
        {
            access_collector(name)
        } else {
            // Is not a captured variable.
            Ok(())
        }
    };

    for constraint in constraint_set.constraints.iter() {
        find_all_variable_accesses_in_expression(
            &constraint.node.left.node,
            &mut access_collector,
        )?;
        find_all_variable_accesses_in_expression(
            &constraint.node.right.node,
            &mut access_collector,
        )?;
    }

    Ok(())
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ConstraintSet {
    variables: Arc<HashableSet<ImString>>,
    captured_values: Arc<HashableMap<ImString, Value>>,
    source: Arc<AstConstraintSet>,
}

impl ConstraintSet {
    pub fn from_ast(
        context: &ExecutionContext,
        source: &AstNode<Arc<AstConstraintSet>>,
    ) -> ExpressionResult<Self> {
        let mut variables = HashSet::new();
        let mut duplicate_variables = Vec::new();
        for variable in source.node.variables.iter().map(|field| &field.node) {
            if !variables.insert(variable.clone()) {
                duplicate_variables.push(variable.clone());
            }
        }

        if !duplicate_variables.is_empty() {
            return Err(DuplicateVariablesError {
                variables: duplicate_variables,
            }
            .to_error(context.stack_trace));
        }

        if variables.contains("config") {
            return Err(
                GenericFailure("`config` is a reserved name for constraint sets".into())
                    .to_error(context.stack_trace),
            );
        }

        let mut captured_values = HashMap::new();
        find_all_captured_variables_in_constraint_set(&source.node, &mut |name| {
            let value = context
                .get_variable_for_closure(
                    [],
                    LocatedStr {
                        location: name.reference.clone(),
                        string: name.node.as_str(),
                    },
                )?
                .clone();

            captured_values.insert(name.node.clone(), value);

            Ok(())
        })?;

        Ok(Self {
            variables: Arc::new(HashableSet::from(HashSet::from_iter(variables))),
            captured_values: Arc::new(HashableMap::from(captured_values)),
            source: source.node.clone(),
        })
    }
}

impl Object for ConstraintSet {
    fn get_type(&self, _context: &ExecutionContext) -> ValueType {
        ValueType::ConstraintSet(self.variables.clone())
    }

    fn format(
        &self,
        _context: &ExecutionContext,
        f: &mut dyn std::fmt::Write,
        _style: Style,
        _precision: Option<u8>,
    ) -> std::fmt::Result {
        write!(f, "<<<")?;

        {
            let mut variables: Vec<_> = self.variables.iter().collect();
            variables.sort();
            let mut variables = variables.iter().peekable();

            while let Some(variable) = variables.next() {
                if variables.peek().is_some() {
                    write!(f, "{variable}, ")?;
                } else {
                    write!(f, "{variable}")?;
                }
            }
        }

        write!(f, ": {} constraints>>>", self.source.constraints.len())?;

        Ok(())
    }

    fn get_attribute(
        &self,
        context: &ExecutionContext,
        attribute: &str,
    ) -> ExpressionResult<Value> {
        match attribute {
            "solve" => Ok(BuiltinFunction::new::<methods::Solve>().into()),
            _ => Err(MissingAttributeError {
                name: attribute.into(),
            }
            .to_error(context.stack_trace)),
        }
    }
}

struct ValueProvider<'l> {
    provided: &'l Dictionary,
    guesses: &'l HashMap<ImString, Scalar>,
    captured: &'l HashableMap<ImString, Value>,
}

impl<'l> ValueProvider<'l> {
    fn get(&self, name: &ImString) -> Option<&Value> {
        self.provided
            .get(name.as_str())
            .or_else(|| self.captured.get(name))
    }
}

impl ConstraintSet {
    fn solve(
        &self,
        context: &ExecutionContext,
        provided: Dictionary,
    ) -> ExpressionResult<Dictionary> {
        // TODO we should permit user configuration, such as precision requirements and initial guesses.
        let guesses: HashMap<_, _> = self
            .variables
            .iter()
            .filter(|name| provided.data.members.contains_key(name.as_str()))
            .map(|name| {
                (
                    name.clone(),
                    Scalar {
                        dimension: todo!(),
                        value: Float::new(0.0).expect("Zero is somehow NaN"),
                    },
                )
            })
            .collect();

        let value_provider = ValueProvider {
            provided: &provided,
            guesses: &guesses,
            captured: &self.captured_values,
        };

        Err(GenericFailure("Solver not implemented".into()).to_error(context.stack_trace))
    }
}

impl StaticTypeName for ConstraintSet {
    fn static_type_name() -> Cow<'static, str> {
        "ConstraintSet".into()
    }
}

#[derive(Debug, Eq, PartialEq)]
struct DuplicateVariablesError {
    pub variables: Vec<ImString>,
}

impl ErrorType for DuplicateVariablesError {}

impl std::fmt::Display for DuplicateVariablesError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Constraint set contains the following duplicate fields: "
        )?;

        let mut variables = self.variables.iter().peekable();

        while let Some(variable) = variables.next() {
            if variables.peek().is_some() {
                write!(f, "{variable}, ")?;
            } else {
                write!(f, "{variable}")?;
            }
        }

        Ok(())
    }
}

mod methods {
    pub struct Solve;
}

pub fn register_methods(database: &mut BuiltinCallableDatabase) {
    struct BuiltFunction {
        signature: Arc<Signature>,
    }

    impl BuiltinCallable for BuiltFunction {
        fn call(
            &self,
            context: &ExecutionContext,
            argument: Dictionary,
        ) -> ExpressionResult<Value> {
            let this = context
                .get_variable(LocatedStr {
                    location: context.stack_trace.bottom().clone(),
                    string: "self",
                })?
                .downcast_ref::<ConstraintSet>(context.stack_trace)?
                .clone();

            let solution = this.solve(context, argument)?;
            Ok(solution.into())
        }

        fn name(&self) -> &str {
            "ConstraintSet::solve"
        }

        fn signature(&self) -> &Arc<Signature> {
            &self.signature
        }
    }

    let callable = BuiltFunction {
        signature: Arc::new(Signature {
            argument_type: StructDefinition {
                members: Arc::new(HashableMap::from(HashMap::new())),
                variadic: true,
            },
            return_type: ValueType::Dictionary(StructDefinition {
                members: Arc::new(HashableMap::new()),
                variadic: true,
            }),
        }),
    };

    database.register::<methods::Solve>(Box::new(callable))
}

// TODO test explicitly passing none for a variable.
// TODO test all 6 constraint types.

#[cfg(test)]
mod test {
    use super::*;
    use crate::{compile::full_compile, execution::test_run, values};

    #[test]
    fn capture_variables() {
        let constraint_set = full_compile("<<<x, y, z, w: a + x :==: b + y, z + c :==: w + d>>>");
        let constraint_set = constraint_set.node.as_constraintset().unwrap();

        let mut captured_variables = HashSet::new();
        find_all_captured_variables_in_constraint_set(
            &constraint_set.node,
            &mut |captured_variable| {
                captured_variables.insert(captured_variable.node.clone());

                Ok(())
            },
        )
        .unwrap();

        assert_eq!(
            captured_variables,
            HashSet::from([
                ImString::from("a"),
                ImString::from("b"),
                ImString::from("c"),
                ImString::from("d")
            ])
        );
    }

    #[test]
    fn formatting() {
        let product = test_run("\"{value}\"::format(value = <<<x, y: x + y :==: 1m, y + x :==: 2m>>>) == \"<<<x, y: 2 constraints>>>\"").unwrap();
        assert_eq!(product, values::Boolean(true).into());
    }
}
