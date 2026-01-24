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

use common_data_types::{Dimension, Float};
use hashable_map::{HashableMap, HashableSet};
use imstr::ImString;

use crate::{
    compile::{
        constraint_set::{
            BinaryExpressionOperation, ConstraintExpression, ConstraintSet as AstConstraintSet,
            Relation, UnaryExpressionOperation,
        },
        AstNode,
    },
    execution::{
        errors::{ErrorType, ExpressionResult, GenericFailure, Raise},
        logging::LocatedStr,
    },
    values::{
        self,
        closure::{BuiltinCallable, Signature},
        scalar::UnwrapNotNan,
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
    use crate::compile::constraint_set;

    fn search_expression(
        variables: &Vec<AstNode<ImString>>,
        expression: &constraint_set::ConstraintExpression,
        access_collector: &mut dyn FnMut(&AstNode<ImString>) -> ExpressionResult<()>,
    ) -> ExpressionResult<()> {
        match expression {
            ConstraintExpression::Parenthesis(expression) => {
                search_expression(variables, &expression.node, access_collector)
            }
            ConstraintExpression::Identifier(ast_node) => {
                if !variables
                    .iter()
                    .any(|variable| variable.node == ast_node.node)
                {
                    access_collector(ast_node)
                } else {
                    // Is not a captured variable.
                    Ok(())
                }
            }
            ConstraintExpression::UnaryExpression(ast_node) => {
                search_expression(variables, &ast_node.node.expression.node, access_collector)
            }
            ConstraintExpression::BinaryExpression(ast_node) => {
                search_expression(variables, &ast_node.node.a.node, access_collector)?;
                search_expression(variables, &ast_node.node.b.node, access_collector)?;
                Ok(())
            }
            ConstraintExpression::MethodCall(ast_node) => {
                search_expression(
                    variables,
                    &ast_node.node.self_dictionary.node,
                    access_collector,
                )?;
                search_expression(variables, &ast_node.node.argument.node, access_collector)?;

                Ok(())
            }
            ConstraintExpression::Scalar(_) => Ok(()),
        }
    }

    for constraint in constraint_set.constraints.iter() {
        search_expression(
            &constraint_set.variables,
            &constraint.node.left.node,
            access_collector,
        )?;
        search_expression(
            &constraint_set.variables,
            &constraint.node.right.node,
            access_collector,
        )?;
    }

    Ok(())
}

fn display_expression(
    context: &ExecutionContext,
    captured_values: &HashableMap<ImString, Value>,
    expression: &ConstraintExpression,
    f: &mut dyn std::fmt::Write,
    style: Style,
    precision: Option<u8>,
) -> std::fmt::Result {
    match expression {
        ConstraintExpression::Parenthesis(ast_node) => {
            write!(f, "(")?;
            display_expression(
                context,
                captured_values,
                &ast_node.node,
                f,
                style,
                precision,
            )?;
            write!(f, ")")?;

            Ok(())
        }
        ConstraintExpression::Scalar(ast_node) => values::Scalar {
            dimension: ast_node.node.dimension,
            value: ast_node.node.value,
        }
        .format(context, f, style, precision),
        ConstraintExpression::Identifier(ast_node) => {
            if let Some(value) = captured_values.get(&ast_node.node) {
                value.format(context, f, style, precision)
            } else {
                write!(f, "{}", ast_node.node)
            }
        }
        ConstraintExpression::UnaryExpression(ast_node) => {
            let operation = match ast_node.node.operation.node {
                UnaryExpressionOperation::Add => "+",
                UnaryExpressionOperation::Sub => "-",
            };

            write!(f, "{operation}")?;
            display_expression(
                context,
                captured_values,
                &ast_node.node.expression.node,
                f,
                style,
                precision,
            )
        }
        ConstraintExpression::BinaryExpression(ast_node) => {
            let operation = match ast_node.node.operation.node {
                BinaryExpressionOperation::Mul => "*",
                BinaryExpressionOperation::Add => "+",
                BinaryExpressionOperation::Sub => "-",
                BinaryExpressionOperation::Div => "/",
            };

            display_expression(
                context,
                captured_values,
                &ast_node.node.a.node,
                f,
                style,
                precision,
            )?;
            write!(f, " {operation} ")?;
            display_expression(
                context,
                captured_values,
                &ast_node.node.b.node,
                f,
                style,
                precision,
            )
        }
        ConstraintExpression::MethodCall(ast_node) => {
            display_expression(
                context,
                captured_values,
                &ast_node.node.self_dictionary.node,
                f,
                style,
                precision,
            )?;
            write!(f, "::")?;
            display_expression(
                context,
                captured_values,
                &ast_node.node.argument.node,
                f,
                style,
                precision,
            )
        }
    }
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
        context: &ExecutionContext,
        f: &mut dyn std::fmt::Write,
        style: Style,
        precision: Option<u8>,
    ) -> std::fmt::Result {
        todo!()
        // write!(f, "<<<")?;

        // {
        //     let mut variables: Vec<_> = self.variables.iter().collect();
        //     variables.sort_unstable();
        //     let mut variables = self.variables.iter().peekable();

        //     while let Some(variable) = variables.next() {
        //         if variables.peek().is_some() {
        //             write!(f, "{variable}, ")?;
        //         } else {
        //             write!(f, "{variable}")?;
        //         }
        //     }
        // }

        // write!(f, ": ")?;

        // {
        //     let mut constraints = self.constraints.iter().peekable();

        //     while let Some(constraint) = constraints.next() {
        //         display_expression(
        //             context,
        //             &self.captured_values,
        //             &self.constraint.left.node,
        //             f,
        //             style,
        //             precision,
        //         )?;

        //         let relation = match self.source.relation {
        //             Relation::Less => "<",
        //             Relation::LessEqual => "<=",
        //             Relation::Equal => "==",
        //             Relation::GreaterEqual => ">=",
        //             Relation::Greater => ">",
        //             Relation::NotEqual => "!=",
        //         };
        //         write!(f, " {} ", relation)?;

        //         display_expression(
        //             context,
        //             &self.captured_values,
        //             &self.source.right.node,
        //             f,
        //             style,
        //             precision,
        //         )?;
        //     }
        // }

        // write!(f, ">>>")?;

        // Ok(())
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
        let value_provider = ValueProvider {
            provided: &provided,
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

// TODO test formatting
// TODO test explicitly passing none for a variable.
// TODO test all 6 constraint types.

#[cfg(test)]
mod test {
    use super::*;
    use crate::compile::full_compile;

    #[test]
    fn capture_variables() {
        let constraint_set = full_compile("<<<x, y, z, w: a + x == b + y, z + c == w + d>>>");
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
}
