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
use selen::api::{add, div, eq, float, ge, gt, le, lt, mul, ne, sub, ExprBuilder, Model, VarId};

use crate::{
    compile::{
        constraint_set::{
            BinaryExpressionOperation, ConstraintSet as AstConstraintSet, ConstraintSetExpression,
            Relation, UnaryExpressionOperation,
        },
        AstNode,
    },
    execution::{
        errors::{ExecutionResult, GenericFailure, Raise},
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
    access_collector: &mut dyn FnMut(&AstNode<ImString>) -> ExecutionResult<()>,
) -> ExecutionResult<()> {
    use crate::compile::constraint_set;

    fn search_expression(
        variables: &Vec<AstNode<ImString>>,
        expression: &constraint_set::ConstraintSetExpression,
        access_collector: &mut dyn FnMut(&AstNode<ImString>) -> ExecutionResult<()>,
    ) -> ExecutionResult<()> {
        match expression {
            ConstraintSetExpression::Parenthesis(expression) => {
                search_expression(variables, &expression.node, access_collector)
            }
            ConstraintSetExpression::Identifier(ast_node) => {
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
            ConstraintSetExpression::UnaryExpression(ast_node) => {
                search_expression(variables, &ast_node.node.expression.node, access_collector)
            }
            ConstraintSetExpression::BinaryExpression(ast_node) => {
                search_expression(variables, &ast_node.node.a.node, access_collector)?;
                search_expression(variables, &ast_node.node.b.node, access_collector)?;
                Ok(())
            }
            ConstraintSetExpression::MethodCall(ast_node) => {
                search_expression(
                    variables,
                    &ast_node.node.self_dictionary.node,
                    access_collector,
                )?;
                search_expression(variables, &ast_node.node.argument.node, access_collector)?;

                Ok(())
            }
            ConstraintSetExpression::Scalar(_) => Ok(()),
        }
    }

    search_expression(
        &constraint_set.variables,
        &constraint_set.left.node,
        access_collector,
    )?;
    search_expression(
        &constraint_set.variables,
        &constraint_set.right.node,
        access_collector,
    )?;

    Ok(())
}

fn display_expression(
    context: &ExecutionContext,
    captured_values: &HashableMap<ImString, Value>,
    expression: &ConstraintSetExpression,
    f: &mut dyn std::fmt::Write,
    style: Style,
    precision: Option<u8>,
) -> std::fmt::Result {
    match expression {
        ConstraintSetExpression::Parenthesis(ast_node) => {
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
        ConstraintSetExpression::Scalar(ast_node) => values::Scalar {
            dimension: ast_node.node.dimension,
            value: ast_node.node.value,
        }
        .format(context, f, style, precision),
        ConstraintSetExpression::Identifier(ast_node) => {
            if let Some(value) = captured_values.get(&ast_node.node) {
                value.format(context, f, style, precision)
            } else {
                write!(f, "{}", ast_node.node)
            }
        }
        ConstraintSetExpression::UnaryExpression(ast_node) => {
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
        ConstraintSetExpression::BinaryExpression(ast_node) => {
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
        ConstraintSetExpression::MethodCall(ast_node) => {
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
    ) -> ExecutionResult<Self> {
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
        write!(f, "<<<")?;

        let mut variables: Vec<_> = self.variables.iter().collect();
        variables.sort_unstable();

        let mut variables = self.variables.iter().peekable();

        while let Some(variable) = variables.next() {
            if variables.peek().is_some() {
                write!(f, "{variable}, ")?;
            } else {
                write!(f, "{variable}")?;
            }
        }

        write!(f, ": ")?;

        display_expression(
            context,
            &self.captured_values,
            &self.source.left.node,
            f,
            style,
            precision,
        )?;

        let relation = match self.source.relation {
            Relation::Less => "<",
            Relation::LessEqual => "<=",
            Relation::Equal => "==",
            Relation::GreaterEqual => ">=",
            Relation::Greater => ">",
            Relation::NotEqual => "!=",
        };
        write!(f, " {} ", relation)?;

        display_expression(
            context,
            &self.captured_values,
            &self.source.right.node,
            f,
            style,
            precision,
        )?;

        write!(f, ">>>")?;

        Ok(())
    }

    fn get_attribute(&self, context: &ExecutionContext, attribute: &str) -> ExecutionResult<Value> {
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
    ) -> ExecutionResult<Dictionary> {
        let value_provider = ValueProvider {
            provided: &provided,
            captured: &self.captured_values,
        };

        let mut m = Model::default();
        let mut dimension = None;

        let mut variables = HashMap::new();

        let left = Self::model_expression(
            context,
            &value_provider,
            &mut m,
            &mut variables,
            &mut dimension,
            &self.source.left.node,
        )?;
        let right = Self::model_expression(
            context,
            &value_provider,
            &mut m,
            &mut variables,
            &mut dimension,
            &self.source.right.node,
        )?;

        Self::build_relation(&mut m, &self.source.relation, left, right);
        let solution = m
            .solve()
            .map_err(|error| SolverError(error).to_error(context.stack_trace))?;

        let dimension = dimension.ok_or_else(|| {
            GenericFailure("Could not determine dimension of constraint set".into())
                .to_error(context.stack_trace)
        })?;

        let mut members = HashMap::new();
        for (variable_name, variable_id) in variables {
            // Values that do not get solved are our inputs.
            if let Some(value) = solution.as_float(variable_id) {
                members.insert(
                    variable_name,
                    Scalar {
                        dimension,
                        value: Float::new(value).unwrap_not_nan(context.stack_trace)?,
                    }
                    .into(),
                );
            }
        }

        // Include the provided values.
        for (name, value) in provided.data.members.iter() {
            members.entry(name.clone()).or_insert(value.clone());
        }

        Ok(Dictionary::new(context, members))
    }

    fn build_relation(
        m: &mut Model,
        relation: &Relation,
        left: impl Into<ExprBuilder>,
        right: impl Into<ExprBuilder>,
    ) {
        match relation {
            Relation::Less => lt(m, left, right),
            Relation::LessEqual => le(m, left, right),
            Relation::Equal => eq(m, left, right),
            Relation::GreaterEqual => ge(m, left, right),
            Relation::Greater => gt(m, left, right),
            Relation::NotEqual => ne(m, left, right),
        }
    }

    fn model_expression(
        context: &ExecutionContext,
        value_provider: &ValueProvider<'_>,
        m: &mut Model,
        variables: &mut HashMap<ImString, VarId>,
        dimension: &mut Option<Dimension>,
        expression: &ConstraintSetExpression,
    ) -> ExecutionResult<ExprBuilder> {
        match expression {
            ConstraintSetExpression::Parenthesis(ast_node) => Self::model_expression(
                context,
                value_provider,
                m,
                variables,
                dimension,
                &ast_node.node,
            ),
            ConstraintSetExpression::Scalar(ast_node) => {
                let value = Scalar {
                    dimension: ast_node.node.dimension,
                    value: ast_node.node.value,
                };

                context.trace_scope(ast_node.reference.clone(), |context| {
                    Self::build_scalar(context, dimension, value)
                })
            }
            ConstraintSetExpression::Identifier(ast_node) => {
                context.trace_scope(ast_node.reference.clone(), |context| {
                    let name = &ast_node.node;
                    if let Some(value) = value_provider
                        .get(name)
                        .filter(|value| !matches!(value, Value::ValueNone(_)))
                    {
                        match value {
                            Value::Scalar(scalar) => {
                                Self::build_scalar(context, dimension, *scalar)
                            }
                            value => Err(GenericFailure(
                                format!(
                                    "{} types are not supported in constraint sets",
                                    value.type_name()
                                )
                                .into(),
                            )
                            .to_error(context.stack_trace)),
                        }
                    } else {
                        let variable = variables
                            .entry(name.clone())
                            .or_insert_with(|| m.float(f64::MIN, f64::MAX));
                        Ok(ExprBuilder::Var(*variable))
                    }
                })
            }
            ConstraintSetExpression::UnaryExpression(ast_node) => {
                let expression = Self::model_expression(
                    context,
                    value_provider,
                    m,
                    variables,
                    dimension,
                    &ast_node.node.expression.node,
                )?;

                match ast_node.node.operation.node {
                    UnaryExpressionOperation::Add => Ok(expression),
                    UnaryExpressionOperation::Sub => Ok(expression.mul(-1)),
                }
            }
            ConstraintSetExpression::BinaryExpression(ast_node) => {
                let a = Self::model_expression(
                    context,
                    value_provider,
                    m,
                    variables,
                    dimension,
                    &ast_node.node.a.node,
                )?;
                let b = Self::model_expression(
                    context,
                    value_provider,
                    m,
                    variables,
                    dimension,
                    &ast_node.node.b.node,
                )?;

                match ast_node.node.operation.node {
                    BinaryExpressionOperation::Mul => Ok(mul(a, b)),
                    BinaryExpressionOperation::Add => Ok(add(a, b)),
                    BinaryExpressionOperation::Sub => Ok(sub(a, b)),
                    BinaryExpressionOperation::Div => Ok(div(a, b)),
                }
            }
            ConstraintSetExpression::MethodCall(ast_node) => {
                context.trace_scope(ast_node.reference.clone(), |context| {
                    Err(
                        GenericFailure("Methods are not yet supported in constraint sets".into())
                            .to_error(context.stack_trace),
                    )
                })
            }
        }
    }

    fn build_scalar(
        context: &ExecutionContext,
        dimension: &mut Option<Dimension>,
        value: Scalar,
    ) -> ExecutionResult<ExprBuilder> {
        if let Some(dimension) = dimension {
            if value.dimension != Dimension::zero() && *dimension != value.dimension {
                return Err(GenericFailure(
                    "All measurements in constraint set must be of the same dimension".into(),
                )
                .to_error(context.stack_trace));
            }
        } else if value.dimension != Dimension::zero() {
            *dimension = Some(value.dimension);
        }

        Ok(ExprBuilder::Val(float(*value.value)))
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

impl std::error::Error for DuplicateVariablesError {}

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

#[derive(Debug)]
struct SolverError(selen::api::SolverError);

impl std::error::Error for SolverError {}

impl std::fmt::Display for SolverError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
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
        fn call(&self, context: &ExecutionContext, argument: Dictionary) -> ExecutionResult<Value> {
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

// TODO test variable capturing.
// TODO test formatting
// TODO test explicitly passing none.
// TODO test all 6 constraint types.
