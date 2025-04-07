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

use std::{cmp::Ordering, fmt::Display};

use crate::compile::{self, BinaryExpressionOperation, SourceReference, UnaryExpressionOperation};

mod errors;
mod formatting;
mod logging;
mod stack;
pub mod values;
use errors::{ErrorType, ExpressionResult, Raise};
use logging::{RuntimeLog, StackScope};
use stack::{ScopeType, Stack};
use values::{Object, Value, ValueType};

/// Caches the products of expressions.
pub struct Cache {}

pub type CacheSignature = [u8; 32];

pub struct CachedExpression {}

pub struct RuntimeContext {}

pub fn execute_expression(
    log: &mut dyn RuntimeLog,
    stack_trace: &mut Vec<SourceReference>,
    stack: &mut Stack,
    expression: &compile::AstNode<compile::Expression>,
) -> ExpressionResult<Value> {
    stack_trace.stack_scope(
        expression.reference.clone(),
        |stack_trace| match &expression.node {
            compile::Expression::BinaryExpression(ast_node) => {
                execute_binary_expression(log, stack_trace, stack, ast_node)
            }
            compile::Expression::Boolean(ast_node) => Ok(values::Boolean(ast_node.node).into()),
            compile::Expression::ClosureDefinition(ast_node) => todo!(),
            compile::Expression::Default(_ast_node) => Ok(values::DefaultValue.into()),
            compile::Expression::DictionaryConstruction(ast_node) => todo!(),
            compile::Expression::If(ast_node) => todo!(),
            compile::Expression::List(ast_node) => todo!(),
            compile::Expression::Parenthesis(ast_node) => todo!(),
            compile::Expression::Path(ast_node) => todo!(),
            compile::Expression::ProceduralBlock(ast_node) => {
                execute_procedural_block(log, stack, stack_trace, ast_node)
            }
            compile::Expression::Scalar(ast_node) => Ok(values::Scalar {
                dimension: ast_node.node.dimension,
                value: ast_node.node.value,
            }
            .into()),
            compile::Expression::SignedInteger(ast_node) => {
                Ok(values::SignedInteger::from(ast_node.node).into())
            }
            compile::Expression::String(ast_node) => todo!(),
            compile::Expression::StructDefinition(ast_node) => todo!(),
            compile::Expression::UnaryExpression(ast_node) => {
                execute_unary_expression(log, stack_trace, stack, ast_node)
            }
            compile::Expression::UnsignedInteger(ast_node) => {
                Ok(values::UnsignedInteger::from(ast_node.node).into())
            }
            compile::Expression::Void(_ast_node) => Ok(values::Void.into()),
        },
    )
}

fn execute_unary_expression(
    log: &mut dyn RuntimeLog,
    stack_trace: &mut Vec<SourceReference>,
    stack: &mut Stack,
    expression: &compile::AstNode<Box<compile::UnaryExpression>>,
) -> ExpressionResult<Value> {
    stack_trace.stack_scope(expression.reference.clone(), |stack_trace| {
        let node = &expression.node;
        let value = execute_expression(log, stack_trace, stack, &node.expression)?;
        match node.operation.node {
            UnaryExpressionOperation::Add => value.unary_plus(log, stack_trace),
            UnaryExpressionOperation::Sub => value.unary_minus(log, stack_trace),
            UnaryExpressionOperation::Not => value.unary_not(log, stack_trace),
        }
    })
}

fn execute_binary_expression(
    log: &mut dyn RuntimeLog,
    stack_trace: &mut Vec<SourceReference>,
    stack: &mut Stack,
    expression: &compile::AstNode<Box<compile::BinaryExpression>>,
) -> ExpressionResult<Value> {
    stack_trace.stack_scope(
        expression.reference.clone(),
        |stack_trace: &mut Vec<SourceReference>| {
            let node = &expression.node;
            let value_a = execute_expression(log, stack_trace, stack, &node.a)?;
            let value_b = execute_expression(log, stack_trace, stack, &node.b)?;
            match node.operation.node {
                BinaryExpressionOperation::NotEq => Ok(values::Boolean(!matches!(
                    value_a.cmp(log, stack_trace, &value_b)?,
                    Ordering::Equal
                ))
                .into()),
                BinaryExpressionOperation::And => value_a.bit_and(log, stack_trace, &value_b),
                BinaryExpressionOperation::AndAnd => value_a.and(log, stack_trace, &value_b),
                BinaryExpressionOperation::Mul => value_a.multiply(log, stack_trace, &value_b),
                BinaryExpressionOperation::MulMul => value_a.exponent(log, stack_trace, &value_b),
                BinaryExpressionOperation::Add => value_a.addition(log, stack_trace, &value_b),
                BinaryExpressionOperation::Sub => value_a.subtraction(log, stack_trace, &value_b),
                BinaryExpressionOperation::DotDot => todo!(),
                BinaryExpressionOperation::DotDotEq => todo!(),
                BinaryExpressionOperation::Div => value_a.divide(log, stack_trace, &value_b),
                BinaryExpressionOperation::DivDiv => {
                    value_a.floor_divide(log, stack_trace, &value_b)
                }
                BinaryExpressionOperation::Lt => Ok(values::Boolean(matches!(
                    value_a.cmp(log, stack_trace, &value_b)?,
                    Ordering::Less
                ))
                .into()),
                BinaryExpressionOperation::LtLt => todo!(),
                BinaryExpressionOperation::LtEq => Ok(values::Boolean(matches!(
                    value_a.cmp(log, stack_trace, &value_b)?,
                    Ordering::Less | Ordering::Equal
                ))
                .into()),
                BinaryExpressionOperation::EqEq => Ok(values::Boolean(matches!(
                    value_a.cmp(log, stack_trace, &value_b)?,
                    Ordering::Equal
                ))
                .into()),
                BinaryExpressionOperation::Gt => Ok(values::Boolean(matches!(
                    value_a.cmp(log, stack_trace, &value_b)?,
                    Ordering::Greater
                ))
                .into()),
                BinaryExpressionOperation::GtEq => Ok(values::Boolean(matches!(
                    value_a.cmp(log, stack_trace, &value_b)?,
                    Ordering::Equal | Ordering::Greater
                ))
                .into()),
                BinaryExpressionOperation::GtGt => todo!(),
                BinaryExpressionOperation::BitXor => value_a.bit_xor(log, stack_trace, &value_b),
                BinaryExpressionOperation::Or => value_a.bit_or(log, stack_trace, &value_b),
                BinaryExpressionOperation::OrOr => value_a.or(log, stack_trace, &value_b),
            }
        },
    )
}

fn execute_procedural_block(
    log: &mut dyn RuntimeLog,
    stack: &mut Stack,
    stack_trace: &mut Vec<SourceReference>,
    block: &compile::AstNode<compile::ProceduralBlock>,
) -> ExpressionResult<Value> {
    stack_trace.stack_scope(block.reference.clone(), |stack_trace| {
        stack.scope(
            [],
            stack_trace,
            ScopeType::Inherited,
            |stack, stack_trace| {
                let mut last_value = Value::Void(values::Void);

                let mut statements = block.node.statements.iter().peekable();

                while let Some(statement) = statements.next() {
                    last_value = execute_statement(log, stack, stack_trace, statement)?;

                    if statements.peek().is_some() {
                        // This was not the last statement, which means it needs to produce a void
                        // value.

                        match last_value {
                            Value::Void(_) => {
                                // Not a problem.
                                continue;
                            }
                            _ => {
                                return Err(stack_trace.stack_scope(
                                    statement.reference.clone(),
                                    |stack_trace| {
                                        MissingSemicolon {
                                            actual_value_type: last_value.get_type(),
                                        }
                                        .to_error(stack_trace.iter())
                                    },
                                ));
                            }
                        }
                    }
                }

                Ok(last_value)
            },
        )?
    })
}

#[derive(Debug, Eq, PartialEq)]
struct MissingSemicolon {
    actual_value_type: ValueType,
}

impl ErrorType for MissingSemicolon {}

impl Display for MissingSemicolon {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Expected void type, found `{}`. Are you missing a semicolon?",
            self.actual_value_type
        )
    }
}

fn execute_statement(
    log: &mut dyn RuntimeLog,
    stack: &mut Stack,
    stack_trace: &mut Vec<SourceReference>,
    statement: &compile::AstNode<compile::Statement>,
) -> ExpressionResult<Value> {
    match &statement.node {
        compile::Statement::Assign(ast_node) => todo!(),
        compile::Statement::Let(ast_node) => todo!(),
        compile::Statement::For(ast_node) => todo!(),
        compile::Statement::Expression(ast_node) => todo!(),
        compile::Statement::ClosedExpression(ast_node) => todo!(),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn none_type() {
        let root = compile::full_compile("test_file.ccm", "()");
        let product = execute_expression(
            &mut Vec::new(),
            &mut Vec::new(),
            &mut Stack::default(),
            &root,
        )
        .unwrap();
        assert_eq!(product, values::Void.into());
    }

    #[test]
    fn default_type() {
        let root = compile::full_compile("test_file.ccm", "default");
        let product = execute_expression(
            &mut Vec::new(),
            &mut Vec::new(),
            &mut Stack::default(),
            &root,
        )
        .unwrap();
        assert_eq!(product, values::DefaultValue.into());
    }

    #[test]
    fn boolean_type() {
        let root = compile::full_compile("test_file.ccm", "true");
        let product = execute_expression(
            &mut Vec::new(),
            &mut Vec::new(),
            &mut Stack::default(),
            &root,
        )
        .unwrap();
        assert_eq!(product, values::Boolean(true).into());
    }

    #[test]
    fn signed_integer_type() {
        let root = compile::full_compile("test_file.ccm", "5i");
        let product = execute_expression(
            &mut Vec::new(),
            &mut Vec::new(),
            &mut Stack::default(),
            &root,
        )
        .unwrap();
        assert_eq!(product, values::SignedInteger::from(5).into());
    }

    #[test]
    fn unsigned_integer_type() {
        let root = compile::full_compile("test_file.ccm", "5u");
        let product = execute_expression(
            &mut Vec::new(),
            &mut Vec::new(),
            &mut Stack::default(),
            &root,
        )
        .unwrap();
        assert_eq!(product, values::UnsignedInteger::from(5).into());
    }

    #[test]
    fn empty_block() {
        let root = compile::full_compile("test_file.ccm", "{}");
        let product = execute_expression(
            &mut Vec::new(),
            &mut Vec::new(),
            &mut Stack::default(),
            &root,
        )
        .unwrap();
        assert_eq!(product, values::Void.into());
    }
}
