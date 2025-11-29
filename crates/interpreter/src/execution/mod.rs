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
mod standard_environment;
pub mod values;
use errors::{ErrorType, ExpressionResult};
use logging::{LocatedStr, RuntimeLog, StackScope};
use stack::Stack;
use values::{Object, StoredValue, Value, ValueType};

fn find_value<'p, 's>(
    log: &mut dyn RuntimeLog,
    stack_trace: &[SourceReference],
    stack: &'s mut Stack,
    path_iter: impl IntoIterator<Item = &'p compile::AstNode<String>>,
) -> ExpressionResult<&'s mut StoredValue> {
    let mut path_iter = path_iter.into_iter().peekable();
    let root = path_iter.next().expect("Path is empty");

    let stack_value = stack.get_variable_mut(
        stack_trace,
        LocatedStr {
            location: root.reference.clone(),
            string: &root.node,
        },
    )?;

    if let Some(sub_path) = path_iter.next() {
        // We need the value off the heap.

        let mut value = stack_value.access_mut(stack_trace)?.get_attribute_mut(
            log,
            stack_trace,
            &LocatedStr {
                location: sub_path.reference.clone(),
                string: &sub_path.node,
            },
        )?;

        // Follow the chain of elements to evaluate the whole path, up to the last element.
        while let Some(sub_path) = path_iter.next() {
            if path_iter.peek().is_none() {
                // That's the last element of the path. We break out early because the
                // last one needs to be a mutable borrow.

                let final_value = value.access_mut(stack_trace)?.get_attribute_mut(
                    log,
                    stack_trace,
                    &LocatedStr {
                        location: sub_path.reference.clone(),
                        string: &sub_path.node,
                    },
                )?;

                return Ok(final_value);
            } else {
                value = value.access_mut(stack_trace)?.get_attribute_mut(
                    log,
                    stack_trace,
                    &LocatedStr {
                        location: sub_path.reference.clone(),
                        string: &sub_path.node,
                    },
                )?;
            }
        }

        Ok(value)
    } else {
        // We just needed the value off the stack.
        Ok(stack_value)
    }
}

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
            compile::Expression::ClosureDefinition(ast_node) => {
                Ok(values::UserClosure::from_ast(log, stack_trace, stack, ast_node)?.into())
            }
            compile::Expression::Default(_ast_node) => Ok(values::DefaultValue.into()),
            compile::Expression::DictionaryConstruction(ast_node) => {
                Ok(values::Dictionary::from_ast(log, stack_trace, stack, ast_node)?.into())
            }
            compile::Expression::If(ast_node) => todo!(),
            compile::Expression::List(ast_node) => todo!(),
            compile::Expression::Parenthesis(ast_node) => {
                execute_expression(log, stack_trace, stack, &ast_node)
            }
            compile::Expression::Path(ast_node) => {
                let path_iter = ast_node.node.path.iter();
                Ok(find_value(log, stack_trace, stack, path_iter)?
                    .take(stack_trace)?
                    .into())
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
            compile::Expression::StructDefinition(ast_node) => Ok(ValueType::from(
                values::StructDefinition::new(log, stack_trace, stack, ast_node)?,
            )
            .into()),
            compile::Expression::UnaryExpression(ast_node) => {
                execute_unary_expression(log, stack_trace, stack, ast_node)
            }
            compile::Expression::UnsignedInteger(ast_node) => {
                Ok(values::UnsignedInteger::from(ast_node.node).into())
            }
            compile::Expression::FunctionCall(ast_node) => todo!(),
            compile::Expression::MethodCall(ast_node) => todo!(),
            compile::Expression::LetIn(ast_node) => {
                execute_let_in(log, stack_trace, stack, ast_node)
            }
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

fn execute_let_in(
    log: &mut dyn RuntimeLog,
    stack_trace: &mut Vec<SourceReference>,
    stack: &mut Stack,
    expression: &compile::AstNode<Box<compile::LetIn>>,
) -> ExpressionResult<Value> {
    stack_trace.stack_scope(expression.reference.clone(), |stack_trace| {
        for assignment in expression.node.assignments.iter() {
            let value = execute_expression(log, stack_trace, stack, &assignment.node.value)?;
            stack.insert_value(assignment.node.ident.node.clone(), value);
        }

        let node = &expression.node;
        execute_expression(log, stack_trace, stack, &node.expression)
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
                    value_a.cmp(log, stack_trace, value_b)?,
                    Ordering::Equal
                ))
                .into()),
                BinaryExpressionOperation::And => value_a.bit_and(log, stack_trace, value_b),
                BinaryExpressionOperation::AndAnd => value_a.and(log, stack_trace, value_b),
                BinaryExpressionOperation::Mul => value_a.multiply(log, stack_trace, value_b),
                BinaryExpressionOperation::MulMul => value_a.exponent(log, stack_trace, value_b),
                BinaryExpressionOperation::Add => value_a.addition(log, stack_trace, value_b),
                BinaryExpressionOperation::Sub => value_a.subtraction(log, stack_trace, value_b),
                BinaryExpressionOperation::DotDot => todo!(),
                BinaryExpressionOperation::DotDotEq => todo!(),
                BinaryExpressionOperation::Div => value_a.divide(log, stack_trace, value_b),
                BinaryExpressionOperation::DivDiv => {
                    value_a.floor_divide(log, stack_trace, value_b)
                }
                BinaryExpressionOperation::Lt => Ok(values::Boolean(matches!(
                    value_a.cmp(log, stack_trace, value_b)?,
                    Ordering::Less
                ))
                .into()),
                BinaryExpressionOperation::LtLt => value_a.left_shift(log, stack_trace, value_b),
                BinaryExpressionOperation::LtEq => Ok(values::Boolean(matches!(
                    value_a.cmp(log, stack_trace, value_b)?,
                    Ordering::Less | Ordering::Equal
                ))
                .into()),
                BinaryExpressionOperation::EqEq => Ok(values::Boolean(matches!(
                    value_a.cmp(log, stack_trace, value_b)?,
                    Ordering::Equal
                ))
                .into()),
                BinaryExpressionOperation::Gt => Ok(values::Boolean(matches!(
                    value_a.cmp(log, stack_trace, value_b)?,
                    Ordering::Greater
                ))
                .into()),
                BinaryExpressionOperation::GtEq => Ok(values::Boolean(matches!(
                    value_a.cmp(log, stack_trace, value_b)?,
                    Ordering::Equal | Ordering::Greater
                ))
                .into()),
                BinaryExpressionOperation::GtGt => value_a.right_shift(log, stack_trace, value_b),
                BinaryExpressionOperation::BitXor => value_a.bit_xor(log, stack_trace, value_b),
                BinaryExpressionOperation::Or => value_a.bit_or(log, stack_trace, value_b),
                BinaryExpressionOperation::OrOr => value_a.or(log, stack_trace, value_b),
            }
        },
    )
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

#[derive(Debug, Eq, PartialEq)]
struct AssignmentTypeMissmatch {
    expected: ValueType,
    got: ValueType,
}

impl ErrorType for AssignmentTypeMissmatch {}

impl Display for AssignmentTypeMissmatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "When assigning a new value to an already defined variable, you cannot change the variable's type. This variable's type was `{}`, but you tried to assign it a value with a `{}` typing.", self.expected, self.got
        )
    }
}

#[cfg(test)]
pub(crate) fn test_run(input: &str) -> ExpressionResult<Value> {
    use standard_environment::build_prelude;

    let root = compile::full_compile(input);
    let prelude = build_prelude();
    let mut stack = Stack::new(prelude);

    execute_expression(&mut Vec::new(), &mut Vec::new(), &mut stack, &root)
}

#[cfg(test)]
mod test {
    use std::sync::Arc;

    use super::*;

    #[test]
    fn default_type() {
        let product = test_run("default").unwrap();
        assert_eq!(product, values::DefaultValue.into());
    }

    #[test]
    fn boolean_type() {
        let product = test_run("true").unwrap();
        assert_eq!(product, values::Boolean(true).into());
    }

    #[test]
    fn signed_integer_type() {
        let product = test_run("5i").unwrap();
        assert_eq!(product, values::SignedInteger::from(5).into());
    }

    #[test]
    fn unsigned_integer_type() {
        let product = test_run("5u").unwrap();
        assert_eq!(product, values::UnsignedInteger::from(5).into());
    }

    #[test]
    fn parenthesis() {
        // Fails because of a type mismatch.
        let product = test_run("(1i + 2i) * 3i").unwrap();
        assert_eq!(product, values::SignedInteger::from(9).into());
    }

    #[test]
    fn struct_definition() {
        let product = test_run("(name: std.types.None = std.consts.None, ...)").unwrap();
        assert_eq!(
            product,
            values::ValueType::Dictionary(values::StructDefinition {
                members: Arc::new(vec![values::StructMember {
                    name: "name".into(),
                    ty: ValueType::TypeNone,
                    default: Some(Value::ValueNone(values::ValueNone))
                }]),
                variadic: true
            })
            .into()
        );
    }

    #[test]
    fn nested_value_access() {
        let product = test_run("let dictionary = (a = (b = 23u)); in dictionary.a.b").unwrap();
        assert_eq!(product, values::UnsignedInteger::from(23).into());
    }

    #[test]
    fn let_in() {
        let product = test_run("let value = 23u; in value").unwrap();
        assert_eq!(product, values::UnsignedInteger::from(23).into());
    }
}
