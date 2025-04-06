use std::cmp::Ordering;

use crate::compile::{self, BinaryExpressionOperation, SourceReference, UnaryExpressionOperation};

mod errors;
mod formatting;
mod logging;
mod stack;
pub mod values;
use errors::ExpressionResult;
use logging::{RuntimeLog, StackScope};
use values::{Object, Value};

/// Caches the products of expressions.
pub struct Cache {}

pub type CacheSignature = [u8; 32];

pub struct CachedExpression {}

pub struct RuntimeContext {}

pub fn execute_expression(
    log: &mut dyn RuntimeLog,
    stack_trace: &mut Vec<SourceReference>,
    expression: &compile::AstNode<compile::Expression>,
) -> ExpressionResult<Value> {
    stack_trace.stack_scope(
        expression.reference.clone(),
        |stack_trace| match &expression.node {
            compile::Expression::BinaryExpression(ast_node) => {
                execute_binary_expression(log, stack_trace, ast_node)
            }
            compile::Expression::Boolean(ast_node) => Ok(values::Boolean(ast_node.node).into()),
            compile::Expression::ClosureDefinition(ast_node) => todo!(),
            compile::Expression::Default(_ast_node) => Ok(values::DefaultValue.into()),
            compile::Expression::DictionaryConstruction(ast_node) => todo!(),
            compile::Expression::If(ast_node) => todo!(),
            compile::Expression::List(ast_node) => todo!(),
            compile::Expression::Parenthesis(ast_node) => todo!(),
            compile::Expression::Path(ast_node) => todo!(),
            compile::Expression::ProceduralBlock(ast_node) => todo!(),
            compile::Expression::Scalar(ast_node) => todo!(),
            compile::Expression::SignedInteger(ast_node) => {
                Ok(values::SignedInteger::from(ast_node.node).into())
            }
            compile::Expression::String(ast_node) => todo!(),
            compile::Expression::StructDefinition(ast_node) => todo!(),
            compile::Expression::UnaryExpression(ast_node) => {
                execute_unary_expression(log, stack_trace, ast_node)
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
    expression: &compile::AstNode<Box<compile::UnaryExpression>>,
) -> ExpressionResult<Value> {
    stack_trace.stack_scope(expression.reference.clone(), |stack_trace| {
        let node = &expression.node;
        let value = execute_expression(log, stack_trace, &node.expression)?;
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
    expression: &compile::AstNode<Box<compile::BinaryExpression>>,
) -> ExpressionResult<Value> {
    stack_trace.stack_scope(
        expression.reference.clone(),
        |stack_trace: &mut Vec<SourceReference>| {
            let node = &expression.node;
            let value_a = execute_expression(log, stack_trace, &node.a)?;
            let value_b = execute_expression(log, stack_trace, &node.b)?;
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn none_type() {
        let root = compile::full_compile("test_file.ccm", "()");
        let product = execute_expression(&mut Vec::new(), &mut Vec::new(), &root).unwrap();
        assert_eq!(product, values::Void.into());
    }

    #[test]
    fn default_type() {
        let root = compile::full_compile("test_file.ccm", "default");
        let product = execute_expression(&mut Vec::new(), &mut Vec::new(), &root).unwrap();
        assert_eq!(product, values::DefaultValue.into());
    }

    #[test]
    fn boolean_type() {
        let root = compile::full_compile("test_file.ccm", "true");
        let product = execute_expression(&mut Vec::new(), &mut Vec::new(), &root).unwrap();
        assert_eq!(product, values::Boolean(true).into());
    }

    #[test]
    fn signed_integer_type() {
        let root = compile::full_compile("test_file.ccm", "5i");
        let product = execute_expression(&mut Vec::new(), &mut Vec::new(), &root).unwrap();
        assert_eq!(product, values::SignedInteger::from(5).into());
    }

    #[test]
    fn unsigned_integer_type() {
        let root = compile::full_compile("test_file.ccm", "5u");
        let product = execute_expression(&mut Vec::new(), &mut Vec::new(), &root).unwrap();
        assert_eq!(product, values::UnsignedInteger::from(5).into());
    }
}
