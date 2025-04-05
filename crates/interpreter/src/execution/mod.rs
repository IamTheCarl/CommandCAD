use crate::compile;

mod errors;
mod formatting;
mod logging;
mod stack;
pub mod values;
use errors::ExpressionResult;
use values::Value;

/// Caches the products of expressions.
pub struct Cache {}

pub type CacheSignature = [u8; 32];

pub struct CachedExpression {}

pub struct RuntimeContext {}

pub fn execute_expression(
    expression: &compile::AstNode<compile::Expression>,
) -> ExpressionResult<Value> {
    match &expression.node {
        compile::Expression::BinaryExpression(ast_node) => todo!(),
        compile::Expression::Boolean(ast_node) => todo!(),
        compile::Expression::ClosureDefinition(ast_node) => todo!(),
        compile::Expression::Default(_ast_node) => Ok(values::DefaultValue.into()),
        compile::Expression::DictionaryConstruction(ast_node) => todo!(),
        compile::Expression::If(ast_node) => todo!(),
        compile::Expression::List(ast_node) => todo!(),
        compile::Expression::Parenthesis(ast_node) => todo!(),
        compile::Expression::Path(ast_node) => todo!(),
        compile::Expression::ProceduralBlock(ast_node) => todo!(),
        compile::Expression::Scalar(ast_node) => todo!(),
        compile::Expression::SignedInteger(ast_node) => todo!(),
        compile::Expression::String(ast_node) => todo!(),
        compile::Expression::StructDefinition(ast_node) => todo!(),
        compile::Expression::UnaryExpression(ast_node) => todo!(),
        compile::Expression::UnsignedInteger(ast_node) => todo!(),
        compile::Expression::Void(_ast_node) => Ok(values::Void.into()),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn none_type() {
        let root = compile::full_compile("test_file.ccm", "()");
        let product = execute_expression(&root).unwrap();
        assert_eq!(product, values::Void.into());
    }

    #[test]
    fn default_type() {
        let root = compile::full_compile("test_file.ccm", "default");
        let product = execute_expression(&root).unwrap();
        assert_eq!(product, values::DefaultValue.into());
    }
}
