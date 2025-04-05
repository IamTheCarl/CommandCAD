use crate::compile::Expression;

mod errors;
mod formatting;
mod logging;
pub mod values;

/// Caches the products of expressions.
pub struct Cache {}

pub type CacheSignature = [u8; 32];

pub struct CachedExpression {}

pub struct RuntimeContext {}

pub fn execute_expression(expression: &Expression) {}
