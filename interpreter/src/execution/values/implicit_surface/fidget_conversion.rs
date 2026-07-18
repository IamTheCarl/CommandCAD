use std::collections::HashMap;

use common_data_types::Dimension;
use fidget::context::Tree;
use indexmap::IndexMap;
use thiserror::Error;

use enum_downcast::EnumDowncast;

use crate::{
    compile::{AstNode, Expression, MethodCall},
    execution::{
        errors::{ExecutionResult, Raise as _},
        values::{closure::UserClosure, dictionary::ArgumentName, Object, Scalar, Value},
        ExecutionContext,
    },
};

/// Dimensionality of the closure parameter for implicit surface evaluation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamDim {
    Vec2,
    Vec3,
}

/// A captured closure with its AST expression and parameter signature.
pub struct CapturedClosure {
    pub param_name: String,
    pub param_dim: ParamDim,
    pub body: std::sync::Arc<AstNode<Expression>>,
}

/// Errors that can occur during AST-to-Fidget conversion.
#[derive(Debug, Error, Clone)]
pub enum FidgetConversionError {
    #[error("Implicit surface expression does not support {0}: {1}")]
    UnsupportedExpression(String, String),

    #[error("Captured value '{0}' has dimensions; implicit surfaces require dimensionless values")]
    DimensionalMismatch(String),

    #[error("Unknown captured variable '{0}'")]
    UnknownCapturedVariable(String),

    #[error("Parameter member access '{0}' is invalid for {1}D closure")]
    InvalidMemberAccess(String, usize),

    #[error("Method '{0}' requires {1} argument(s), got {2}")]
    WrongArgumentCount(String, usize, usize),

    #[error("Method '{0}' requires argument '{1}', not '{2}'")]
    MissingArgument(String, String, String),

    #[error("Cannot call closure in implicit surface expression: {0}")]
    ClosureCallError(String),

    #[error("Nested closure call is not supported: {0}")]
    NestedClosureCall(String),

    #[error("Dimensional mismatch in SDF expression: cannot combine dimensions {0:?} and {1:?}")]
    DimensionalIncompatible(Dimension, Dimension),
}

/// Converts a Command CAD AST expression to a Fidget Tree.
///
/// `captured_values` maps variable names (from the closure's captured environment)
/// to their resolved f64 constant values. These become `Tree::Const` nodes.
///
/// `closures` maps variable names to captured closures that can be inlined.
///
/// `param_dim` specifies whether the closure parameter is Vector2 or Vector3,
/// which determines which member accesses (x, y, z) are valid.
///
/// Returns `(Tree, Dimension)` where `Dimension` tracks the expression's physical
/// dimension for consistency checking.
pub fn ast_to_fidget(
    node: &AstNode<Expression>,
    captured_values: &HashMap<String, f64>,
    closures: &HashMap<String, CapturedClosure>,
    param_dim: ParamDim,
) -> Result<(Tree, Dimension), FidgetConversionError> {
    match &node.node {
        Expression::MethodCall(method_call) => {
            handle_method_call(&method_call.node, captured_values, closures, param_dim)
        }
        Expression::FunctionCall(func_call) => {
            handle_function_call(&func_call.node, captured_values, closures, param_dim)
        }
        Expression::BinaryExpression(bin) => {
            let (left, left_dim) = ast_to_fidget(&bin.node.a, captured_values, closures, param_dim)?;
            let (right, right_dim) = ast_to_fidget(&bin.node.b, captured_values, closures, param_dim)?;
            match &bin.node.operation.node {
                crate::compile::BinaryExpressionOperation::Add => {
                    if left_dim != right_dim {
                        return Err(FidgetConversionError::DimensionalIncompatible(left_dim, right_dim));
                    }
                    Ok((left + right, left_dim))
                }
                crate::compile::BinaryExpressionOperation::Sub => {
                    if left_dim != right_dim {
                        return Err(FidgetConversionError::DimensionalIncompatible(left_dim, right_dim));
                    }
                    Ok((left - right, left_dim))
                }
                crate::compile::BinaryExpressionOperation::Mul => {
                    // Dimension multiplication: add exponents
                    Ok((left * right, left_dim + right_dim))
                }
                crate::compile::BinaryExpressionOperation::Div => {
                    // Dimension division: subtract exponents
                    Ok((left / right, left_dim - right_dim))
                }
                op => Err(FidgetConversionError::UnsupportedExpression(
                    "operator".into(),
                    format!("{:?}", op),
                )),
            }
        }

        Expression::UnaryExpression(unary) => {
            let (operand, dim) = ast_to_fidget(&unary.node.expression, captured_values, closures, param_dim)?;
            match &unary.node.operation.node {
                crate::compile::UnaryExpressionOperation::Sub => Ok((-operand, dim)),
                crate::compile::UnaryExpressionOperation::Add => Ok((operand, dim)),
                crate::compile::UnaryExpressionOperation::Not => Err(FidgetConversionError::UnsupportedExpression(
                    "operator".into(),
                    "Not".into(),
                )),
            }
        }

        Expression::Scalar(scalar) => {
            Ok((Tree::constant(scalar.node.value.into_inner()), scalar.node.dimension))
        }

        Expression::SignedInteger(int_val) => {
            Ok((Tree::constant(int_val.node as f64), Dimension::zero()))
        }

        Expression::UnsignedInteger(uint_val) => {
            Ok((Tree::constant(uint_val.node as f64), Dimension::zero()))
        }

        Expression::Boolean(bool_val) => {
            // Booleans are not valid in SDF expressions, but we can convert to 0.0/1.0
            Ok((Tree::constant(if bool_val.node { 1.0 } else { 0.0 }), Dimension::zero()))
        }

        Expression::MemberAccess(member_box) => {
            let member_access = &**member_box;
            let base_is_identifier = matches!(&member_access.node.base.node, Expression::Identifier(_));

            if base_is_identifier {
                if let Expression::Identifier(ident) = &member_access.node.base.node {
                    let ident_str = ident.node.as_str();

                    if let Some(&val) = captured_values.get(ident_str) {
                        return Ok((Tree::constant(val), Dimension::zero()));
                    }

                    match member_access.node.member.node.as_str() {
                        "x" => Ok((Tree::x(), Dimension::length())),
                        "y" => Ok((Tree::y(), Dimension::length())),
                        "z" => match param_dim {
                            ParamDim::Vec3 => Ok((Tree::z(), Dimension::length())),
                            ParamDim::Vec2 => Err(FidgetConversionError::InvalidMemberAccess(
                                "z".into(), 2,
                            )),
                        },
                        other => {
                            Err(FidgetConversionError::UnknownCapturedVariable(other.into()))
                        }
                    }
                } else {
                    Err(FidgetConversionError::UnsupportedExpression(
                        "member access".into(),
                        "non-parameter base".into(),
                    ))
                }
            } else {
                // Non-identifier base — could be a method call result, handled below
                Err(FidgetConversionError::UnsupportedExpression(
                    "member access".into(),
                    "complex member chain".into(),
                ))
            }
        }

        Expression::Identifier(ident) => {
            let name = ident.node.as_str();
            if let Some(&val) = captured_values.get(name) {
                Ok((Tree::constant(val), Dimension::zero()))
            } else {
                Err(FidgetConversionError::UnknownCapturedVariable(name.into()))
            }
        }

        Expression::Parenthesis(inner) => ast_to_fidget(inner, captured_values, closures, param_dim),

        // Unsupported expression types
        Expression::ClosureDefinition(_)
        | Expression::DictionaryConstruction(_)
        | Expression::If(_)
        | Expression::List(_)
        | Expression::Self_(_)
        | Expression::Vector2(_)
        | Expression::Vector3(_)
        | Expression::Vector4(_)
        | Expression::String(_)
        | Expression::StructDefinition(_)
        | Expression::LetIn(_)
        | Expression::ConstraintSet(_)
        | Expression::Malformed(_) => {
            Err(FidgetConversionError::UnsupportedExpression(
                "expression".into(),
                format!("{:?}", node.node),
            ))
        }
    }
}

/// Handle a method call like `value::method(args)`.
fn handle_method_call(
    method_call: &MethodCall,
    captured_values: &HashMap<String, f64>,
    closures: &HashMap<String, CapturedClosure>,
    param_dim: ParamDim,
) -> Result<(Tree, Dimension), FidgetConversionError> {
    let method_name = method_call.to_call.node.as_str();
    let self_ast = &method_call.self_dictionary;
    let args_dict = &method_call.argument.node;

    // Check for std::min/std::max before evaluating self
    if method_name == "min" || method_name == "max" {
        let assignments = &args_dict.assignments;
        if assignments.len() == 2 {
            let self_is_std = matches!(&self_ast.node, Expression::Identifier(id) if id.node == "std");
            if self_is_std {
                let arg1 = &assignments[0].node.assignment;
                let arg2 = &assignments[1].node.assignment;
                let (tree1, dim1) = ast_to_fidget(arg1, captured_values, closures, param_dim)?;
                let (tree2, dim2) = ast_to_fidget(arg2, captured_values, closures, param_dim)?;
                if dim1 != dim2 {
                    return Err(FidgetConversionError::DimensionalIncompatible(dim1, dim2));
                }
                if method_name == "min" {
                    return Ok((tree1.min(tree2), dim1));
                } else {
                    return Ok((tree1.max(tree2), dim1));
                }
            }
        }
    }

    let (self_tree, self_dim) = ast_to_fidget(self_ast, captured_values, closures, param_dim)?;

    match method_name {
        // Unary methods (no arguments) — preserve dimension
        "abs" => Ok((self_tree.abs(), self_dim)),
        "sqrt" => Ok((self_tree.sqrt(), self_dim / 2)),
        "sin" => Ok((self_tree.sin(), Dimension::zero())),
        "cos" => Ok((self_tree.cos(), Dimension::zero())),
        "tan" => Ok((self_tree.tan(), Dimension::zero())),
        "asin" => Ok((self_tree.asin(), Dimension::zero())),
        "acos" => Ok((self_tree.acos(), Dimension::zero())),
        "atan" => Ok((self_tree.atan(), Dimension::zero())),
        "exp" => Ok((self_tree.exp(), Dimension::zero())),
        "ln" => Ok((self_tree.ln(), Dimension::zero())),
        "square" => Ok((self_tree.clone() * self_tree, self_dim + self_dim)),
        "neg" => Ok((-self_tree, self_dim)),
        "recip" => Ok((Tree::constant(1.0) / self_tree, -self_dim)),
        "floor" => Ok((self_tree.floor(), self_dim)),
        "ceil" => Ok((self_tree.ceil(), self_dim)),
        "round" => Ok((self_tree.round(), self_dim)),
        "trunc" => Ok((self_tree.floor(), self_dim)),
        "cbrt" => {
            // cbrt(x) = x^(1/3) — dimension / 3
            Ok(((self_tree.clone().ln() / Tree::constant(3.0)).exp(), self_dim / 3))
        }
        "signum" => {
            // signum(x) = x / abs(x) — dimensionless
            let abs_self = self_tree.abs();
            Ok((self_tree.clone() / abs_self, Dimension::zero()))
        }
        "is_finite" => {
            // Not directly supported — return 1.0 (assume finite for SDFs)
            Ok((Tree::constant(1.0), Dimension::zero()))
        }
        "is_infinite" => {
            // Not directly supported — return 0.0 (assume finite for SDFs)
            Ok((Tree::constant(0.0), Dimension::zero()))
        }
        "is_normal" => {
            // Not directly supported — return 1.0 (assume normal for SDFs)
            Ok((Tree::constant(1.0), Dimension::zero()))
        }
        "is_sign_negative" => {
            // Not directly supported — return 0.0 (always false for SDFs)
            Ok((Tree::constant(0.0), Dimension::zero()))
        }
        "is_sign_positive" => {
            // Not directly supported — return 0.0 (always false for SDFs)
            Ok((Tree::constant(0.0), Dimension::zero()))
        }

        // Binary methods (one argument)
        "min" => {
            let arg = extract_single_arg(args_dict, method_name)?;
            let (arg_tree, arg_dim) = ast_to_fidget(arg, captured_values, closures, param_dim)?;
            if self_dim != arg_dim {
                return Err(FidgetConversionError::DimensionalIncompatible(self_dim, arg_dim));
            }
            Ok((self_tree.min(arg_tree), self_dim))
        }
        "max" => {
            let arg = extract_single_arg(args_dict, method_name)?;
            let (arg_tree, arg_dim) = ast_to_fidget(arg, captured_values, closures, param_dim)?;
            if self_dim != arg_dim {
                return Err(FidgetConversionError::DimensionalIncompatible(self_dim, arg_dim));
            }
            Ok((self_tree.max(arg_tree), self_dim))
        }
        "pow" => {
            let arg = extract_single_arg(args_dict, method_name)?;
            // Check if the exponent is a constant integer — use repeated multiplication
            // to avoid ln(x) which produces NaN for x <= 0 during meshing
            if let Expression::Scalar(s) = &arg.node {
                let exp_val = s.node.value.into_inner();
                if exp_val == exp_val.floor() && (0.0..100.0).contains(&exp_val) {
                    let n = exp_val as usize;
                    let mut result = Tree::constant(1.0);
                    for _ in 0..n {
                        result *= self_tree.clone();
                    }
                    return Ok((result, self_dim * (n as i8)));
                }
            }
            // x^y = exp(y * ln(x)) — only for non-integer exponents
            let (arg_tree, _) = ast_to_fidget(arg, captured_values, closures, param_dim)?;
            Ok(((arg_tree * self_tree.clone().ln()).exp(), Dimension::zero()))
        }
        "hypot" => {
            let arg = extract_single_arg(args_dict, method_name)?;
            let (arg_tree, arg_dim) = ast_to_fidget(arg, captured_values, closures, param_dim)?;
            if self_dim != arg_dim {
                return Err(FidgetConversionError::DimensionalIncompatible(self_dim, arg_dim));
            }
            let sum_sq = self_tree.clone() * self_tree + arg_tree.clone() * arg_tree;
            Ok((sum_sq.sqrt(), self_dim))
        }

        // Ternary methods (two arguments)
        "clamp" => {
            let min_arg = extract_named_arg(args_dict, method_name, "min")?;
            let max_arg = extract_named_arg(args_dict, method_name, "max")?;
            let (min_tree, min_dim) = ast_to_fidget(min_arg, captured_values, closures, param_dim)?;
            let (max_tree, max_dim) = ast_to_fidget(max_arg, captured_values, closures, param_dim)?;
            if self_dim != min_dim || self_dim != max_dim {
                return Err(FidgetConversionError::DimensionalIncompatible(self_dim, min_dim));
            }
            // clamp(x, min, max) = max(min, min(x, max))
            let clamped = self_tree.clone().min(max_tree);
            Ok((min_tree.max(clamped), self_dim))
        }

        "copysign" => {
            // copysign(x, s) = abs(x) * sign(s) — simplified
            // For SDFs, this is rarely needed; return x for now
            Ok((self_tree, self_dim))
        }

        _ => Err(FidgetConversionError::UnsupportedExpression(
            "method".into(),
            method_name.into(),
        )),
    }
}

/// Extract a single unnamed argument from a method call.
fn extract_single_arg<'a>(
    dict: &'a crate::compile::DictionaryConstruction,
    method_name: &str,
) -> Result<&'a AstNode<Expression>, FidgetConversionError> {
    let assignments = &dict.assignments;
    if assignments.len() != 1 {
        return Err(FidgetConversionError::WrongArgumentCount(
            method_name.into(),
            1,
            assignments.len(),
        ));
    }
    Ok(&assignments.first().unwrap().node.assignment)
}

/// Extract a named argument from a method call.
fn extract_named_arg<'a>(
    dict: &'a crate::compile::DictionaryConstruction,
    method_name: &str,
    arg_name: &str,
) -> Result<&'a AstNode<Expression>, FidgetConversionError> {
    let assignments = &dict.assignments;
    for assignment in assignments.iter() {
        if let ArgumentName::Named(name) = &assignment.node.name {
            if name.as_str() == arg_name {
                return Ok(&assignment.node.assignment);
            }
        }
    }
    Err(FidgetConversionError::MissingArgument(
        method_name.into(),
        arg_name.into(),
        "none".into(),
    ))
}

/// Handle a function call like `closure(args)` or `std::min(a, b)`.
fn handle_function_call(
    func_call: &crate::compile::FunctionCall,
    captured_values: &HashMap<String, f64>,
    closures: &HashMap<String, CapturedClosure>,
    param_dim: ParamDim,
) -> Result<(Tree, Dimension), FidgetConversionError> {
    // Check if the callee is a captured closure or std function
    match &func_call.to_call.node {
        Expression::Identifier(ident) => {
            let callee_name = ident.node.as_str();

            if let Some(closure) = closures.get(callee_name) {
                // Get the argument expression
                let args_dict = &func_call.argument.node;
                let assignments = &args_dict.assignments;

                if assignments.len() != 1 {
                    return Err(FidgetConversionError::ClosureCallError(format!(
                        "Closure '{}' expects 1 argument, got {}",
                        callee_name, assignments.len()
                    )));
                }

                let arg_assignment = &assignments[0].node.assignment;

                // Check if the argument is the parameter itself (direct inlining)
                // e.g., a(p) where p is the closure parameter
                let arg_is_param = matches!(&arg_assignment.node, Expression::Identifier(id)
                    if id.node == "p");

                if arg_is_param {
                    // Direct inlining: a(p) means use the closure's body with p as-is
                    return ast_to_fidget(&closure.body, captured_values, closures, closure.param_dim);
                }

                // For other cases, we need to evaluate the argument first
                // This handles cases like a(p.x + 1.0) but is more limited
                Err(FidgetConversionError::ClosureCallError(format!(
                    "Complex argument in closure call '{}' is not supported",
                    callee_name
                )))
            } else {
                handle_std_function_call(callee_name, func_call, captured_values, closures, param_dim)
            }
        }
        Expression::MemberAccess(member) => {
            // Handle std::min, std::max as member access (std.min, std.max)
            if let Expression::Identifier(base_ident) = &member.node.base.node {
                let base_name = base_ident.node.as_str();
                let method_name = member.node.member.node.as_str();

                if base_name == "std" {
                    return handle_std_function_call(
                        &format!("std::{}", method_name),
                        func_call, captured_values, closures, param_dim
                    );
                }
            }
            Err(FidgetConversionError::ClosureCallError(
                "Function callee must be an identifier or std::function".into(),
            ))
        }
        _ => {
            Err(FidgetConversionError::ClosureCallError(
                "Function callee must be an identifier".into(),
            ))
        }
    }
}

/// Handle std::min and std::max function calls.
fn handle_std_function_call(
    callee_name: &str,
    func_call: &crate::compile::FunctionCall,
    captured_values: &HashMap<String, f64>,
    closures: &HashMap<String, CapturedClosure>,
    param_dim: ParamDim,
) -> Result<(Tree, Dimension), FidgetConversionError> {
    let is_std_min = callee_name == "std::min" || callee_name == "min";
    let is_std_max = callee_name == "std::max" || callee_name == "max";

    if is_std_min {
        let args_dict = &func_call.argument.node;
        let assignments = &args_dict.assignments;

        if assignments.len() != 2 {
            return Err(FidgetConversionError::ClosureCallError(format!(
                "std::min expects 2 arguments, got {}",
                assignments.len()
            )));
        }

        let arg1 = &assignments[0].node.assignment;
        let arg2 = &assignments[1].node.assignment;

        let (tree1, dim1) = ast_to_fidget(arg1, captured_values, closures, param_dim)?;
        let (tree2, dim2) = ast_to_fidget(arg2, captured_values, closures, param_dim)?;

        if dim1 != dim2 {
            return Err(FidgetConversionError::DimensionalIncompatible(dim1, dim2));
        }

        Ok((tree1.min(tree2), dim1))
    } else if is_std_max {
        let args_dict = &func_call.argument.node;
        let assignments = &args_dict.assignments;

        if assignments.len() != 2 {
            return Err(FidgetConversionError::ClosureCallError(format!(
                "std::max expects 2 arguments, got {}",
                assignments.len()
            )));
        }

        let arg1 = &assignments[0].node.assignment;
        let arg2 = &assignments[1].node.assignment;

        let (tree1, dim1) = ast_to_fidget(arg1, captured_values, closures, param_dim)?;
        let (tree2, dim2) = ast_to_fidget(arg2, captured_values, closures, param_dim)?;

        if dim1 != dim2 {
            return Err(FidgetConversionError::DimensionalIncompatible(dim1, dim2));
        }

        Ok((tree1.max(tree2), dim1))
    } else {
        Err(FidgetConversionError::ClosureCallError(format!(
            "Unknown function or closure: {}",
            callee_name
        )))
    }
}

/// Resolves captured values from the closure's captured_values map to f64 constants.
///
/// Checks that all captured values are dimensionless (no physical units).
/// `current_expression` is the expression of the closure being converted, used to skip
/// self-referential captures (a closure should not capture itself).
pub fn resolve_captured_values(
    captured: &IndexMap<ArgumentName, Value>,
    context: &ExecutionContext<'_>,
    _current_expression: &AstNode<Expression>,
) -> ExecutionResult<(HashMap<String, f64>, HashMap<String, CapturedClosure>)> {
    let mut map = HashMap::new();
    let mut closures_map = HashMap::new();

    for (name, value) in captured {
        match name {
            ArgumentName::Named(var_name) => {
                // Check if this is a closure
                if let Some(closure) = value.enum_downcast_ref::<UserClosure>() {
                    // Extract closure info for inlining
                    let members = &closure.signature().argument_type.members;
                    if members.len() != 1 {
                        return Err(ClosureCaptureError("closure has multiple parameters".into()).to_error(context));
                    }

                    let (_param_name, param_type) = members.iter().next().unwrap();
                    let param_dim = match &param_type.ty {
                        super::super::ValueType::Vector2(Some(dim)) if dim.length == 1 => ParamDim::Vec2,
                        super::super::ValueType::Vector3(Some(dim)) if dim.length == 1 => ParamDim::Vec3,
                        _ => {
                            return Err(ClosureCaptureError("closure parameter must have length dimension".into()).to_error(context));
                        }
                    };

                    closures_map.insert(
                        var_name.to_string(),
                        CapturedClosure {
                            param_name: members.keys().next().unwrap().to_string(),
                            param_dim,
                            body: closure.expression().clone(),
                        },
                    );
                } else if matches!(value, Value::Dictionary(_)) {
                    // Skip dictionary-typed captured values (e.g., self-referential closures)
                    continue;
                } else {
                    let f64_val = value_to_f64(value, context)?;
                    map.insert(var_name.to_string(), f64_val);
                }
            }
            ArgumentName::Positional(_) => {
                // Closures always use named params, but handle gracefully
            }
        }
    }

    Ok((map, closures_map))
}

/// Converts a Value to f64, checking for dimensional compatibility.
fn value_to_f64(value: &Value, context: &ExecutionContext<'_>) -> ExecutionResult<f64> {
    // Use enum_downcast_ref to access the inner typed values
    if let Some(s) = value.enum_downcast_ref::<Scalar>() {
        if !s.dimension.is_zero_dimension() {
            return Err(DimensionalScalarError(s.dimension).to_error(context));
        }
        return Ok(s.value.into_inner());
    }
    if let Some(u) = value.enum_downcast_ref::<crate::execution::values::UnsignedInteger>() {
        return Ok(u.0 as f64);
    }
    if let Some(i) = value.enum_downcast_ref::<crate::execution::values::SignedInteger>() {
        return Ok(i.0 as f64);
    }
    if let Some(b) = value.enum_downcast_ref::<crate::execution::values::Boolean>() {
        return Ok(if b.0 { 1.0 } else { 0.0 });
    }
    Err(UnsupportedCapturedTypeError(value.type_name()).to_error(context))
}

#[derive(Debug, Error, Clone)]
#[error("Captured value has dimensions; implicit surfaces require dimensionless values")]
pub struct DimensionalScalarError(pub Dimension);

#[derive(Debug, Error, Clone)]
#[error("Captured value type '{0}' is not supported in implicit surface closures (only Scalar, Int, UInt, Bool)")]
pub struct UnsupportedCapturedTypeError(pub std::borrow::Cow<'static, str>);

#[derive(Debug, Error, Clone)]
#[error("Cannot capture closure for implicit surface: {0}")]
pub struct ClosureCaptureError(pub String);

#[cfg(test)]
mod fidget_conversion_tests {
    use std::collections::HashMap;

    use crate::compile::full_compile;
    use fidget::context::Context;
    use fidget::jit::JitShape;
    use fidget::shape::EzShape;

    use crate::execution::values::{Boolean, SignedInteger, UnsignedInteger};

    use super::*;

    fn eval_tree(tree: &Tree, x: f64, y: f64, z: f64) -> f64 {
        let mut ctx = Context::new();
        let node = ctx.import(tree);
        let shape = JitShape::new(&ctx, node).expect("Shape creation failed");
        let tape = shape.ez_point_tape();
        let mut eval = JitShape::new_point_eval();
        let (result, _) = eval.eval(&tape, x as f32, y as f32, z as f32).expect("Evaluation failed");
        result as f64
    }

    fn eval_tree_2d(tree: &Tree, x: f64, y: f64) -> f64 {
        eval_tree(tree, x, y, 0.0)
    }

    // --- Scalar literals ---

    #[test]
    fn scalar_dimensionless() {
        let node = full_compile("1.5");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 1.5).abs() < 1e-10);
    }

    #[test]
    fn scalar_zero() {
        let node = full_compile("0.0");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 0.0).abs() < 1e-10);
    }

    #[test]
    fn scalar_negative() {
        let node = full_compile("-3.14");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        let val = eval_tree_2d(&tree, 0.0, 0.0);
        assert!((val - (-3.14)).abs() < 1e-5);
    }

    #[test]
    fn scalar_with_dimensions_passed_through() {
        // Dimensional scalars are now passed through with their dimension.
        // The top-level check (ast_to_shape) verifies the final result is Length.
        let node = full_compile("1.5mm");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (_tree, dim) = result.unwrap();
        assert_eq!(dim, Dimension::length());
    }

    // --- Integer literals ---

    #[test]
    fn signed_integer_positive() {
        let node = full_compile("42i");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 42.0).abs() < 1e-10);
    }

    #[test]
    fn signed_integer_negative() {
        let node = full_compile("-7i");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - (-7.0)).abs() < 1e-10);
    }

    #[test]
    fn unsigned_integer() {
        let node = full_compile("99u");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 99.0).abs() < 1e-10);
    }

    // --- Boolean literals ---

    #[test]
    fn boolean_true() {
        let node = full_compile("true");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 1.0).abs() < 1e-10);
    }

    #[test]
    fn boolean_false() {
        let node = full_compile("false");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 0.0).abs() < 1e-10);
    }

    // --- Binary expressions ---

    #[test]
    fn binary_add() {
        let node = full_compile("2.0 + 3.0");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 5.0).abs() < 1e-10);
    }

    #[test]
    fn binary_sub() {
        let node = full_compile("10.0 - 4.0");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 6.0).abs() < 1e-10);
    }

    #[test]
    fn binary_mul() {
        let node = full_compile("3.0 * 4.0");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 12.0).abs() < 1e-10);
    }

    #[test]
    fn binary_div() {
        let node = full_compile("10.0 / 2.0");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 5.0).abs() < 1e-10);
    }

    #[test]
    fn binary_nested() {
        let node = full_compile("(1.0 + 2.0) * 3.0");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        // (1.0 + 2.0) * 3.0 = 9.0
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 9.0).abs() < 1e-10);
    }

    #[test]
    fn parenthesis_basic() {
        let node = full_compile("(5.0)");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 5.0).abs() < 1e-10);
    }

    #[test]
    fn parenthesis_with_parameter() {
        let node = full_compile("(p.x + p.y) * 2.0");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree(&tree, 3.0, 4.0, 0.0) - 14.0).abs() < 1e-10);
    }

    #[test]
    fn parenthesis_nested() {
        let node = full_compile("((1.0 + 2.0) * (3.0 + 4.0))");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        // (1.0 + 2.0) * (3.0 + 4.0) = 3.0 * 7.0 = 21.0
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 21.0).abs() < 1e-10);
    }

    #[test]
    fn binary_with_parameter_x() {
        let node = full_compile("p.x - 1.0m");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree(&tree, 5.0, 0.0, 0.0) - 4.0).abs() < 1e-10);
    }

    #[test]
    fn binary_with_parameter_y() {
        let node = full_compile("p.y - 2.0m");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree(&tree, 0.0, 3.0, 0.0) - 1.0).abs() < 1e-10);
    }

    #[test]
    fn binary_with_parameter_z() {
        let node = full_compile("p.z - 5.0m");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree(&tree, 0.0, 0.0, 7.0) - 2.0).abs() < 1e-10);
    }

    #[test]
    fn binary_with_parameter_z_rejected_vec2() {
        let node = full_compile("p.z - 5.0m");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec2);
        assert!(matches!(
            result,
            Err(FidgetConversionError::InvalidMemberAccess(_, 2))
        ));
    }

    #[test]
    fn binary_with_parameters() {
        let node = full_compile("p.x + p.y");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree(&tree, 3.0, 4.0, 0.0) - 7.0).abs() < 1e-10);
    }

    #[test]
    fn binary_all_three_params() {
        let node = full_compile("p.x + p.y + p.z");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree(&tree, 1.0, 2.0, 3.0) - 6.0).abs() < 1e-10);
    }

    // --- Unary expressions ---

    #[test]
    fn unary_negation() {
        let node = full_compile("-5.0");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - (-5.0)).abs() < 1e-10);
    }

    #[test]
    fn unary_positive() {
        let node = full_compile("+5.0");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 5.0).abs() < 1e-10);
    }

    #[test]
    fn unary_negation_of_parameter() {
        let node = full_compile("-p.x");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree(&tree, 3.0, 0.0, 0.0) - (-3.0)).abs() < 1e-10);
    }

    #[test]
    fn unary_not_rejected() {
        let node = full_compile("!true");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(matches!(
            result,
            Err(FidgetConversionError::UnsupportedExpression(op, _)) if op == "operator"
        ));
    }

    // --- Parameter member access ---

    #[test]
    fn member_x() {
        let node = full_compile("p.x");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree(&tree, 2.5, 0.0, 0.0) - 2.5).abs() < 1e-10);
    }

    #[test]
    fn member_y() {
        let node = full_compile("p.y");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree(&tree, 0.0, 3.5, 0.0) - 3.5).abs() < 1e-10);
    }

    #[test]
    fn member_z_vec3() {
        let node = full_compile("p.z");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree(&tree, 0.0, 0.0, 4.5) - 4.5).abs() < 1e-10);
    }

    #[test]
    fn member_z_rejected_vec2() {
        let node = full_compile("p.z");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec2);
        assert!(matches!(
            result,
            Err(FidgetConversionError::InvalidMemberAccess(_, 2))
        ));
    }

    // --- Captured values ---

    #[test]
    fn captured_value_used() {
        let node = full_compile("radius");
        let mut captured = HashMap::new();
        captured.insert("radius".to_string(), 5.0);
        let result = ast_to_fidget(&node, &captured, &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 5.0).abs() < 1e-10);
    }

    #[test]
    fn captured_value_in_expression() {
        let node = full_compile("radius * 2.0");
        let mut captured = HashMap::new();
        captured.insert("radius".to_string(), 3.0);
        let result = ast_to_fidget(&node, &captured, &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 6.0).abs() < 1e-10);
    }

    #[test]
    fn multiple_captured_values() {
        let node = full_compile("a + b");
        let mut captured = HashMap::new();
        captured.insert("a".to_string(), 10.0);
        captured.insert("b".to_string(), 20.0);
        let result = ast_to_fidget(&node, &captured, &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 30.0).abs() < 1e-10);
    }

    #[test]
    fn unknown_captured_variable() {
        let node = full_compile("unknown_var");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(matches!(
            result,
            Err(FidgetConversionError::UnknownCapturedVariable(name)) if name == "unknown_var"
        ));
    }

    // --- Unary methods ---

    #[test]
    fn method_abs_positive() {
        let node = full_compile("5.0::abs()");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 5.0).abs() < 1e-10);
    }

    #[test]
    fn method_abs_negative() {
        let node = full_compile("5.0::neg()::abs()");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 5.0).abs() < 1e-10);
    }

    #[test]
    fn method_sqrt() {
        let node = full_compile("16.0::sqrt()");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 4.0).abs() < 1e-10);
    }

    #[test]
    fn method_sin() {
        let node = full_compile("0.0::sin()");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 0.0).abs() < 1e-10);
    }

    #[test]
    fn method_cos() {
        let node = full_compile("0.0::cos()");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 1.0).abs() < 1e-10);
    }

    #[test]
    fn method_square() {
        let node = full_compile("4.0::square()");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 16.0).abs() < 1e-10);
    }

    #[test]
    fn method_neg() {
        let node = full_compile("7.0::neg()");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - (-7.0)).abs() < 1e-10);
    }

    #[test]
    fn method_recip() {
        let node = full_compile("4.0::recip()");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 0.25).abs() < 1e-10);
    }

    #[test]
    fn method_floor() {
        let node = full_compile("3.7::floor()");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 3.0).abs() < 1e-10);
    }

    #[test]
    fn method_ceil() {
        let node = full_compile("3.2::ceil()");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 4.0).abs() < 1e-10);
    }

    #[test]
    fn method_round() {
        let node = full_compile("3.5::round()");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        let val = eval_tree_2d(&tree, 0.0, 0.0);
        assert!((val - 4.0).abs() < 1e-10 || (val - 3.0).abs() < 1e-10);
    }

    #[test]
    fn method_cbrt() {
        let node = full_compile("27.0::cbrt()");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 3.0).abs() < 1e-10);
    }

    #[test]
    fn method_pow_integer() {
        let node = full_compile("3.0::pow(exp = 2)");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 9.0).abs() < 1e-10);
    }

    #[test]
    fn method_pow_integer_zero() {
        let node = full_compile("5.0::pow(exp = 0)");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 1.0).abs() < 1e-10);
    }

    #[test]
    fn method_pow_with_parameter() {
        // p.x::pow(2) should work even when p.x is negative (uses x*x, not exp(ln(x)))
        let node = full_compile("p.x::pow(exp = 2)");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        // (-3)^2 = 9
        assert!((eval_tree(&tree, -3.0, 0.0, 0.0) - 9.0).abs() < 1e-10);
    }

    #[test]
    fn method_is_finite_returns_one() {
        let node = full_compile("5.0::is_finite()");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 1.0).abs() < 1e-10);
    }

    #[test]
    fn method_is_infinite_returns_zero() {
        let node = full_compile("5.0::is_infinite()");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 0.0).abs() < 1e-10);
    }

    // --- Binary methods ---

    #[test]
    fn method_min() {
        let node = full_compile("3.0::min(5.0)");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 3.0).abs() < 1e-10);
    }

    #[test]
    fn method_max() {
        let node = full_compile("3.0::max(5.0)");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 5.0).abs() < 1e-10);
    }

    #[test]
    fn method_pow() {
        let node = full_compile("2.0::pow(exp = 3.0)");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 8.0).abs() < 1e-10);
    }

    #[test]
    fn method_hypot() {
        let node = full_compile("3.0::hypot(4.0)");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 5.0).abs() < 1e-10);
    }

    // --- Ternary methods ---

    #[test]
    fn method_clamp_basic() {
        let node = full_compile("5.0::clamp(min = 0.0, max = 10.0)");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 5.0).abs() < 1e-10);
    }

    #[test]
    fn method_clamp_below_min() {
        let node = full_compile("5.0::neg()::clamp(min = 0.0, max = 10.0)");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 0.0).abs() < 1e-10);
    }

    #[test]
    fn method_clamp_above_max() {
        let node = full_compile("15.0::clamp(min = 0.0, max = 10.0)");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 10.0).abs() < 1e-10);
    }

    // --- Complex expressions ---

    #[test]
    fn complex_expression_with_params_and_methods() {
        let node = full_compile("p.x::abs() + p.y::abs()");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree(&tree, -3.0, 4.0, 0.0) - 7.0).abs() < 1e-10);
    }

    #[test]
    fn expression_with_captured_and_params() {
        // Captured values are dimensionless; multiply by p.x (Length) to get Length
        let node = full_compile("radius * p.x");
        let mut captured = HashMap::new();
        captured.insert("radius".to_string(), 2.0);
        let result = ast_to_fidget(&node, &captured, &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree(&tree, 3.0, 0.0, 0.0) - 6.0).abs() < 1e-10);
    }

    #[test]
    fn nested_methods() {
        let node = full_compile("5.0::neg()::abs()::sqrt()");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 2.2360679775).abs() < 1e-5);
    }

    #[test]
    fn chained_operations() {
        let node = full_compile("p.x * 2.0 + p.y * 3.0");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree(&tree, 1.0, 2.0, 0.0) - 8.0).abs() < 1e-10);
    }

    // --- Error cases ---

    #[test]
    fn unsupported_binary_operator() {
        let node = full_compile("1 == 2");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(matches!(
            result,
            Err(FidgetConversionError::UnsupportedExpression(op, _)) if op == "operator"
        ));
    }

    #[test]
    fn unsupported_expression_closure() {
        let node = full_compile("() -> thing: 1.0");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(matches!(
            result,
            Err(FidgetConversionError::UnsupportedExpression(expr, _)) if expr == "expression"
        ));
    }

    #[test]
    fn unsupported_expression_if() {
        let node = full_compile("if true then 1.0 else 2.0");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(matches!(
            result,
            Err(FidgetConversionError::UnsupportedExpression(expr, _)) if expr == "expression"
        ));
    }

    #[test]
    fn unsupported_expression_list() {
        let node = full_compile("[1.0, 2.0]");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(matches!(
            result,
            Err(FidgetConversionError::UnsupportedExpression(expr, _)) if expr == "expression"
        ));
    }

    #[test]
    fn unsupported_expression_vector2() {
        let node = full_compile("Vector2(1.0, 2.0)");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        // Vector2(...) is parsed as a function call, which is now handled by handle_function_call
        assert!(matches!(
            result,
            Err(FidgetConversionError::ClosureCallError(_))
        ));
    }

    #[test]
    fn unsupported_method() {
        let node = full_compile("5.0::unknown_method()");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(matches!(
            result,
            Err(FidgetConversionError::UnsupportedExpression(op, _)) if op == "method"
        ));
    }

    #[test]
    fn method_min_wrong_arg_count() {
        let node = full_compile("5.0::min(1.0, 2.0)");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(matches!(
            result,
            Err(FidgetConversionError::WrongArgumentCount(name, expected, got))
                if name == "min" && expected == 1 && got == 2
        ));
    }

    #[test]
    fn method_pow_missing_exp_arg() {
        let node = full_compile("2.0::pow()");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(matches!(
            result,
            Err(FidgetConversionError::WrongArgumentCount(name, expected, got))
                if name == "pow" && expected == 1 && got == 0
        ));
    }

    #[test]
    fn method_pow_positional_arg() {
        let node = full_compile("2.0::pow(3.0)");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 8.0).abs() < 1e-10);
    }

    #[test]
    fn method_pow_named_arg() {
        let node = full_compile("2.0::pow(exp = 3.0)");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(result.is_ok());
        let (tree, _dim) = result.unwrap();
        assert!((eval_tree_2d(&tree, 0.0, 0.0) - 8.0).abs() < 1e-10);
    }

    #[test]
    fn method_clamp_missing_min_arg() {
        let node = full_compile("5.0::clamp(max = 10.0)");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(matches!(
            result,
            Err(FidgetConversionError::MissingArgument(name, arg, _))
                if name == "clamp" && arg == "min"
        ));
    }

    #[test]
    fn method_clamp_missing_max_arg() {
        let node = full_compile("5.0::clamp(min = 0.0)");
        let result = ast_to_fidget(&node, &HashMap::new(), &HashMap::new(), ParamDim::Vec3);
        assert!(matches!(
            result,
            Err(FidgetConversionError::MissingArgument(name, arg, _))
                if name == "clamp" && arg == "max"
        ));
    }

    // --- resolve_captured_values ---

    #[test]
    fn resolve_captured_values_dimensionless_scalar() {
        use common_data_types::Float;
        use indexmap::indexmap;

        let captured: IndexMap<ArgumentName, Value> = indexmap! {
            "val".into() => Value::Scalar(Scalar {
                dimension: Dimension::zero(),
                value: Float::new(3.14).unwrap(),
            }),
        };

        crate::execution::test_context([], |ctx| {
            let result = resolve_captured_values(&captured, ctx, &full_compile("0.0"));
            assert!(result.is_ok());
            let (map, _closures) = result.unwrap();
            assert_eq!(map.get("val"), Some(&3.14));
        });
    }

    #[test]
    fn resolve_captured_values_signed_integer() {
        use indexmap::indexmap;

        let captured: IndexMap<ArgumentName, Value> = indexmap! {
            "count".into() => Value::SignedInteger(SignedInteger::from(42)),
        };

        crate::execution::test_context([], |ctx| {
            let result = resolve_captured_values(&captured, ctx, &full_compile("0.0"));
            assert!(result.is_ok());
            let (map, _closures) = result.unwrap();
            assert_eq!(map.get("count"), Some(&42.0));
        });
    }

    #[test]
    fn resolve_captured_values_unsigned_integer() {
        use indexmap::indexmap;

        let captured: IndexMap<ArgumentName, Value> = indexmap! {
            "count".into() => Value::UnsignedInteger(UnsignedInteger::from(42u64)),
        };

        crate::execution::test_context([], |ctx| {
            let result = resolve_captured_values(&captured, ctx, &full_compile("0.0"));
            assert!(result.is_ok());
            let (map, _closures) = result.unwrap();
            assert_eq!(map.get("count"), Some(&42.0));
        });
    }

    #[test]
    fn resolve_captured_values_boolean() {
        use indexmap::indexmap;

        let captured: IndexMap<ArgumentName, Value> = indexmap! {
            "flag".into() => Value::Boolean(Boolean(true)),
        };

        crate::execution::test_context([], |ctx| {
            let result = resolve_captured_values(&captured, ctx, &full_compile("0.0"));
            assert!(result.is_ok());
            let (map, _closures) = result.unwrap();
            assert_eq!(map.get("flag"), Some(&1.0));
        });
    }

    #[test]
    fn resolve_captured_values_boolean_false() {
        use indexmap::indexmap;

        let captured: IndexMap<ArgumentName, Value> = indexmap! {
            "flag".into() => Value::Boolean(Boolean(false)),
        };

        crate::execution::test_context([], |ctx| {
            let result = resolve_captured_values(&captured, ctx, &full_compile("0.0"));
            assert!(result.is_ok());
            let (map, _closures) = result.unwrap();
            assert_eq!(map.get("flag"), Some(&0.0));
        });
    }

    #[test]
    fn resolve_captured_values_multiple() {
        use common_data_types::Float;
        use indexmap::indexmap;

        let captured: IndexMap<ArgumentName, Value> = indexmap! {
            "radius".into() => Value::Scalar(Scalar {
                dimension: Dimension::zero(),
                value: Float::new(5.0).unwrap(),
            }),
            "count".into() => Value::SignedInteger(SignedInteger::from(3)),
            "enabled".into() => Value::Boolean(Boolean(true)),
        };

        crate::execution::test_context([], |ctx| {
            let result = resolve_captured_values(&captured, ctx, &full_compile("0.0"));
            assert!(result.is_ok());
            let (map, _closures) = result.unwrap();
            assert_eq!(map.get("radius"), Some(&5.0));
            assert_eq!(map.get("count"), Some(&3.0));
            assert_eq!(map.get("enabled"), Some(&1.0));
        });
    }

    // --- Integration: language-level closure::to_implicit() ---

    #[test]
    fn integration_2d_closure_to_implicit() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run(
            "let c = (p: std.vector2.Length) -> std.scalar.Length: p.x::abs() + p.y::abs(); in c::to_implicit()"
        );
        if let Err(ref e) = result {
            eprintln!("integration_2d error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface2D(_)));
    }

    #[test]
    fn integration_3d_closure_to_implicit() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run(
            "let c = (p: std.vector3.Length) -> std.scalar.Length: p.x::abs() + p.y::abs() + p.z::abs(); in c::to_implicit()"
        );
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_2d_closure_with_captured_to_implicit() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run(
            "let radius = 5.0; c = (p: std.vector2.Length) -> std.scalar.Length: p.x - radius * 1.0m; in c::to_implicit()"
        );
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface2D(_)));
    }

    #[test]
    fn integration_3d_closure_with_methods_to_implicit() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        let result = test_run(
            "let c = (p: std.vector3.Length) -> std.scalar.Length: p.x::abs() + p.y::abs() + p.z::abs(); in c::to_implicit()"
        );
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_closure_composition_via_call() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        // Test that calling a captured closure works: a(p) inlines the closure body
        let result = test_run(
            "let a = (p: std.vector3.Length) -> std.scalar.Length: (p.x * p.x + p.y * p.y + p.z * p.z)::sqrt() - 1.0m; \
             b = (p: std.vector3.Length) -> std.scalar.Length: a(p); \
             in b::to_implicit()"
        );
        if let Err(ref e) = result {
            eprintln!("closure composition error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_std_min_function() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        // Test std::min function for surface composition
        let result = test_run(
            "let a = (p: std.vector3.Length) -> std.scalar.Length: (p.x * p.x + p.y * p.y + p.z * p.z)::sqrt() - 1.0m; \
             b = (p: std.vector3.Length) -> std.scalar.Length: (p.x * p.x + p.y * p.y + p.z * p.z)::sqrt() - 2.0m; \
             combined = (p: std.vector3.Length) -> std.scalar.Length: std::min(a(p), b(p)); \
             in combined::to_implicit()"
        );
        if let Err(ref e) = result {
            eprintln!("std::min composition error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_std_max_function() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        // Test std::max function for surface composition
        let result = test_run(
            "let a = (p: std.vector3.Length) -> std.scalar.Length: (p.x * p.x + p.y * p.y + p.z * p.z)::sqrt() - 1.0m; \
             b = (p: std.vector3.Length) -> std.scalar.Length: (p.x * p.x + p.y * p.y + p.z * p.z)::sqrt() - 2.0m; \
             combined = (p: std.vector3.Length) -> std.scalar.Length: std::max(a(p), b(p)); \
             in combined::to_implicit()"
        );
        if let Err(ref e) = result {
            eprintln!("std::max composition error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::Surface3D(_)));
    }

    #[test]
    fn integration_sphere_to_mesh() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        // Reproduce GUI panic: sphere with to_mesh()
        let result = test_run(
            "let s3d = (p: std.vector3.Length) -> std.scalar.Length: (p.x * p.x + p.y * p.y + p.z * p.z)::sqrt() - 1.0m; \
             in s3d::to_implicit()::to_mesh()"
        );
        if let Err(ref e) = result {
            eprintln!("sphere to_mesh error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::ManifoldMesh3D(_)));
    }

    #[test]
    fn integration_sphere_to_mesh_with_length_return_type() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        // Test with std.scalar.Length return type and dimensional expression
        let result = test_run(
            "let s3d = (p: std.vector3.Length) -> std.scalar.Length: (p.x * p.x + p.y * p.y + p.z * p.z)::sqrt() - 1.0m; \
             in s3d::to_implicit()::to_mesh()"
        );
        if let Err(ref e) = result {
            eprintln!("sphere Length return type error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::ManifoldMesh3D(_)));
    }

    #[test]
    fn integration_sphere_pow_positional() {
        use crate::execution::test_run;
        use crate::execution::values::Value;

        // Test pow with positional argument (user requested this)
        let result = test_run(
            "let s3d = (p: std.vector3.Length) -> std.scalar.Length: \
             (p.x::pow(2.0) + p.y::pow(2.0) + p.z::pow(2.0))::sqrt() - 1.0m; \
             in s3d::to_implicit()::to_mesh()"
        );
        if let Err(ref e) = result {
            eprintln!("sphere pow positional error: {:?}", e);
        }
        assert!(result.is_ok());
        let value = result.unwrap();
        assert!(matches!(value, Value::ManifoldMesh3D(_)));
    }
}
