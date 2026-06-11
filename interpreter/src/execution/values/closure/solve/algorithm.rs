use imstr::ImString;
use indexmap::IndexMap;

use crate::{
    compile::{AstNode, Expression},
    execution::{
        errors::{ExecutionResult, Raise},
        values::{
            boolean::Boolean,
            dictionary::{ArgumentName, Dictionary},
            integer::Integer,
            scalar::Scalar as RuntimeScalar,
            Object, SignedInteger, Value,
        },
        ExecutionContext,
    },
};

use super::{
    collect_free_vars, get_method_inverse_callable, get_method_name, infer_sym_expr_type,
    sym_expr_contains_var, BinOp, BoolOp, InverseOp, SolveResult, SymExpr, UnaryOp,
};

/// Error types for solving.
#[derive(Debug, Eq, PartialEq)]
pub enum SolveError {
    VariableNotFound { variable: ImString },
    VariableAppearsMultipleTimes { variable: ImString },
    NonInvertibleOperation { operation: String, source: crate::compile::SourceReference },
    NoSolution { operation: String, source: crate::compile::SourceReference },
    #[allow(dead_code)]
    NotDifferentiable { operation: String, source: crate::compile::SourceReference },
    #[allow(dead_code)]
    NotIntegrable { operation: String, source: crate::compile::SourceReference },
}

impl std::error::Error for SolveError {}

impl std::fmt::Display for SolveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SolveError::VariableNotFound { variable } => {
                write!(f, "variable '{}' not found in expression", variable)
            }
            SolveError::VariableAppearsMultipleTimes { variable } => {
                write!(
                    f,
                    "variable '{}' appears multiple times; expression is non-linear",
                    variable
                )
            }
            SolveError::NonInvertibleOperation { operation, .. } => {
                write!(f, "operation '{}' is not invertible", operation)
            }
            SolveError::NoSolution { operation, .. } => {
                write!(f, "no solution: {}", operation)
            }
            SolveError::NotDifferentiable { operation, .. } => {
                write!(f, "operation '{}' is not differentiable", operation)
            }
            SolveError::NotIntegrable { operation, .. } => {
                write!(f, "operation '{}' is not integrable", operation)
            }
        }
    }
}

/// Check if a SymExpr is a zero scalar constant.
fn is_zero_scalar(expr: &SymExpr) -> bool {
    match expr {
        SymExpr::Scalar(s) => s.value == 0.0,
        SymExpr::Integer(i) => *i == 0,
        _ => false,
    }
}

/// Convert an Expression AST node to a SymExpr for symbolic manipulation.
pub fn expression_to_sym_expr(
    expr: &AstNode<Expression>,
    context: &ExecutionContext,
) -> ExecutionResult<SymExpr> {
    match &expr.node {
        Expression::Identifier(ident) => Ok(SymExpr::Var(ident.node.clone())),
        Expression::Scalar(scalar) => Ok(SymExpr::Scalar(RuntimeScalar {
            dimension: scalar.node.dimension,
            value: scalar.node.value,
        })),
        Expression::SignedInteger(int) => Ok(SymExpr::Integer(int.node)),
        Expression::UnsignedInteger(int) => {
            Ok(SymExpr::Integer(int.node as _))
        }
        Expression::Boolean(b) => Ok(SymExpr::Boolean(Boolean(b.node))),
        Expression::BinaryExpression(binop) => {
            let left = expression_to_sym_expr(&binop.node.a, context)?;
            let right = expression_to_sym_expr(&binop.node.b, context)?;

            let op = match &binop.node.operation.node {
                crate::compile::BinaryExpressionOperation::Add => BinOp::Add,
                crate::compile::BinaryExpressionOperation::Sub => BinOp::Sub,
                crate::compile::BinaryExpressionOperation::Mul => BinOp::Mul,
                crate::compile::BinaryExpressionOperation::Div => BinOp::Div,
                crate::compile::BinaryExpressionOperation::MulMul => BinOp::Pow,
                _ => {
                    // Comparison and boolean ops are not invertible in the traditional sense
                    // We'll treat them as errors for now
                    return Err(SolveError::NonInvertibleOperation {
                        operation: format!("{:?}", binop.node.operation.node),
                        source: expr.reference.clone(),
                    }
                    .to_error(context));
                }
            };

            Ok(SymExpr::BinOp(op, Box::new(left), Box::new(right)))
        }
        Expression::UnaryExpression(unary) => {
            let inner = expression_to_sym_expr(&unary.node.expression, context)?;

            let op = match &unary.node.operation.node {
                crate::compile::UnaryExpressionOperation::Add => UnaryOp::Neg,
                crate::compile::UnaryExpressionOperation::Sub => UnaryOp::Neg,
                crate::compile::UnaryExpressionOperation::Not => UnaryOp::Not,
            };

            Ok(SymExpr::UnaryOp(op, Box::new(inner)))
        }
        Expression::MethodCall(method) => {
            let self_expr = expression_to_sym_expr(&method.node.self_dictionary, context)?;
            let args: Vec<SymExpr> = method
                .node
                .argument
                .node
                .assignments
                .iter()
                .map(|a| expression_to_sym_expr(&a.node.assignment, context))
                .collect::<Result<_, _>>()?;

            let args_names: Vec<crate::execution::values::dictionary::ArgumentName> = method
                .node
                .argument
                .node
                .assignments
                .iter()
                .map(|a| a.node.name.clone())
                .collect();

            Ok(SymExpr::MethodCall { args_names, 
                method_name: method.node.to_call.node.clone(),
                self_expr: Box::new(self_expr),
                args,
            })
        }
        Expression::Parenthesis(inner) => expression_to_sym_expr(inner, context),
        Expression::Vector2(v) => {
            Ok(SymExpr::Vector(vec![
                expression_to_sym_expr(&v.node.x, context)?,
                expression_to_sym_expr(&v.node.y, context)?,
            ]))
        }
        Expression::Vector3(v) => {
            Ok(SymExpr::Vector(vec![
                expression_to_sym_expr(&v.node.x, context)?,
                expression_to_sym_expr(&v.node.y, context)?,
                expression_to_sym_expr(&v.node.z, context)?,
            ]))
        }
        Expression::Vector4(v) => {
            Ok(SymExpr::Vector(vec![
                expression_to_sym_expr(&v.node.x, context)?,
                expression_to_sym_expr(&v.node.y, context)?,
                expression_to_sym_expr(&v.node.z, context)?,
                expression_to_sym_expr(&v.node.w, context)?,
            ]))
        }
        Expression::MemberAccess(member_access) => {
            let base = expression_to_sym_expr(&member_access.node.base, context)?;
            Ok(SymExpr::MemberAccess {
                base: Box::new(base),
                member: member_access.node.member.node.clone(),
            })
        }
        Expression::ClosureDefinition(_)
        | Expression::DictionaryConstruction(_)
        | Expression::If(_)
        | Expression::List(_)
        | Expression::Self_(_)
        | Expression::String(_)
        | Expression::StructDefinition(_)
        | Expression::FunctionCall(_)
        | Expression::LetIn(_)
        | Expression::Malformed(_) => {
            Err(SolveError::NonInvertibleOperation {
                operation: format!("{:?}", expr.node),
                source: expr.reference.clone(),
            }
            .to_error(context))
        }
    }
}

/// Compute the ValueType of an expression AST node by walking the tree.
/// This is used to infer the return type of a closure body for inverse typing.
pub fn ast_return_type(expr: &AstNode<Expression>) -> crate::execution::values::ValueType {
    match &expr.node {
        Expression::Scalar(s) => crate::execution::values::ValueType::Scalar(Some(s.node.dimension)),
        Expression::SignedInteger(_) => crate::execution::values::ValueType::SignedInteger,
        Expression::UnsignedInteger(_) => crate::execution::values::ValueType::UnsignedInteger,
        Expression::Boolean(_) => crate::execution::values::ValueType::Boolean,
        Expression::String(_) => crate::execution::values::ValueType::String,
        Expression::Identifier(_) => {
            crate::execution::values::ValueType::Scalar(None)
        }
        Expression::BinaryExpression(binop) => {
            let left_type = ast_return_type(&binop.node.a);
            let right_type = ast_return_type(&binop.node.b);
            match binop.node.operation.node {
                crate::compile::BinaryExpressionOperation::Add
                | crate::compile::BinaryExpressionOperation::Sub => {
                    // Add/sub: dimensions must match, return left's dimension
                    if let (
                        crate::execution::values::ValueType::Scalar(Some(l)),
                        crate::execution::values::ValueType::Scalar(Some(r)),
                    ) = (&left_type, &right_type)
                    {
                        if l == r {
                            crate::execution::values::ValueType::Scalar(Some(*l))
                        } else {
                            crate::execution::values::ValueType::Scalar(None)
                        }
                    } else {
                        crate::execution::values::ValueType::Scalar(None)
                    }
                }
                crate::compile::BinaryExpressionOperation::Mul => {
                    // Mul: multiply dimensions (e.g., Length * Length = Area)
                    if let (
                        crate::execution::values::ValueType::Scalar(Some(l)),
                        crate::execution::values::ValueType::Scalar(Some(r)),
                    ) = (&left_type, &right_type)
                    {
                        crate::execution::values::ValueType::Scalar(Some(*l + *r))
                    } else {
                        crate::execution::values::ValueType::Scalar(None)
                    }
                }
                crate::compile::BinaryExpressionOperation::Div => {
                    // Div: divide dimensions (e.g., Area / Length = Length)
                    if let (
                        crate::execution::values::ValueType::Scalar(Some(l)),
                        crate::execution::values::ValueType::Scalar(Some(r)),
                    ) = (&left_type, &right_type)
                    {
                        crate::execution::values::ValueType::Scalar(Some(*l - *r))
                    } else {
                        crate::execution::values::ValueType::Scalar(None)
                    }
                }
                crate::compile::BinaryExpressionOperation::MulMul => {
                    // Pow: base keeps its dimension, exponent must be zero-dimension
                    if let crate::execution::values::ValueType::Scalar(Some(dim)) = &left_type {
                        if let (
                            crate::execution::values::ValueType::Scalar(Some(exp_dim)),
                        ) = (&right_type,)
                        {
                            if exp_dim.is_zero_dimension() {
                                crate::execution::values::ValueType::Scalar(Some(*dim))
                            } else {
                                crate::execution::values::ValueType::Scalar(None)
                            }
                        } else {
                            crate::execution::values::ValueType::Scalar(None)
                        }
                    } else {
                        crate::execution::values::ValueType::Scalar(None)
                    }
                }
                _ => crate::execution::values::ValueType::Scalar(None),
            }
        }
        Expression::UnaryExpression(unary) => {
            // Unary ops don't change dimension
            ast_return_type(&unary.node.expression)
        }
        Expression::MethodCall(method) => {
            let method_name = &method.node.to_call.node;
            // Default to Scalar for method calls — type will be validated at runtime
            if method_name == "to_signed_integer" || method_name == "to_unsigned_integer" {
                crate::execution::values::ValueType::SignedInteger
            } else {
                crate::execution::values::ValueType::Scalar(None)
            }
        }
        Expression::Parenthesis(inner) => ast_return_type(inner),
        Expression::MemberAccess(_) => crate::execution::values::ValueType::Scalar(None),
        Expression::ClosureDefinition(_)
        | Expression::DictionaryConstruction(_)
        | Expression::If(_)
        | Expression::List(_)
        | Expression::Self_(_)
        | Expression::StructDefinition(_)
        | Expression::FunctionCall(_)
        | Expression::LetIn(_)
        | Expression::Malformed(_) => crate::execution::values::ValueType::Scalar(None),
        Expression::Vector2(_) => {
            crate::execution::values::ValueType::Vector2(None)
        }
        Expression::Vector3(_) => {
            crate::execution::values::ValueType::Vector3(None)
        }
        Expression::Vector4(_) => {
            crate::execution::values::ValueType::Vector4(None)
        }
    }
}

/// Trace the path from root to target, applying inverse operations.
fn trace_and_inverse(
    context: &ExecutionContext,
    body: &SymExpr,
    target: &str,
    result_name: &ImString,
    db: &crate::execution::values::BuiltinCallableDatabase,
) -> ExecutionResult<SymExpr> {
    trace_inner(context, body, target, result_name, db, &mut SymExpr::Var(result_name.clone()))
}

#[allow(clippy::only_used_in_recursion)]
fn trace_inner(
    context: &ExecutionContext,
    current: &SymExpr,
    target: &str,
    result_name: &ImString,
    db: &crate::execution::values::BuiltinCallableDatabase,
    accumulated: &mut SymExpr,
) -> ExecutionResult<SymExpr> {
    match current {
        SymExpr::Var(v) => {
            if v == target {
                // Found the target — return the accumulated inverse expression
                Ok(accumulated.clone())
            } else {
                // Not the target — this shouldn't happen if the tree is well-formed
                Err(SolveError::VariableNotFound {
                    variable: v.clone(),
                }
                .to_error(
                    context,
                ))
            }
        }
        SymExpr::BinOp(op, left, right) => {
            let left_contains = sym_expr_contains_var(left, target);
            let right_contains = sym_expr_contains_var(right, target);

            if !left_contains && !right_contains {
                return Err(SolveError::VariableNotFound {
                    variable: target.into(),
                }
                .to_error(
                    context,
                ));
            }

            if left_contains && right_contains {
                return Err(SolveError::VariableAppearsMultipleTimes {
                    variable: target.into(),
                }
                .to_error(
                    context,
                ));
            }

            if !op.is_invertible() {
                return Err(SolveError::NonInvertibleOperation {
                    operation: format!("{:?}", op),
                    source: crate::compile::SourceReference {
                        file: std::sync::Arc::new(std::path::PathBuf::from("solve")),
                        range: tree_sitter::Range { start_byte: 0, end_byte: 0, start_point: tree_sitter::Point { row: 0, column: 0 }, end_point: tree_sitter::Point { row: 0, column: 0 } },
                    },
                }
                .to_error(
                    context,
                ));
            }

            let new_accumulated = if left_contains {
                op.isolate(true, right)
            } else {
                op.isolate(false, left)
            };

            // Replace __result__ in the new accumulated expression with what we've built so far
            let mut new_accumulated = substitute_result(&new_accumulated, accumulated);

            let next = if left_contains { left.as_ref() } else { right.as_ref() };
            trace_inner(context, next, target, result_name, db, &mut new_accumulated)
        }
        SymExpr::BoolOp(op, _, _) => {
            Err(SolveError::NonInvertibleOperation {
                operation: format!("{:?}", op),
                source: crate::compile::SourceReference {
                    file: std::sync::Arc::new(std::path::PathBuf::from("solve")),
                    range: tree_sitter::Range { start_byte: 0, end_byte: 0, start_point: tree_sitter::Point { row: 0, column: 0 }, end_point: tree_sitter::Point { row: 0, column: 0 } },
                },
            }
            .to_error(
                context,
            ))
        }
        SymExpr::UnaryOp(op, inner) => {
            if !op.is_invertible() {
                return Err(SolveError::NonInvertibleOperation {
                    operation: format!("{:?}", op),
                    source: crate::compile::SourceReference {
                        file: std::sync::Arc::new(std::path::PathBuf::from("solve")),
                        range: tree_sitter::Range { start_byte: 0, end_byte: 0, start_point: tree_sitter::Point { row: 0, column: 0 }, end_point: tree_sitter::Point { row: 0, column: 0 } },
                    },
                }
                .to_error(
                    context,
                ));
            }

            let new_accumulated = op.isolate(true, inner);
            let mut new_accumulated = substitute_result(&new_accumulated, accumulated);

            trace_inner(context, inner.as_ref(), target, result_name, db, &mut new_accumulated)
        }
        SymExpr::MethodCall {
            method_name,
            self_expr,
            args,
            args_names: _,
            ..
        } => {
            // Check if target is in self_expr or args
            let self_contains = sym_expr_contains_var(self_expr, target);
            let arg_contains: Vec<_> = args
                .iter()
                .enumerate()
                .filter(|(_, a)| sym_expr_contains_var(a, target))
                .collect();

            if !self_contains && arg_contains.is_empty() {
                return Err(SolveError::VariableNotFound {
                    variable: target.into(),
                }
                .to_error(
                    context,
                ));
            }

            if self_contains {
                // Target is in the receiver — apply method inverse
                let inverse_type_id = match get_method_inverse_callable(db, method_name) {
                    Some(id) => id,
                    None => {
                        return Err(SolveError::NonInvertibleOperation {
                            operation: method_name.to_string(),
                            source: crate::compile::SourceReference {
                                file: std::sync::Arc::new(std::path::PathBuf::from("solve")),
                                range: tree_sitter::Range { start_byte: 0, end_byte: 0, start_point: tree_sitter::Point { row: 0, column: 0 }, end_point: tree_sitter::Point { row: 0, column: 0 } },
                            },
                        }
                        .to_error(
                            context,
                        ));
                    }
                };

                let inverse_method_name = match get_method_name(db, inverse_type_id) {
                    Some(name) => name,
                    None => {
                        return Err(SolveError::NonInvertibleOperation {
                            operation: method_name.to_string(),
                            source: crate::compile::SourceReference {
                                file: std::sync::Arc::new(std::path::PathBuf::from("solve")),
                                range: tree_sitter::Range { start_byte: 0, end_byte: 0, start_point: tree_sitter::Point { row: 0, column: 0 }, end_point: tree_sitter::Point { row: 0, column: 0 } },
                            },
                        }
                        .to_error(
                            context,
                        ));
                    }
                };

                // Generate the correct args for the inverse method
                let (inv_args, inv_args_names): (Vec<SymExpr>, Vec<crate::execution::values::dictionary::ArgumentName>) = 
                    match inverse_method_name.as_str() {
                        "pow" => {
                            // Inverse of sqrt is pow(__result__, 2)
                            (vec![SymExpr::Integer(2)], vec![crate::execution::values::dictionary::ArgumentName::Named("exp".into())])
                        }
                        "cbrt" => {
                            // Inverse of cbrt is pow(__result__, 3)
                            (vec![SymExpr::Integer(3)], vec![crate::execution::values::dictionary::ArgumentName::Named("exp".into())])
                        }
                        _ => (vec![], vec![]),
                    };

                // Wrap accumulated with the inverse method call
                let mut new_accumulated = SymExpr::MethodCall { args_names: inv_args_names, 
                    method_name: inverse_method_name,
                    self_expr: Box::new(accumulated.clone()),
                    args: inv_args,
                };

                trace_inner(context, self_expr.as_ref(), target, result_name, db, &mut new_accumulated)
            } else if let Some((arg_idx, _)) = arg_contains.first() {
                // Target is in an argument — this is complex and may not be invertible
                // For now, return an error
                Err(SolveError::NonInvertibleOperation {
                    operation: format!("method '{}' with target in arg {}", method_name, arg_idx),
                    source: crate::compile::SourceReference {
                        file: std::sync::Arc::new(std::path::PathBuf::from("solve")),
                        range: tree_sitter::Range { start_byte: 0, end_byte: 0, start_point: tree_sitter::Point { row: 0, column: 0 }, end_point: tree_sitter::Point { row: 0, column: 0 } },
                    },
                }
                .to_error(
                    context,
                ))
            } else {
                Err(SolveError::VariableNotFound {
                    variable: target.into(),
                }
                .to_error(
                    context,
                ))
            }
        }
        SymExpr::Scalar(_) | SymExpr::Integer(_) | SymExpr::Boolean(_) => {
            // Constants — shouldn't contain the target
            Err(SolveError::VariableNotFound {
                variable: target.into(),
            }
            .to_error(
                context,
            ))
        }
        SymExpr::Vector(comps) => {
            let mut first_result: Option<SymExpr> = None;
            for comp in comps {
                let result = trace_inner(context, comp, target, result_name, db, &mut SymExpr::Var(result_name.clone()))?;
                match &first_result {
                    Some(first) => {
                        if !sym_exprs_equal(first, &result) {
                            return Err(SolveError::NonInvertibleOperation {
                                operation: format!("vector with inconsistent component solutions for '{}'", target),
                                source: crate::compile::SourceReference {
                                    file: std::sync::Arc::new(std::path::PathBuf::from("solve")),
                                    range: tree_sitter::Range { start_byte: 0, end_byte: 0, start_point: tree_sitter::Point { row: 0, column: 0 }, end_point: tree_sitter::Point { row: 0, column: 0 } },
                                },
                            }
                            .to_error(context))
                        }
                    }
                    None => { first_result = Some(result); }
                }
            }
            first_result.ok_or_else(|| SolveError::VariableNotFound {
                variable: target.into(),
            }.to_error(context))
        }
        SymExpr::MemberAccess { base, member } => {
            let mut new_accumulated = SymExpr::MemberAccess {
                base: Box::new(accumulated.clone()),
                member: member.clone(),
            };
            trace_inner(context, base.as_ref(), target, result_name, db, &mut new_accumulated)
        }
    }
}

/// Substitute __result__ in an expression with the given value.
pub(crate) fn substitute_result(expr: &SymExpr, replacement: &SymExpr) -> SymExpr {
    match expr {
        SymExpr::Var(v) if v == "__result__" => replacement.clone(),
        SymExpr::Var(v) => SymExpr::Var(v.clone()),
        SymExpr::Scalar(s) => SymExpr::Scalar(*s),
        SymExpr::Integer(i) => SymExpr::Integer(*i),
        SymExpr::Boolean(b) => SymExpr::Boolean(*b),
        SymExpr::BinOp(op, left, right) => SymExpr::BinOp(
            op.clone(),
            Box::new(substitute_result(left, replacement)),
            Box::new(substitute_result(right, replacement)),
        ),
        SymExpr::BoolOp(op, left, right) => SymExpr::BoolOp(
            op.clone(),
            Box::new(substitute_result(left, replacement)),
            Box::new(substitute_result(right, replacement)),
        ),
        SymExpr::UnaryOp(op, inner) => {
            SymExpr::UnaryOp(op.clone(), Box::new(substitute_result(inner, replacement)))
        }
        SymExpr::MethodCall {
            method_name,
            self_expr,
            args,
            args_names,
        } => SymExpr::MethodCall {
            args_names: args_names.clone(),
            method_name: method_name.clone(),
            self_expr: Box::new(substitute_result(self_expr, replacement)),
            args: args
                .iter()
                .map(|a| substitute_result(a, replacement))
                .collect(),
        },
 SymExpr::Vector(comps) => SymExpr::Vector(
             comps.iter().map(|c| substitute_result(c, replacement)).collect()
         ),
        SymExpr::MemberAccess { base, member } => SymExpr::MemberAccess {
            base: Box::new(substitute_result(base, replacement)),
            member: member.clone(),
        },
    }
}

/// Check if two SymExpr trees are structurally equal.
pub(crate) fn sym_exprs_equal(a: &SymExpr, b: &SymExpr) -> bool {
    match (a, b) {
        (SymExpr::Var(va), SymExpr::Var(vb)) => va == vb,
        (SymExpr::Scalar(sa), SymExpr::Scalar(sb)) => sa == sb,
        (SymExpr::Integer(ia), SymExpr::Integer(ib)) => ia == ib,
        (SymExpr::Boolean(ba), SymExpr::Boolean(bb)) => ba == bb,
        (SymExpr::BinOp(opa, la, ra), SymExpr::BinOp(opb, lb, rb)) => {
            opa == opb && sym_exprs_equal(la, lb) && sym_exprs_equal(ra, rb)
        }
        (SymExpr::BoolOp(opa, la, ra), SymExpr::BoolOp(opb, lb, rb)) => {
            opa == opb && sym_exprs_equal(la, lb) && sym_exprs_equal(ra, rb)
        }
        (SymExpr::UnaryOp(opa, ia), SymExpr::UnaryOp(opb, ib)) => {
            opa == opb && sym_exprs_equal(ia, ib)
        }
        (SymExpr::MethodCall { method_name: ma, self_expr: sa, args: aa, args_names: ana },
         SymExpr::MethodCall { method_name: mb, self_expr: sb, args: ab, args_names: anb }) => {
            ma == mb && sym_exprs_equal(sa, sb) && ana == anb && 
            aa.len() == ab.len() && aa.iter().zip(ab.iter()).all(|(x, y)| sym_exprs_equal(x, y))
        }
        (SymExpr::Vector(va), SymExpr::Vector(vb)) => {
            va.len() == vb.len() && va.iter().zip(vb.iter()).all(|(x, y)| sym_exprs_equal(x, y))
        }
        (SymExpr::MemberAccess { base: ba, member: ma }, SymExpr::MemberAccess { base: bb, member: mb }) => {
            ma == mb && sym_exprs_equal(ba, bb)
        }
        _ => false,
    }
}

/// Evaluate a constant SymExpr (one with no free variables) to a Value.
fn evaluate_constant_expr(
    expr: &SymExpr,
    context: &ExecutionContext,
) -> ExecutionResult<Value> {
    match expr {
        SymExpr::Scalar(s) => Ok(Value::Scalar(RuntimeScalar {
            dimension: s.dimension,
            value: s.value,
        })),
        SymExpr::Integer(i) => Ok(Value::SignedInteger(Integer(*i))),
        SymExpr::Boolean(b) => Ok(Value::Boolean(*b)),
        SymExpr::Var(v) => Err(SolveError::VariableNotFound {
            variable: v.clone(),
        }
        .to_error(context)),
        SymExpr::BinOp(op, left, right) => {
            let left_val = evaluate_constant_expr(left, context)?;
            let right_val = evaluate_constant_expr(right, context)?;
            match op {
                BinOp::Add => left_val.addition(context, right_val),
                BinOp::Sub => left_val.subtraction(context, right_val),
                BinOp::Mul => left_val.multiply(context, right_val),
                BinOp::Div => left_val.divide(context, right_val),
                BinOp::Pow => left_val.exponent(context, right_val),
            }
        }
        SymExpr::BoolOp(op, left, right) => {
            let left_val = evaluate_constant_expr(left, context)?;
            let right_val = evaluate_constant_expr(right, context)?;
            match op {
                BoolOp::And => left_val.and(context, right_val),
                BoolOp::Or => left_val.or(context, right_val),
            }
        }
        SymExpr::UnaryOp(op, inner) => {
            let inner_val = evaluate_constant_expr(inner, context)?;
            match op {
                UnaryOp::Neg => inner_val.unary_minus(context),
                UnaryOp::Not => inner_val.unary_not(context),
            }
        }
        SymExpr::MethodCall {
            method_name,
            self_expr,
            args,
            ..
        } => {
            let self_val = evaluate_constant_expr(self_expr, context)?;
            let arg_vals: Vec<Value> = args
                .iter()
                .map(|a| evaluate_constant_expr(a, context))
                .collect::<Result<_, _>>()?;
            let arg_dict = Dictionary::new(
                context,
                arg_vals
                    .into_iter()
                    .enumerate()
                    .map(|(i, v)| (ArgumentName::Positional(i), v))
                    .collect::<IndexMap<_, _>>(),
            );
            self_val.get_attribute(context, method_name)?.call(context, arg_dict)
        }
        SymExpr::Vector(comps) => {
            let vals: Vec<common_data_types::Float> = comps
                .iter()
                .map(|c| {
                    let v = evaluate_constant_expr(c, context).unwrap();
                    match v {
                        Value::Scalar(s) => s.value,
                        _ => common_data_types::Float::new(0.0).unwrap(),
                    }
                })
                .collect();
            
            let dim = comps.first()
                .and_then(|c| {
                    match c {
                        SymExpr::Scalar(s) => Some(s.dimension),
                        _ => None,
                    }
                })
                .unwrap_or_else(common_data_types::Dimension::zero);
            
            if vals.len() == 2 {
                Ok(Value::Vector2(super::super::super::vector::Vector2 {
                    dimension: dim,
                    value: nalgebra::Vector2::new(vals[0].into_inner(), vals[1].into_inner()),
                }))
            } else if vals.len() == 3 {
                Ok(Value::Vector3(super::super::super::vector::Vector3 {
                    dimension: dim,
                    value: nalgebra::Vector3::new(vals[0].into_inner(), vals[1].into_inner(), vals[2].into_inner()),
                }))
            } else if vals.len() == 4 {
                Ok(Value::Vector4(super::super::super::vector::Vector4 {
                    dimension: dim,
                    value: nalgebra::Vector4::new(vals[0].into_inner(), vals[1].into_inner(), vals[2].into_inner(), vals[3].into_inner()),
                }))
            } else {
                Err(crate::execution::errors::Error {
                    ty: Box::new(crate::execution::errors::StringError(
                        format!("unsupported vector size: {}", vals.len())
                    )),
                    trace: vec![],
                    failure_chain: vec![],
                })
            }
        }
        SymExpr::MemberAccess { base, member } => {
            let base_val = evaluate_constant_expr(base, context)?;
            base_val.get_attribute(context, member.as_str())
        }
    }
}

/// Solve for a variable in a closure body.
#[allow(clippy::too_many_arguments)]
pub fn solve_for(
    context: &ExecutionContext,
    body: &AstNode<Expression>,
    _params: &[ImString],
    captured: &IndexMap<crate::execution::values::dictionary::ArgumentName, crate::execution::values::Value>,
    target: &ImString,
    result_name: ImString,
    _target_param_type: Option<crate::execution::values::ValueType>,
    return_type: Option<crate::execution::values::ValueType>,
    param_types: IndexMap<ImString, crate::execution::values::ValueType>,
) -> ExecutionResult<SolveResult> {
    // Convert AST to SymExpr
    let sym_body = expression_to_sym_expr(body, context)?;

    // Infer the actual type of the body expression (may differ from declared return type).
    let inferred_target_param_type = infer_sym_expr_type(&sym_body);

    // Simplify common non-linear patterns (x*x → x^2, x+x → x*2, etc.)
    let sym_body = super::simplify_sym_expr(&sym_body);

    // Try polynomial solving for degree 2-3 with mixed terms.
    // Pure squaring (x*x) and pure cubing (x*x*x) are handled by the old trace_and_inverse path.
    if let Some(poly) = super::extract_polynomial(&sym_body, target, &result_name) {
        let has_linear_or_lower = match poly.degree() {
            2 => {
                let coefficients = &poly.coefficients;
                let b = &coefficients[1];
                !is_zero_scalar(b)
            }
            3 => {
                let coefficients = &poly.coefficients;
                let b = &coefficients[2];
                let c = &coefficients[1];
                !is_zero_scalar(b) || !is_zero_scalar(c)
            }
            _ => false,
        };

        if has_linear_or_lower {
            let result = match poly.degree() {
                2 => {
                    let coefficients = &poly.coefficients;
                    let a = &coefficients[2];
                    let b = &coefficients[1];
                    let c = &coefficients[0];
                    super::solve_quadratic(a, b, c, context)?
                }
                3 => {
                    let coefficients = &poly.coefficients;
                    let a = &coefficients[3];
                    let b = &coefficients[2];
                    let c = &coefficients[1];
                    let d = &coefficients[0];
                    super::solve_cubic(a, b, c, d, context)?
                }
                _ => return Err(SolveError::NoSolution {
                    operation: format!("polynomial degree {} is not supported (max degree 3)", poly.degree()),
                    source: body.reference.clone(),
                }.to_error(context)),
            };
            let free_var_names: Vec<ImString> = collect_free_vars(&result, target);
            let captured: IndexMap<ImString, Value> = free_var_names
                .iter()
                .filter_map(|name| {
                    captured
                        .iter()
                        .find_map(|(arg_name, value)| {
                            if let ArgumentName::Named(n) = arg_name {
                                if n == name {
                                    Some((name.clone(), value.clone()))
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        })
                })
                .collect();

            return Ok(SolveResult {
                body: result,
                captured,
                result_name,
                target_param_name: target.clone(),
                target_param_type: Some(inferred_target_param_type),
                return_type: return_type.clone(),
                param_types: param_types.clone(),
            });
        }
    }

    // After simplification, check if the target was eliminated (e.g., x-x=0)
    if !sym_expr_contains_var(&sym_body, target) {
        let constant_value = evaluate_constant_expr(&sym_body, context)?;
        match constant_value.downcast_ref::<RuntimeScalar>(context) {
            Ok(scalar) => {
                if (*scalar.value).abs() < 1e-10 {
                    return Ok(SolveResult {
                        body: SymExpr::Var(result_name.clone()),
                        captured: IndexMap::new(),
                        result_name,
                        target_param_name: target.clone(),
                        target_param_type: Some(inferred_target_param_type.clone()),
                        return_type: return_type.clone(),
                        param_types: param_types.clone(),
                    });
                }
            }
            Err(_) => {
                if let Ok(integer) = constant_value.downcast_ref::<SignedInteger>(context) {
                    if integer.0 == 0 {
                        return Ok(SolveResult {
                            body: SymExpr::Var(result_name.clone()),
                            captured: IndexMap::new(),
                            result_name,
                            target_param_name: target.clone(),
                            target_param_type: Some(inferred_target_param_type.clone()),
                            return_type: return_type.clone(),
                            param_types: param_types.clone(),
                        });
                    }
                }
            }
        }
        return Err(SolveError::NoSolution {
            operation: format!(
                "expression simplifies to a non-zero constant (no solution for '{}')",
                target
            ),
            source: body.reference.clone(),
        }
        .to_error(context));
    }

    // Trace the path and apply inverses
    let db = context.database;
    let result = trace_and_inverse(context, &sym_body, target, &result_name, db)?;

    // Collect free variable names (captured from original closure)
    let free_var_names: Vec<ImString> = collect_free_vars(&result, target);

    // Look up captured values from the original closure
    let captured: IndexMap<ImString, Value> = free_var_names
        .iter()
        .filter_map(|name| {
            captured
                .iter()
                .find_map(|(arg_name, value)| {
                    if let ArgumentName::Named(n) = arg_name {
                        if n == name {
                            Some((name.clone(), value.clone()))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
        })
        .collect();

    Ok(SolveResult {
        body: result,
        captured,
        result_name,
        target_param_name: target.clone(),
        target_param_type: Some(inferred_target_param_type),
        return_type,
        param_types,
    })
}

#[cfg(test)]
#[allow(unused_imports)]
mod test {
    use super::*;
    use crate::compile::full_compile;
    use super::super::count_var_occurrences;
    use crate::execution::test_context;
    use common_data_types::Float;
    use crate::execution::values::scalar::Scalar;

    #[test]
    fn expression_to_sym_expr_scalar() {
        let root = full_compile("5.0");
        let sym = test_context([], |ctx| expression_to_sym_expr(&root, ctx)).unwrap();
        match sym {
            SymExpr::Scalar(s) => {
                assert_eq!(*s.value, 5.0);
            }
            _ => panic!("Expected Scalar, got {:?}", sym),
        }
    }

    #[test]
    fn expression_to_sym_expr_identifier() {
        let root = full_compile("a");
        let sym = test_context([], |ctx| expression_to_sym_expr(&root, ctx)).unwrap();
        match sym {
            SymExpr::Var(v) => assert_eq!(v.as_str(), "a"),
            _ => panic!("Expected Var, got {:?}", sym),
        }
    }

    #[test]
    fn expression_to_sym_expr_binary_add() {
        let root = full_compile("a + 1");
        let sym = test_context([], |ctx| expression_to_sym_expr(&root, ctx)).unwrap();
        match sym {
            SymExpr::BinOp(BinOp::Add, left, right) => {
                assert!(matches!(left.as_ref(), SymExpr::Var(_)));
                assert!(matches!(right.as_ref(), SymExpr::Scalar(_)));
            }
            _ => panic!("Expected BinOp(Add, ...), got {:?}", sym),
        }
    }

    #[test]
    fn expression_to_sym_expr_method_call() {
        let root = full_compile("a::sin()");
        let sym = test_context([], |ctx| expression_to_sym_expr(&root, ctx)).unwrap();
        match sym {
       SymExpr::MethodCall {
            method_name,
            self_expr,
            args,
            args_names: _,
            ..
        } => {
                assert_eq!(method_name.as_str(), "sin");
                assert!(matches!(self_expr.as_ref(), SymExpr::Var(_)));
                assert!(args.is_empty());
            }
            _ => panic!("Expected MethodCall, got {:?}", sym),
        }
    }

    #[test]
    fn expression_to_sym_expr_negation() {
        let root = full_compile("-a");
        let sym = test_context([], |ctx| expression_to_sym_expr(&root, ctx)).unwrap();
        match sym {
            SymExpr::UnaryOp(UnaryOp::Neg, inner) => {
                assert!(matches!(inner.as_ref(), SymExpr::Var(_)));
            }
            _ => panic!("Expected UnaryOp(Neg, ...), got {:?}", sym),
        }
    }

    #[test]
    fn substitute_result_basic() {
        let expr = SymExpr::Var("__result__".into());
        let replacement = SymExpr::Scalar(Scalar {
            dimension: common_data_types::Dimension::zero(),
            value: Float::new(5.0).unwrap(),
        });
        let result = substitute_result(&expr, &replacement);
        assert_eq!(result, replacement);
    }

    #[test]
    fn substitute_result_in_binop() {
        let expr = SymExpr::BinOp(
            BinOp::Add,
            Box::new(SymExpr::Var("__result__".into())),
            Box::new(SymExpr::Scalar(Scalar {
                dimension: common_data_types::Dimension::zero(),
                value: Float::new(1.0).unwrap(),
            })),
        );
        let replacement = SymExpr::Var("a".into());
        let result = substitute_result(&expr, &replacement);
        match result {
            SymExpr::BinOp(BinOp::Add, left, right) => {
                assert_eq!(*left, SymExpr::Var("a".into()));
                assert!(matches!(right.as_ref(), SymExpr::Scalar(_)));
            }
            _ => panic!("Expected BinOp, got {:?}", result),
        }
    }

    #[test]
    fn substitute_result_in_method_call() {
        let expr = SymExpr::MethodCall { args_names: vec![], 
            method_name: "sin".into(),
            self_expr: Box::new(SymExpr::Var("__result__".into())),
            args: vec![],
        };
        let replacement = SymExpr::Var("a".into());
        let result = substitute_result(&expr, &replacement);
        match result {
            SymExpr::MethodCall { self_expr, .. } => {
                assert_eq!(*self_expr, SymExpr::Var("a".into()));
            }
            _ => panic!("Expected MethodCall, got {:?}", result),
        }
    }

    #[test]
    fn count_var_occurrences_in_nested() {
        let root = full_compile("(a + 1) * 2");
        let sym = test_context([], |ctx| expression_to_sym_expr(&root, ctx)).unwrap();
        assert_eq!(count_var_occurrences(&sym, "a"), 1);
    }

    #[test]
    fn count_var_occurrences_nonlinear() {
        let root = full_compile("a + a");
        let sym = test_context([], |ctx| expression_to_sym_expr(&root, ctx)).unwrap();
        assert_eq!(count_var_occurrences(&sym, "a"), 2);
    }

    #[test]
    fn solve_x_squared_simplifies() {
        // x*x should simplify to x^2, then solve to pow(result_name, 2)
        // Note: the Pow inverse wraps with pow(__result__, exponent) — a pre-existing behavior
        let root = full_compile("x * x");
        test_context([], |ctx| {
            let result = solve_for(
                ctx,
                &root,
                &["x".into()],
                &IndexMap::new(),
                &"x".into(),
                "original_result".into(),
                None,

                None,
                IndexMap::new(),
            );
            assert!(result.is_ok());
            let solve_result = result.unwrap();
            // Should be MethodCall("sqrt", original_result)
            match solve_result.body {
                SymExpr::MethodCall {
                    method_name,
                    self_expr,
                    args,
                    ..
                } => {
                    assert_eq!(method_name.as_str(), "sqrt");
                    assert!(matches!(
                        *self_expr,
                        SymExpr::Var(v) if v == "original_result"
                    ));
                    assert_eq!(args.len(), 0);
                }
                _ => panic!("Expected MethodCall(sqrt, ...), got {:?}", solve_result.body),
            }
        });
    }

    #[test]
    fn solve_x_doubled_simplifies() {
        // x+x should simplify to x*2, then solve to original_result/2
        let root = full_compile("x + x");
        test_context([], |ctx| {
            let result = solve_for(
                ctx,
                &root,
                &["x".into()],
                &IndexMap::new(),
                &"x".into(),
                "original_result".into(),
                None,

                None,
                IndexMap::new(),
            );
            assert!(result.is_ok());
            let solve_result = result.unwrap();
            // Should be Div(original_result, 2)
            match solve_result.body {
                SymExpr::BinOp(BinOp::Div, left, right) => {
                    assert!(matches!(
                        left.as_ref(),
                        SymExpr::Var(v) if v == "original_result"
                    ));
                    assert!(matches!(right.as_ref(), SymExpr::Integer(2)));
                }
                _ => panic!("Expected BinOp(Div, ...), got {:?}", solve_result.body),
            }
        });
    }

    #[test]
    fn solve_x_squared_already_linear() {
        // x*2 should work without simplification
        let root = full_compile("x * 2");
        test_context([], |ctx| {
            let result = solve_for(
                ctx,
                &root,
                &["x".into()],
                &IndexMap::new(),
                &"x".into(),
                "original_result".into(),
                None,

                None,
                IndexMap::new(),
            );
            assert!(result.is_ok());
            let solve_result = result.unwrap();
            // Should be Div(original_result, Scalar(2.0))
            match solve_result.body {
                SymExpr::BinOp(BinOp::Div, left, right) => {
                    assert!(matches!(
                        left.as_ref(),
                        SymExpr::Var(v) if v == "original_result"
                    ));
                    assert!(matches!(right.as_ref(), SymExpr::Scalar(_)));
                }
                _ => panic!("Expected BinOp(Div, ...), got {:?}", solve_result.body),
            }
        });
    }

    #[test]
    fn solve_eliminated_variable_identity() {
        // x - x simplifies to 0, so any value of x works
        let root = full_compile("x - x");
        test_context([], |ctx| {
            let result = solve_for(
                ctx,
                &root,
                &["x".into()],
                &IndexMap::new(),
                &"result".into(),
                "original_result".into(),
                None,

                None,
                IndexMap::new(),
            );
            assert!(result.is_ok());
            let solve_result = result.unwrap();
            // Should be an identity: just return the result_name
            assert!(matches!(solve_result.body, SymExpr::Var(v) if v == "original_result"));
            assert!(solve_result.captured.is_empty());
        });
    }

    #[test]
    fn solve_eliminated_variable_nonzero() {
        // x - x + 5 simplifies to 5, which is non-zero
        let root = full_compile("x - x + 5");
        test_context([], |ctx| {
            let result = solve_for(
                ctx,
                &root,
                &["x".into()],
                &IndexMap::new(),
                &"result".into(),
                "original_result".into(),
                None,

                None,
                IndexMap::new(),
            );
            assert!(result.is_err());
            let err = result.unwrap_err();
            // Should be a NoSolution error
            let err_str = format!("{}", err.ty);
            assert!(err_str.contains("no solution"));
        });
    }

    #[test]
    fn solve_eliminated_variable_integer_zero() {
        // x - x simplifies to integer 0
        let root = full_compile("x - x");
        test_context([], |ctx| {
            let result = solve_for(
                ctx,
                &root,
                &["x".into()],
                &IndexMap::new(),
                &"result".into(),
                "original_result".into(),
                None,

                None,
                IndexMap::new(),
            );
            assert!(result.is_ok());
            let solve_result = result.unwrap();
            assert!(matches!(solve_result.body, SymExpr::Var(v) if v == "original_result"));
        });
    }

    #[test]
    fn solve_method_call_inverse_sin() {
        // x::sin() for x should give Scalar::asin(original_result)
        let root = full_compile("x::sin()");
        test_context([], |ctx| {
            let result = solve_for(
                ctx,
                &root,
                &["x".into()],
                &IndexMap::new(),
                &"x".into(),
                "original_result".into(),
                None,

                None,
                IndexMap::new(),
            );
            assert!(result.is_ok());
            let solve_result = result.unwrap();
            match solve_result.body {
                SymExpr::MethodCall {
                    method_name,
                    self_expr,
                    args,
                    ..
                } => {
                    assert_eq!(method_name.as_str(), "asin");
                    assert!(matches!(
                        *self_expr,
                        SymExpr::Var(v) if v == "original_result"
                    ));
                    assert!(args.is_empty());
                }
                _ => panic!("Expected MethodCall(Scalar::asin, ...), got {:?}", solve_result.body),
            }
        });
    }

    #[test]
    fn solve_method_call_inverse_with_addition() {
        // x::sin() + 1 for x should give Scalar::asin(original_result - 1)
        let root = full_compile("x::sin() + 1");
        test_context([], |ctx| {
            let result = solve_for(
                ctx,
                &root,
                &["x".into()],
                &IndexMap::new(),
                &"x".into(),
                "original_result".into(),
                None,

                None,
                IndexMap::new(),
            );
            assert!(result.is_ok());
            let solve_result = result.unwrap();
            match solve_result.body {
                SymExpr::MethodCall {
                    method_name,
                    self_expr,
                    args,
                    ..
                } => {
                    assert_eq!(method_name.as_str(), "asin");
                    assert!(matches!(
                        *self_expr,
                        SymExpr::BinOp(BinOp::Sub, _, _)
                    ));
                    assert!(args.is_empty());
                }
                _ => panic!("Expected MethodCall(Scalar::asin, ...), got {:?}", solve_result.body),
            }
        });
    }

    #[test]
    fn solve_method_call_inverse_with_captured_variable() {
        // x::sin() + factor for x should give Scalar::asin(original_result - factor)
        let root = full_compile("x::sin() + factor");
        let captured: IndexMap<crate::execution::values::dictionary::ArgumentName, crate::execution::values::Value> = {
            let mut map = IndexMap::new();
            use crate::execution::values::{scalar::Scalar as RuntimeScalar, dictionary::ArgumentName};
            map.insert(
                ArgumentName::Named("factor".into()),
                RuntimeScalar {
                    dimension: common_data_types::Dimension::zero(),
                    value: Float::new(2.0).unwrap(),
                }.into(),
            );
            map
        };
        test_context([], |ctx| {
            let result = solve_for(
                ctx,
                &root,
                &["x".into()],
                &captured,
                &"x".into(),
                "original_result".into(),
                None,

                None,
                IndexMap::new(),
            );
            assert!(result.is_ok());
            let solve_result = result.unwrap();
            match solve_result.body {
                SymExpr::MethodCall {
                    method_name,
                    self_expr,
                    args,
                    ..
                } => {
                    assert_eq!(method_name.as_str(), "asin");
                    assert!(matches!(
                        *self_expr,
                        SymExpr::BinOp(BinOp::Sub, _, _)
                    ));
                    assert!(args.is_empty());
                }
                _ => panic!("Expected MethodCall(Scalar::asin, ...), got {:?}", solve_result.body),
            }
            // The captured variable should be preserved
            assert_eq!(solve_result.captured.len(), 1);
        });
    }

    #[test]
    fn solve_method_call_inverse_sinh() {
        // x::sinh() for x should give Scalar::asinh(original_result)
        let root = full_compile("x::sinh()");
        test_context([], |ctx| {
            let result = solve_for(
                ctx,
                &root,
                &["x".into()],
                &IndexMap::new(),
                &"x".into(),
                "original_result".into(),
                None,

                None,
                IndexMap::new(),
            );
            assert!(result.is_ok());
            let solve_result = result.unwrap();
            match solve_result.body {
                SymExpr::MethodCall {
                    method_name,
                    self_expr,
                    args,
                    ..
                } => {
                    assert_eq!(method_name.as_str(), "asinh");
                    assert!(matches!(
                        *self_expr,
                        SymExpr::Var(v) if v == "original_result"
                    ));
                    assert!(args.is_empty());
                }
                _ => panic!("Expected MethodCall(Scalar::asinh, ...), got {:?}", solve_result.body),
            }
        });
    }

    #[test]
    fn solve_method_call_inverse_not_found() {
        // x::pow(y) for x should work (pow has inverse via recip for exponent)
        // but x::cossin() for x should fail (no single inverse)
        let root = full_compile("x::cossin()");
        test_context([], |ctx| {
            let result = solve_for(
                ctx,
                &root,
                &["x".into()],
                &IndexMap::new(),
                &"x".into(),
                "original_result".into(),
                None,

                None,
                IndexMap::new(),
            );
            assert!(result.is_err());
            let err = result.unwrap_err();
            let err_str = format!("{}", err.ty);
            assert!(err_str.contains("not invertible"));
        });
    }
}
