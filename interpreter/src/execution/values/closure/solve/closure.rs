use std::sync::Arc;

use imstr::ImString;
use indexmap::IndexMap;
use std::path::PathBuf;

use crate::{
    compile::{AstNode, Expression},
    execution::{
        errors::ExecutionResult,
        values::{
            closure::{Signature, UserClosure, UserClosureInternals},
            dictionary::ArgumentName,
            Object, Value, ValueType,
        },
        ExecutionContext,
    },
};

use super::SolveResult;

impl SolveResult {
    /// Convert the SolveResult into a UserClosure.
    pub fn into_closure(
        self,
        context: &ExecutionContext,
        _original: &UserClosure,
    ) -> ExecutionResult<UserClosure> {
    // Build the argument type for the inverse closure
        // The inverse closure takes: result_name (one param) + captured vars
        let mut members: IndexMap<ArgumentName, crate::execution::values::value_type::StructMember> = IndexMap::new();

        // Add the result parameter — use the stored target_param_type if available,
        // otherwise fall back to inferring from the body expression.
        let result_param_type = self.target_param_type.clone().unwrap_or_else(|| infer_sym_expr_type(&self.body));
        members.insert(
            ArgumentName::Named(self.result_name.clone()),
            crate::execution::values::value_type::StructMember {
                ty: result_param_type.clone(),
                default: None,
            },
        );

        // Add captured variables as parameters with their actual types.
        // For variables that are parameters of the original closure, use param_types.
        // For actual captured variables from enclosing scopes, use get_type().
        let mut captured_names: IndexMap<ImString, ValueType> = IndexMap::new();
        for (captured_name, captured_value) in &self.captured {
            let captured_type = self
                .param_types
                .get(captured_name)
                .cloned()
                .unwrap_or_else(|| captured_value.get_type(context));
            captured_names.insert(captured_name.clone(), captured_type);
        }

        // Also include parameters from the original closure that appear in the result expression.
        // These are parameters that weren't solved for and need to be passed to the inverse closure.
        // Exclude the result_name parameter (already added above) and the target parameter name.
        for (param_name, param_type) in &self.param_types {
            if !captured_names.contains_key(param_name)
                && param_name != &self.result_name
                && param_name != &self.target_param_name
            {
                captured_names.insert(param_name.clone(), param_type.clone());
            }
        }

        // Build the members map from the combined captured/parameter names
        for (name, ty) in captured_names {
            members.insert(
                ArgumentName::Named(name.clone()),
                crate::execution::values::value_type::StructMember {
                    ty,
                    default: None,
                },
            );
        }

        let argument_type = crate::execution::values::value_type::StructDefinition {
            members: Arc::new(members),
            variadic: false,
        };

        // Build the body expression from SymExpr
        let body_expr = sym_expr_to_expression(&self.body, context)?;

        // The return type of the inverse closure is the type of the target variable being solved for.
        // When solving f(x) = y for x, the inverse is g(y) = x, so the return type is x's type.
        let return_type = self.return_type.clone().unwrap_or_else(|| infer_sym_expr_type(&self.body));

        let signature = Arc::new(Signature {
            argument_type,
            return_type,
        });

        // Convert captured values to ArgumentName-keyed map
        let captured_values: IndexMap<ArgumentName, Value> = self
            .captured
            .into_iter()
            .map(|(name, value)| (ArgumentName::Named(name), value))
            .collect();

        Ok(UserClosure {
            data: Arc::new(UserClosureInternals {
                signature,
                captured_values,
                expression: Arc::new(body_expr),
            }),
        })
    }
}

/// Convert a SymExpr back to an Expression AST node.
fn sym_expr_to_expression(
    expr: &super::SymExpr,
    context: &ExecutionContext,
) -> ExecutionResult<AstNode<Expression>> {
    let file = Arc::new(PathBuf::from("solve"));
    let source_ref = context.stack_trace.bottom();

    match expr {
        super::SymExpr::Var(v) => Ok(AstNode::new_from_source(
            file.clone(),
            source_ref,
            Expression::Identifier(AstNode::new_from_source(
                file.clone(),
                source_ref,
                v.clone(),
            )),
        )),
        super::SymExpr::Scalar(s) => Ok(AstNode::new_from_source(
            file.clone(),
            source_ref,
            Expression::Scalar(AstNode::new_from_source(
                file.clone(),
                source_ref,
                crate::compile::Scalar {
                    dimension: s.dimension,
                    value: s.value,
                },
            )),
        )),
        super::SymExpr::Integer(i) => {
            let zero_dim = common_data_types::Dimension::zero();
            Ok(AstNode::new_from_source(
                file.clone(),
                source_ref,
                Expression::Scalar(AstNode::new_from_source(
                    file.clone(),
                    source_ref,
                    crate::compile::Scalar {
                        dimension: zero_dim,
                        value: common_data_types::Float::new(*i as f64).unwrap(),
                    },
                )),
            ))
        }
        super::SymExpr::Boolean(b) => Ok(AstNode::new_from_source(
            file.clone(),
            source_ref,
            Expression::Boolean(AstNode::new_from_source(
                file.clone(),
                source_ref,
                b.0,
            )),
        )),
        super::SymExpr::BinOp(op, left, right) => {
            let left_expr = sym_expr_to_expression(left, context)?;
            let right_expr = sym_expr_to_expression(right, context)?;

            let binop = match op {
                super::BinOp::Add => {
                    crate::compile::BinaryExpressionOperation::Add
                }
                super::BinOp::Sub => {
                    crate::compile::BinaryExpressionOperation::Sub
                }
                super::BinOp::Mul => {
                    crate::compile::BinaryExpressionOperation::Mul
                }
                super::BinOp::Div => {
                    crate::compile::BinaryExpressionOperation::Div
                }
                super::BinOp::Pow => {
                    crate::compile::BinaryExpressionOperation::MulMul
                }
            };

            Ok(AstNode::new_from_source(
                file.clone(),
                source_ref,
                Expression::BinaryExpression(AstNode::new_from_source(
                    file.clone(),
                    source_ref,
                    Box::new(crate::compile::BinaryExpression {
                        operation: AstNode::new_from_source(
                            file.clone(),
                            source_ref,
                            binop,
                        ),
                        a: left_expr,
                        b: right_expr,
                    }),
                )),
            ))
        }
        super::SymExpr::BoolOp(op, left, right) => {
            let left_expr = sym_expr_to_expression(left, context)?;
            let right_expr = sym_expr_to_expression(right, context)?;

            let binop = match op {
                super::BoolOp::And => {
                    crate::compile::BinaryExpressionOperation::AndAnd
                }
                super::BoolOp::Or => {
                    crate::compile::BinaryExpressionOperation::OrOr
                }
            };

            Ok(AstNode::new_from_source(
                file.clone(),
                source_ref,
                Expression::BinaryExpression(AstNode::new_from_source(
                    file.clone(),
                    source_ref,
                    Box::new(crate::compile::BinaryExpression {
                        operation: AstNode::new_from_source(
                            file.clone(),
                            source_ref,
                            binop,
                        ),
                        a: left_expr,
                        b: right_expr,
                    }),
                )),
            ))
        }
        super::SymExpr::UnaryOp(op, inner) => {
            let inner_expr = sym_expr_to_expression(inner, context)?;

            let unary_op = match op {
                super::UnaryOp::Neg => {
                    crate::compile::UnaryExpressionOperation::Sub
                }
                super::UnaryOp::Not => {
                    crate::compile::UnaryExpressionOperation::Not
                }
            };

            Ok(AstNode::new_from_source(
                file.clone(),
                source_ref,
                Expression::UnaryExpression(AstNode::new_from_source(
                    file.clone(),
                    source_ref,
                    Box::new(crate::compile::UnaryExpression {
                        operation: AstNode::new_from_source(
                            file.clone(),
                            source_ref,
                            unary_op,
                        ),
                        expression: inner_expr,
                    }),
                )),
            ))
        }
        super::SymExpr::MethodCall {
            method_name,
            self_expr,
            args,
            args_names,
        } => {
           let self_expr = sym_expr_to_expression(self_expr, context)?;
            let args_expr: Vec<AstNode<Expression>> = args
                .iter()
                .map(|a| sym_expr_to_expression(a, context))
                .collect::<Result<_, _>>()?;

            let num_args = args_expr.len();

            // Build a dictionary construction for the method arguments
            let assignments: Vec<AstNode<crate::compile::DictionaryMemberAssignment>> = args_expr
                .into_iter()
                .enumerate()
                .zip(args_names.iter().cloned())
                .map(|((i, expr), arg_name)| {
                    AstNode::new_from_source(
                        file.clone(),
                        source_ref,
                        crate::compile::DictionaryMemberAssignment {
                            index: i,
                            dependencies: Default::default(),
                            name: arg_name,
                            assignment: expr,
                        },
                    )
                })
                .collect();

            let dict_construction = crate::compile::DictionaryConstruction {
                assignments,
                #[allow(clippy::single_range_in_vec_init)]
                compute_groups: vec![0..num_args],
            };

            Ok(AstNode::new_from_source(
                file.clone(),
                source_ref,
                Expression::MethodCall(AstNode::new_from_source(
                    file.clone(),
                    source_ref,
                    Box::new(crate::compile::MethodCall {
                        self_dictionary: self_expr,
                        to_call: AstNode::new_from_source(
                            file.clone(),
                            source_ref,
                            method_name.clone(),
                        ),
                        argument: AstNode::new_from_source(
                            file.clone(),
                            source_ref,
                            dict_construction,
                        ),
                    }),
                )),
            ))
        }
    }
}

/// Infer the ValueType of a SymExpr.
pub fn infer_sym_expr_type(expr: &super::SymExpr) -> ValueType {
    match expr {
        super::SymExpr::Scalar(s) => ValueType::Scalar(Some(s.dimension)),
        super::SymExpr::Integer(_) => ValueType::SignedInteger,
        super::SymExpr::Boolean(_) => ValueType::Boolean,
        super::SymExpr::Var(_) => ValueType::Scalar(None),
        super::SymExpr::BinOp(_, _, _) => ValueType::Scalar(None),
        super::SymExpr::BoolOp(_, _, _) => ValueType::Boolean,
        super::SymExpr::UnaryOp(_, _) => ValueType::Scalar(None),
        super::SymExpr::MethodCall {
            method_name, ..
        } => {
            // Default to Scalar for method calls — type will be validated at runtime
            if method_name == "to_signed_integer" || method_name == "to_unsigned_integer" {
                ValueType::SignedInteger
            } else {
                ValueType::Scalar(None)
            }
        }
    }
}
