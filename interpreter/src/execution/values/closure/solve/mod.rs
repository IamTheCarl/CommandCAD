mod algorithm;
mod closure;
mod polynom;

pub use algorithm::{ast_return_type, expression_to_sym_expr, solve_for};
pub use closure::infer_sym_expr_type;

pub use polynom::{extract_polynomial, solve_cubic, solve_quadratic, Polynomial};

use std::any::TypeId;

use imstr::ImString;
use indexmap::IndexMap;

use super::BuiltinCallableDatabase;

use common_data_types::{Dimension, Float};

use crate::execution::values::{
    boolean::Boolean,
    scalar::Scalar,
    ValueType,
    Value,
};

/// Symbolic expression tree — used for inversion, not execution.
#[derive(Debug, Clone, PartialEq)]
pub enum SymExpr {
    Var(ImString),
    Scalar(Scalar),
    Integer(i64),
    Boolean(Boolean),
    BinOp(BinOp, Box<SymExpr>, Box<SymExpr>),
    BoolOp(BoolOp, Box<SymExpr>, Box<SymExpr>),
    UnaryOp(UnaryOp, Box<SymExpr>),
    MethodCall {
        method_name: ImString,
        self_expr: Box<SymExpr>,
        args: Vec<SymExpr>,
        args_names: Vec<crate::execution::values::dictionary::ArgumentName>,
    },
    Vector(Vec<SymExpr>),
    MemberAccess {
        base: Box<SymExpr>,
        member: ImString,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BoolOp {
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
    Neg,
}

impl std::fmt::Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Pow => write!(f, "**"),
        }
    }
}

impl std::fmt::Display for BoolOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BoolOp::And => write!(f, "and"),
            BoolOp::Or => write!(f, "or"),
        }
    }
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Not => write!(f, "!"),
            UnaryOp::Neg => write!(f, "-"),
        }
    }
}

/// Differentiate a SymExpr with respect to a variable.
/// Returns the derivative as a new SymExpr.
pub fn differentiate(expr: &SymExpr, var: &str) -> SymExpr {
    match expr {
        // Constants have zero derivative
        SymExpr::Scalar(_) | SymExpr::Integer(_) | SymExpr::Boolean(_) => {
            SymExpr::Scalar(Scalar {
                dimension: Dimension::zero(),
                value: Float::new(0.0).unwrap(),
            })
        }
        // d/dx(x) = 1
        SymExpr::Var(v) if v == var => {
            SymExpr::Scalar(Scalar {
                dimension: Dimension::zero(),
                value: Float::new(1.0).unwrap(),
            })
        }
        // d/dx(captured) = 0 (captured variable not being differentiated w.r.t.)
        SymExpr::Var(_) => {
            SymExpr::Scalar(Scalar {
                dimension: Dimension::zero(),
                value: Float::new(0.0).unwrap(),
            })
        }
        // Sum rule: d/dx(u + v) = du/dx + dv/dx
        SymExpr::BinOp(BinOp::Add, left, right) => {
            let du = differentiate(left, var);
            let dv = differentiate(right, var);
            SymExpr::BinOp(BinOp::Add, Box::new(du), Box::new(dv))
        }
        // Difference rule: d/dx(u - v) = du/dx - dv/dx
        SymExpr::BinOp(BinOp::Sub, left, right) => {
            let du = differentiate(left, var);
            let dv = differentiate(right, var);
            SymExpr::BinOp(BinOp::Sub, Box::new(du), Box::new(dv))
        }
        // Product rule: d/dx(u*v) = du/dx * v + u * dv/dx
        SymExpr::BinOp(BinOp::Mul, left, right) => {
            let du = differentiate(left, var);
            let dv = differentiate(right, var);
            SymExpr::BinOp(
                BinOp::Add,
                Box::new(SymExpr::BinOp(
                    BinOp::Mul,
                    Box::new(du),
                    Box::new(right.as_ref().clone()),
                )),
                Box::new(SymExpr::BinOp(
                    BinOp::Mul,
                    Box::new(left.as_ref().clone()),
                    Box::new(dv),
                )),
            )
        }
        // Quotient rule: d/dx(u/v) = (du*v - u*dv) / v²
        SymExpr::BinOp(BinOp::Div, left, right) => {
            let du = differentiate(left, var);
            let dv = differentiate(right, var);
      let v = right.as_ref().clone();
            SymExpr::BinOp(
                BinOp::Div,
                Box::new(SymExpr::BinOp(
                    BinOp::Sub,
                    Box::new(SymExpr::BinOp(
                        BinOp::Mul,
                        Box::new(du),
                        Box::new(v.clone()),
                    )),
                    Box::new(SymExpr::BinOp(
                        BinOp::Mul,
                        Box::new(left.as_ref().clone()),
                        Box::new(dv),
                    )),
                )),
                Box::new(SymExpr::BinOp(BinOp::Mul, Box::new(v.clone()), Box::new(v))),
            )
        }
        // Chain + power rule: d/dx(u^n) = n * u^(n-1) * du/dx
        SymExpr::BinOp(BinOp::Pow, base, exp) => {
            let n = exp.as_ref().clone();
            let one = SymExpr::Integer(1);
            let n_minus_1 = SymExpr::BinOp(BinOp::Sub, Box::new(n.clone()), Box::new(one));
            let db = differentiate(base, var);
            SymExpr::BinOp(
                BinOp::Mul,
                Box::new(SymExpr::BinOp(
                    BinOp::Mul,
                    Box::new(n),
                    Box::new(SymExpr::BinOp(
                        BinOp::Pow,
                        Box::new(*base.clone()),
                        Box::new(n_minus_1),
                    )),
                )),
                Box::new(db),
            )
        }
        // Unary negation: d/dx(-u) = -du/dx
        SymExpr::UnaryOp(UnaryOp::Neg, inner) => {
            let di = differentiate(inner, var);
            SymExpr::UnaryOp(UnaryOp::Neg, Box::new(di))
        }
        // Unary not: d/dx(!u) = 0 (boolean, treated as constant)
        SymExpr::UnaryOp(UnaryOp::Not, _) => {
            SymExpr::Scalar(Scalar {
                dimension: Dimension::zero(),
                value: Float::new(0.0).unwrap(),
            })
        }
        // Method calls with chain rule for known differentiable functions
        SymExpr::MethodCall {
            method_name,
            self_expr,
            args,
            args_names,
        } => {
            let ds = differentiate(self_expr, var);

            match method_name.as_str() {
                "sin" => SymExpr::BinOp(
                    BinOp::Mul,
                    Box::new(SymExpr::MethodCall {
                        method_name: "cos".into(),
                        self_expr: self_expr.clone(),
                        args: vec![],
                        args_names: vec![],
                    }),
                    Box::new(ds),
                ),
                "cos" => SymExpr::BinOp(
                    BinOp::Mul,
                    Box::new(SymExpr::UnaryOp(
                        UnaryOp::Neg,
                        Box::new(SymExpr::MethodCall {
                            method_name: "sin".into(),
                            self_expr: self_expr.clone(),
                            args: vec![],
                            args_names: vec![],
                        }),
                    )),
                    Box::new(ds),
                ),
                "tan" => {
                    let tan_self = SymExpr::MethodCall {
                        method_name: "tan".into(),
                        self_expr: self_expr.clone(),
                        args: vec![],
                        args_names: vec![],
                    };
                    let one = SymExpr::Scalar(Scalar {
                        dimension: Dimension::zero(),
                        value: Float::new(1.0).unwrap(),
                    });
                    SymExpr::BinOp(
                        BinOp::Mul,
                        Box::new(SymExpr::BinOp(
                            BinOp::Add,
                            Box::new(one),
                            Box::new(SymExpr::BinOp(
                                BinOp::Pow,
                                Box::new(tan_self),
                                Box::new(SymExpr::Integer(2)),
                            )),
                        )),
                        Box::new(ds),
                    )
                }
                "sinh" => SymExpr::BinOp(
                    BinOp::Mul,
                    Box::new(SymExpr::MethodCall {
                        method_name: "cosh".into(),
                        self_expr: self_expr.clone(),
                        args: vec![],
                        args_names: vec![],
                    }),
                    Box::new(ds),
                ),
                "cosh" => SymExpr::BinOp(
                    BinOp::Mul,
                    Box::new(SymExpr::MethodCall {
                        method_name: "sinh".into(),
                        self_expr: self_expr.clone(),
                        args: vec![],
                        args_names: vec![],
                    }),
                    Box::new(ds),
                ),
                "exp" => SymExpr::BinOp(
                    BinOp::Mul,
                    Box::new(SymExpr::MethodCall {
                        method_name: "exp".into(),
                        self_expr: self_expr.clone(),
                        args: vec![],
                        args_names: vec![],
                    }),
                    Box::new(ds),
                ),
                "log" => SymExpr::BinOp(
                    BinOp::Div,
                    Box::new(SymExpr::Scalar(Scalar {
                        dimension: Dimension::zero(),
                        value: Float::new(1.0).unwrap(),
                    })),
                    Box::new(self_expr.as_ref().clone()),
                ),
                "asin" => SymExpr::BinOp(
                    BinOp::Div,
                    Box::new(SymExpr::Scalar(Scalar {
                        dimension: Dimension::zero(),
                        value: Float::new(1.0).unwrap(),
                    })),
                    Box::new(SymExpr::MethodCall {
                        method_name: "sqrt".into(),
                        self_expr: Box::new(SymExpr::BinOp(
                            BinOp::Sub,
                            Box::new(SymExpr::Scalar(Scalar {
                                dimension: Dimension::zero(),
                                value: Float::new(1.0).unwrap(),
                            })),
                            Box::new(SymExpr::BinOp(
                                BinOp::Pow,
                                Box::new(self_expr.as_ref().clone()),
                                Box::new(SymExpr::Integer(2)),
                            )),
                        )),
                        args: vec![],
                        args_names: vec![],
                    }),
                ),
                "acos" => SymExpr::BinOp(
                    BinOp::Div,
                    Box::new(SymExpr::UnaryOp(
                        UnaryOp::Neg,
                        Box::new(SymExpr::Scalar(Scalar {
                            dimension: Dimension::zero(),
                            value: Float::new(1.0).unwrap(),
                        })),
                    )),
                    Box::new(SymExpr::MethodCall {
                        method_name: "sqrt".into(),
                        self_expr: Box::new(SymExpr::BinOp(
                            BinOp::Sub,
                            Box::new(SymExpr::Scalar(Scalar {
                                dimension: Dimension::zero(),
                                value: Float::new(1.0).unwrap(),
                            })),
                            Box::new(SymExpr::BinOp(
                                BinOp::Pow,
                                Box::new(self_expr.as_ref().clone()),
                                Box::new(SymExpr::Integer(2)),
                            )),
                        )),
                        args: vec![],
                        args_names: vec![],
                    }),
                ),
                "atan" => SymExpr::BinOp(
                    BinOp::Div,
                    Box::new(SymExpr::Scalar(Scalar {
                        dimension: Dimension::zero(),
                        value: Float::new(1.0).unwrap(),
                    })),
                    Box::new(SymExpr::BinOp(
                        BinOp::Add,
                        Box::new(SymExpr::Scalar(Scalar {
                            dimension: Dimension::zero(),
                            value: Float::new(1.0).unwrap(),
                        })),
                        Box::new(SymExpr::BinOp(
                            BinOp::Pow,
                            Box::new(self_expr.as_ref().clone()),
                            Box::new(SymExpr::Integer(2)),
                        )),
                    )),
                ),
                "asinh" => SymExpr::BinOp(
                    BinOp::Div,
                    Box::new(SymExpr::Scalar(Scalar {
                        dimension: Dimension::zero(),
                        value: Float::new(1.0).unwrap(),
                    })),
                    Box::new(SymExpr::MethodCall {
                        method_name: "sqrt".into(),
                        self_expr: Box::new(SymExpr::BinOp(
                            BinOp::Add,
                            Box::new(SymExpr::Scalar(Scalar {
                                dimension: Dimension::zero(),
                                value: Float::new(1.0).unwrap(),
                            })),
                            Box::new(SymExpr::BinOp(
                                BinOp::Pow,
                                Box::new(self_expr.as_ref().clone()),
                                Box::new(SymExpr::Integer(2)),
                            )),
                        )),
                        args: vec![],
                        args_names: vec![],
                    }),
                ),
                "acosh" => SymExpr::BinOp(
                    BinOp::Div,
                    Box::new(SymExpr::Scalar(Scalar {
                        dimension: Dimension::zero(),
                        value: Float::new(1.0).unwrap(),
                    })),
                    Box::new(SymExpr::MethodCall {
                        method_name: "sqrt".into(),
                        self_expr: Box::new(SymExpr::BinOp(
                            BinOp::Sub,
                            Box::new(SymExpr::BinOp(
                                BinOp::Pow,
                                Box::new(self_expr.as_ref().clone()),
                                Box::new(SymExpr::Integer(2)),
                            )),
                            Box::new(SymExpr::Scalar(Scalar {
                                dimension: Dimension::zero(),
                                value: Float::new(1.0).unwrap(),
                            })),
                        )),
                        args: vec![],
                        args_names: vec![],
                    }),
                ),
                "atanh" => SymExpr::BinOp(
                    BinOp::Div,
                    Box::new(SymExpr::Scalar(Scalar {
                        dimension: Dimension::zero(),
                        value: Float::new(1.0).unwrap(),
                    })),
                    Box::new(SymExpr::BinOp(
                        BinOp::Sub,
                        Box::new(SymExpr::Scalar(Scalar {
                            dimension: Dimension::zero(),
                            value: Float::new(1.0).unwrap(),
                        })),
                        Box::new(SymExpr::BinOp(
                            BinOp::Pow,
                            Box::new(self_expr.as_ref().clone()),
                            Box::new(SymExpr::Integer(2)),
                        )),
                    )),
                ),
                "abs" => SymExpr::BinOp(
                    BinOp::Mul,
                    Box::new(SymExpr::MethodCall {
                        method_name: "signum".into(),
                        self_expr: self_expr.clone(),
                        args: vec![],
                        args_names: vec![],
                    }),
                    Box::new(ds),
                ),
                "cbrt" => SymExpr::BinOp(
                    BinOp::Div,
                    Box::new(SymExpr::Scalar(Scalar {
                        dimension: Dimension::zero(),
                        value: Float::new(1.0).unwrap(),
                    })),
                    Box::new(SymExpr::BinOp(
                        BinOp::Mul,
                        Box::new(SymExpr::Integer(3)),
                        Box::new(SymExpr::BinOp(
                            BinOp::Pow,
                            Box::new(SymExpr::MethodCall {
                                method_name: "cbrt".into(),
                                self_expr: self_expr.clone(),
                                args: vec![],
                                args_names: vec![],
                            }),
                            Box::new(SymExpr::Integer(2)),
                        )),
                    )),
                ),
                "sqrt" => SymExpr::BinOp(
                    BinOp::Div,
                    Box::new(SymExpr::Scalar(Scalar {
                        dimension: Dimension::zero(),
                        value: Float::new(1.0).unwrap(),
                    })),
                    Box::new(SymExpr::BinOp(
                        BinOp::Mul,
                        Box::new(SymExpr::Integer(2)),
                        Box::new(SymExpr::MethodCall {
                            method_name: "sqrt".into(),
                            self_expr: self_expr.clone(),
                            args: vec![],
                            args_names: vec![],
                        }),
                    )),
                ),
                "recip" => SymExpr::BinOp(
                    BinOp::Div,
                    Box::new(SymExpr::UnaryOp(
                        UnaryOp::Neg,
                        Box::new(ds),
                    )),
                    Box::new(SymExpr::BinOp(
                        BinOp::Pow,
                        Box::new(self_expr.as_ref().clone()),
                        Box::new(SymExpr::Integer(2)),
                    )),
                ),
                "tanh" => SymExpr::BinOp(
                    BinOp::Mul,
                    Box::new(SymExpr::BinOp(
                        BinOp::Sub,
                        Box::new(SymExpr::Scalar(Scalar {
                            dimension: Dimension::zero(),
                            value: Float::new(1.0).unwrap(),
                        })),
                        Box::new(SymExpr::BinOp(
                            BinOp::Pow,
                            Box::new(SymExpr::MethodCall {
                                method_name: "tanh".into(),
                                self_expr: self_expr.clone(),
                                args: vec![],
                                args_names: vec![],
                            }),
                            Box::new(SymExpr::Integer(2)),
                        )),
                    )),
                    Box::new(ds),
                ),
                "signum" => SymExpr::Scalar(Scalar {
                    dimension: Dimension::zero(),
                    value: Float::new(0.0).unwrap(),
                }),
                "pow" => {
                    let exp = args.first().and_then(|a| {
                        match a {
                            SymExpr::Integer(i) => Some(*i),
                            _ => None,
                        }
                    });
                    if let Some(exp) = exp {
                        let n_minus_1 = SymExpr::Integer(exp - 1);
                        SymExpr::BinOp(
                            BinOp::Mul,
                            Box::new(SymExpr::BinOp(
                                BinOp::Mul,
                                Box::new(SymExpr::Integer(exp)),
                                Box::new(SymExpr::BinOp(
                                    BinOp::Pow,
                                    Box::new(self_expr.as_ref().clone()),
                                    Box::new(n_minus_1),
                                )),
                            )),
                            Box::new(ds),
                        )
                    } else {
                        SymExpr::MethodCall {
                            method_name: "not_differentiable".into(),
                            self_expr: Box::new(SymExpr::Var("pow".into())),
                            args: args.clone(),
                            args_names: args_names.clone(),
                        }
                    }
                }
                // All other methods are not differentiable
                _ => SymExpr::MethodCall {
                    method_name: "not_differentiable".into(),
                    self_expr: Box::new(SymExpr::Var(method_name.clone())),
                    args: vec![],
                    args_names: vec![],
                },
            }
        }
        // BoolOps are not differentiable
        SymExpr::BoolOp(_, _, _) => SymExpr::Scalar(Scalar {
            dimension: Dimension::zero(),
            value: Float::new(0.0).unwrap(),
        }),
      SymExpr::Vector(comps) => SymExpr::Vector(
             comps.iter()
                 .map(|c| differentiate(c, var))
                 .collect()
         ),
        SymExpr::MemberAccess { base, member } => {
            let deriv_base = differentiate(base.as_ref(), var);
            SymExpr::MemberAccess {
                base: Box::new(deriv_base),
                member: member.clone(),
            }
        }
    }
}

/// Check if a SymExpr is a constant (doesn't contain the given variable).
fn is_constant_wrt(expr: &SymExpr, var: &str) -> bool {
    !sym_expr_contains_var(expr, var)
}

/// Extract a constant factor from a multiplication expression.
/// Returns (constant_factor, rest_of_expression) or None if no constant factor.
fn extract_constant_factor(expr: &SymExpr, var: &str) -> Option<(SymExpr, SymExpr)> {
    match expr {
        SymExpr::BinOp(BinOp::Mul, left, right) => {
            if is_constant_wrt(left, var) {
                Some((left.as_ref().clone(), right.as_ref().clone()))
            } else if is_constant_wrt(right, var) {
                Some((right.as_ref().clone(), left.as_ref().clone()))
            } else {
                None
            }
        }
        SymExpr::BinOp(BinOp::Div, left, right) => {
            if is_constant_wrt(right, var) && !is_constant_wrt(left, var) {
                Some((right.as_ref().clone(), left.as_ref().clone()))
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Integrate a SymExpr with respect to a variable.
/// Returns Result because some integrals don't have closed forms.
pub fn integrate(expr: &SymExpr, var: &str, max_recursion: usize) -> Result<SymExpr, String> {
    integrate_inner(expr, var, max_recursion)
}

fn integrate_inner(expr: &SymExpr, var: &str, max_recursion: usize) -> Result<SymExpr, String> {
    match expr {
        // Constants integrate to 0 (c=0 as specified)
        SymExpr::Scalar(_) | SymExpr::Integer(_) | SymExpr::Boolean(_) => {
            Ok(SymExpr::Scalar(Scalar {
                dimension: Dimension::zero(),
                value: Float::new(0.0).unwrap(),
            }))
        }
        // ∫x dx = x²/2
        SymExpr::Var(v) if v == var => {
            Ok(SymExpr::BinOp(
                BinOp::Div,
                Box::new(SymExpr::BinOp(
                    BinOp::Pow,
                    Box::new(SymExpr::Var(var.into())),
                    Box::new(SymExpr::Integer(2)),
                )),
                Box::new(SymExpr::Integer(2)),
            ))
        }
        // Captured variable: ∫c dx = 0
        SymExpr::Var(_) => {
            Ok(SymExpr::Scalar(Scalar {
                dimension: Dimension::zero(),
                value: Float::new(0.0).unwrap(),
            }))
        }
        // Linearity: ∫(u ± v) = ∫u ± ∫v
        SymExpr::BinOp(BinOp::Add, left, right) => {
            let iu = integrate_inner(left, var, max_recursion)?;
            let iv = integrate_inner(right, var, max_recursion)?;
            Ok(SymExpr::BinOp(BinOp::Add, Box::new(iu), Box::new(iv)))
        }
        SymExpr::BinOp(BinOp::Sub, left, right) => {
            let iu = integrate_inner(left, var, max_recursion)?;
            let iv = integrate_inner(right, var, max_recursion)?;
            Ok(SymExpr::BinOp(BinOp::Sub, Box::new(iu), Box::new(iv)))
        }
        // Constant factor: ∫(c*u) = c*∫u
        SymExpr::BinOp(BinOp::Mul, _left, _right) => {
            if let Some((c, rest)) = extract_constant_factor(expr, var) {
                let ir = integrate_inner(&rest, var, max_recursion)?;
                return Ok(SymExpr::BinOp(
                    BinOp::Mul,
                    Box::new(c),
                    Box::new(ir),
                ));
            }
            // No constant factor - try integration by parts
            integrate_by_parts(expr, var, max_recursion)
        }
        SymExpr::BinOp(BinOp::Div, left, right) => {
            // ∫(u/v) = ∫u * (1/v) if v is constant, else by parts
            if is_constant_wrt(right, var) && !is_constant_wrt(left, var) {
                let il = integrate_inner(left, var, max_recursion)?;
                return Ok(SymExpr::BinOp(
                    BinOp::Div,
                    Box::new(il),
                    Box::new(right.as_ref().clone()),
                ));
            }
            // Otherwise try by parts (convert to multiplication)
            let inv_right = SymExpr::BinOp(
                BinOp::Pow,
                Box::new(right.as_ref().clone()),
                Box::new(SymExpr::Integer(-1)),
            );
            let mul_expr = SymExpr::BinOp(
                BinOp::Mul,
                Box::new(left.as_ref().clone()),
                Box::new(inv_right),
            );
            integrate_by_parts(&mul_expr, var, max_recursion)
        }
        // Power rule: ∫x^n dx = x^(n+1)/(n+1) for n ≠ -1
        SymExpr::BinOp(BinOp::Pow, base, exp) => {
            if let SymExpr::Var(v) = base.as_ref() {
                if v == var {
                    if let SymExpr::Integer(n) = exp.as_ref() {
                        if *n == -1 {
                            return Err(format!(
                                "operation 'Pow({}, -1)' is not integrable",
                                v
                            ));
                        }
                        let n_plus_1 = n + 1;
                        return Ok(SymExpr::BinOp(
                            BinOp::Div,
                            Box::new(SymExpr::BinOp(
                                BinOp::Pow,
                                Box::new(SymExpr::Var(var.into())),
                                Box::new(SymExpr::Integer(n_plus_1)),
                            )),
                            Box::new(SymExpr::Integer(n_plus_1)),
                        ));
                    }
                }
            }
            // Base is not a simple variable - try by parts
            integrate_by_parts(expr, var, max_recursion)
        }
        // Method calls - standard integrals
        SymExpr::MethodCall {
            method_name,
            self_expr,
            args,
            args_names: _,
        } => {
            if !args.is_empty() {
                return Err(format!("operation '{}' is not integrable", method_name));
            }

            match method_name.as_str() {
                "sin" => Ok(SymExpr::UnaryOp(
                    UnaryOp::Neg,
                    Box::new(SymExpr::MethodCall {
                        method_name: "cos".into(),
                        self_expr: self_expr.clone(),
                        args: vec![],
                        args_names: vec![],
                    }),
                )),
                "cos" => Ok(SymExpr::MethodCall {
                    method_name: "sin".into(),
                    self_expr: self_expr.clone(),
                    args: vec![],
                    args_names: vec![],
                }),
                "sinh" => Ok(SymExpr::MethodCall {
                    method_name: "cosh".into(),
                    self_expr: self_expr.clone(),
                    args: vec![],
                    args_names: vec![],
                }),
                "cosh" => Ok(SymExpr::MethodCall {
                    method_name: "sinh".into(),
                    self_expr: self_expr.clone(),
                    args: vec![],
                    args_names: vec![],
                }),
                "exp" => Ok(SymExpr::MethodCall {
                    method_name: "exp".into(),
                    self_expr: self_expr.clone(),
                    args: vec![],
                    args_names: vec![],
                }),
                "tan" => {
                    let cos_self = SymExpr::MethodCall {
                        method_name: "cos".into(),
                        self_expr: self_expr.clone(),
                        args: vec![],
                        args_names: vec![],
                    };
                    let abs_cos = SymExpr::MethodCall {
                        method_name: "abs".into(),
                        self_expr: Box::new(cos_self),
                        args: vec![],
                        args_names: vec![],
                    };
                    let log_abs_cos = SymExpr::MethodCall {
                        method_name: "log".into(),
                        self_expr: Box::new(abs_cos),
                        args: vec![],
                        args_names: vec![],
                    };
                    Ok(SymExpr::UnaryOp(UnaryOp::Neg, Box::new(log_abs_cos)))
                }
                "cbrt" => {
                    let coeff = SymExpr::Scalar(Scalar {
                        dimension: Dimension::zero(),
                        value: Float::new(0.75).unwrap(),
                    });
                    let exp = SymExpr::BinOp(
                        BinOp::Div,
                        Box::new(SymExpr::Integer(4)),
                        Box::new(SymExpr::Integer(3)),
                    );
                    Ok(SymExpr::BinOp(
                        BinOp::Mul,
                        Box::new(coeff),
                        Box::new(SymExpr::BinOp(
                            BinOp::Pow,
                            Box::new(SymExpr::Var(var.into())),
                            Box::new(exp),
                        )),
                    ))
                }
                "sqrt" => {
                    let coeff = SymExpr::Scalar(Scalar {
                        dimension: Dimension::zero(),
                        value: Float::new(0.666667).unwrap(),
                    });
                    let exp = SymExpr::BinOp(
                        BinOp::Div,
                        Box::new(SymExpr::Integer(3)),
                        Box::new(SymExpr::Integer(2)),
                    );
                    Ok(SymExpr::BinOp(
                        BinOp::Mul,
                        Box::new(coeff),
                        Box::new(SymExpr::BinOp(
                            BinOp::Pow,
                            Box::new(SymExpr::Var(var.into())),
                            Box::new(exp),
                        )),
                    ))
                }
                "recip" => Ok(SymExpr::MethodCall {
                    method_name: "log".into(),
                    self_expr: Box::new(SymExpr::Var(var.into())),
                    args: vec![],
                    args_names: vec![],
                }),
                _ => Err(format!("operation '{}' is not integrable", method_name)),
            }
        }
        // BoolOps are not integrable
        SymExpr::BoolOp(_, _, _) => {
            Err("operation 'BoolOp' is not integrable".to_string())
        }
        // UnaryOp - integrate the inner expression and apply negation if needed
        SymExpr::UnaryOp(UnaryOp::Neg, inner) => {
            let ii = integrate_inner(inner, var, max_recursion)?;
            Ok(SymExpr::UnaryOp(UnaryOp::Neg, Box::new(ii)))
        }
        SymExpr::UnaryOp(UnaryOp::Not, _) => {
            Err("operation 'Not' is not integrable".to_string())
        }
        SymExpr::Vector(comps) => {
            let mut results = Vec::with_capacity(comps.len());
            for comp in comps {
                results.push(integrate_inner(comp, var, max_recursion)?);
            }
            Ok(SymExpr::Vector(results))
        }
        SymExpr::MemberAccess { base, member } => {
            let int_base = integrate_inner(base.as_ref(), var, max_recursion)?;
            Ok(SymExpr::MemberAccess {
                base: Box::new(int_base),
                member: member.clone(),
            })
        }
    }
}

/// Integration by parts: ∫u*v dx = u*∫v dx - ∫(du/dx * ∫v dx) dx
fn integrate_by_parts(expr: &SymExpr, var: &str, max_recursion: usize) -> Result<SymExpr, String> {
    if max_recursion == 0 {
        return Err(
            "operation 'Mul (integration by parts limit reached)' is not integrable".to_string()
        );
    }

    match expr {
        SymExpr::BinOp(BinOp::Mul, left, right) => {
            // Try to find a polynomial factor (variable raised to integer power or the variable itself)
            // Use it as u (since differentiating reduces its degree)
            let (u, v) = find_polynomial_factor(left, right, var)
                .or_else(|| find_polynomial_factor(right, left, var))
                .unwrap_or_else(|| (left.as_ref().clone(), right.as_ref().clone()));

            // Compute ∫v dx
            let iv = integrate_inner(&v, var, max_recursion - 1)?;

            // Compute du/dx (derivative of u)
            let du = differentiate(&u, var);

            // Compute ∫(du * ∫v dx) dx
            let du_times_iv = SymExpr::BinOp(
                BinOp::Mul,
                Box::new(du),
                Box::new(iv.clone()),
            );
            let i_du_iv = integrate_inner(&du_times_iv, var, max_recursion - 1)?;

            // Result: u * ∫v dx - ∫(du * ∫v dx) dx
            Ok(SymExpr::BinOp(
                BinOp::Sub,
                Box::new(SymExpr::BinOp(
                    BinOp::Mul,
                    Box::new(u.clone()),
                    Box::new(iv),
                )),
                Box::new(i_du_iv),
            ))
        }
        _ => {
            Err("operation 'Mul' is not integrable".to_string())
        }
    }
}

/// Try to find a polynomial factor in left or right of a product.
fn find_polynomial_factor(
    candidate: &SymExpr,
    rest: &SymExpr,
    var: &str,
) -> Option<(SymExpr, SymExpr)> {
    if is_polynomial_wrt(candidate, var) {
        Some((candidate.clone(), rest.clone()))
    } else {
        None
    }
}

/// Check if a SymExpr is a polynomial in the given variable.
fn is_polynomial_wrt(expr: &SymExpr, var: &str) -> bool {
    match expr {
        SymExpr::Var(v) if v == var => true,
        SymExpr::BinOp(BinOp::Pow, base, exp) => {
            if let SymExpr::Var(v) = base.as_ref() {
                if v == var {
                    is_constant_wrt(exp, var)
                } else {
                    false
                }
            } else {
                false
            }
        }
        SymExpr::BinOp(BinOp::Mul, left, right) => {
            is_polynomial_wrt(left, var) && is_polynomial_wrt(right, var)
        }
        SymExpr::BinOp(BinOp::Add | BinOp::Sub, left, right) => {
            is_polynomial_wrt(left, var) && is_polynomial_wrt(right, var)
        }
        SymExpr::UnaryOp(UnaryOp::Neg, inner) => is_polynomial_wrt(inner, var),
        SymExpr::Scalar(_) | SymExpr::Integer(_) | SymExpr::Boolean(_) => true,
        _ => false,
    }
}

/// Precedence levels for binary operations (higher = binds tighter).
fn binop_precedence(op: &BinOp) -> u8 {
    match op {
        BinOp::Pow => 4,
        BinOp::Mul | BinOp::Div => 3,
        BinOp::Add | BinOp::Sub => 2,
    }
}

/// Format a SymExpr with parentheses only when needed for precedence.
fn format_sym_expr_with_parens(expr: &SymExpr, parent_precedence: u8, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match expr {
        SymExpr::Var(v) => write!(f, "{}", v),
        SymExpr::Scalar(s) => {
            let unit_name = units::get_base_unit_name(&s.dimension);
            if s.value == common_data_types::Float::new(0.0).unwrap() && unit_name.is_none() {
                write!(f, "0.0")
            } else if let Some(unit) = unit_name {
                write!(f, "{:.1}{}", s.value, unit)
            } else {
                write!(f, "{:.1}", s.value)
            }
        }
        SymExpr::Integer(i) => write!(f, "{}.0", i),
        SymExpr::Boolean(b) => write!(f, "{}", b.0),
        SymExpr::BinOp(op, left, right) => {
            let prec = binop_precedence(op);
            let needs_parens = prec < parent_precedence;
            if needs_parens {
                write!(f, "(")?;
            }
            format_sym_expr_with_parens(left, prec, f)?;
            write!(f, " {} ", op)?;
            format_sym_expr_with_parens(right, prec + 1, f)?;
            if needs_parens {
                write!(f, ")")?;
            }
            Ok(())
        }
        SymExpr::BoolOp(op, left, right) => {
            let prec = 1;
            let needs_parens = prec < parent_precedence;
            if needs_parens {
                write!(f, "(")?;
            }
            format_sym_expr_with_parens(left, prec, f)?;
            write!(f, " {} ", op)?;
            format_sym_expr_with_parens(right, prec + 1, f)?;
            if needs_parens {
                write!(f, ")")?;
            }
            Ok(())
        }
        SymExpr::UnaryOp(op, inner) => {
            let prec = 5;
            let needs_parens = prec < parent_precedence;
            if needs_parens {
                write!(f, "(")?;
            }
            write!(f, "{}", op)?;
            format_sym_expr_with_parens(inner, prec, f)?;
            if needs_parens {
                write!(f, ")")?;
            }
            Ok(())
        }
        SymExpr::MethodCall { method_name, self_expr, args, args_names } => {
            format_sym_expr_with_parens(self_expr, 6, f)?;
            if args.is_empty() {
                write!(f, "::{}", method_name)
            } else {
                write!(f, "::{}", method_name)?;
                write!(f, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    if let Some(name) = args_names.get(i) {
                        match name {
                            crate::execution::values::dictionary::ArgumentName::Named(n) => {
                                write!(f, "{}=", n)?;
                            }
                            crate::execution::values::dictionary::ArgumentName::Positional(_) => {}
                        }
                    }
                    format_sym_expr_with_parens(arg, 0, f)?;
                }
                write!(f, ")")
            }
        }
        SymExpr::Vector(comps) => {
            if comps.len() == 2 {
                write!(f, "Vector2(")?;
                format_sym_expr_with_parens(&comps[0], 0, f)?;
                write!(f, ", ")?;
                format_sym_expr_with_parens(&comps[1], 0, f)?;
            } else if comps.len() == 3 {
                write!(f, "Vector3(")?;
                format_sym_expr_with_parens(&comps[0], 0, f)?;
                write!(f, ", ")?;
                format_sym_expr_with_parens(&comps[1], 0, f)?;
                write!(f, ", ")?;
                format_sym_expr_with_parens(&comps[2], 0, f)?;
            } else if comps.len() == 4 {
                write!(f, "Vector4(")?;
                format_sym_expr_with_parens(&comps[0], 0, f)?;
                write!(f, ", ")?;
                format_sym_expr_with_parens(&comps[1], 0, f)?;
                write!(f, ", ")?;
                format_sym_expr_with_parens(&comps[2], 0, f)?;
                write!(f, ", ")?;
                format_sym_expr_with_parens(&comps[3], 0, f)?;
            } else {
                write!(f, "Vector(")?;
                for (i, comp) in comps.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    format_sym_expr_with_parens(comp, 0, f)?;
                }
            }
            write!(f, ")")
        }
        SymExpr::MemberAccess { base, member } => {
            format_sym_expr_with_parens(base, 0, f)?;
            write!(f, ".{}", member)
        }
    }
}

impl std::fmt::Display for SymExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        format_sym_expr_with_parens(self, 0, f)
    }
}

/// Operation-specific inverse logic.
pub trait InverseOp {
    /// Given parent_op and sibling_expr, compute what isolates the child.
    /// For example: parent = (child + 5), sibling = 5 → inverse = parent - 5
    fn isolate(&self, target_is_left: bool, sibling: &SymExpr) -> SymExpr;

    /// Is this operation invertible? (abs is not strictly invertible)
    fn is_invertible(&self) -> bool;
}

impl InverseOp for BinOp {
    fn isolate(&self, target_is_left: bool, sibling: &SymExpr) -> SymExpr {
        match self {
            BinOp::Add => SymExpr::BinOp(
                BinOp::Sub,
                Box::new(SymExpr::Var("__result__".into())),
                Box::new(sibling.clone()),
            ),
            BinOp::Sub => {
                if target_is_left {
                    SymExpr::BinOp(
                        BinOp::Add,
                        Box::new(SymExpr::Var("__result__".into())),
                        Box::new(sibling.clone()),
                    )
                } else {
                    SymExpr::BinOp(
                        BinOp::Sub,
                        Box::new(SymExpr::Var("__result__".into())),
                        Box::new(sibling.clone()),
                    )
                }
            }
            BinOp::Mul => SymExpr::BinOp(
                BinOp::Div,
                Box::new(SymExpr::Var("__result__".into())),
                Box::new(sibling.clone()),
            ),
            BinOp::Div => {
                if target_is_left {
                    SymExpr::BinOp(
                        BinOp::Mul,
                        Box::new(SymExpr::Var("__result__".into())),
                        Box::new(sibling.clone()),
                    )
                } else {
                    SymExpr::BinOp(
                        BinOp::Div,
                        Box::new(SymExpr::Var("__result__".into())),
                        Box::new(sibling.clone()),
                    )
                }
            }
            BinOp::Pow => {
                match sibling {
                    SymExpr::Integer(2) => {
                        // x^2 inverse: sqrt(__result__)
                        SymExpr::MethodCall {
                            method_name: "sqrt".into(),
                            self_expr: Box::new(SymExpr::Var("__result__".into())),
                            args: vec![],
                            args_names: vec![],
                        }
                    }
                    SymExpr::Integer(3) => {
                        // x^3 inverse: cbrt(__result__)
                        SymExpr::MethodCall {
                            method_name: "cbrt".into(),
                            self_expr: Box::new(SymExpr::Var("__result__".into())),
                            args: vec![],
                            args_names: vec![],
                        }
                    }
                    SymExpr::Integer(n) if *n != 0 => {
                        let reciprocal = 1.0 / (*n as f64);
                        SymExpr::MethodCall {
                            method_name: "pow".into(),
                            self_expr: Box::new(SymExpr::Var("__result__".into())),
                            args: vec![SymExpr::Scalar(Scalar {
                                dimension: common_data_types::Dimension::zero(),
                                value: common_data_types::Float::new(reciprocal).unwrap(),
                            })],
                            args_names: vec![crate::execution::values::dictionary::ArgumentName::Named("exp".into())],
                        }
                    }
                    _ => SymExpr::MethodCall {
                        method_name: "pow".into(),
                        self_expr: Box::new(SymExpr::Var("__result__".into())),
                        args: vec![SymExpr::BinOp(
                            BinOp::Div,
                            Box::new(SymExpr::Integer(1)),
                            Box::new(sibling.clone()),
                        )],
                        args_names: vec![crate::execution::values::dictionary::ArgumentName::Named("exp".into())],
                    },
                }
            }
        }
    }

    fn is_invertible(&self) -> bool {
        true
    }
}

impl InverseOp for BoolOp {
    fn isolate(&self, _target_is_left: bool, _sibling: &SymExpr) -> SymExpr {
        // AND/OR are not invertible — this should never be called for invertible ops
        panic!("BoolOp is not invertible")
    }

    fn is_invertible(&self) -> bool {
        false
    }
}

impl InverseOp for UnaryOp {
    fn isolate(&self, _target_is_left: bool, _sibling: &SymExpr) -> SymExpr {
        match self {
            UnaryOp::Neg => SymExpr::UnaryOp(UnaryOp::Neg, Box::new(SymExpr::Var("__result__".into()))),
            UnaryOp::Not => SymExpr::UnaryOp(UnaryOp::Not, Box::new(SymExpr::Var("__result__".into()))),
        }
    }

    fn is_invertible(&self) -> bool {
        true
    }
}

/// Represents the type of a parameter in SolveResult.
#[derive(Debug, Clone, PartialEq)]
pub enum ParamType {
    Scalar,
    Integer,
    Boolean,
}

/// Result of solving for a variable.
#[derive(Debug)]
pub struct SolveResult {
    pub body: SymExpr,
    /// Captured variables with their values from the original closure.
    pub captured: IndexMap<ImString, Value>,
    pub result_name: ImString,
    /// The name of the target variable being solved for (e.g., "x").
    /// Used to exclude it from the inverse closure's parameters.
    pub target_param_name: ImString,
    /// The ValueType of the input parameter of the inverse closure (original closure's return type).
    /// Used to give the inverse closure's `wanted_output` parameter the correct dimension.
    pub target_param_type: Option<ValueType>,
    /// The return type of the inverse closure (type of the target variable being solved for).
    pub return_type: Option<ValueType>,
    /// Map of parameter names to their types in the original closure.
    /// Used to give the inverse closure's captured parameters the correct types.
    pub param_types: IndexMap<ImString, ValueType>,
}

/// Look up a method's inverse TypeId from the callable database.
/// Tries both the exact name (e.g., "Scalar::sin") and matching by suffix (e.g., "sin" matches "Scalar::sin").
pub fn get_method_inverse_callable(
    db: &BuiltinCallableDatabase,
    method_name: &str,
) -> Option<TypeId> {
    // Try exact match first (e.g., "Scalar::sin")
    if let Some(id) = db.get_callable_id(method_name) {
        if let Some(inverse) = db.get_inverse(id) {
            return Some(inverse);
        }
    }
    // Try matching by suffix (find any registered name ending with "::method_name")
    for (full_name, id) in &db.names {
        if full_name.ends_with(&format!("::{}", method_name)) {
            if let Some(inverse) = db.get_inverse(*id) {
                return Some(inverse);
            }
        }
    }
    None
}

/// Look up the method name for a given TypeId.
/// Returns the short name (e.g., "asin") rather than the full registered name (e.g., "Scalar::asin").
pub fn get_method_name(db: &BuiltinCallableDatabase, type_id: TypeId) -> Option<ImString> {
    db.get_method_name(type_id).map(|name| {
        if let Some(pos) = name.rfind("::") {
            ImString::from(&name[pos + 2..])
        } else {
            name
        }
    })
}

/// Check if a SymExpr contains a variable.
pub fn sym_expr_contains_var(expr: &SymExpr, var: &str) -> bool {
    match expr {
        SymExpr::Var(v) => v == var,
        SymExpr::Scalar(_) | SymExpr::Integer(_) | SymExpr::Boolean(_) => false,
        SymExpr::BinOp(_, left, right) => {
            sym_expr_contains_var(left, var) || sym_expr_contains_var(right, var)
        }
        SymExpr::BoolOp(_, left, right) => {
            sym_expr_contains_var(left, var) || sym_expr_contains_var(right, var)
        }
        SymExpr::UnaryOp(_, inner) => sym_expr_contains_var(inner, var),
        SymExpr::MethodCall {
            self_expr, args, ..
        } => {
            sym_expr_contains_var(self_expr, var)
                || args.iter().any(|a| sym_expr_contains_var(a, var))
        }
  SymExpr::Vector(comps) => comps.iter().any(|c| sym_expr_contains_var(c, var)),
        SymExpr::MemberAccess { base, member: _ } => sym_expr_contains_var(base, var),
    }
}

/// Collect all free variables (excluding the target and constants).
pub fn collect_free_vars(expr: &SymExpr, exclude: &str) -> Vec<ImString> {
    let mut vars = Vec::new();
    collect_free_vars_inner(expr, exclude, &mut vars);
    vars.sort();
    vars.dedup();
    vars
}

fn collect_free_vars_inner(expr: &SymExpr, exclude: &str, vars: &mut Vec<ImString>) {
    match expr {
        SymExpr::Var(v) if v != exclude && v != "__result__" => {
            if !vars.contains(&v.clone()) {
                vars.push(v.clone());
            }
        }
        SymExpr::Var(_) => {}
        SymExpr::BinOp(_, left, right) => {
            collect_free_vars_inner(left, exclude, vars);
            collect_free_vars_inner(right, exclude, vars);
        }
        SymExpr::BoolOp(_, left, right) => {
            collect_free_vars_inner(left, exclude, vars);
            collect_free_vars_inner(right, exclude, vars);
        }
        SymExpr::UnaryOp(_, inner) => {
            collect_free_vars_inner(inner, exclude, vars);
        }
        SymExpr::MethodCall { self_expr, args, .. } => {
            collect_free_vars_inner(self_expr, exclude, vars);
            for arg in args {
                collect_free_vars_inner(arg, exclude, vars);
            }
        }
        SymExpr::Scalar(_) | SymExpr::Integer(_) | SymExpr::Boolean(_) => {}
        SymExpr::Vector(comps) => {
            for comp in comps {
                collect_free_vars_inner(comp, exclude, vars);
            }
        }
        SymExpr::MemberAccess { base, member: _ } => {
            collect_free_vars_inner(base, exclude, vars);
        }
    }
}

/// Check if a variable appears more than once in the expression.
pub fn count_var_occurrences(expr: &SymExpr, var: &str) -> usize {
    match expr {
        SymExpr::Var(v) if v == var => 1,
        SymExpr::Var(_) => 0,
        SymExpr::BinOp(_, left, right) => {
            count_var_occurrences(left, var) + count_var_occurrences(right, var)
        }
        SymExpr::BoolOp(_, left, right) => {
            count_var_occurrences(left, var) + count_var_occurrences(right, var)
        }
        SymExpr::UnaryOp(_, inner) => count_var_occurrences(inner, var),
        SymExpr::MethodCall { self_expr, args, .. } => {
            let mut count = count_var_occurrences(self_expr, var);
            for arg in args {
                count += count_var_occurrences(arg, var);
            }
            count
        }
        SymExpr::Scalar(_) | SymExpr::Integer(_) | SymExpr::Boolean(_) => 0,
 SymExpr::Vector(comps) => comps.iter().map(|c| count_var_occurrences(c, var)).sum(),
        SymExpr::MemberAccess { base, member: _ } => count_var_occurrences(base, var),
    }
}
/// Post-order traversal: simplify children first, then apply rules at this node.
pub fn simplify_sym_expr(expr: &SymExpr) -> SymExpr {
    match expr {
        SymExpr::BinOp(op, left, right) => {
            let left = Box::new(simplify_sym_expr(left));
            let right = Box::new(simplify_sym_expr(right));
            apply_binop_rules(op, &left, &right)
        }
        SymExpr::UnaryOp(op, inner) => {
            let inner = Box::new(simplify_sym_expr(inner));
            apply_unary_rules(op, &inner)
        }
        SymExpr::MethodCall {
            method_name,
            self_expr,
            args,
            args_names,
            ..
        } => SymExpr::MethodCall {
            method_name: method_name.clone(),
            self_expr: Box::new(simplify_sym_expr(self_expr)),
            args: args.iter().map(simplify_sym_expr).collect(),
            args_names: args_names.clone(),
        },
    SymExpr::Vector(comps) => SymExpr::Vector(
             comps.iter().map(simplify_sym_expr).collect()
         ),
        SymExpr::MemberAccess { base, member } => SymExpr::MemberAccess {
            base: Box::new(simplify_sym_expr(base)),
            member: member.clone(),
        },
        _ => expr.clone(),
    }
}

/// Apply identity and commutativity rules at a BinOp node.
fn apply_binop_rules(op: &BinOp, left: &SymExpr, right: &SymExpr) -> SymExpr {
    match op {
        BinOp::Mul => {
            // x*x → x^2 (both operands are the same variable)
            if let (SymExpr::Var(a), SymExpr::Var(b)) = (left, right) {
                if a == b {
                    return SymExpr::BinOp(
                        BinOp::Pow,
                        Box::new(left.clone()),
                        Box::new(SymExpr::Integer(2)),
                    );
                }
            }
            // Pow(x,n) * x → x^(n+1)
            if let (SymExpr::BinOp(BinOp::Pow, base, exp), SymExpr::Var(v)) = (left, right) {
                if let SymExpr::Var(b) = base.as_ref() {
                    if b == v {
                        if let SymExpr::Integer(n) = exp.as_ref() {
                            return SymExpr::BinOp(
                                BinOp::Pow,
                                Box::new(*base.clone()),
                                Box::new(SymExpr::Integer(n + 1)),
                            );
                        }
                    }
                }
            }
            // x * Pow(x,n) → x^(n+1) (commutativity)
            if let (SymExpr::Var(v), SymExpr::BinOp(BinOp::Pow, base, exp)) = (left, right) {
                if let SymExpr::Var(b) = base.as_ref() {
                    if b == v {
                        if let SymExpr::Integer(n) = exp.as_ref() {
                            return SymExpr::BinOp(
                                BinOp::Pow,
                                Box::new(*base.clone()),
                                Box::new(SymExpr::Integer(n + 1)),
                            );
                        }
                    }
                }
            }
            // x*1 → x, 1*x → x
            if matches!(right, SymExpr::Integer(1)) {
                return left.clone();
            }
            if matches!(left, SymExpr::Integer(1)) {
                return right.clone();
            }
            SymExpr::BinOp(op.clone(), Box::new(left.clone()), Box::new(right.clone()))
        }
        BinOp::Add => {
            // x+x → x*2 (both operands are the same variable)
            if let (SymExpr::Var(a), SymExpr::Var(b)) = (left, right) {
                if a == b {
                    return SymExpr::BinOp(
                        BinOp::Mul,
                        Box::new(left.clone()),
                        Box::new(SymExpr::Integer(2)),
                    );
                }
            }
            // x+0 → x, 0+x → x
            if let SymExpr::Scalar(s) = left {
                if s.value == 0.0 {
                    return right.clone();
                }
            }
            if let SymExpr::Scalar(s) = right {
                if s.value == 0.0 {
                    return left.clone();
                }
            }
            SymExpr::BinOp(op.clone(), Box::new(left.clone()), Box::new(right.clone()))
        }
        BinOp::Sub => {
            // x-x → 0 (both operands are the same variable)
            if let (SymExpr::Var(a), SymExpr::Var(b)) = (left, right) {
                if a == b {
                    return SymExpr::Scalar(Scalar {
                        dimension: common_data_types::Dimension::zero(),
                        value: common_data_types::Float::new(0.0).unwrap(),
                    });
                }
            }
            // x-0 → x
            if let SymExpr::Scalar(s) = right {
                if s.value == 0.0 {
                    return left.clone();
                }
            }
            SymExpr::BinOp(op.clone(), Box::new(left.clone()), Box::new(right.clone()))
        }
        BinOp::Div => {
            // x/x → 1 (both operands are the same variable)
            if let (SymExpr::Var(a), SymExpr::Var(b)) = (left, right) {
                if a == b {
                    return SymExpr::Integer(1);
                }
            }
            // x/1 → x
            if matches!(right, SymExpr::Integer(1)) {
                return left.clone();
            }
            SymExpr::BinOp(op.clone(), Box::new(left.clone()), Box::new(right.clone()))
        }
        BinOp::Pow => {
            // x**1 → x
            if matches!(right, SymExpr::Integer(1)) {
                return left.clone();
            }
            SymExpr::BinOp(op.clone(), Box::new(left.clone()), Box::new(right.clone()))
        }
    }
}

/// Apply identity and commutativity rules at a UnaryOp node.
fn apply_unary_rules(op: &UnaryOp, inner: &SymExpr) -> SymExpr {
    match op {
        UnaryOp::Neg => {
            // -(-x) → x (double negation)
            if let SymExpr::UnaryOp(UnaryOp::Neg, nested) = inner {
                return (**nested).clone();
            }
            // -0 → 0
            if let SymExpr::Scalar(s) = inner {
                if s.value == 0.0 {
                    return inner.clone();
                }
            }
            SymExpr::UnaryOp(op.clone(), Box::new(inner.clone()))
        }
        UnaryOp::Not => {
            // !(!x) → x (double negation)
            if let SymExpr::UnaryOp(UnaryOp::Not, nested) = inner {
                return (**nested).clone();
            }
            SymExpr::UnaryOp(op.clone(), Box::new(inner.clone()))
        }
    }
}

#[cfg(test)]
fn test_context<R>(f: impl FnOnce(&crate::execution::ExecutionContext) -> R) -> R {
    crate::execution::test_context([], f)
}

#[cfg(test)]
mod test {
    use super::*;
    use common_data_types::Dimension;
    use crate::execution::{run_assert_eq, test_run};
    use super::algorithm::{substitute_result, sym_exprs_equal};
    

    #[test]
    fn sym_expr_contains_var_basic() {
        let expr = SymExpr::Var("a".into());
        assert!(sym_expr_contains_var(&expr, "a"));
        assert!(!sym_expr_contains_var(&expr, "b"));
    }

    #[test]
    fn sym_expr_contains_var_in_binop() {
        let expr = SymExpr::BinOp(
            BinOp::Add,
            Box::new(SymExpr::Var("a".into())),
            Box::new(SymExpr::Scalar(Scalar {
                dimension: Dimension::zero(),
                value: common_data_types::Float::new(1.0).unwrap(),
            })),
        );
        assert!(sym_expr_contains_var(&expr, "a"));
        assert!(!sym_expr_contains_var(&expr, "b"));
    }

    #[test]
    fn sym_expr_contains_var_in_method_call() {
        let expr = SymExpr::MethodCall { args_names: vec![], 
            method_name: "sin".into(),
            self_expr: Box::new(SymExpr::Var("a".into())),
            args: vec![],
        };
        assert!(sym_expr_contains_var(&expr, "a"));
    }

    #[test]
    fn count_var_occurrences_single() {
        let expr = SymExpr::Var("a".into());
        assert_eq!(count_var_occurrences(&expr, "a"), 1);
        assert_eq!(count_var_occurrences(&expr, "b"), 0);
    }

    #[test]
    fn count_var_occurrences_multiple() {
        let expr = SymExpr::BinOp(
            BinOp::Add,
            Box::new(SymExpr::Var("a".into())),
            Box::new(SymExpr::BinOp(
                BinOp::Add,
                Box::new(SymExpr::Var("a".into())),
                Box::new(SymExpr::Scalar(Scalar {
                    dimension: Dimension::zero(),
                    value: common_data_types::Float::new(1.0).unwrap(),
                })),
            )),
        );
        assert_eq!(count_var_occurrences(&expr, "a"), 2);
    }

    #[test]
    fn binop_isolate_add() {
        let sibling = SymExpr::Scalar(Scalar {
            dimension: Dimension::zero(),
            value: common_data_types::Float::new(5.0).unwrap(),
        });
        let result = BinOp::Add.isolate(true, &sibling);
        match result {
            SymExpr::BinOp(BinOp::Sub, _, right) => {
                assert!(matches!(right.as_ref(), SymExpr::Scalar(_)));
            }
            _ => panic!("Expected BinOp(Sub, ...)), got {:?}", result),
        }
    }

    #[test]
    fn binop_isolate_sub_target_left() {
        let sibling = SymExpr::Scalar(Scalar {
            dimension: Dimension::zero(),
            value: common_data_types::Float::new(5.0).unwrap(),
        });
        let result = BinOp::Sub.isolate(true, &sibling);
        match result {
            SymExpr::BinOp(BinOp::Add, _, _) => {}
            _ => panic!("Expected BinOp(Add, ...)), got {:?}", result),
        }
    }

    #[test]
    fn binop_isolate_sub_target_right() {
        let sibling = SymExpr::Scalar(Scalar {
            dimension: Dimension::zero(),
            value: common_data_types::Float::new(5.0).unwrap(),
        });
        let result = BinOp::Sub.isolate(false, &sibling);
        match result {
            SymExpr::BinOp(BinOp::Sub, _, _) => {}
            _ => panic!("Expected BinOp(Sub, ...)), got {:?}", result),
        }
    }

    #[test]
    fn unary_neg_is_self_inverse() {
        let inner = SymExpr::Scalar(Scalar {
            dimension: Dimension::zero(),
            value: common_data_types::Float::new(5.0).unwrap(),
        });
        let result = UnaryOp::Neg.isolate(true, &inner);
        match result {
            SymExpr::UnaryOp(UnaryOp::Neg, _) => {}
            _ => panic!("Expected UnaryOp(Neg, ...)), got {:?}", result),
        }
    }

    #[test]
    fn collect_free_vars_basic() {
        let expr = SymExpr::BinOp(
            BinOp::Add,
            Box::new(SymExpr::Var("a".into())),
            Box::new(SymExpr::Var("b".into())),
        );
        let vars = collect_free_vars(&expr, "__result__");
        assert_eq!(vars, vec![ImString::from("a"), ImString::from("b")]);
    }

    #[test]
    fn collect_free_vars_excludes_result() {
        let expr = SymExpr::Var("__result__".into());
        let vars = collect_free_vars(&expr, "__result__");
        assert!(vars.is_empty());
    }

    #[test]
    fn binop_is_invertible() {
        assert!(BinOp::Add.is_invertible());
        assert!(BinOp::Sub.is_invertible());
        assert!(BinOp::Mul.is_invertible());
        assert!(BinOp::Div.is_invertible());
        assert!(BinOp::Pow.is_invertible());
    }

    #[test]
    fn boolop_not_invertible() {
        assert!(!BoolOp::And.is_invertible());
        assert!(!BoolOp::Or.is_invertible());
    }

    #[test]
    fn unaryop_is_invertible() {
        assert!(UnaryOp::Neg.is_invertible());
        assert!(UnaryOp::Not.is_invertible());
    }

    // Phase 1: Simplifier unit tests

    #[test]
    fn simplify_x_mul_x() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::BinOp(BinOp::Mul, Box::new(x.clone()), Box::new(x.clone()));
        let result = simplify_sym_expr(&expr);
        match result {
            SymExpr::BinOp(BinOp::Pow, base, exp) => {
                assert!(matches!(base.as_ref(), SymExpr::Var(v) if v == "x"));
                assert!(matches!(exp.as_ref(), SymExpr::Integer(2)));
            }
            _ => panic!("Expected Pow, got {:?}", result),
        }
    }

    #[test]
    fn simplify_x_add_x() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::BinOp(BinOp::Add, Box::new(x.clone()), Box::new(x.clone()));
        let result = simplify_sym_expr(&expr);
        match result {
            SymExpr::BinOp(BinOp::Mul, base, exp) => {
                assert!(matches!(base.as_ref(), SymExpr::Var(v) if v == "x"));
                assert!(matches!(exp.as_ref(), SymExpr::Integer(2)));
            }
            _ => panic!("Expected Mul, got {:?}", result),
        }
    }

    #[test]
    fn simplify_x_sub_x() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::BinOp(BinOp::Sub, Box::new(x.clone()), Box::new(x.clone()));
        let result = simplify_sym_expr(&expr);
        assert!(matches!(result, SymExpr::Scalar(s) if s.value == 0.0));
    }

    #[test]
    fn simplify_x_div_x() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::BinOp(BinOp::Div, Box::new(x.clone()), Box::new(x.clone()));
        let result = simplify_sym_expr(&expr);
        assert!(matches!(result, SymExpr::Integer(1)));
    }

    #[test]
    fn simplify_x_mul_1() {
        let x = SymExpr::Var("x".into());
        let one = SymExpr::Integer(1);
        let expr = SymExpr::BinOp(BinOp::Mul, Box::new(x.clone()), Box::new(one));
        let result = simplify_sym_expr(&expr);
        assert!(matches!(result, SymExpr::Var(v) if v == "x"));
    }

    #[test]
    fn simplify_1_mul_x() {
        let x = SymExpr::Var("x".into());
        let one = SymExpr::Integer(1);
        let expr = SymExpr::BinOp(BinOp::Mul, Box::new(one), Box::new(x.clone()));
        let result = simplify_sym_expr(&expr);
        assert!(matches!(result, SymExpr::Var(v) if v == "x"));
    }

    #[test]
    fn simplify_x_add_0() {
        let x = SymExpr::Var("x".into());
        let zero = SymExpr::Scalar(Scalar {
            dimension: Dimension::zero(),
            value: common_data_types::Float::new(0.0).unwrap(),
        });
        let expr = SymExpr::BinOp(BinOp::Add, Box::new(x.clone()), Box::new(zero));
        let result = simplify_sym_expr(&expr);
        assert!(matches!(result, SymExpr::Var(v) if v == "x"));
    }

    #[test]
    fn simplify_0_add_x() {
        let x = SymExpr::Var("x".into());
        let zero = SymExpr::Scalar(Scalar {
            dimension: Dimension::zero(),
            value: common_data_types::Float::new(0.0).unwrap(),
        });
        let expr = SymExpr::BinOp(BinOp::Add, Box::new(zero), Box::new(x.clone()));
        let result = simplify_sym_expr(&expr);
        assert!(matches!(result, SymExpr::Var(v) if v == "x"));
    }

    #[test]
    fn simplify_x_sub_0() {
        let x = SymExpr::Var("x".into());
        let zero = SymExpr::Scalar(Scalar {
            dimension: Dimension::zero(),
            value: common_data_types::Float::new(0.0).unwrap(),
        });
        let expr = SymExpr::BinOp(BinOp::Sub, Box::new(x.clone()), Box::new(zero));
        let result = simplify_sym_expr(&expr);
        assert!(matches!(result, SymExpr::Var(v) if v == "x"));
    }

    #[test]
    fn simplify_x_div_1() {
        let x = SymExpr::Var("x".into());
        let one = SymExpr::Integer(1);
        let expr = SymExpr::BinOp(BinOp::Div, Box::new(x.clone()), Box::new(one));
        let result = simplify_sym_expr(&expr);
        assert!(matches!(result, SymExpr::Var(v) if v == "x"));
    }

    #[test]
    fn simplify_x_pow_1() {
        let x = SymExpr::Var("x".into());
        let one = SymExpr::Integer(1);
        let expr = SymExpr::BinOp(BinOp::Pow, Box::new(x.clone()), Box::new(one));
        let result = simplify_sym_expr(&expr);
        assert!(matches!(result, SymExpr::Var(v) if v == "x"));
    }

    #[test]
    fn simplify_double_neg() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::UnaryOp(
            UnaryOp::Neg,
            Box::new(SymExpr::UnaryOp(UnaryOp::Neg, Box::new(x.clone()))),
        );
        let result = simplify_sym_expr(&expr);
        assert!(matches!(result, SymExpr::Var(v) if v == "x"));
    }

    #[test]
    fn simplify_double_not() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::UnaryOp(
            UnaryOp::Not,
            Box::new(SymExpr::UnaryOp(UnaryOp::Not, Box::new(x.clone()))),
        );
        let result = simplify_sym_expr(&expr);
        assert!(matches!(result, SymExpr::Var(v) if v == "x"));
    }

    #[test]
    fn simplify_no_change_different_vars() {
        let x = SymExpr::Var("x".into());
        let y = SymExpr::Var("y".into());
        let expr = SymExpr::BinOp(BinOp::Add, Box::new(x), Box::new(y));
        let result = simplify_sym_expr(&expr);
        assert!(matches!(&result, SymExpr::BinOp(BinOp::Add, left, right)
            if matches!(left.as_ref(), SymExpr::Var(v) if v == "x")
            && matches!(right.as_ref(), SymExpr::Var(v) if v == "y")));
    }

    #[test]
    fn simplify_nested_bottom_up() {
        // (x+x) + (x+x) → (x*2) + (x*2) — inner reductions happen, but parent
        // Add doesn't match x+x since operands are now Mul(x,2), not Var(x)
        let x = SymExpr::Var("x".into());
        let inner1 = SymExpr::BinOp(BinOp::Add, Box::new(x.clone()), Box::new(x.clone()));
        let inner2 = SymExpr::BinOp(BinOp::Add, Box::new(x.clone()), Box::new(x.clone()));
        let expr = SymExpr::BinOp(BinOp::Add, Box::new(inner1), Box::new(inner2));
        let result = simplify_sym_expr(&expr);
        // After bottom-up: inner x+x reduces to x*2, but outer Add(x*2, x*2) doesn't match
        match result {
            SymExpr::BinOp(BinOp::Add, left, right) => {
                assert!(matches!(left.as_ref(), SymExpr::BinOp(BinOp::Mul, _, _)));
                assert!(matches!(right.as_ref(), SymExpr::BinOp(BinOp::Mul, _, _)));
            }
            _ => panic!("Expected Add(Mul(x,2), Mul(x,2)), got {:?}", result),
        }
    }

    #[test]
    fn simplify_x_mul_x_nested() {
        // (x*x) * x → x^2 * x → x^3
        let x = SymExpr::Var("x".into());
        let inner = SymExpr::BinOp(BinOp::Mul, Box::new(x.clone()), Box::new(x.clone()));
        let expr = SymExpr::BinOp(BinOp::Mul, Box::new(inner), Box::new(x.clone()));
        let result = simplify_sym_expr(&expr);
        // Pow(x,2) * x → Pow(x,3) via new simplification rule
        match result {
            SymExpr::BinOp(BinOp::Pow, base, exp) => {
                assert!(matches!(*base, SymExpr::Var(v) if v == "x"));
                assert!(matches!(*exp, SymExpr::Integer(3)));
            }
            _ => panic!("Expected Pow(x,3), got {:?}", result),
        }
    }

    #[test]
    fn symexpr_display_var() {
        let expr = SymExpr::Var("x".into());
        assert_eq!(format!("{}", expr), "x");
    }

    #[test]
    fn symexpr_display_binop_add() {
        let x = SymExpr::Var("x".into());
        let y = SymExpr::Var("y".into());
        let expr = SymExpr::BinOp(BinOp::Add, Box::new(x), Box::new(y));
        assert_eq!(format!("{}", expr), "x + y");
    }

    #[test]
    fn symexpr_display_binop_mul_add_parens() {
        let x = SymExpr::Var("x".into());
        let y = SymExpr::Var("y".into());
        let z = SymExpr::Var("z".into());
        let add = SymExpr::BinOp(BinOp::Add, Box::new(x), Box::new(y));
        let expr = SymExpr::BinOp(BinOp::Mul, Box::new(add), Box::new(z));
        assert_eq!(format!("{}", expr), "(x + y) * z");
    }

    #[test]
    fn symexpr_display_binop_add_mul_no_parens() {
        let x = SymExpr::Var("x".into());
        let y = SymExpr::Var("y".into());
        let z = SymExpr::Var("z".into());
        let mul = SymExpr::BinOp(BinOp::Mul, Box::new(x), Box::new(y));
        let expr = SymExpr::BinOp(BinOp::Add, Box::new(mul), Box::new(z));
        assert_eq!(format!("{}", expr), "x * y + z");
    }

    #[test]
    fn symexpr_display_method_call() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::MethodCall {
            method_name: "sqrt".into(),
            self_expr: Box::new(x),
            args: vec![],
            args_names: vec![],
        };
        assert_eq!(format!("{}", expr), "x::sqrt");
    }

    #[test]
    fn symexpr_display_method_call_with_args() {
        let x = SymExpr::Var("x".into());
        let two = SymExpr::Integer(2);
        let expr = SymExpr::MethodCall {
            method_name: "pow".into(),
            self_expr: Box::new(x),
            args: vec![two],
            args_names: vec![crate::execution::values::dictionary::ArgumentName::Named("exp".into())],
        };
        assert_eq!(format!("{}", expr), "x::pow(exp=2.0)");
    }

    #[test]
    fn symexpr_display_sqrt_of_sub() {
        let result = SymExpr::Var("original_result".into());
        let y = SymExpr::Var("y".into());
        let sub = SymExpr::BinOp(BinOp::Sub, Box::new(result), Box::new(SymExpr::BinOp(BinOp::Mul, Box::new(y.clone()), Box::new(y))));
        let expr = SymExpr::MethodCall {
            method_name: "sqrt".into(),
            self_expr: Box::new(sub),
            args: vec![],
            args_names: vec![],
        };
        assert_eq!(format!("{}", expr), "(original_result - y * y)::sqrt");
    }

    #[test]
    fn differentiate_x_mul_x() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::BinOp(BinOp::Mul, Box::new(x.clone()), Box::new(x.clone()));
        let result = differentiate(&expr, "x");
        // d/dx(x²) = 1*x + x*1 (product rule, not simplified)
        match result {
            SymExpr::BinOp(BinOp::Add, left, right) => {
                assert!(matches!(left.as_ref(), SymExpr::BinOp(BinOp::Mul, _, _)));
                assert!(matches!(right.as_ref(), SymExpr::BinOp(BinOp::Mul, _, _)));
            }
            _ => panic!("Expected Add(Mul, Mul), got {:?}", result),
        }
    }

    #[test]
    fn differentiate_constant() {
        let expr = SymExpr::Scalar(Scalar {
            dimension: Dimension::zero(),
            value: Float::new(5.0).unwrap(),
        });
        let result = differentiate(&expr, "x");
        assert!(matches!(result, SymExpr::Scalar(s) if s.value == Float::new(0.0).unwrap()));
    }

    #[test]
    fn differentiate_sin_x() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::MethodCall {
            method_name: "sin".into(),
            self_expr: Box::new(x),
            args: vec![],
            args_names: vec![],
        };
        let result = differentiate(&expr, "x");
        // d/dx(sin(x)) = cos(x)
        match result {
            SymExpr::BinOp(BinOp::Mul, left, right) => {
                assert!(matches!(left.as_ref(), SymExpr::MethodCall { method_name, .. } if method_name == "cos"));
                assert!(matches!(right.as_ref(), SymExpr::Scalar(s) if s.value == Float::new(1.0).unwrap()));
            }
            _ => panic!("Expected Mul(cos(x), 1), got {:?}", result),
        }
    }

    #[test]
    fn integrate_x() {
        let x = SymExpr::Var("x".into());
        let result = integrate(&x, "x", 5).unwrap();
        // ∫x dx = x²/2
        match result {
            SymExpr::BinOp(BinOp::Div, left, right) => {
                assert!(matches!(right.as_ref(), SymExpr::Integer(2)));
                assert!(matches!(left.as_ref(), SymExpr::BinOp(BinOp::Pow, _, _)));
            }
            _ => panic!("Expected Div(x², 2), got {:?}", result),
        }
    }

    #[test]
    fn integrate_x_pow_2() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::BinOp(BinOp::Pow, Box::new(x.clone()), Box::new(SymExpr::Integer(2)));
        let result = integrate(&expr, "x", 5).unwrap();
        // ∫x² dx = x³/3
        match result {
            SymExpr::BinOp(BinOp::Div, left, right) => {
                assert!(matches!(right.as_ref(), SymExpr::Integer(3)));
                assert!(matches!(left.as_ref(), SymExpr::BinOp(BinOp::Pow, _, _)));
            }
            _ => panic!("Expected Div(x³, 3), got {:?}", result),
        }
    }

    #[test]
    fn integrate_sin_x() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::MethodCall {
            method_name: "sin".into(),
            self_expr: Box::new(x),
            args: vec![],
            args_names: vec![],
        };
        let result = integrate(&expr, "x", 5).unwrap();
        // ∫sin(x) dx = -cos(x)
        match result {
            SymExpr::UnaryOp(UnaryOp::Neg, inner) => {
                assert!(matches!(inner.as_ref(), SymExpr::MethodCall { method_name, .. } if method_name == "cos"));
            }
            _ => panic!("Expected Neg(cos(x)), got {:?}", result),
        }
    }

    #[test]
    fn integrate_by_parts_x_sin() {
        let x = SymExpr::Var("x".into());
        let sin_x = SymExpr::MethodCall {
            method_name: "sin".into(),
            self_expr: Box::new(x.clone()),
            args: vec![],
            args_names: vec![],
        };
        let expr = SymExpr::BinOp(BinOp::Mul, Box::new(x), Box::new(sin_x));
        let result = integrate(&expr, "x", 5);
        // ∫x*sin(x) dx should succeed (integration by parts)
        assert!(result.is_ok(), "Integration by parts should succeed: {:?}", result);
    }

    #[test]
    fn integrate_max_fails() {
        let x = SymExpr::Var("x".into());
        let five = SymExpr::Scalar(Scalar {
            dimension: Dimension::zero(),
            value: Float::new(5.0).unwrap(),
        });
        let expr = SymExpr::MethodCall {
            method_name: "max".into(),
            self_expr: Box::new(x),
            args: vec![five],
            args_names: vec![],
        };
        let result = integrate(&expr, "x", 5);
        assert!(result.is_err(), "Integration of max should fail");
        assert!(result.unwrap_err().contains("not integrable"));
    }

    #[test]
    fn differentiate_abs_x() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::MethodCall {
            method_name: "abs".into(),
            self_expr: Box::new(x),
            args: vec![],
            args_names: vec![],
        };
        let result = differentiate(&expr, "x");
        // d/dx|x| = signum(x) * 1
        match result {
            SymExpr::BinOp(BinOp::Mul, left, right) => {
                assert!(matches!(left.as_ref(), SymExpr::MethodCall { method_name, .. } if method_name == "signum"));
                assert!(matches!(right.as_ref(), SymExpr::Scalar(s) if s.value == Float::new(1.0).unwrap()));
            }
            _ => panic!("Expected Mul(signum(x), 1), got {:?}", result),
        }
    }

    #[test]
    fn differentiate_cbrt_x() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::MethodCall {
            method_name: "cbrt".into(),
            self_expr: Box::new(x),
            args: vec![],
            args_names: vec![],
        };
        let result = differentiate(&expr, "x");
        // d/dx∛x = 1/(3*∛x²)
        match result {
            SymExpr::BinOp(BinOp::Div, _, right) => {
                assert!(matches!(right.as_ref(), SymExpr::BinOp(BinOp::Mul, _, _)));
            }
            _ => panic!("Expected Div(1, 3*cbrt(x)²), got {:?}", result),
        }
    }

    #[test]
    fn differentiate_sqrt_x() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::MethodCall {
            method_name: "sqrt".into(),
            self_expr: Box::new(x),
            args: vec![],
            args_names: vec![],
        };
        let result = differentiate(&expr, "x");
        // d/dx√x = 1/(2*√x)
        match result {
            SymExpr::BinOp(BinOp::Div, _, right) => {
                assert!(matches!(right.as_ref(), SymExpr::BinOp(BinOp::Mul, _, _)));
            }
            _ => panic!("Expected Div(1, 2*sqrt(x)), got {:?}", result),
        }
    }

    #[test]
    fn differentiate_recip_x() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::MethodCall {
            method_name: "recip".into(),
            self_expr: Box::new(x),
            args: vec![],
            args_names: vec![],
        };
        let result = differentiate(&expr, "x");
        // d/dx(1/x) = -1/x²
        match result {
            SymExpr::BinOp(BinOp::Div, left, right) => {
                assert!(matches!(left.as_ref(), SymExpr::UnaryOp(UnaryOp::Neg, _)));
                assert!(matches!(right.as_ref(), SymExpr::BinOp(BinOp::Pow, _, _)));
            }
            _ => panic!("Expected Div(-1, x²), got {:?}", result),
        }
    }

    #[test]
    fn differentiate_tanh_x() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::MethodCall {
            method_name: "tanh".into(),
            self_expr: Box::new(x),
            args: vec![],
            args_names: vec![],
        };
        let result = differentiate(&expr, "x");
        // d/dxtanh(x) = 1 - tanh(x)²
        match result {
            SymExpr::BinOp(BinOp::Mul, left, right) => {
                assert!(matches!(left.as_ref(), SymExpr::BinOp(BinOp::Sub, _, _)));
                assert!(matches!(right.as_ref(), SymExpr::Scalar(s) if s.value == Float::new(1.0).unwrap()));
            }
            _ => panic!("Expected Mul(1-tanh(x)², 1), got {:?}", result),
        }
    }

    #[test]
    fn differentiate_signum_x() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::MethodCall {
            method_name: "signum".into(),
            self_expr: Box::new(x),
            args: vec![],
            args_names: vec![],
        };
        let result = differentiate(&expr, "x");
        // d/dxsignum(x) = 0
        assert!(matches!(result, SymExpr::Scalar(s) if s.value == Float::new(0.0).unwrap()));
    }

    #[test]
    fn differentiate_pow_x() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::MethodCall {
            method_name: "pow".into(),
            self_expr: Box::new(x),
            args: vec![SymExpr::Integer(3)],
            args_names: vec![],
        };
        let result = differentiate(&expr, "x");
        // d/dx(x³) = 3*x²
        match result {
            SymExpr::BinOp(BinOp::Mul, left, right) => {
                assert!(matches!(left.as_ref(), SymExpr::BinOp(BinOp::Mul, _, _)));
                assert!(matches!(right.as_ref(), SymExpr::Scalar(s) if s.value == Float::new(1.0).unwrap()));
            }
            _ => panic!("Expected Mul(3*x², 1), got {:?}", result),
        }
    }

    #[test]
    fn integrate_cbrt_x() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::MethodCall {
            method_name: "cbrt".into(),
            self_expr: Box::new(x),
            args: vec![],
            args_names: vec![],
        };
        let result = integrate(&expr, "x", 5).unwrap();
        // ∫∛x dx = 0.75 * x^(4/3)
        match result {
            SymExpr::BinOp(BinOp::Mul, left, right) => {
                assert!(matches!(left.as_ref(), SymExpr::Scalar(_)));
                assert!(matches!(right.as_ref(), SymExpr::BinOp(BinOp::Pow, _, _)));
            }
            _ => panic!("Expected Mul(0.75, x^(4/3)), got {:?}", result),
        }
    }

    #[test]
    fn integrate_sqrt_x() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::MethodCall {
            method_name: "sqrt".into(),
            self_expr: Box::new(x),
            args: vec![],
            args_names: vec![],
        };
        let result = integrate(&expr, "x", 5).unwrap();
        // ∫√x dx = 0.666667 * x^(3/2)
        match result {
            SymExpr::BinOp(BinOp::Mul, left, right) => {
                assert!(matches!(left.as_ref(), SymExpr::Scalar(_)));
                assert!(matches!(right.as_ref(), SymExpr::BinOp(BinOp::Pow, _, _)));
            }
            _ => panic!("Expected Mul(0.666667, x^(3/2)), got {:?}", result),
        }
    }

    #[test]
    fn integrate_recip_x() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::MethodCall {
            method_name: "recip".into(),
            self_expr: Box::new(x),
            args: vec![],
            args_names: vec![],
        };
        let result = integrate(&expr, "x", 5).unwrap();
        // ∫(1/x) dx = log(x)
        match result {
            SymExpr::MethodCall { method_name, .. } => {
                assert_eq!(method_name, "log");
            }
            _ => panic!("Expected log(x), got {:?}", result),
        }
    }

    #[test]
    fn derive_x_squared_e2e() {
        run_assert_eq(
            "let f = (x: std.scalar.Number) -> std.scalar.Number: x*x; in f::derive(wanted_output = \"x\")(3)",
            "6",
        );
    }

    #[test]
    fn derive_constant_e2e() {
        run_assert_eq(
            "let f = (x: std.scalar.Number) -> std.scalar.Number: 5.0; in f::derive(wanted_output = \"x\")(3)",
            "0",
        );
    }

    #[test]
    fn integrate_x_e2e() {
        run_assert_eq(
            "let f = (x: std.scalar.Number) -> std.scalar.Number: x; in f::integrate(wanted_output = \"x\")(3)",
            "4.5",
        );
    }

    #[test]
    fn integrate_x_pow_2_e2e() {
        run_assert_eq(
            "let f = (x: std.scalar.Number) -> std.scalar.Number: x*x; in f::integrate(wanted_output = \"x\")(3)",
            "9",
        );
    }

    #[test]
    fn integrate_max_fails_e2e() {
        let result = test_run(
            "let f = (x: std.scalar.Number) -> std.scalar.Number: x::max(other = 5); in f::integrate(wanted_output = \"x\")(0)",
        );
        assert!(result.is_err(), "Integration of max should fail");
    }

    #[test]
    fn derive_abs_x_e2e() {
        run_assert_eq(
            "let f = (x: std.scalar.Number) -> std.scalar.Number: x::abs(); in f::derive(wanted_output = \"x\")(5)",
            "1",
        );
    }

    #[test]
    fn derive_cbrt_x_e2e() {
        run_assert_eq(
            "let f = (x: std.scalar.Number) -> std.scalar.Number: x::cbrt(); in f::derive(wanted_output = \"x\")(1)",
            "0.3333333333333333",
        );
    }

    #[test]
    fn derive_sqrt_x_e2e() {
        run_assert_eq(
            "let f = (x: std.scalar.Number) -> std.scalar.Number: x::sqrt(); in f::derive(wanted_output = \"x\")(1)",
            "0.5",
        );
    }

    #[test]
    fn derive_recip_x_e2e() {
        run_assert_eq(
            "let f = (x: std.scalar.Number) -> std.scalar.Number: x::recip(); in f::derive(wanted_output = \"x\")(2)",
            "-0.25",
        );
    }

    #[test]
    fn derive_signum_x_e2e() {
        run_assert_eq(
            "let f = (x: std.scalar.Number) -> std.scalar.Number: x::signum(); in f::derive(wanted_output = \"x\")(5)",
            "0",
        );
    }

    #[test]
    fn derive_pow_x_e2e() {
        run_assert_eq(
            "let f = (x: std.scalar.Number) -> std.scalar.Number: x*x*x; in f::derive(wanted_output = \"x\")(2)",
            "12",
        );
    }

    #[test]
    fn integrate_cbrt_x_e2e() {
        run_assert_eq(
            "let f = (x: std.scalar.Number) -> std.scalar.Number: x::cbrt(); in f::integrate(wanted_output = \"x\")(1)",
            "0.75",
        );
    }

    #[test]
    fn integrate_sqrt_x_e2e() {
        run_assert_eq(
            "let f = (x: std.scalar.Number) -> std.scalar.Number: x::sqrt(); in f::integrate(wanted_output = \"x\")(1)",
            "0.666667",
        );
    }

    #[test]
    fn integrate_recip_x_e2e() {
        run_assert_eq(
            "let f = (x: std.scalar.Number) -> std.scalar.Number: x::recip(); in f::integrate(wanted_output = \"x\")(1)",
            "0",
        );
    }

    #[test]
    fn differentiate_vector2() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::Vector(vec![
            SymExpr::BinOp(BinOp::Mul, Box::new(x.clone()), Box::new(x.clone())),
            SymExpr::Scalar(Scalar { dimension: Dimension::zero(), value: Float::new(3.0).unwrap() }),
        ]);
        let result = differentiate(&expr, "x");
        match result {
            SymExpr::Vector(comps) => {
                assert_eq!(comps.len(), 2);
                // d/dx(x*x) = x + x (product rule), d/dx(3) = 0
                assert!(matches!(&comps[0], SymExpr::BinOp(BinOp::Add, _, _)));
                assert!(matches!(&comps[1], SymExpr::Scalar(_)));
            }
            _ => panic!("Expected Vector result, got {:?}", result),
        }
    }

    #[test]
    fn differentiate_vector3() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::Vector(vec![
            x.clone(),
            SymExpr::Integer(5),
            SymExpr::BinOp(BinOp::Mul, Box::new(x.clone()), Box::new(x)),
        ]);
        let result = differentiate(&expr, "x");
        match result {
            SymExpr::Vector(comps) => {
                assert_eq!(comps.len(), 3);
                // d/dx(x) = 1, d/dx(5) = 0, d/dx(x*x) = 2x
            }
            _ => panic!("Expected Vector result, got {:?}", result),
        }
    }

    #[test]
    fn integrate_vector2() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::Vector(vec![
            x.clone(),
            SymExpr::Integer(2),
        ]);
        let result = integrate(&expr, "x", 5).unwrap();
        match result {
            SymExpr::Vector(comps) => {
                assert_eq!(comps.len(), 2);
                // ∫x dx = 0.5*x^2, ∫2 dx = 2x
            }
            _ => panic!("Expected Vector result, got {:?}", result),
        }
    }

    #[test]
    fn integrate_vector3() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::Vector(vec![
            SymExpr::BinOp(BinOp::Mul, Box::new(x.clone()), Box::new(x.clone())),
            x.clone(),
            SymExpr::Integer(1),
        ]);
        let result = integrate(&expr, "x", 5).unwrap();
        match result {
            SymExpr::Vector(comps) => {
                assert_eq!(comps.len(), 3);
            }
            _ => panic!("Expected Vector result, got {:?}", result),
        }
    }

    #[test]
    fn sym_expr_contains_var_in_vector() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::Vector(vec![
            x.clone(),
            SymExpr::Integer(5),
        ]);
        assert!(sym_expr_contains_var(&expr, "x"));
        assert!(!sym_expr_contains_var(&expr, "y"));
    }

    #[test]
    fn count_var_occurrences_in_vector() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::Vector(vec![
            SymExpr::BinOp(BinOp::Mul, Box::new(x.clone()), Box::new(x.clone())),
            x,
        ]);
        assert_eq!(count_var_occurrences(&expr, "x"), 3);
    }

    #[test]
    fn collect_free_vars_in_vector() {
        let x = SymExpr::Var("x".into());
        let y = SymExpr::Var("y".into());
        let expr = SymExpr::Vector(vec![x, y]);
        let vars = collect_free_vars(&expr, "result");
        assert!(vars.contains(&ImString::from("x")));
        assert!(vars.contains(&ImString::from("y")));
    }

    #[test]
    fn simplify_sym_expr_vector() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::Vector(vec![
            SymExpr::BinOp(BinOp::Mul, Box::new(x.clone()), Box::new(SymExpr::Integer(1))),
            SymExpr::Integer(0),
        ]);
        let result = simplify_sym_expr(&expr);
        match result {
            SymExpr::Vector(comps) => {
                assert_eq!(comps.len(), 2);
                // x*1 → x, 0 stays 0
            }
            _ => panic!("Expected Vector result, got {:?}", result),
        }
    }

    #[test]
    fn infer_sym_expr_type_vector2() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::Vector(vec![x, SymExpr::Integer(5)]);
        let ty = infer_sym_expr_type(&expr);
        assert!(matches!(ty, ValueType::Vector2(_)));
    }

    #[test]
    fn infer_sym_expr_type_vector3() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::Vector(vec![x, SymExpr::Integer(5), SymExpr::Integer(10)]);
        let ty = infer_sym_expr_type(&expr);
        assert!(matches!(ty, ValueType::Vector3(_)));
    }

    #[test]
    fn infer_sym_expr_type_vector4() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::Vector(vec![x, SymExpr::Integer(5), SymExpr::Integer(10), SymExpr::Integer(15)]);
        let ty = infer_sym_expr_type(&expr);
        assert!(matches!(ty, ValueType::Vector4(_)));
    }

    #[test]
    fn display_vector2() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::Vector(vec![x.clone(), SymExpr::Integer(5)]);
        let s = expr.to_string();
        assert!(s.contains("Vector2"));
        assert!(s.contains("x"));
        assert!(s.contains("5.0"));
    }

    #[test]
    fn display_vector3() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::Vector(vec![x.clone(), SymExpr::Integer(5), SymExpr::Integer(10)]);
        let s = expr.to_string();
        assert!(s.contains("Vector3"));
    }

    #[test]
    fn display_vector4() {
        let x = SymExpr::Var("x".into());
        let expr = SymExpr::Vector(vec![x, SymExpr::Integer(1), SymExpr::Integer(2), SymExpr::Integer(3)]);
        let s = expr.to_string();
        assert!(s.contains("Vector4"));
    }

    #[test]
    fn expression_to_sym_expr_vector2() {
        let result = test_run(
            "let v = (1.0, 2.0); in v",
        );
        assert!(result.is_ok());
    }

    #[test]
    fn expression_to_sym_expr_vector3() {
        let result = test_run(
            "let v = (1.0, 2.0, 3.0); in v",
        );
        assert!(result.is_ok());
    }

    #[test]
    fn differentiate_vector_e2e() {
        run_assert_eq(
            "let f = (x: std.scalar.Number) -> std.vector2.Any: {x*x, x*3}; in f::derive(wanted_output = \"x\")(2)",
            "{4, 3}",
        );
    }

    #[test]
    fn integrate_vector_e2e() {
        run_assert_eq(
            "let f = (x: std.scalar.Number) -> std.vector2.Any: {x, x*2}; in f::integrate(wanted_output = \"x\")(3)",
            "{4.5, 9}",
        );
    }

    #[test]
    fn differentiate_vector3_e2e() {
        run_assert_eq(
            "let f = (t: std.scalar.Number) -> std.vector3.Any: {t*t, t*t*t, t*5}; in f::derive(wanted_output = \"t\")(2)",
            "{4, 12, 5}",
        );
    }

#[test]
    fn integrate_vector3_e2e() {
        run_assert_eq(
            "let f = (t: std.scalar.Number) -> std.vector3.Any: {t, t*t, t*t*t}; in f::integrate(wanted_output = \"t\")(1)",
            "{0.5, 1/3.0, 1/4.0}",
        );
    }

    #[test]
    fn differentiate_member_access_vector() {
        let result = test_run(
            "let f = (v: std.vector2.Any) -> std.scalar.Number: v.x*v.x + v.y*v.y; in f::derive(wanted_output = \"v\")(1)",
        );
        assert!(result.is_err());
    }

    #[test]
    fn sym_expr_member_access_display() {
        let var_x = SymExpr::Var("x".into());
        let member = SymExpr::MemberAccess {
            base: Box::new(var_x),
            member: "x".into(),
        };
        let s = member.to_string();
        assert_eq!(s, "x.x");
    }

    #[test]
    fn sym_expr_member_access_contains_var() {
        let var_x = SymExpr::Var("x".into());
        let member = SymExpr::MemberAccess {
            base: Box::new(var_x),
            member: "y".into(),
        };
        assert!(sym_expr_contains_var(&member, "x"));
        assert!(!sym_expr_contains_var(&member, "z"));
    }

    #[test]
    fn sym_expr_member_access_count_occurrences() {
        let var_x = SymExpr::Var("x".into());
        let member = SymExpr::MemberAccess {
            base: Box::new(var_x),
            member: "x".into(),
        };
        assert_eq!(count_var_occurrences(&member, "x"), 1);
    }

    #[test]
    fn sym_expr_member_access_collect_free_vars() {
        let var_x = SymExpr::Var("x".into());
        let member = SymExpr::MemberAccess {
            base: Box::new(var_x),
            member: "y".into(),
        };
        let vars = collect_free_vars(&member, "target");
        assert_eq!(vars.len(), 1);
        assert_eq!(vars[0], "x");
    }

    #[test]
    fn sym_expr_member_access_simplify() {
        let x = SymExpr::Var("x".into());
        let member = SymExpr::MemberAccess {
            base: Box::new(SymExpr::BinOp(BinOp::Add, Box::new(x.clone()), Box::new(SymExpr::Scalar(Scalar { dimension: Dimension::zero(), value: Float::new(0.0).unwrap() })))),
            member: "x".into(),
        };
        let simplified = simplify_sym_expr(&member);
        match simplified {
            SymExpr::MemberAccess { base, member: m } => {
                assert_eq!(m, "x");
                assert!(matches!(*base, SymExpr::Var(_)));
            }
            _ => panic!("Expected MemberAccess"),
        }
    }

    #[test]
    fn sym_expr_member_access_substitute() {
        let x = SymExpr::Var("__result__".into());
        let member = SymExpr::MemberAccess {
            base: Box::new(x),
            member: "x".into(),
        };
        let replacement = SymExpr::Scalar(Scalar { dimension: Dimension::zero(), value: Float::new(5.0).unwrap() });
        let substituted = substitute_result(&member, &replacement);
        match substituted {
            SymExpr::MemberAccess { base, member: m } => {
                assert_eq!(m, "x");
                match *base {
                    SymExpr::Scalar(s) => assert_eq!(s.value, 5.0),
                    _ => panic!("Expected Scalar, got {:?}", base),
                }
            }
            _ => panic!("Expected MemberAccess, got {:?}", substituted),
        }
    }

    #[test]
    fn sym_expr_member_access_equal() {
        let x1 = SymExpr::Var("x".into());
        let x2 = SymExpr::Var("x".into());
        let m1 = SymExpr::MemberAccess { base: Box::new(x1), member: "y".into() };
        let m2 = SymExpr::MemberAccess { base: Box::new(x2), member: "y".into() };
        assert!(sym_exprs_equal(&m1, &m2));
    }

    #[test]
    fn infer_sym_expr_type_member_access() {
        let x = SymExpr::Var("x".into());
        let member = SymExpr::MemberAccess {
            base: Box::new(x),
            member: "x".into(),
        };
        let ty = infer_sym_expr_type(&member);
        assert!(matches!(ty, ValueType::Scalar(_)));
    }

    #[test]
    fn differentiate_member_access() {
        let x = SymExpr::Var("x".into());
        let member = SymExpr::MemberAccess {
            base: Box::new(SymExpr::BinOp(BinOp::Mul, Box::new(x.clone()), Box::new(x))),
            member: "x".into(),
        };
        let deriv = differentiate(&member, "x");
        match deriv {
            SymExpr::MemberAccess { base, member: m } => {
                assert_eq!(m, "x");
                assert!(matches!(*base, SymExpr::BinOp(_, _, _)));
            }
            _ => panic!("Expected MemberAccess"),
        }
    }

    #[test]
    fn integrate_member_access() {
        let x = SymExpr::Var("x".into());
        let member = SymExpr::MemberAccess {
            base: Box::new(x),
            member: "y".into(),
        };
        let integrated = integrate(&member, "x", 5).unwrap();
        match integrated {
            SymExpr::MemberAccess { base, member: m } => {
                assert_eq!(m, "y");
                assert!(matches!(*base, SymExpr::BinOp(_, _, _)));
            }
            _ => panic!("Expected MemberAccess"),
        }
    }

    #[test]
    fn differentiate_member_access_e2e() {
        run_assert_eq(
            "let f = (x: std.scalar.Number) -> std.scalar.Number: x*x; in f::derive(wanted_output = \"x\")(3)",
            "6",
        );
    }

    #[test]
    fn integrate_member_access_e2e() {
        run_assert_eq(
            "let f = (x: std.scalar.Number) -> std.scalar.Number: x; in f::integrate(wanted_output = \"x\")(2)",
            "2",
        );
    }
}
