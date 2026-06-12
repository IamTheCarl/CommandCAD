use super::{BinOp as BinOpType, Scalar as ScalarStruct, SymExpr, UnaryOp};
use common_data_types::Dimension;

/// Represents a polynomial: poly[i] = coefficient of x^i
#[derive(Debug, Clone)]
pub struct Polynomial {
    pub coefficients: Vec<SymExpr>, // coefficients[0] = constant term
}

impl Polynomial {
    pub fn degree(&self) -> usize {
        self.coefficients.len().saturating_sub(1)
    }

    pub fn is_zero(&self) -> bool {
        self.coefficients
            .iter()
            .all(|c| matches!(c, SymExpr::Scalar(s) if s.value == 0.0))
    }
}

/// Zero scalar constant.
fn zero_scalar() -> ScalarStruct {
    ScalarStruct {
        dimension: Dimension::zero(),
        value: common_data_types::Float::new(0.0).unwrap(),
    }
}

/// One scalar constant.
fn one_scalar() -> ScalarStruct {
    ScalarStruct {
        dimension: Dimension::zero(),
        value: common_data_types::Float::new(1.0).unwrap(),
    }
}

/// Extract polynomial coefficients from a SymExpr.
/// Returns None if the expression is not a polynomial in the target variable.
/// The constant term (index 0) already accounts for -result.
pub fn extract_polynomial(
    expr: &SymExpr,
    target: &str,
    result_name: &str,
) -> Option<Polynomial> {
    let mut terms: Vec<(usize, SymExpr)> = Vec::new();

    // Decompose the expression into terms by walking Add/Sub tree
    decompose_terms(expr, target, &mut terms, 1);

    if terms.is_empty() {
        return None;
    }

    // Sum coefficients for same power
    let mut coeff_map: std::collections::HashMap<usize, SymExpr> = std::collections::HashMap::new();
    for (power, coeff) in terms {
        let existing = coeff_map
            .entry(power)
            .or_insert_with(|| SymExpr::Scalar(zero_scalar()));

        let new_coeff = match existing {
            SymExpr::Scalar(a) => match coeff {
                SymExpr::Scalar(b) => SymExpr::Scalar(ScalarStruct {
                    dimension: a.dimension + b.dimension,
                    value: a.value + b.value,
                }),
                SymExpr::Integer(b) => SymExpr::Scalar(ScalarStruct {
                    dimension: a.dimension,
                    value: a.value + common_data_types::Float::new(b as f64).unwrap(),
                }),
                c => SymExpr::BinOp(BinOpType::Add, Box::new(existing.clone()), Box::new(c)),
            },
            SymExpr::Integer(a) => match coeff {
                SymExpr::Scalar(b) => SymExpr::Scalar(ScalarStruct {
                    dimension: b.dimension,
                    value: b.value + common_data_types::Float::new(*a as f64).unwrap(),
                }),
                SymExpr::Integer(b) => SymExpr::Integer(*a + b),
                c => SymExpr::BinOp(BinOpType::Add, Box::new(existing.clone()), Box::new(c)),
            },
            _ => SymExpr::BinOp(BinOpType::Add, Box::new(existing.clone()), Box::new(coeff)),
        };

        *existing = new_coeff;
    }

    // Build coefficients vector sorted by power
    let max_power = *coeff_map.keys().max()?;
    let mut coefficients = Vec::with_capacity(max_power + 1);
    for i in 0..=max_power {
        coefficients.push(
            coeff_map
                .get(&i)
                .cloned()
                .unwrap_or_else(|| SymExpr::Scalar(zero_scalar())),
        );
    }

    // Adjust constant term: subtract result
    if !coefficients.is_empty() {
        let constant = &coefficients[0];
        coefficients[0] = SymExpr::BinOp(
            BinOpType::Sub,
            Box::new(constant.clone()),
            Box::new(SymExpr::Var(result_name.into())),
        );
    }

    let poly = Polynomial { coefficients };

    // Only return if degree is 2 or 3
    if poly.degree() >= 2 && poly.degree() <= 3 {
        Some(poly)
    } else {
        None
    }
}

/// Recursively decompose an expression into terms.
/// `sign` is +1 for positive terms, -1 for negative terms (from Sub).
fn decompose_terms(expr: &SymExpr, target: &str, terms: &mut Vec<(usize, SymExpr)>, sign: i8) {
    match expr {
        SymExpr::BinOp(BinOpType::Add, left, right) => {
            decompose_terms(left, target, terms, sign);
            decompose_terms(right, target, terms, sign);
        }
        SymExpr::BinOp(BinOpType::Sub, left, right) => {
            decompose_terms(left, target, terms, sign);
            decompose_terms(right, target, terms, -sign);
        }
        _ => {
            // Extract (power, coefficient) from this term
            if let Some((power, coeff)) = extract_monomial(expr, target) {
                if sign == -1 {
                    let negated = SymExpr::UnaryOp(UnaryOp::Neg, Box::new(coeff));
                    terms.push((power, negated));
                } else {
                    terms.push((power, coeff));
                }
            }
        }
    }
}

/// Extract (power, coefficient) from a monomial term.
/// Returns None if the term is not a monomial in the target variable.
fn extract_monomial(expr: &SymExpr, target: &str) -> Option<(usize, SymExpr)> {
    match expr {
        // Just the target variable: x → (1, 1)
        SymExpr::Var(v) if v == target => Some((1, SymExpr::Scalar(one_scalar()))),

        // x^n → (n, 1)
        SymExpr::BinOp(BinOpType::Pow, base, exp) => {
            if let SymExpr::Var(v) = base.as_ref() {
                if v == target {
                    if let SymExpr::Integer(n) = exp.as_ref() {
                        return Some((*n as usize, SymExpr::Scalar(one_scalar())));
                    }
                }
            }
            None
        }

        // Product: recursively extract power and coefficient from all factors
        SymExpr::BinOp(BinOpType::Mul, left, right) => {
            let left_result = extract_monomial(left.as_ref(), target)?;
            let right_result = extract_monomial(right.as_ref(), target)?;

            // Combine: add powers, multiply coefficients
            let total_power = left_result.0 + right_result.0;
            let coeff = if left_result.0 > 0 && right_result.0 > 0 {
                // Both sides contain the target variable - this is still a monomial
                // as long as they're the same variable (e.g., x * x = x^2)
                multiply_scalar_exprs(&left_result.1, &right_result.1)?
            } else if left_result.0 > 0 {
                multiply_scalar_exprs(&left_result.1, &right_result.1)?
            } else {
                multiply_scalar_exprs(&right_result.1, &left_result.1)?
            };
            Some((total_power, coeff))
        }

        // Scalar constant: (0, scalar_value)
        SymExpr::Scalar(_s) => Some((0, expr.clone())),
        SymExpr::Integer(i) => Some((0, SymExpr::Scalar(ScalarStruct {
            dimension: Dimension::zero(),
            value: common_data_types::Float::new(*i as f64).unwrap(),
        }))),

        // Unary negation: -term → negate the term
        SymExpr::UnaryOp(UnaryOp::Neg, inner) => {
            if let Some((power, coeff)) = extract_monomial(inner.as_ref(), target) {
                Some((power, SymExpr::UnaryOp(UnaryOp::Neg, Box::new(coeff))))
            } else {
                None
            }
        }

        _ => None,
    }
}

/// Multiply two scalar SymExprs together.
fn multiply_scalar_exprs(a: &SymExpr, b: &SymExpr) -> Option<SymExpr> {
    match (a, b) {
        (SymExpr::Scalar(sa), SymExpr::Scalar(sb)) => Some(SymExpr::Scalar(ScalarStruct {
            dimension: sa.dimension,
            value: sa.value * sb.value,
        })),
        (SymExpr::Integer(a), SymExpr::Integer(b)) => Some(SymExpr::Integer(*a * *b)),
        (SymExpr::Scalar(a), SymExpr::Integer(b)) => Some(SymExpr::Scalar(ScalarStruct {
            dimension: a.dimension,
            value: a.value * common_data_types::Float::new(*b as f64).unwrap(),
        })),
        (SymExpr::Integer(a), SymExpr::Scalar(b)) => Some(SymExpr::Scalar(ScalarStruct {
            dimension: b.dimension,
            value: b.value * common_data_types::Float::new(*a as f64).unwrap(),
        })),
        _ => Some(SymExpr::BinOp(BinOpType::Mul, Box::new(a.clone()), Box::new(b.clone()))),
    }
}

/// Solve a quadratic equation ax² + bx + c = 0.
/// Returns the "+" root as a SymExpr.
pub fn solve_quadratic(
    a: &SymExpr,
    b: &SymExpr,
    c: &SymExpr,
    _context: &crate::execution::ExecutionContext,
) -> crate::execution::errors::ExecutionResult<SymExpr> {
    // The discriminant b² - 4ac must have consistent dimensions.
    // When b is zero (dimensionless), we need to cast it to match 4ac's dimension.
    let a_dim = extract_dimension(a);
    let c_dim = extract_dimension(c);

    // Compute reference dimension for the discriminant from 4ac.
    // 4ac has dimension D_a * D_c.
    let disc_dim = if a_dim == Dimension::zero() {
        c_dim
    } else if c_dim == Dimension::zero() {
        a_dim
    } else {
        // Both have dimensions — use c's dimension as it typically carries the result dimension
        c_dim
    };

    // Compute b² with proper dimension.
    let b_squared = SymExpr::BinOp(BinOpType::Mul, Box::new(b.clone()), Box::new(b.clone()));

    // 4ac
    let four = cast_to_dimension(&SymExpr::Scalar(ScalarStruct {
        dimension: Dimension::zero(),
        value: common_data_types::Float::new(4.0).unwrap(),
    }), &disc_dim);
    let four_a = SymExpr::BinOp(BinOpType::Mul, Box::new(four), Box::new(a.clone()));
    let four_ac = SymExpr::BinOp(BinOpType::Mul, Box::new(four_a), Box::new(c.clone()));

    // Cast b_squared to disc_dim if needed (handles b=0 case)
    let b_squared_dim = extract_dimension(&b_squared);
    let b_squared = if b_squared_dim != disc_dim && b_squared_dim == Dimension::zero() {
        cast_to_dimension(&b_squared, &disc_dim)
    } else {
        b_squared
    };

    let discriminant = SymExpr::BinOp(BinOpType::Sub, Box::new(b_squared), Box::new(four_ac));

    // sqrt(D)
    let sqrt_disc = SymExpr::MethodCall {
        method_name: "sqrt".into(),
        self_expr: Box::new(discriminant),
        args: vec![],
        args_names: vec![],
    };

    // -b
    let neg_b = SymExpr::UnaryOp(UnaryOp::Neg, Box::new(b.clone()));

    // (-b + sqrt(D)) / (2a)
    // Cast -b to disc_dim if needed (handles b=0 case)
    let neg_b_dim = extract_dimension(&neg_b);
    let neg_b = if neg_b_dim != disc_dim && neg_b_dim == Dimension::zero() {
        cast_to_dimension(&neg_b, &disc_dim)
    } else {
        neg_b
    };
    let numerator = SymExpr::BinOp(BinOpType::Add, Box::new(neg_b), Box::new(sqrt_disc));
    let two = SymExpr::Scalar(ScalarStruct {
        dimension: Dimension::zero(),
        value: common_data_types::Float::new(2.0).unwrap(),
    });
    let denominator = SymExpr::BinOp(BinOpType::Mul, Box::new(two), Box::new(a.clone()));

    Ok(SymExpr::BinOp(BinOpType::Div, Box::new(numerator), Box::new(denominator)))
}

/// Cast a dimensionless expression to the given dimension.
/// If the expression already has the target dimension, returns it unchanged.
fn cast_to_dimension(expr: &SymExpr, target_dim: &Dimension) -> SymExpr {
    let expr_dim = extract_dimension(expr);
    if expr_dim == *target_dim {
        return expr.clone();
    }
    // For dimensionless expressions, create a new scalar with the target dimension
    // This handles cases like b=0 (dimensionless) that need to be subtracted from 4ac (has dimension)
    if expr_dim == Dimension::zero() && *target_dim != Dimension::zero() {
        // Evaluate the expression to get its numeric value, then create a dimensioned scalar
        if let SymExpr::Scalar(s) = expr {
            return SymExpr::Scalar(ScalarStruct {
                dimension: *target_dim,
                value: s.value,
            });
        }
        // For BinOp like 0*0, evaluate to get the value
        if let SymExpr::BinOp(op, left, right) = expr {
            if let (SymExpr::Scalar(ls), SymExpr::Scalar(rs)) = (left.as_ref(), right.as_ref()) {
                let value = match op {
                    BinOpType::Add => ls.value + rs.value,
                    BinOpType::Sub => ls.value - rs.value,
                    BinOpType::Mul => ls.value * rs.value,
                    BinOpType::Div => ls.value / rs.value,
                    _ => ls.value,
                };
                return SymExpr::Scalar(ScalarStruct {
                    dimension: *target_dim,
                    value,
                });
            }
        }
    }
    expr.clone()
}

/// Extract the dimension from a SymExpr.
/// For Scalars, returns the dimension directly.
/// For other expressions, tries to find a scalar child's dimension.
fn extract_dimension(expr: &SymExpr) -> Dimension {
    match expr {
        SymExpr::Scalar(s) => s.dimension,
        SymExpr::Integer(_) => Dimension::zero(),
        SymExpr::BinOp(_, left, right) => {
            // Prefer non-zero dimension
            let l = extract_dimension(left);
            let r = extract_dimension(right);
            if l == Dimension::zero() { r } else { l }
        }
        SymExpr::UnaryOp(_, inner) => extract_dimension(inner),
        SymExpr::Var(_) => Dimension::zero(),
        SymExpr::MethodCall { self_expr, .. } => extract_dimension(self_expr),
        SymExpr::Boolean(_) => Dimension::zero(),
        SymExpr::BoolOp(_, left, right) => {
            let l = extract_dimension(left);
            let r = extract_dimension(right);
            if l == Dimension::zero() { r } else { l }
        }
SymExpr::Vector(comps) => comps.first()
            .map(extract_dimension)
            .unwrap_or_else(Dimension::zero),
        SymExpr::MemberAccess { base, member: _ } => extract_dimension(base),
    }
}

/// Solve a cubic equation ax³ + bx² + cx + d = 0.
/// Returns the first real root as a SymExpr using Cardano's formula.
pub fn solve_cubic(
    a: &SymExpr,
    b: &SymExpr,
    c: &SymExpr,
    d: &SymExpr,
    _context: &crate::execution::ExecutionContext,
) -> crate::execution::errors::ExecutionResult<SymExpr> {
    // Depress the cubic: substitute x = t - b/(3a)
    // p = (3ac - b²) / (3a²)
    // q = (2b³ - 9abc + 27a²d) / (27a³)

    let three = SymExpr::Scalar(ScalarStruct {
        dimension: Dimension::zero(),
        value: common_data_types::Float::new(3.0).unwrap(),
    });
    let two = SymExpr::Scalar(ScalarStruct {
        dimension: Dimension::zero(),
        value: common_data_types::Float::new(2.0).unwrap(),
    });
    let nine = SymExpr::Scalar(ScalarStruct {
        dimension: Dimension::zero(),
        value: common_data_types::Float::new(9.0).unwrap(),
    });
    let twentyseven = SymExpr::Scalar(ScalarStruct {
        dimension: Dimension::zero(),
        value: common_data_types::Float::new(27.0).unwrap(),
    });

    // p = (3ac - b²) / (3a²)
    let a_c = SymExpr::BinOp(BinOpType::Mul, Box::new(a.clone()), Box::new(c.clone()));
    let three_ac = SymExpr::BinOp(BinOpType::Mul, Box::new(three.clone()), Box::new(a_c));
    let b_squared = SymExpr::BinOp(BinOpType::Mul, Box::new(b.clone()), Box::new(b.clone()));
    let p_numerator = SymExpr::BinOp(BinOpType::Sub, Box::new(three_ac), Box::new(b_squared));
    let a_squared = SymExpr::BinOp(BinOpType::Mul, Box::new(a.clone()), Box::new(a.clone()));
    let p_denominator = SymExpr::BinOp(BinOpType::Mul, Box::new(three.clone()), Box::new(a_squared.clone()));
    let p = SymExpr::BinOp(BinOpType::Div, Box::new(p_numerator), Box::new(p_denominator));

    // q = (2b³ - 9abc + 27a²d) / (27a³)
    let b_sq = SymExpr::BinOp(BinOpType::Mul, Box::new(b.clone()), Box::new(b.clone()));
    let b_cubed = SymExpr::BinOp(BinOpType::Mul, Box::new(b_sq), Box::new(b.clone()));
    let two_b_cubed = SymExpr::BinOp(BinOpType::Mul, Box::new(two.clone()), Box::new(b_cubed));
    let a_b = SymExpr::BinOp(BinOpType::Mul, Box::new(a.clone()), Box::new(b.clone()));
    let a_b_c = SymExpr::BinOp(BinOpType::Mul, Box::new(a_b), Box::new(c.clone()));
    let nine_abc = SymExpr::BinOp(BinOpType::Mul, Box::new(nine.clone()), Box::new(a_b_c));
    let a_sq_d = SymExpr::BinOp(BinOpType::Mul, Box::new(a_squared.clone()), Box::new(d.clone()));
    let twentyseven_a_sq_d = SymExpr::BinOp(BinOpType::Mul, Box::new(twentyseven.clone()), Box::new(a_sq_d));
    let q_numerator = SymExpr::BinOp(BinOpType::Add, Box::new(SymExpr::BinOp(BinOpType::Sub, Box::new(two_b_cubed), Box::new(nine_abc))), Box::new(twentyseven_a_sq_d));
    let a_cubed = SymExpr::BinOp(BinOpType::Mul, Box::new(a_squared.clone()), Box::new(a.clone()));
    let q_denominator = SymExpr::BinOp(BinOpType::Mul, Box::new(twentyseven.clone()), Box::new(a_cubed));
    let q = SymExpr::BinOp(BinOpType::Div, Box::new(q_numerator), Box::new(q_denominator));

    // Discriminant: Δ = q²/4 + p³/27
    let q_squared = SymExpr::BinOp(BinOpType::Mul, Box::new(q.clone()), Box::new(q.clone()));
    let four = SymExpr::Scalar(ScalarStruct {
        dimension: Dimension::zero(),
        value: common_data_types::Float::new(4.0).unwrap(),
    });
    let q_squared_over_4 = SymExpr::BinOp(BinOpType::Div, Box::new(q_squared), Box::new(four));
    let p_sq = SymExpr::BinOp(BinOpType::Mul, Box::new(p.clone()), Box::new(p.clone()));
    let p_cubed = SymExpr::BinOp(BinOpType::Mul, Box::new(p_sq), Box::new(p.clone()));
    let p_cubed_over_27 = SymExpr::BinOp(BinOpType::Div, Box::new(p_cubed), Box::new(twentyseven.clone()));
    let delta = SymExpr::BinOp(BinOpType::Add, Box::new(q_squared_over_4), Box::new(p_cubed_over_27));

    // Try Δ ≥ 0 case first (one real root)
    // u = cbrt(-q/2 + sqrt(Δ))
    // v = cbrt(-q/2 - sqrt(Δ))
    // t = u + v
    // x = t - b/(3a)

    let two = SymExpr::Scalar(ScalarStruct {
        dimension: Dimension::zero(),
        value: common_data_types::Float::new(2.0).unwrap(),
    });
    let neg_q_over_2 = SymExpr::UnaryOp(UnaryOp::Neg, Box::new(SymExpr::BinOp(BinOpType::Div, Box::new(q.clone()), Box::new(two))));

    let sqrt_delta = SymExpr::MethodCall {
        method_name: "sqrt".into(),
        self_expr: Box::new(delta),
        args: vec![],
        args_names: vec![],
    };

    let u_arg = SymExpr::BinOp(BinOpType::Add, Box::new(SymExpr::UnaryOp(UnaryOp::Neg, Box::new(neg_q_over_2.clone()))), Box::new(sqrt_delta.clone()));
    let v_arg = SymExpr::BinOp(BinOpType::Sub, Box::new(SymExpr::UnaryOp(UnaryOp::Neg, Box::new(neg_q_over_2))), Box::new(sqrt_delta));

    let u = SymExpr::MethodCall {
        method_name: "cbrt".into(),
        self_expr: Box::new(u_arg),
        args: vec![],
        args_names: vec![],
    };

    let v = SymExpr::MethodCall {
        method_name: "cbrt".into(),
        self_expr: Box::new(v_arg),
        args: vec![],
        args_names: vec![],
    };

    let t = SymExpr::BinOp(BinOpType::Add, Box::new(u), Box::new(v));

    // x = t - b/(3a)
    let three = SymExpr::Scalar(ScalarStruct {
        dimension: Dimension::zero(),
        value: common_data_types::Float::new(3.0).unwrap(),
    });
    let b_over_3a = SymExpr::BinOp(
        BinOpType::Div,
        Box::new(b.clone()),
        Box::new(SymExpr::BinOp(BinOpType::Mul, Box::new(three), Box::new(a.clone()))),
    );

    let x = SymExpr::BinOp(BinOpType::Sub, Box::new(t), Box::new(b_over_3a));

    Ok(x)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::compile::full_compile;
    use crate::execution::values::closure::solve::algorithm::expression_to_sym_expr;
    use crate::execution::values::closure::solve::simplify_sym_expr;

    fn test_context<R>(f: impl FnOnce(&crate::execution::ExecutionContext) -> R) -> R {
        crate::execution::test_context([], f)
    }

    #[test]
    fn extract_polynomial_quadratic_simple() {
        let expr = full_compile("x * x + 3 * x + 2");
        let sym = test_context(|ctx| expression_to_sym_expr(&expr, ctx)).unwrap();
        let sym = simplify_sym_expr(&sym);
        let poly = extract_polynomial(&sym, "x", "original_result");
        assert!(poly.is_some());
        let poly = poly.unwrap();
        assert_eq!(poly.degree(), 2);
    }

    #[test]
    fn extract_polynomial_cubic_simple() {
        let expr = full_compile("x * x * x + x * x + x");
        let sym = test_context(|ctx| expression_to_sym_expr(&expr, ctx)).unwrap();
        let sym = simplify_sym_expr(&sym);
        let poly = extract_polynomial(&sym, "x", "original_result");
        assert!(poly.is_some());
        let poly = poly.unwrap();
        assert_eq!(poly.degree(), 3);
    }

    #[test]
    fn extract_polynomial_linear_returns_none() {
        let expr = full_compile("x + 5");
        let sym = test_context(|ctx| expression_to_sym_expr(&expr, ctx)).unwrap();
        let poly = extract_polynomial(&sym, "x", "original_result");
        assert!(poly.is_none());
    }

    #[test]
    fn extract_polynomial_non_poly_returns_none() {
        let expr = full_compile("x * y");
        let sym = test_context(|ctx| expression_to_sym_expr(&expr, ctx)).unwrap();
        let poly = extract_polynomial(&sym, "x", "original_result");
        assert!(poly.is_none());
    }

    #[test]
    fn extract_polynomial_with_result_in_constant() {
        let expr = full_compile("x * x - 5");
        let sym = test_context(|ctx| expression_to_sym_expr(&expr, ctx)).unwrap();
        let sym = simplify_sym_expr(&sym);
        let poly = extract_polynomial(&sym, "x", "original_result");
        assert!(poly.is_some());
        let poly = poly.unwrap();
        assert_eq!(poly.degree(), 2);
    }
}
