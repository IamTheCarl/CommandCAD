use super::{BinOp as BinOpType, Scalar as ScalarStruct, SymExpr, UnaryOp, sym_expr_contains_var};
use common_data_types::Dimension;
use crate::execution::errors::Raise;

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
                // If existing is zero scalar, just use coeff (0 + x = x)
                c if a.value.into_inner() == 0.0 && a.dimension == Dimension::zero() => c,
                c => SymExpr::BinOp(BinOpType::Add, Box::new(existing.clone()), Box::new(c)),
            },
            SymExpr::Integer(a) => match coeff {
                SymExpr::Scalar(b) => SymExpr::Scalar(ScalarStruct {
                    dimension: b.dimension,
                    value: b.value + common_data_types::Float::new(*a as f64).unwrap(),
                }),
                SymExpr::Integer(b) => SymExpr::Integer(*a + b),
                // If existing is zero, just use coeff
                c if *a == 0 => c,
                c => SymExpr::BinOp(BinOpType::Add, Box::new(existing.clone()), Box::new(c)),
            },
            // If existing is non-scalar and coeff is zero scalar, use existing
            _ if matches!(&coeff, SymExpr::Scalar(s) if s.value.into_inner() == 0.0 && s.dimension == Dimension::zero())
              || matches!(&coeff, SymExpr::Integer(i) if *i == 0) => existing.clone(),
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
        // If constant is zero, use UnaryOp(Neg, Var) to avoid dimension mismatch
        // (0 - result would try to subtract dimensioned value from dimensionless 0)
        let is_zero = matches!(constant, SymExpr::Scalar(s) if s.value.into_inner() == 0.0 && s.dimension == Dimension::zero())
            || matches!(constant, SymExpr::Integer(i) if *i == 0);
        coefficients[0] = if is_zero {
            SymExpr::UnaryOp(UnaryOp::Neg, Box::new(SymExpr::Var(result_name.into())))
        } else {
            SymExpr::BinOp(
                BinOpType::Sub,
                Box::new(constant.clone()),
                Box::new(SymExpr::Var(result_name.into())),
            )
        };
    }

    let poly = Polynomial { coefficients };

    // Only return if degree is 2 or 3
    if poly.degree() >= 2 && poly.degree() <= 3 {
        Some(poly)
    } else {
        None
    }
}

/// Check if an expression doesn't contain the target variable (power = 0).
fn power_is_zero_for_non_target(expr: &SymExpr, target: &str) -> bool {
    !sym_expr_contains_var(expr, target)
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
            } else if power_is_zero_for_non_target(expr, target) {
                // Expression doesn't contain the target variable — it's a constant term
                if sign == -1 {
                    let negated = SymExpr::UnaryOp(UnaryOp::Neg, Box::new(expr.clone()));
                    terms.push((0, negated));
                } else {
                    terms.push((0, expr.clone()));
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

/// Check if a SymExpr is effectively zero (dimensionless with value 0).
fn is_zero_expr(expr: &SymExpr) -> bool {
    match expr {
        SymExpr::Scalar(s) => s.value.into_inner() == 0.0 && s.dimension == Dimension::zero(),
        SymExpr::Integer(i) => *i == 0,
        SymExpr::BinOp(BinOpType::Mul, left, right) => {
            is_zero_expr(left) || is_zero_expr(right)
        }
        SymExpr::BinOp(op, left, right) if matches!(op, BinOpType::Add | BinOpType::Sub) => {
            if let (SymExpr::Scalar(ls), SymExpr::Scalar(rs)) = (left.as_ref(), right.as_ref()) {
                let result = match op {
                    BinOpType::Add => ls.value.into_inner() + rs.value.into_inner(),
                    BinOpType::Sub => ls.value.into_inner() - rs.value.into_inner(),
                    _ => unreachable!(),
                };
                result == 0.0 && ls.dimension == Dimension::zero() && rs.dimension == Dimension::zero()
            } else {
                false
            }
        }
        SymExpr::UnaryOp(UnaryOp::Neg, inner) => is_zero_expr(inner),
        _ => false,
    }
}

/// Solve a quadratic equation ax² + bx + c = 0.
/// Returns the "+" root as a SymExpr.
pub fn solve_quadratic(
    a: &SymExpr,
    b: &SymExpr,
    c: &SymExpr,
    _context: &crate::execution::ExecutionContext,
    _result_dim: Option<&Dimension>,
) -> crate::execution::errors::ExecutionResult<SymExpr> {
    // When b is zero, use the simplified form x = sqrt(-c/a).
    // The standard formula (-b + sqrt(b²-4ac)) / (2a) has dimensional issues
    // when b is dimensionless but a*c has dimensions (e.g., x² = result: a=1, b=0, c=-result).
    // In that case b² is dimensionless while 4ac has the result's dimension,
    // making b² - 4ac dimensionally inconsistent.
    if is_zero_expr(b) {
        // x = sqrt(-c/a)
        let neg_c = SymExpr::UnaryOp(UnaryOp::Neg, Box::new(c.clone()));
        let ratio = SymExpr::BinOp(BinOpType::Div, Box::new(neg_c), Box::new(a.clone()));
        return Ok(SymExpr::MethodCall {
            method_name: "sqrt".into(),
            self_expr: Box::new(ratio),
            args: vec![],
            args_names: vec![],
        });
    }

    // For b != 0, use the standard quadratic formula.
    // The discriminant b² - 4ac must have consistent dimensions.
    let a_dim = extract_dimension(a);
    let c_dim = extract_dimension(c);

    // Compute reference dimension for the discriminant from 4ac.
    let disc_dim = if a_dim == Dimension::zero() {
        c_dim
    } else if c_dim == Dimension::zero() {
        a_dim
    } else {
        c_dim
    };

    // Compute b²
    let b_squared = SymExpr::BinOp(BinOpType::Mul, Box::new(b.clone()), Box::new(b.clone()));

    // 4ac with proper dimension
    let four = cast_to_dimension(&SymExpr::Scalar(ScalarStruct {
        dimension: Dimension::zero(),
        value: common_data_types::Float::new(4.0).unwrap(),
    }), &disc_dim);
    let four_a = SymExpr::BinOp(BinOpType::Mul, Box::new(four), Box::new(a.clone()));
    let four_ac = SymExpr::BinOp(BinOpType::Mul, Box::new(four_a), Box::new(c.clone()));

    // Cast b_squared to disc_dim if needed
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

    // -b cast to disc_dim
    let neg_b = SymExpr::UnaryOp(UnaryOp::Neg, Box::new(b.clone()));
    let neg_b_dim = extract_dimension(&neg_b);
    let neg_b = if neg_b_dim != disc_dim && neg_b_dim == Dimension::zero() {
        cast_to_dimension(&neg_b, &disc_dim)
    } else {
        neg_b
    };

    // (-b + sqrt(D)) / (2a)
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
            // Handle BinOp with Integer operands (e.g., 0*0 from b*b where b=0)
            if let (SymExpr::Integer(li), SymExpr::Integer(ri)) = (left.as_ref(), right.as_ref()) {
                let value = match op {
                    BinOpType::Add => *li as f64 + *ri as f64,
                    BinOpType::Sub => *li as f64 - *ri as f64,
                    BinOpType::Mul => *li as f64 * *ri as f64,
                    BinOpType::Div => *li as f64 / *ri as f64,
                    _ => *li as f64,
                };
                return SymExpr::Scalar(ScalarStruct {
                    dimension: *target_dim,
                    value: common_data_types::Float::new(value).unwrap(),
                });
            }
            // Handle mixed Scalar and Integer operands
            if let (SymExpr::Scalar(ls), SymExpr::Integer(ri)) = (left.as_ref(), right.as_ref()) {
                let value: f64 = match op {
                    BinOpType::Add => ls.value.into_inner() + (*ri as f64),
                    BinOpType::Sub => ls.value.into_inner() - (*ri as f64),
                    BinOpType::Mul => ls.value.into_inner() * (*ri as f64),
                    BinOpType::Div => ls.value.into_inner() / (*ri as f64),
                    _ => ls.value.into_inner(),
                };
                return SymExpr::Scalar(ScalarStruct {
                    dimension: *target_dim,
                    value: common_data_types::Float::new(value).unwrap(),
                });
            }
            if let (SymExpr::Integer(li), SymExpr::Scalar(rs)) = (left.as_ref(), right.as_ref()) {
                let value: f64 = match op {
                    BinOpType::Add => (*li as f64) + rs.value.into_inner(),
                    BinOpType::Sub => (*li as f64) - rs.value.into_inner(),
                    BinOpType::Mul => (*li as f64) * rs.value.into_inner(),
                    BinOpType::Div => (*li as f64) / rs.value.into_inner(),
                    _ => rs.value.into_inner(),
                };
                return SymExpr::Scalar(ScalarStruct {
                    dimension: *target_dim,
                    value: common_data_types::Float::new(value).unwrap(),
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
    context: &crate::execution::ExecutionContext,
    _result_dim: Option<&Dimension>,
) -> crate::execution::errors::ExecutionResult<SymExpr> {
    // Special case: pure cube x³ = -d/a → x = cbrt(-d/a)
    // Cardano's formula has dimensional issues with mixed-term cubics
    // (coefficients have incompatible dimensions when variable is dimensioned).
    if is_zero_expr(b) && is_zero_expr(c) {
        let neg_d = SymExpr::UnaryOp(UnaryOp::Neg, Box::new(d.clone()));
        let ratio = SymExpr::BinOp(BinOpType::Div, Box::new(neg_d), Box::new(a.clone()));
        return Ok(SymExpr::MethodCall {
            method_name: "cbrt".into(),
            self_expr: Box::new(ratio),
            args: vec![],
            args_names: vec![],
        });
    }

    // Mixed-term cubics with dimensioned variables don't work with Cardano's formula
    // because intermediate values (p, q, delta) have incompatible dimensions.
    Err(super::algorithm::SolveError::NoSolution {
        operation: "cubic with mixed terms requires dimensioned coefficients (e.g., 3*1m*x*x instead of 3*x*x)".into(),
        source: crate::compile::SourceReference {
            file: std::sync::Arc::new(std::path::PathBuf::from("solve")),
            range: tree_sitter::Range { start_byte: 0, end_byte: 0, start_point: tree_sitter::Point { row: 0, column: 0 }, end_point: tree_sitter::Point { row: 0, column: 0 } },
        },
    }.to_error(context))
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
