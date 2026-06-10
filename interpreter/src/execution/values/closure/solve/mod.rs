mod algorithm;
mod closure;

pub use algorithm::{ast_return_type, solve_for};
pub use closure::infer_sym_expr_type;

use std::any::TypeId;

use imstr::ImString;
use indexmap::IndexMap;

use super::BuiltinCallableDatabase;

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
    fn isolate(&self, _target_is_left: bool, sibling: &SymExpr) -> SymExpr {
        match self {
            UnaryOp::Neg => SymExpr::UnaryOp(UnaryOp::Neg, Box::new(sibling.clone())),
            UnaryOp::Not => SymExpr::UnaryOp(UnaryOp::Not, Box::new(sibling.clone())),
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
    }
}

/// Apply algebraic simplification rules to a SymExpr.
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
        match result {
            SymExpr::BinOp(BinOp::Mul, left, right) => {
                // x^2 * x → Mul(Pow(x, 2), x) — the second x*x match won't fire because
                // after simplifying the first level, we get Pow(x,2) which is not Var(x)
                assert!(matches!(left.as_ref(), SymExpr::BinOp(BinOp::Pow, _, _)));
                assert!(matches!(right.as_ref(), SymExpr::Var(v) if v == "x"));
            }
            _ => panic!("Expected Mul(Pow(x,2), x), got {:?}", result),
        }
    }
}
