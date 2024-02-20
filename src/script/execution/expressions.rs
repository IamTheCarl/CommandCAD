use std::cmp::Ordering;

use crate::script::{
    parsing::{ArithmeticExpression, Comparison, Expression, Factor, Term, Trailer},
    Span,
};

use super::{
    types::{Object, Range, Structure, Value},
    ExecutionContext, Failure,
};

type Result<S, R> = std::result::Result<R, Failure<S>>;

// TODO this should not be returning a control flow.
pub fn run_expression<'a, S: Span>(
    context: &mut ExecutionContext<'a, S>,
    expression: &Expression<S>,
) -> Result<S, Value<'a, S>> {
    match expression {
        Expression::And(a, b) => {
            let a_value = run_expression(context, a)?;
            let b_value = run_comparison(context, b)?;

            a_value.and(&mut context.log, a.get_span(), &b_value)
        }
        Expression::Or(a, b) => {
            let a_value = run_expression(context, a)?;
            let b_value = run_comparison(context, b)?;

            a_value.or(&mut context.log, a.get_span(), &b_value)
        }
        Expression::Buffer(comparison) => run_comparison(context, comparison),
    }
}

pub fn run_comparison<'a, S: Span>(
    context: &mut ExecutionContext<'a, S>,
    comparison: &Comparison<S>,
) -> Result<S, Value<'a, S>> {
    fn cmp<S: Span>(
        context: &mut ExecutionContext<S>,
        a: &Comparison<S>,
        b: &ArithmeticExpression<S>,
    ) -> Result<S, Ordering> {
        let value_a = run_comparison(context, a)?;
        let value_b = run_arithmetic_expression(context, b)?;

        value_a.cmp(&mut context.log, a.get_span(), &value_b)
    }

    match comparison {
        // TODO it would be valuable to test if both sides are equivalent statements and print a warning if so.
        Comparison::LessThan(a, b) => Ok(matches!(cmp(context, a, b)?, Ordering::Less).into()),
        Comparison::LessThanEqual(a, b) => {
            Ok(matches!(cmp(context, a, b)?, Ordering::Less | Ordering::Equal).into())
        }
        Comparison::Equal(a, b) => Ok(matches!(cmp(context, a, b)?, Ordering::Equal).into()),
        Comparison::GreaterThanEqual(a, b) => {
            Ok(matches!(cmp(context, a, b)?, Ordering::Greater | Ordering::Equal).into())
        }
        Comparison::GreaterThan(a, b) => {
            Ok(matches!(cmp(context, a, b)?, Ordering::Greater).into())
        }
        Comparison::None(expression) => run_arithmetic_expression(context, expression),
    }
}

pub fn run_arithmetic_expression<'a, S: Span>(
    context: &mut ExecutionContext<'a, S>,
    expression: &ArithmeticExpression<S>,
) -> Result<S, Value<'a, S>> {
    match expression {
        ArithmeticExpression::Addition(a, b) => {
            let value_a = run_arithmetic_expression(context, a)?;
            let value_b = run_term(context, b)?;

            value_a.addition(&mut context.log, a.get_span(), &value_b)
        }
        ArithmeticExpression::Subtraction(a, b) => {
            let value_a = run_arithmetic_expression(context, a)?;
            let value_b = run_term(context, b)?;

            value_a.subtraction(&mut context.log, a.get_span(), &value_b)
        }
        ArithmeticExpression::Term(term) => run_term(context, term),
    }
}

pub fn run_term<'a, S: Span>(
    context: &mut ExecutionContext<'a, S>,
    term: &Term<S>,
) -> Result<S, Value<'a, S>> {
    match term {
        Term::Multiply(a, b) => {
            let a_value = run_term(context, a)?;
            let b_value = run_trailer(context, b)?;

            a_value.multiply(&mut context.log, a.get_span(), &b_value)
        }
        Term::Divide(a, b) => {
            let a_value = run_term(context, a)?;
            let b_value = run_trailer(context, b)?;

            a_value.divide(&mut context.log, a.get_span(), &b_value)
        }
        Term::Range(range) => Ok(Range::from_parsed(context, range)?.into()),
        Term::Trailer(trailer) => run_trailer(context, trailer),
    }
}

pub fn run_trailer<'a, S: Span>(
    context: &mut ExecutionContext<'a, S>,
    trailer: &Trailer<S>,
) -> Result<S, Value<'a, S>> {
    match trailer {
        Trailer::None(factor) => run_factor(context, factor),
        Trailer::Attribute(trailer, attribute) => {
            let value = run_trailer(context, trailer)?;

            value.attribute(&mut context.log, trailer.get_span(), attribute)
        }
        Trailer::Call(trailer, arguments) => {
            let value = run_trailer(context, trailer)?;

            let mut evaluated_arguments = Vec::with_capacity(arguments.len());

            for argument in arguments.iter() {
                let argument = run_expression(context, argument)?;
                evaluated_arguments.push(argument);
            }

            value.call(context, trailer.get_span(), evaluated_arguments, arguments)
        }
        Trailer::MethodCall(trailer, attribute, arguments) => {
            let value = run_trailer(context, trailer)?;

            let mut evaluated_arguments = Vec::with_capacity(arguments.len());

            for argument in arguments.iter() {
                let argument = run_expression(context, argument)?;
                evaluated_arguments.push(argument);
            }

            value.method_call(
                context,
                trailer.get_span(),
                attribute,
                evaluated_arguments,
                arguments,
            )
        }
        Trailer::Index(trailer, index) => {
            let value = run_trailer(context, trailer)?;
            let index = run_expression(context, index)?;

            value.index(&mut context.log, trailer.get_span(), index)
        }
    }
}

pub fn run_factor<'a, S: Span>(
    context: &mut ExecutionContext<'a, S>,
    factor: &Factor<S>,
) -> Result<S, Value<'a, S>> {
    match factor {
        Factor::Litteral(litteral) => Value::from_litteral(context, litteral),
        Factor::Variable(variable) => context.stack.get_variable(variable).cloned(),
        Factor::Parenthesis(expression) => run_expression(context, expression),
        Factor::UnaryPlus(factor) => {
            run_factor(context, factor)?.unary_plus(&mut context.log, factor.get_span())
        }
        Factor::UnaryMinus(factor) => {
            run_factor(context, factor)?.unary_minus(&mut context.log, factor.get_span())
        }
        Factor::UnaryLogicalNot(factor) => {
            run_factor(context, factor)?.unary_logical_not(&mut context.log, factor.get_span())
        }
        Factor::StructInitalization(initalization) => {
            Structure::initalization(context, initalization)
        }
    }
}

#[cfg(test)]
mod test {
    use super::super::types::DefaultValue;
    use super::*;
    use ordered_float::NotNan;

    use crate::script::{
        execution::{
            types::{function::IntoBuiltinFunction, List, Measurement, Number, SString},
            ModuleScope,
        },
        module::Module,
        parsing::Litteral,
    };

    #[test]
    fn expression_straight_number() {
        let mut context = ExecutionContext::default();

        assert_eq!(
            run_expression(&mut context, &Expression::parse("24").unwrap().1),
            Ok(Value::Number(NotNan::new(24.0).unwrap()))
        );
    }

    #[test]
    fn expression_logical_operators() {
        let mut context = ExecutionContext::default();

        assert_eq!(
            run_expression(&mut context, &Expression::parse("true").unwrap().1),
            Ok(Value::Boolean(true))
        );
        assert_eq!(
            run_expression(&mut context, &Expression::parse("false").unwrap().1),
            Ok(Value::Boolean(false))
        );

        // Not
        assert_eq!(
            run_expression(&mut context, &Expression::parse("!true").unwrap().1),
            Ok(Value::Boolean(false))
        );
        assert_eq!(
            run_expression(&mut context, &Expression::parse("!false").unwrap().1),
            Ok(Value::Boolean(true))
        );

        // And
        assert_eq!(
            run_expression(
                &mut context,
                &Expression::parse("false && false").unwrap().1
            ),
            Ok(Value::Boolean(false))
        );
        assert_eq!(
            run_expression(&mut context, &Expression::parse("true && false").unwrap().1),
            Ok(Value::Boolean(false))
        );
        assert_eq!(
            run_expression(&mut context, &Expression::parse("false && true").unwrap().1),
            Ok(Value::Boolean(false))
        );
        assert_eq!(
            run_expression(&mut context, &Expression::parse("true && true").unwrap().1),
            Ok(Value::Boolean(true))
        );

        // Or
        assert_eq!(
            run_expression(
                &mut context,
                &Expression::parse("false || false").unwrap().1
            ),
            Ok(Value::Boolean(false))
        );
        assert_eq!(
            run_expression(&mut context, &Expression::parse("true || false").unwrap().1),
            Ok(Value::Boolean(true))
        );
        assert_eq!(
            run_expression(&mut context, &Expression::parse("false || true").unwrap().1),
            Ok(Value::Boolean(true))
        );
        assert_eq!(
            run_expression(&mut context, &Expression::parse("true || true").unwrap().1),
            Ok(Value::Boolean(true))
        );
    }

    #[test]
    fn value_from_litteral() {
        let mut context = ExecutionContext::default();

        // Measurement
        assert_eq!(
            Value::from_litteral(&mut context, &Litteral::parse("22mm").unwrap().1),
            Ok(
                Measurement::try_from(uom::si::f64::Length::new::<uom::si::length::millimeter>(
                    22.0
                ))
                .unwrap()
                .into()
            )
        );
        // Number
        assert_eq!(
            Value::from_litteral(&mut context, &Litteral::parse("22").unwrap().1),
            Ok(NotNan::new(22.0).unwrap().into())
        );
        // String
        assert_eq!(
            Value::from_litteral(&mut context, &Litteral::parse("\"test\"").unwrap().1),
            Ok(SString::from("test").into())
        );
        // List
        assert_eq!(
            Value::from_litteral(&mut context, &Litteral::parse("[1, 2, 3]").unwrap().1),
            Ok(List::from([
                Number::new(1.0).unwrap().into(),
                Number::new(2.0).unwrap().into(),
                Number::new(3.0).unwrap().into()
            ])
            .into())
        );
        // Boolean
        assert_eq!(
            Value::from_litteral(&mut context, &Litteral::parse("true").unwrap().1),
            Ok(true.into())
        );
        assert_eq!(
            Value::from_litteral(&mut context, &Litteral::parse("false").unwrap().1),
            Ok(false.into())
        );
        // Default
        assert_eq!(
            Value::from_litteral(&mut context, &Litteral::parse("default").unwrap().1),
            Ok(DefaultValue.into())
        );
    }

    #[test]
    fn variable_access() {
        let mut context = ExecutionContext::default();
        context.stack.new_variable(&"global_scope", true.into());

        context.new_scope(|context| {
            context.stack.new_variable(&"sub_scope", false.into());

            assert_eq!(
                run_expression(context, &Expression::parse("sub_scope").unwrap().1),
                Ok(Value::Boolean(false))
            );

            assert_eq!(
                run_expression(context, &Expression::parse("global_scope").unwrap().1),
                Ok(Value::Boolean(true))
            );

            assert_eq!(
                run_expression(context, &Expression::parse("non_existant_scope").unwrap().1),
                Err(Failure::VariableNotInScope(
                    "non_existant_scope",
                    "non_existant_scope".into()
                ))
            );
        });
    }

    #[test]
    fn unary_operators() {
        let mut context = ExecutionContext::default();

        assert_eq!(
            run_expression(&mut context, &Expression::parse("+15").unwrap().1),
            Ok(Value::Number(NotNan::new(15.0).unwrap()))
        );

        assert_eq!(
            run_expression(&mut context, &Expression::parse("-15").unwrap().1),
            Ok(Value::Number(NotNan::new(-15.0).unwrap()))
        );
    }

    #[test]
    fn parenthasis() {
        let mut context = ExecutionContext::default();

        assert_eq!(
            run_expression(&mut context, &Expression::parse("(1 + 2) * 3").unwrap().1),
            Ok(Number::new(9.0).unwrap().into())
        );
    }

    #[test]
    fn attribute_access() {
        let mut log = Vec::new();

        let module = Module::load(
            &mut log,
            "test_module.ccm",
            "struct DefaultStruct { value: Number = 42 }",
        )
        .unwrap();

        assert!(log.is_empty());

        let module_scope = ModuleScope::new(&module);

        let mut context = ExecutionContext::new(module_scope);

        assert_eq!(
            run_expression(
                &mut context,
                &Expression::parse("struct DefaultStruct { ..default }.value")
                    .unwrap()
                    .1
            ),
            Ok(NotNan::new(42.0).unwrap().into())
        );
    }

    #[test]
    fn method_call() {
        let mut context = ExecutionContext::default();

        assert_eq!(
            run_expression(&mut context, &Expression::parse("24.25.floor()").unwrap().1),
            Ok(NotNan::new(24.0).unwrap().into())
        );
    }

    #[test]
    fn call() {
        let mut context = ExecutionContext::default();

        fn my_function<'a, S: Span>(
            _context: &mut ExecutionContext<'a, S>,
            _span: &S,
        ) -> Result<S, Value<'a, S>> {
            Ok(Number::new(42.0).unwrap().into())
        }

        context.stack.new_variable(
            &"my_function",
            my_function::<&str>.into_builtin_function().into(),
        );

        assert_eq!(
            run_expression(&mut context, &Expression::parse("my_function()").unwrap().1),
            Ok(Number::new(42.0).unwrap().into())
        );
    }

    #[test]
    fn index() {
        let mut context = ExecutionContext::default();

        assert_eq!(
            run_expression(&mut context, &Expression::parse("[1, 2, 3][0]").unwrap().1),
            Ok(Number::new(1.0).unwrap().into())
        );
    }

    #[test]
    fn multiply() {
        let mut context = ExecutionContext::default();

        assert_eq!(
            run_expression(&mut context, &Expression::parse("1 + 2 * 3").unwrap().1),
            Ok(Number::new(7.0).unwrap().into())
        );
    }

    #[test]
    fn divide() {
        let mut context = ExecutionContext::default();

        assert_eq!(
            run_expression(&mut context, &Expression::parse("9 / 3").unwrap().1),
            Ok(Number::new(3.0).unwrap().into())
        );
    }

    #[test]
    fn range() {
        let mut context = ExecutionContext::default();

        assert_eq!(
            run_expression(&mut context, &Expression::parse("..").unwrap().1),
            Ok(Range {
                lower_bound: None,
                upper_bound_is_inclusive: false,
                upper_bound: None
            }
            .into())
        );
        assert_eq!(
            run_expression(&mut context, &Expression::parse("..=").unwrap().1),
            Err(Failure::MissingUpperBound("..="))
        );
        assert_eq!(
            run_expression(&mut context, &Expression::parse("5..").unwrap().1),
            Ok(Range {
                lower_bound: Some(Number::new(5.0).unwrap()),
                upper_bound_is_inclusive: false,
                upper_bound: None
            }
            .into())
        );
        assert_eq!(
            run_expression(&mut context, &Expression::parse("..5").unwrap().1),
            Ok(Range {
                lower_bound: None,
                upper_bound_is_inclusive: false,
                upper_bound: Some(Number::new(5.0).unwrap())
            }
            .into())
        );
        assert_eq!(
            run_expression(&mut context, &Expression::parse("..=5").unwrap().1),
            Ok(Range {
                lower_bound: None,
                upper_bound_is_inclusive: true,
                upper_bound: Some(Number::new(5.0).unwrap())
            }
            .into())
        );
    }

    #[test]
    fn addition() {
        let mut context = ExecutionContext::default();

        assert_eq!(
            run_expression(&mut context, &Expression::parse("1 + 2").unwrap().1),
            Ok(Number::new(3.0).unwrap().into())
        );
    }

    #[test]
    fn subtraction() {
        let mut context = ExecutionContext::default();

        assert_eq!(
            run_expression(&mut context, &Expression::parse("1 + 2").unwrap().1),
            Ok(Number::new(3.0).unwrap().into())
        );
    }

    #[test]
    fn comparisions() {
        let mut context = ExecutionContext::default();

        // LessThan(_, _)
        assert_eq!(
            run_expression(&mut context, &Expression::parse("1 < 2").unwrap().1),
            Ok(true.into())
        );
        assert_eq!(
            run_expression(&mut context, &Expression::parse("2 < 2").unwrap().1),
            Ok(false.into())
        );
        assert_eq!(
            run_expression(&mut context, &Expression::parse("2 < 1").unwrap().1),
            Ok(false.into())
        );

        // LessThanEqual(_, _)
        assert_eq!(
            run_expression(&mut context, &Expression::parse("1 <= 2").unwrap().1),
            Ok(true.into())
        );
        assert_eq!(
            run_expression(&mut context, &Expression::parse("2 <= 2").unwrap().1),
            Ok(true.into())
        );
        assert_eq!(
            run_expression(&mut context, &Expression::parse("2 <= 1").unwrap().1),
            Ok(false.into())
        );

        // Equal(_, _)
        assert_eq!(
            run_expression(&mut context, &Expression::parse("1 == 2").unwrap().1),
            Ok(false.into())
        );
        assert_eq!(
            run_expression(&mut context, &Expression::parse("2 == 2").unwrap().1),
            Ok(true.into())
        );
        assert_eq!(
            run_expression(&mut context, &Expression::parse("2 == 1").unwrap().1),
            Ok(false.into())
        );

        // GreaterThanEqual(_, _)
        assert_eq!(
            run_expression(&mut context, &Expression::parse("1 >= 2").unwrap().1),
            Ok(false.into())
        );
        assert_eq!(
            run_expression(&mut context, &Expression::parse("2 >= 2").unwrap().1),
            Ok(true.into())
        );
        assert_eq!(
            run_expression(&mut context, &Expression::parse("2 >= 1").unwrap().1),
            Ok(true.into())
        );

        // GreaterThan(_, _)
        assert_eq!(
            run_expression(&mut context, &Expression::parse("1 > 2").unwrap().1),
            Ok(false.into())
        );
        assert_eq!(
            run_expression(&mut context, &Expression::parse("2 > 2").unwrap().1),
            Ok(false.into())
        );
        assert_eq!(
            run_expression(&mut context, &Expression::parse("2 > 1").unwrap().1),
            Ok(true.into())
        );
    }
}
