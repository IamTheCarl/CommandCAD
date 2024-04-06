/*
 * Copyright 2024 James Carl
 * AGPL-3.0-only or AGPL-3.0-or-later
 *
 * This file is part of Command Cad.
 *
 * Command CAD is free software: you can redistribute it and/or modify it under the terms of
 * the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License along with this
 * program. If not, see <https://www.gnu.org/licenses/>.
 */

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
pub fn run_expression<S: Span>(
    context: &mut ExecutionContext<S>,
    expression: &Expression<S>,
) -> Result<S, Value<S>> {
    match expression {
        Expression::And(a, b) => {
            let a_value = run_expression(context, a)?;
            let b_value = run_comparison(context, b)?;

            a_value.and(context.log, a.get_span(), &b_value)
        }
        Expression::Or(a, b) => {
            let a_value = run_expression(context, a)?;
            let b_value = run_comparison(context, b)?;

            a_value.or(context.log, a.get_span(), &b_value)
        }
        Expression::Buffer(comparison) => run_comparison(context, comparison),
    }
}

pub fn run_comparison<S: Span>(
    context: &mut ExecutionContext<S>,
    comparison: &Comparison<S>,
) -> Result<S, Value<S>> {
    fn cmp<S: Span>(
        context: &mut ExecutionContext<S>,
        a: &Comparison<S>,
        b: &ArithmeticExpression<S>,
    ) -> Result<S, Ordering> {
        let value_a = run_comparison(context, a)?;
        let value_b = run_arithmetic_expression(context, b)?;

        value_a.cmp(context.log, a.get_span(), &value_b)
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

pub fn run_arithmetic_expression<S: Span>(
    context: &mut ExecutionContext<S>,
    expression: &ArithmeticExpression<S>,
) -> Result<S, Value<S>> {
    match expression {
        ArithmeticExpression::Addition(a, b) => {
            let value_a = run_arithmetic_expression(context, a)?;
            let value_b = run_term(context, b)?;

            value_a.addition(context.log, a.get_span(), &value_b)
        }
        ArithmeticExpression::Subtraction(a, b) => {
            let value_a = run_arithmetic_expression(context, a)?;
            let value_b = run_term(context, b)?;

            value_a.subtraction(context.log, a.get_span(), &value_b)
        }
        ArithmeticExpression::Term(term) => run_term(context, term),
    }
}

pub fn run_term<S: Span>(context: &mut ExecutionContext<S>, term: &Term<S>) -> Result<S, Value<S>> {
    match term {
        Term::Multiply(a, b) => {
            let a_value = run_term(context, a)?;
            let b_value = run_trailer(context, b)?;

            a_value.multiply(context.log, a.get_span(), &b_value)
        }
        Term::Divide(a, b) => {
            let a_value = run_term(context, a)?;
            let b_value = run_trailer(context, b)?;

            a_value.divide(context.log, a.get_span(), &b_value)
        }
        Term::Range(range) => Ok(Range::from_parsed(context, range)?.into()),
        Term::Trailer(trailer) => run_trailer(context, trailer),
    }
}

pub fn run_trailer<S: Span>(
    context: &mut ExecutionContext<S>,
    trailer: &Trailer<S>,
) -> Result<S, Value<S>> {
    match trailer {
        Trailer::None(factor) => run_factor(context, factor),
        Trailer::Attribute(trailer, attribute) => {
            let value = run_trailer(context, trailer)?;

            value.attribute(context.log, trailer.get_span(), attribute)
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
        Trailer::StructInitalization(definition, initalization) => {
            Structure::initalization(context, definition, initalization)
        }
        Trailer::Index(trailer, index) => {
            let value = run_trailer(context, trailer)?;
            let index = run_expression(context, index)?;

            value.index(context.log, trailer.get_span(), index)
        }
    }
}

pub fn run_factor<S: Span>(
    context: &mut ExecutionContext<S>,
    factor: &Factor<S>,
) -> Result<S, Value<S>> {
    match factor {
        Factor::Litteral(litteral) => Value::from_litteral(context, litteral),
        Factor::Variable(variable) => Ok(context.stack.get_variable(variable)?.clone()),
        Factor::Parenthesis(expression) => run_expression(context, expression),
        Factor::UnaryPlus(factor) => {
            run_factor(context, factor)?.unary_plus(context.log, factor.get_span())
        }
        Factor::UnaryMinus(factor) => {
            run_factor(context, factor)?.unary_minus(context.log, factor.get_span())
        }
        Factor::UnaryLogicalNot(factor) => {
            run_factor(context, factor)?.unary_logical_not(context.log, factor.get_span())
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use common_data_types::Number;

    use crate::script::{
        execution::{
            types::{function::IntoBuiltinFunction, DefaultValue, List, SString, Scalar},
            Module,
        },
        parsing::Litteral,
        Runtime,
    };

    #[test]
    fn expression_straight_number() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(context, &Expression::parse("24").unwrap().1),
                Ok(Number::new(24.0).unwrap().into())
            );
        });
    }

    #[test]
    fn expression_logical_operators() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(context, &Expression::parse("true").unwrap().1),
                Ok(Value::Boolean(true))
            );
            assert_eq!(
                run_expression(context, &Expression::parse("false").unwrap().1),
                Ok(Value::Boolean(false))
            );

            // Not
            assert_eq!(
                run_expression(context, &Expression::parse("!true").unwrap().1),
                Ok(Value::Boolean(false))
            );
            assert_eq!(
                run_expression(context, &Expression::parse("!false").unwrap().1),
                Ok(Value::Boolean(true))
            );

            // And
            assert_eq!(
                run_expression(context, &Expression::parse("false && false").unwrap().1),
                Ok(Value::Boolean(false))
            );
            assert_eq!(
                run_expression(context, &Expression::parse("true && false").unwrap().1),
                Ok(Value::Boolean(false))
            );
            assert_eq!(
                run_expression(context, &Expression::parse("false && true").unwrap().1),
                Ok(Value::Boolean(false))
            );
            assert_eq!(
                run_expression(context, &Expression::parse("true && true").unwrap().1),
                Ok(Value::Boolean(true))
            );

            // Or
            assert_eq!(
                run_expression(context, &Expression::parse("false || false").unwrap().1),
                Ok(Value::Boolean(false))
            );
            assert_eq!(
                run_expression(context, &Expression::parse("true || false").unwrap().1),
                Ok(Value::Boolean(true))
            );
            assert_eq!(
                run_expression(context, &Expression::parse("false || true").unwrap().1),
                Ok(Value::Boolean(true))
            );
            assert_eq!(
                run_expression(context, &Expression::parse("true || true").unwrap().1),
                Ok(Value::Boolean(true))
            );
        });
    }

    #[test]
    fn value_from_litteral() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            // Scalar
            assert_eq!(
                Value::from_litteral(context, &Litteral::parse("22mm").unwrap().1),
                Ok(
                    Scalar::try_from(uom::si::f64::Length::new::<uom::si::length::millimeter>(
                        22.0
                    ))
                    .unwrap()
                    .into()
                )
            );
            // Number
            assert_eq!(
                Value::from_litteral(context, &Litteral::parse("22").unwrap().1),
                Ok(Number::new(22.0).unwrap().into())
            );
            // String
            assert_eq!(
                Value::from_litteral(context, &Litteral::parse("\"test\"").unwrap().1),
                Ok(SString::from("test").into())
            );
            // List
            assert_eq!(
                Value::from_litteral(context, &Litteral::parse("[1, 2, 3]").unwrap().1),
                Ok(List::from([
                    Number::new(1.0).unwrap().into(),
                    Number::new(2.0).unwrap().into(),
                    Number::new(3.0).unwrap().into()
                ])
                .into())
            );
            // Boolean
            assert_eq!(
                Value::from_litteral(context, &Litteral::parse("true").unwrap().1),
                Ok(true.into())
            );
            assert_eq!(
                Value::from_litteral(context, &Litteral::parse("false").unwrap().1),
                Ok(false.into())
            );
            // Default
            assert_eq!(
                Value::from_litteral(context, &Litteral::parse("default").unwrap().1),
                Ok(DefaultValue.into())
            );
        });
    }

    #[test]
    fn variable_access() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            context.stack.new_variable(&"global_scope", true.into());

            context.new_scope(|context| {
                context.stack.new_variable(&"sub_scope", false.into());

                assert_eq!(
                    run_expression(
                        context,
                        Box::leak(Box::new(Expression::parse("sub_scope").unwrap().1))
                    ),
                    Ok(Value::Boolean(false))
                );

                assert_eq!(
                    run_expression(
                        context,
                        Box::leak(Box::new(Expression::parse("global_scope").unwrap().1))
                    ),
                    Ok(Value::Boolean(true))
                );

                assert_eq!(
                    run_expression(
                        context,
                        Box::leak(Box::new(Expression::parse("non_existant_scope").unwrap().1))
                    ),
                    Err(Failure::VariableNotInScope(
                        "non_existant_scope",
                        "non_existant_scope".into()
                    ))
                );
            });
        });
    }

    #[test]
    fn unary_operators() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(context, &Expression::parse("+15").unwrap().1),
                Ok(Number::new(15.0).unwrap().into())
            );

            assert_eq!(
                run_expression(context, &Expression::parse("-15").unwrap().1),
                Ok(Number::new(-15.0).unwrap().into())
            );
        });
    }

    #[test]
    fn parenthasis() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(context, &Expression::parse("(1 + 2) * 3").unwrap().1),
                Ok(Number::new(9.0).unwrap().into())
            );
        });
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

        ExecutionContext::new(&mut module.into(), |context| {
            assert_eq!(
                run_expression(
                    context,
                    &Expression::parse("DefaultStruct { ..default }.value")
                        .unwrap()
                        .1
                ),
                Ok(Number::new(42.0).unwrap().into())
            );
        });
    }

    #[test]
    fn method_call() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(context, &Expression::parse("24.25.floor()").unwrap().1),
                Ok(Number::new(24.0).unwrap().into())
            );
        });
    }

    #[test]
    fn call() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            fn my_function<S: Span>(
                _context: &mut ExecutionContext<S>,
                _span: &S,
            ) -> Result<S, Value<S>> {
                Ok(Number::new(42.0).unwrap().into())
            }

            let my_function = my_function::<&'static str>.into_builtin_function();

            context
                .stack
                .new_variable(&"my_function", my_function.into());

            assert_eq!(
                run_expression(context, &Expression::parse("my_function()").unwrap().1),
                Ok(Number::new(42.0).unwrap().into())
            );
        });
    }

    #[test]
    fn index() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(context, &Expression::parse("[1, 2, 3][0]").unwrap().1),
                Ok(Number::new(1.0).unwrap().into())
            );
        });
    }

    #[test]
    fn multiply() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(context, &Expression::parse("1 + 2 * 3").unwrap().1),
                Ok(Number::new(7.0).unwrap().into())
            );
        });
    }

    #[test]
    fn divide() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(context, &Expression::parse("9 / 3").unwrap().1),
                Ok(Number::new(3.0).unwrap().into())
            );
        });
    }

    #[test]
    fn range() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(context, &Expression::parse("..").unwrap().1),
                Ok(Range {
                    lower_bound: None,
                    upper_bound_is_inclusive: false,
                    upper_bound: None
                }
                .into())
            );
            assert_eq!(
                run_expression(context, &Expression::parse("..=").unwrap().1),
                Err(Failure::MissingUpperBound("..="))
            );
            assert_eq!(
                run_expression(context, &Expression::parse("5..").unwrap().1),
                Ok(Range {
                    lower_bound: Some(Number::new(5.0).unwrap().into()),
                    upper_bound_is_inclusive: false,
                    upper_bound: None
                }
                .into())
            );
            assert_eq!(
                run_expression(context, &Expression::parse("..5").unwrap().1),
                Ok(Range {
                    lower_bound: None,
                    upper_bound_is_inclusive: false,
                    upper_bound: Some(Number::new(5.0).unwrap().into())
                }
                .into())
            );
            assert_eq!(
                run_expression(context, &Expression::parse("..=5").unwrap().1),
                Ok(Range {
                    lower_bound: None,
                    upper_bound_is_inclusive: true,
                    upper_bound: Some(Number::new(5.0).unwrap().into())
                }
                .into())
            );
        });
    }

    #[test]
    fn addition() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(context, &Expression::parse("1 + 2").unwrap().1),
                Ok(Number::new(3.0).unwrap().into())
            );
        });
    }

    #[test]
    fn subtraction() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(context, &Expression::parse("1 + 2").unwrap().1),
                Ok(Number::new(3.0).unwrap().into())
            );
        });
    }

    #[test]
    fn comparisions() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            // LessThan(_, _)
            assert_eq!(
                run_expression(context, &Expression::parse("1 < 2").unwrap().1),
                Ok(true.into())
            );
            assert_eq!(
                run_expression(context, &Expression::parse("2 < 2").unwrap().1),
                Ok(false.into())
            );
            assert_eq!(
                run_expression(context, &Expression::parse("2 < 1").unwrap().1),
                Ok(false.into())
            );

            // LessThanEqual(_, _)
            assert_eq!(
                run_expression(context, &Expression::parse("1 <= 2").unwrap().1),
                Ok(true.into())
            );
            assert_eq!(
                run_expression(context, &Expression::parse("2 <= 2").unwrap().1),
                Ok(true.into())
            );
            assert_eq!(
                run_expression(context, &Expression::parse("2 <= 1").unwrap().1),
                Ok(false.into())
            );

            // Equal(_, _)
            assert_eq!(
                run_expression(context, &Expression::parse("1 == 2").unwrap().1),
                Ok(false.into())
            );
            assert_eq!(
                run_expression(context, &Expression::parse("2 == 2").unwrap().1),
                Ok(true.into())
            );
            assert_eq!(
                run_expression(context, &Expression::parse("2 == 1").unwrap().1),
                Ok(false.into())
            );

            // GreaterThanEqual(_, _)
            assert_eq!(
                run_expression(context, &Expression::parse("1 >= 2").unwrap().1),
                Ok(false.into())
            );
            assert_eq!(
                run_expression(context, &Expression::parse("2 >= 2").unwrap().1),
                Ok(true.into())
            );
            assert_eq!(
                run_expression(context, &Expression::parse("2 >= 1").unwrap().1),
                Ok(true.into())
            );

            // GreaterThan(_, _)
            assert_eq!(
                run_expression(context, &Expression::parse("1 > 2").unwrap().1),
                Ok(false.into())
            );
            assert_eq!(
                run_expression(context, &Expression::parse("2 > 2").unwrap().1),
                Ok(false.into())
            );
            assert_eq!(
                run_expression(context, &Expression::parse("2 > 1").unwrap().1),
                Ok(true.into())
            );
        });
    }
}
