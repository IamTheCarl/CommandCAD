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
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::char as nom_char,
    combinator::{cut, flat_map, map, value},
    error::context,
    multi::fold_many0,
    sequence::{delimited, pair},
};

use crate::script::{
    parsing::{space0, VResult},
    Span,
};

use super::arithmetic::ArithmeticExpression;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Comparison<S: Span> {
    LessThan(Box<Self>, ArithmeticExpression<S>),
    LessThanEqual(Box<Self>, ArithmeticExpression<S>),
    Equal(Box<Self>, ArithmeticExpression<S>),
    GreaterThanEqual(Box<Self>, ArithmeticExpression<S>),
    GreaterThan(Box<Self>, ArithmeticExpression<S>),
    None(ArithmeticExpression<S>),
}

impl<S: Span> Comparison<S> {
    pub(super) fn parse(input: S) -> VResult<S, Self> {
        #[derive(Clone)]
        enum Operator {
            LessThan,
            LessThanEqual,
            Equal,
            GreaterThanEqual,
            GreaterThan,
        }

        alt((
            flat_map(ArithmeticExpression::parse, |first_expression| {
                fold_many0(
                    pair(
                        delimited(
                            space0,
                            alt((
                                value(Operator::LessThanEqual, tag("<=")),
                                value(Operator::Equal, tag("==")),
                                value(Operator::GreaterThanEqual, tag(">=")),
                                value(Operator::LessThan, nom_char('<')),
                                value(Operator::GreaterThan, nom_char('>')),
                            )),
                            space0,
                        ),
                        context(
                            "Expected right side arithmetic expression.",
                            cut(ArithmeticExpression::parse),
                        ),
                    ),
                    move || Comparison::None(first_expression.clone()),
                    |comparison, (operator, expression)| match operator {
                        Operator::LessThan => Self::LessThan(Box::new(comparison), expression),
                        Operator::LessThanEqual => {
                            Self::LessThanEqual(Box::new(comparison), expression)
                        }
                        Operator::Equal => Self::Equal(Box::new(comparison), expression),
                        Operator::GreaterThanEqual => {
                            Self::GreaterThanEqual(Box::new(comparison), expression)
                        }
                        Operator::GreaterThan => {
                            Self::GreaterThan(Box::new(comparison), expression)
                        }
                    },
                )
            }),
            map(ArithmeticExpression::parse, Self::None),
        ))(input)
    }

    pub fn get_span(&self) -> &S {
        match self {
            Comparison::LessThan(spanable, _) => spanable.get_span(),
            Comparison::LessThanEqual(spanable, _) => spanable.get_span(),
            Comparison::Equal(spanable, _) => spanable.get_span(),
            Comparison::GreaterThanEqual(spanable, _) => spanable.get_span(),
            Comparison::GreaterThan(spanable, _) => spanable.get_span(),
            Comparison::None(spanable) => spanable.get_span(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::script::parsing::statements::expression::{
        factor::Factor, term::Term, trailer::Trailer,
    };

    use super::*;

    #[test]
    fn comparison() {
        assert_eq!(
            Comparison::parse("a"),
            Ok((
                "",
                Comparison::None(ArithmeticExpression::Term(Term::Trailer(Trailer::None(
                    Factor::Variable("a")
                ))))
            ))
        );

        assert_eq!(
            Comparison::parse("a < b"),
            Ok((
                "",
                Comparison::LessThan(
                    Box::new(Comparison::None(ArithmeticExpression::Term(Term::Trailer(
                        Trailer::None(Factor::Variable("a"))
                    )))),
                    ArithmeticExpression::Term(Term::Trailer(Trailer::None(Factor::Variable("b")))),
                )
            ))
        );

        assert_eq!(
            Comparison::parse("a <= b"),
            Ok((
                "",
                Comparison::LessThanEqual(
                    Box::new(Comparison::None(ArithmeticExpression::Term(Term::Trailer(
                        Trailer::None(Factor::Variable("a"))
                    )))),
                    ArithmeticExpression::Term(Term::Trailer(Trailer::None(Factor::Variable("b")))),
                )
            ))
        );

        assert_eq!(
            Comparison::parse("a == b"),
            Ok((
                "",
                Comparison::Equal(
                    Box::new(Comparison::None(ArithmeticExpression::Term(Term::Trailer(
                        Trailer::None(Factor::Variable("a"))
                    )))),
                    ArithmeticExpression::Term(Term::Trailer(Trailer::None(Factor::Variable("b")))),
                )
            ))
        );

        assert_eq!(
            Comparison::parse("a >= b"),
            Ok((
                "",
                Comparison::GreaterThanEqual(
                    Box::new(Comparison::None(ArithmeticExpression::Term(Term::Trailer(
                        Trailer::None(Factor::Variable("a"))
                    )))),
                    ArithmeticExpression::Term(Term::Trailer(Trailer::None(Factor::Variable("b")))),
                )
            ))
        );

        assert_eq!(
            Comparison::parse("a > b"),
            Ok((
                "",
                Comparison::GreaterThan(
                    Box::new(Comparison::None(ArithmeticExpression::Term(Term::Trailer(
                        Trailer::None(Factor::Variable("a"))
                    )))),
                    ArithmeticExpression::Term(Term::Trailer(Trailer::None(Factor::Variable("b")))),
                )
            ))
        );
    }
}
