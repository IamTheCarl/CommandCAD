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
    combinator::{cut, flat_map, map, value},
    error::context,
    multi::fold_many0,
    sequence::{delimited, pair},
};

use crate::script::{
    parsing::{space0, VResult},
    Span,
};

pub use self::{
    arithmetic::ArithmeticExpression, comparison::Comparison, factor::Factor, term::Term,
    trailer::Trailer,
};

mod arithmetic;
mod comparison;
mod factor;
mod term;
mod trailer;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression<S: Span> {
    And(Box<Self>, Comparison<S>),
    Or(Box<Self>, Comparison<S>),
    Buffer(Comparison<S>),
}

impl<S: Span> Expression<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        Self::parse_impl(Trailer::parse, input)
    }

    pub fn parse_no_struct_initalization(input: S) -> VResult<S, Self> {
        Self::parse_impl(Trailer::parse_no_struct_initalization, input)
    }

    fn parse_impl(trailer_parser: fn(S) -> VResult<S, Trailer<S>>, input: S) -> VResult<S, Self> {
        #[derive(Clone)]
        enum Operator {
            And,
            Or,
        }

        alt((
            flat_map(
                |input| Comparison::parser(trailer_parser, input),
                move |first_comp| {
                    fold_many0(
                        pair(
                            delimited(
                                space0,
                                alt((
                                    value(Operator::And, tag("&&")),
                                    value(Operator::Or, tag("||")),
                                )),
                                space0,
                            ),
                            context(
                                "Expected right side comparison or arithmetic expression",
                                cut(move |input| Comparison::parser(trailer_parser, input)),
                            ),
                        ),
                        move || Self::Buffer(first_comp.clone()),
                        |expression, (operator, comparison)| match operator {
                            Operator::And => Self::And(Box::new(expression), comparison),
                            Operator::Or => Self::Or(Box::new(expression), comparison),
                        },
                    )
                },
            ),
            map(
                |input| Comparison::parser(trailer_parser, input),
                Self::Buffer,
            ),
        ))(input)
    }

    pub fn get_span(&self) -> &S {
        match self {
            Expression::And(spanable, _) => spanable.get_span(),
            Expression::Or(spanable, _) => spanable.get_span(),
            Expression::Buffer(spanable) => spanable.get_span(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::script::parsing::{
        statements::expression::{
            arithmetic::ArithmeticExpression, factor::Factor, term::Term, trailer::Trailer,
        },
        List, Litteral,
    };

    use super::*;

    #[test]
    fn expression() {
        assert_eq!(
            Expression::parse("a"),
            Ok((
                "",
                Expression::Buffer(Comparison::None(ArithmeticExpression::Term(Term::Trailer(
                    Trailer::None(Factor::Variable("a"))
                ))),)
            ))
        );

        assert_eq!(
            Expression::parse("a || b"),
            Ok((
                "",
                Expression::Or(
                    Box::new(Expression::Buffer(Comparison::None(
                        ArithmeticExpression::Term(Term::Trailer(Trailer::None(Factor::Variable(
                            "a"
                        ))))
                    ))),
                    Comparison::None(ArithmeticExpression::Term(Term::Trailer(Trailer::None(
                        Factor::Variable("b")
                    ))))
                )
            ))
        );

        assert_eq!(
            Expression::parse("a && b"),
            Ok((
                "",
                Expression::And(
                    Box::new(Expression::Buffer(Comparison::None(
                        ArithmeticExpression::Term(Term::Trailer(Trailer::None(Factor::Variable(
                            "a"
                        ))))
                    ))),
                    Comparison::None(ArithmeticExpression::Term(Term::Trailer(Trailer::None(
                        Factor::Variable("b")
                    ))))
                )
            ))
        );
    }
}
