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

use super::term::Term;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ArithmeticExpression<S: Span> {
    Addition(Box<Self>, Term<S>),
    Subtraction(Box<Self>, Term<S>),
    Term(Term<S>),
}

impl<S: Span> ArithmeticExpression<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        #[derive(Clone)]
        enum Operator {
            Addition,
            Subtraction,
        }

        alt((
            flat_map(Term::parse, |first_term| {
                fold_many0(
                    pair(
                        delimited(
                            space0,
                            alt((
                                value(Operator::Addition, nom_char('+')),
                                value(Operator::Subtraction, nom_char('-')),
                            )),
                            space0,
                        ),
                        context("Expected right side term", cut(Term::parse)),
                    ),
                    move || ArithmeticExpression::Term(first_term.clone()),
                    |expression, (operator, factor)| match operator {
                        Operator::Addition => Self::Addition(Box::new(expression), factor),
                        Operator::Subtraction => Self::Subtraction(Box::new(expression), factor),
                    },
                )
            }),
            map(Term::parse, Self::Term),
        ))(input)
    }

    pub fn get_span(&self) -> &S {
        match self {
            ArithmeticExpression::Addition(spanable, _) => spanable.get_span(),
            ArithmeticExpression::Subtraction(spanable, _) => spanable.get_span(),
            ArithmeticExpression::Term(spanable) => spanable.get_span(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::script::parsing::statements::expression::{factor::Factor, trailer::Trailer};

    use super::*;

    #[test]
    fn arithmetic_expression() {
        // Addition(Box<Self>, Term<S>),
        assert_eq!(
            ArithmeticExpression::parse("a + b"),
            Ok((
                "",
                ArithmeticExpression::Addition(
                    Box::new(ArithmeticExpression::Term(Term::Trailer(Trailer::None(
                        Factor::Variable("a")
                    )))),
                    Term::Trailer(Trailer::None(Factor::Variable("b")))
                )
            ))
        );
        // Subtraction(Box<Self>, Term<S>),
        assert_eq!(
            ArithmeticExpression::parse("a - b"),
            Ok((
                "",
                ArithmeticExpression::Subtraction(
                    Box::new(ArithmeticExpression::Term(Term::Trailer(Trailer::None(
                        Factor::Variable("a")
                    )))),
                    Term::Trailer(Trailer::None(Factor::Variable("b")))
                )
            ))
        );
        // Term(Term<S>),
        assert_eq!(
            ArithmeticExpression::parse("a"),
            Ok((
                "",
                ArithmeticExpression::Term(Term::Trailer(Trailer::None(Factor::Variable("a"))))
            ))
        );

        // Order of operation tests.
        assert_eq!(
            ArithmeticExpression::parse("+a + b"),
            Ok((
                "",
                ArithmeticExpression::Addition(
                    Box::new(ArithmeticExpression::Term(Term::Trailer(Trailer::None(
                        Factor::UnaryPlus(Box::new(Factor::Variable("a")))
                    )))),
                    Term::Trailer(Trailer::None(Factor::Variable("b")))
                )
            ))
        );
    }
}
