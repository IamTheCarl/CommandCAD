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
    bytes::complete::tag,
    character::complete::char as nom_char,
    multi::separated_list0,
    sequence::{delimited, pair, terminated},
};

use super::{space0, Expression, Span, VResult};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct List<S: Span> {
    pub starting_span: S,
    pub expressions: Vec<Expression<S>>,
}

impl<S: Span> List<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        let (input, (starting_span, expressions)) = terminated(
            pair(
                tag("["),
                separated_list0(nom_char(','), delimited(space0, Expression::parse, space0)),
            ),
            nom_char(']'),
        )(input)?;

        Ok((
            input,
            Self {
                starting_span,
                expressions,
            },
        ))
    }

    pub fn get_span(&self) -> &S {
        &self.starting_span
    }
}

#[cfg(test)]
mod test {
    use crate::script::parsing::{ArithmeticExpression, Comparison, Factor, Term, Trailer};

    use super::*;

    #[test]
    fn list() {
        assert_eq!(
            List::parse("[]"),
            Ok((
                "",
                List {
                    starting_span: "[",
                    expressions: vec![]
                }
            ))
        );
        assert_eq!(
            List::parse("[one]"),
            Ok((
                "",
                List {
                    starting_span: "[",
                    expressions: vec![Expression::Buffer(Comparison::None(
                        ArithmeticExpression::Term(Term::Trailer(Trailer::None(Factor::Variable(
                            "one"
                        ))))
                    ))]
                }
            ))
        );
        assert_eq!(
            List::parse("[one, two]"),
            Ok((
                "",
                List {
                    starting_span: "[",
                    expressions: vec![
                        Expression::Buffer(Comparison::None(ArithmeticExpression::Term(
                            Term::Trailer(Trailer::None(Factor::Variable("one"),))
                        ))),
                        Expression::Buffer(Comparison::None(ArithmeticExpression::Term(
                            Term::Trailer(Trailer::None(Factor::Variable("two")))
                        )))
                    ]
                }
            ))
        );
        assert_eq!(
            List::parse("[one, two, three]"),
            Ok((
                "",
                List {
                    starting_span: "[",
                    expressions: vec![
                        Expression::Buffer(Comparison::None(ArithmeticExpression::Term(
                            Term::Trailer(Trailer::None(Factor::Variable("one"),))
                        ))),
                        Expression::Buffer(Comparison::None(ArithmeticExpression::Term(
                            Term::Trailer(Trailer::None(Factor::Variable("two"),))
                        ))),
                        Expression::Buffer(Comparison::None(ArithmeticExpression::Term(
                            Term::Trailer(Trailer::None(Factor::Variable("three"),))
                        )))
                    ]
                }
            ))
        );
        assert!(List::parse("[one, two, three, ]").is_err());
        assert!(List::parse("[one two]").is_err());
    }
}
