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
    parsing::{space0, Range, VResult},
    Span,
};

use super::trailer::Trailer;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Term<S: Span> {
    Multiply(Box<Self>, Trailer<S>),
    Divide(Box<Self>, Trailer<S>),
    Range(Range<S>),
    Trailer(Trailer<S>),
}

impl<S: Span> Term<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        #[derive(Clone)]
        enum Operator {
            Multiplication,
            Division,
        }

        alt((
            map(Range::parse, Self::Range),
            flat_map(Trailer::parse, |first_trailer| {
                fold_many0(
                    pair(
                        delimited(
                            space0,
                            alt((
                                value(Operator::Multiplication, nom_char('*')),
                                value(Operator::Division, nom_char('/')),
                            )),
                            space0,
                        ),
                        context("Expected right side term.", cut(Trailer::parse)),
                    ),
                    move || Self::Trailer(first_trailer.clone()),
                    |term, (operator, accessor)| match operator {
                        Operator::Multiplication => Self::Multiply(Box::new(term), accessor),
                        Operator::Division => Self::Divide(Box::new(term), accessor),
                    },
                )
            }),
            map(Trailer::parse, Self::Trailer),
        ))(input)
    }

    pub fn get_span(&self) -> &S {
        match self {
            Term::Multiply(spanable, _) => spanable.get_span(),
            Term::Divide(spanable, _) => spanable.get_span(),
            Term::Range(spannable) => spannable.get_span(),
            Term::Trailer(spanable) => spanable.get_span(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::script::parsing::statements::expression::factor::Factor;

    use super::*;

    #[test]
    fn term() {
        // Multiply(Box<Self>, Factor<S>),
        assert_eq!(
            Term::parse("x * y"),
            Ok((
                "",
                Term::Multiply(
                    Box::new(Term::Trailer(Trailer::None(Factor::Variable("x")))),
                    Trailer::None(Factor::Variable("y"))
                )
            ))
        );
        // Divide(Box<Self>, Factor<S>),
        assert_eq!(
            Term::parse("x / y"),
            Ok((
                "",
                Term::Divide(
                    Box::new(Term::Trailer(Trailer::None(Factor::Variable("x")))),
                    Trailer::None(Factor::Variable("y"))
                )
            ))
        );
        // Factor(Factor<S>),
        assert_eq!(
            Term::parse("x"),
            Ok(("", Term::Trailer(Trailer::None(Factor::Variable("x")))))
        );

        // Range(Range<S>),
        assert_eq!(
            Term::parse("a..b"),
            Ok((
                "",
                Term::Range(Range {
                    comparison_operator: "..",
                    upper_bound: Some(Trailer::None(Factor::Variable("b"))),
                    lower_bound: Some(Trailer::None(Factor::Variable("a"))),
                    upper_bound_is_inclusive: false,
                })
            ))
        );
        assert_eq!(
            Term::parse(".."),
            Ok((
                "",
                Term::Range(Range {
                    comparison_operator: "..",
                    upper_bound: None,
                    lower_bound: None,
                    upper_bound_is_inclusive: false,
                })
            ))
        );
    }
}
