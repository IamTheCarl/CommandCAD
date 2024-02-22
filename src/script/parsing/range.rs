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
    combinator::{map, opt},
    sequence::{delimited, tuple},
};

use crate::script::parsing::space0;

use super::{Span, Trailer, VResult};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Range<S: Span> {
    pub comparison_operator: S,
    pub lower_bound: Option<Trailer<S>>,
    pub upper_bound_is_inclusive: bool,
    pub upper_bound: Option<Trailer<S>>,
}

impl<S: Span> Range<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        enum RangeType<S> {
            Exclusive(S),
            Inclusive(S),
        }

        map(
            tuple((
                opt(Trailer::parse),
                delimited(
                    space0,
                    alt((
                        map(tag("..="), RangeType::Inclusive),
                        map(tag(".."), RangeType::Exclusive),
                    )),
                    space0,
                ),
                opt(Trailer::parse),
            )),
            |(lower_bound, range_type, upper_bound)| {
                let (comparison_operator, upper_bound_is_inclusive) = match range_type {
                    RangeType::Inclusive(operator) => (operator, true),
                    RangeType::Exclusive(operator) => (operator, false),
                };

                Self {
                    comparison_operator,
                    upper_bound,
                    upper_bound_is_inclusive,
                    lower_bound,
                }
            },
        )(input)
    }

    pub fn get_span(&self) -> &S {
        self.lower_bound
            .as_ref()
            .map(|expression| expression.get_span())
            .unwrap_or(&self.comparison_operator)
    }
}

#[cfg(test)]
mod test {
    use crate::script::parsing::Factor;

    use super::*;

    #[test]
    fn range() {
        assert_eq!(
            Range::parse(".."),
            Ok((
                "",
                Range {
                    comparison_operator: "..",
                    upper_bound: None,
                    upper_bound_is_inclusive: false,
                    lower_bound: None,
                }
            ))
        );
        assert_eq!(
            Range::parse("..="),
            Ok((
                "",
                Range {
                    comparison_operator: "..=",
                    upper_bound: None,
                    upper_bound_is_inclusive: true,
                    lower_bound: None,
                }
            ))
        );
        assert_eq!(
            Range::parse("a..b"),
            Ok((
                "",
                Range {
                    comparison_operator: "..",
                    upper_bound: Some(Trailer::None(Factor::Variable("b"))),
                    lower_bound: Some(Trailer::None(Factor::Variable("a"))),
                    upper_bound_is_inclusive: false,
                }
            ))
        );

        assert_eq!(*Range::parse("..").unwrap().1.get_span(), "..");
        assert_eq!(*Range::parse("a..").unwrap().1.get_span(), "a");
    }
}
