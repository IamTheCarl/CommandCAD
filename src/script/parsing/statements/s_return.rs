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
    combinator::{map, opt},
    sequence::{pair, preceded},
};

use crate::script::{
    parsing::{space1, take_keyword, Expression, VResult},
    Span,
};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Return<S: Span> {
    pub starting_span: S,
    pub expression: Option<Expression<S>>,
}

impl<S: Span> Return<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        map(
            pair(
                take_keyword("return"),
                opt(preceded(space1, Expression::parse)),
            ),
            |(starting_span, expression)| Self {
                starting_span,
                expression,
            },
        )(input)
    }

    pub fn get_span(&self) -> &S {
        &self.starting_span
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn statement_return() {
        assert_eq!(
            Return::parse("return"),
            Ok((
                "",
                Return {
                    starting_span: "return",
                    expression: None
                }
            ))
        );

        assert_eq!(
            Return::parse("return a"),
            Ok((
                "",
                Return {
                    starting_span: "return",
                    expression: Some(Expression::parse("a").unwrap().1)
                }
            ))
        );
        assert!(Return::parse("returna").is_err());
    }
}
