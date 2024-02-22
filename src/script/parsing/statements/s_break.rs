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
    character::complete::char as nom_char,
    combinator::{map, opt},
    sequence::{pair, preceded},
};

use crate::script::{
    parsing::{parse_name, space0, space1, take_keyword, Expression, VResult},
    Span,
};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Break<S: Span> {
    pub starting_span: S,
    pub loop_name: Option<S>,
    pub expression: Option<Expression<S>>,
}

impl<S: Span> Break<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        map(
            pair(
                take_keyword("break"),
                pair(
                    opt(preceded(pair(space0, nom_char('\'')), parse_name)),
                    opt(preceded(space1, Expression::parse)),
                ),
            ),
            |(starting_span, (loop_name, expression))| Break {
                starting_span,
                loop_name,
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
    fn statement_break() {
        assert_eq!(
            Break::parse("break"),
            Ok((
                "",
                Break {
                    starting_span: "break",
                    loop_name: None,
                    expression: None
                }
            ))
        );

        assert_eq!(
            Break::parse("break a"),
            Ok((
                "",
                Break {
                    starting_span: "break",
                    loop_name: None,
                    expression: Some(Expression::parse("a").unwrap().1)
                }
            ))
        );

        assert_eq!(
            Break::parse("break 'my_loop"),
            Ok((
                "",
                Break {
                    starting_span: "break",
                    loop_name: Some("my_loop"),
                    expression: None
                }
            ))
        );

        assert_eq!(
            Break::parse("break 'my_loop a"),
            Ok((
                "",
                Break {
                    starting_span: "break",
                    loop_name: Some("my_loop"),
                    expression: Some(Expression::parse("a").unwrap().1)
                }
            ))
        );
    }
}
