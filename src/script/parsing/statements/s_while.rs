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
    combinator::{cut, map, opt},
    error::context,
    sequence::{delimited, pair, tuple},
};

use crate::script::{
    parsing::{parse_name, space0, take_keyword, Block, Expression, VResult},
    Span,
};

#[derive(Debug, Eq, PartialEq)]
pub struct While<S: Span> {
    pub starting_span: S,
    pub name: Option<S>,
    pub expression: Expression<S>,
    pub block: Block<S>,
}

impl<S: Span> While<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        map(
            tuple((
                opt(delimited(
                    nom_char('\''),
                    parse_name,
                    pair(nom_char(':'), space0),
                )),
                pair(
                    take_keyword("while"),
                    cut(pair(
                        delimited(
                            space0,
                            context(
                                "Missing while loop condition expression",
                                Expression::parse_no_struct_initalization,
                            ),
                            space0,
                        ),
                        context("While loop is missing block", Block::parse),
                    )),
                ),
            )),
            |(name, (starting_span, (expression, block)))| Self {
                starting_span,
                name,
                expression,
                block,
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
    fn statement_while() {
        assert_eq!(
            While::parse("while a {}"),
            Ok((
                "",
                While {
                    starting_span: "while",
                    name: None,
                    expression: Expression::parse("a").unwrap().1,
                    block: Block { statements: vec![] }
                }
            ))
        );

        assert_eq!(
            While::parse("'my_while_loop: while a {}"),
            Ok((
                "",
                While {
                    starting_span: "while",
                    name: Some("my_while_loop"),
                    expression: Expression::parse("a").unwrap().1,
                    block: Block { statements: vec![] }
                }
            ))
        );
    }
}
