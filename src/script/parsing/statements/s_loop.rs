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
    sequence::{delimited, pair, preceded, tuple},
};

use crate::script::{
    parsing::{parse_name, space0, take_keyword, Block, VResult},
    Span,
};

#[derive(Debug, Eq, PartialEq)]
pub struct Loop<S: Span> {
    pub starting_span: S,
    pub name: Option<S>,
    pub block: Block<S>,
}

impl<S: Span> Loop<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        map(
            tuple((
                opt(delimited(
                    nom_char('\''),
                    parse_name,
                    pair(nom_char(':'), space0),
                )),
                pair(
                    take_keyword("loop"),
                    cut(preceded(
                        space0,
                        context("Loop is missing its block", Block::parse),
                    )),
                ),
            )),
            |(name, (starting_span, block))| Self {
                starting_span,
                name,
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
    fn statement_loop() {
        assert_eq!(
            Loop::parse("loop {}"),
            Ok((
                "",
                Loop {
                    starting_span: "loop",
                    name: None,
                    block: Block { statements: vec![] }
                }
            ))
        );

        assert_eq!(
            Loop::parse("'my_loop: loop {}"),
            Ok((
                "",
                Loop {
                    starting_span: "loop",
                    name: Some("my_loop"),
                    block: Block { statements: vec![] }
                }
            ))
        );
    }
}
