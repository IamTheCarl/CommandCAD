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

use super::assign::Assignable;

#[derive(Debug, Eq, PartialEq)]
pub struct For<S: Span> {
    pub starting_span: S,
    pub name: Option<S>,
    pub variable_assignment: Assignable<S>,
    pub iterator_expression: Expression<S>,
    pub block: Block<S>,
}

impl<S: Span> For<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        map(
            tuple((
                opt(delimited(
                    nom_char('\''),
                    parse_name,
                    pair(nom_char(':'), space0),
                )),
                pair(
                    take_keyword("for"),
                    cut(tuple((
                        delimited(
                            space0,
                            context("Missing variable assignment", Assignable::parse),
                            space0,
                        ),
                        context("Missing `in` keyword", take_keyword("in")),
                        delimited(
                            space0,
                            context(
                                "Missing iterator expression",
                                Expression::parse_no_struct_initalization,
                            ),
                            space0,
                        ),
                        Block::parse,
                    ))),
                ),
            )),
            |(name, (starting_span, (variable_assignment, _in, iterator_expression, block)))| {
                Self {
                    starting_span,
                    name,
                    variable_assignment,
                    iterator_expression,
                    block,
                }
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
    fn statement_for() {
        assert_eq!(
            For::parse("for a in b {}"),
            Ok((
                "",
                For {
                    starting_span: "for",
                    name: None,
                    variable_assignment: Assignable::parse("a").unwrap().1,
                    iterator_expression: Expression::parse("b").unwrap().1,
                    block: Block { statements: vec![] }
                }
            ))
        );

        assert_eq!(
            For::parse("'my_for_loop: for a in b {}"),
            Ok((
                "",
                For {
                    starting_span: "for",
                    name: Some("my_for_loop"),
                    variable_assignment: Assignable::parse("a").unwrap().1,
                    iterator_expression: Expression::parse("b").unwrap().1,
                    block: Block { statements: vec![] }
                }
            ))
        );
    }
}
