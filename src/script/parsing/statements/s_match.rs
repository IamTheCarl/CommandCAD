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
    character::complete::char as nom_char,
    combinator::map,
    error::context,
    multi::separated_list0,
    sequence::{delimited, pair, separated_pair, terminated},
};

use crate::script::{
    parsing::{space0, space1, take_keyword, Block, BlockStatement, Expression, Litteral, VResult},
    Span,
};

use super::Statement;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Match<S: Span> {
    pub starting_span: S,
    pub expression: Expression<S>,
    pub branches: Vec<MatchBranch<S>>,
}

impl<S: Span> Match<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        map(
            pair(
                terminated(take_keyword("match"), space1),
                pair(
                    Expression::parse,
                    delimited(
                        pair(space0, nom_char('{')),
                        separated_list0(
                            nom_char(','),
                            delimited(space0, MatchBranch::parse, space0),
                        ),
                        pair(space0, nom_char('}')),
                    ),
                ),
            ),
            |(starting_span, (expression, branches))| Self {
                starting_span,
                expression,
                branches,
            },
        )(input)
    }

    pub fn get_span(&self) -> &S {
        &self.starting_span
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct MatchBranch<S: Span> {
    pub litteral: Litteral<S>,
    pub block: Block<S>,
}

impl<S: Span> MatchBranch<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            separated_pair(
                context("Match branch must start with a litteral", Litteral::parse),
                delimited(
                    space0,
                    context("Match branch missing `=>` token", tag("=>")),
                    space0,
                ),
                alt((
                    Block::parse,
                    map(Expression::parse, |expression| Block {
                        statements: vec![BlockStatement::Open(Statement::Expression(expression))],
                    }),
                )),
            ),
            |(litteral, block)| Self { litteral, block },
        )(input)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn match_branch() {
        assert_eq!(
            MatchBranch::parse("1 => {}"),
            Ok((
                "",
                MatchBranch {
                    litteral: Litteral::parse("1").unwrap().1,
                    block: Block { statements: vec![] }
                }
            ))
        );

        assert_eq!(
            MatchBranch::parse("1 => false"),
            Ok((
                "",
                MatchBranch {
                    litteral: Litteral::parse("1").unwrap().1,
                    block: Block {
                        statements: vec![BlockStatement::Open(Statement::Expression(
                            Expression::parse("false").unwrap().1
                        ))]
                    }
                }
            ))
        );
    }

    #[test]
    fn statement_match() {
        assert_eq!(
            Match::parse("match a {}"),
            Ok((
                "",
                Match {
                    starting_span: "match",
                    expression: Expression::parse("a").unwrap().1,
                    branches: vec![]
                }
            ))
        );

        assert_eq!(
            Match::parse("match a { 1 => {} }"),
            Ok((
                "",
                Match {
                    starting_span: "match",
                    expression: Expression::parse("a").unwrap().1,
                    branches: vec![MatchBranch {
                        litteral: Litteral::parse("1").unwrap().1,
                        block: Block { statements: vec![] }
                    }]
                }
            ))
        );

        assert_eq!(
            Match::parse("match a { 1 => {}, 2 => {}}"),
            Ok((
                "",
                Match {
                    starting_span: "match",
                    expression: Expression::parse("a").unwrap().1,
                    branches: vec![
                        MatchBranch {
                            litteral: Litteral::parse("1").unwrap().1,
                            block: Block { statements: vec![] }
                        },
                        MatchBranch {
                            litteral: Litteral::parse("2").unwrap().1,
                            block: Block { statements: vec![] }
                        }
                    ]
                }
            ))
        );

        assert!(Match::parse("match a { not a branch }").is_err(),);
    }
}
