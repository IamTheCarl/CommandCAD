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
    combinator::{cut, map, opt},
    error::context,
    sequence::{delimited, pair, preceded, terminated, tuple},
};

use crate::script::{
    parsing::{space0, space1, take_keyword, Block, Expression, VResult},
    Span,
};

#[derive(Debug, Eq, PartialEq)]
pub struct If<S: Span> {
    pub starting_span: S,
    pub expression: Expression<S>,
    pub block: Block<S>,
    pub else_statement: Option<Else<S>>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Else<S: Span> {
    Else(Block<S>),
    IfElse(Box<If<S>>),
}

impl<S: Span> If<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        map(
            pair(
                terminated(take_keyword("if"), space1),
                tuple((
                    context(
                        "An expression is required for the if condition",
                        Expression::parse_no_struct_initalization,
                    ),
                    delimited(
                        space0,
                        context("Expected a code block for if statement", Block::parse),
                        space0,
                    ),
                    opt(preceded(
                        take_keyword("else"),
                        context(
                            "Expected a code block or another if statement after `else` keyword",
                            cut(alt((
                                map(preceded(space0, Block::parse), |block| Else::Else(block)),
                                map(preceded(space1, Self::parse), |if_statement| {
                                    Else::IfElse(Box::new(if_statement))
                                }),
                            ))),
                        ),
                    )),
                )),
            ),
            |(starting_span, (expression, block, else_statement))| Self {
                starting_span,
                expression,
                block,
                else_statement,
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
    fn statement_if() {
        assert_eq!(
            If::parse("if a {}"),
            Ok((
                "",
                If {
                    starting_span: "if",
                    expression: Expression::parse("a").unwrap().1,
                    block: Block { statements: vec![] },
                    else_statement: None
                }
            ))
        );

        assert_eq!(
            If::parse("if a {} else {}"),
            Ok((
                "",
                If {
                    starting_span: "if",
                    expression: Expression::parse("a").unwrap().1,
                    block: Block { statements: vec![] },
                    else_statement: Some(Else::Else(Block { statements: vec![] }))
                }
            ))
        );

        assert_eq!(
            If::parse("if a {} else if b {}"),
            Ok((
                "",
                If {
                    starting_span: "if",
                    expression: Expression::parse("a").unwrap().1,
                    block: Block { statements: vec![] },
                    else_statement: Some(Else::IfElse(Box::new(If {
                        starting_span: "if",
                        expression: Expression::parse("b").unwrap().1,
                        block: Block { statements: vec![] },
                        else_statement: None,
                    })))
                }
            ))
        );

        assert!(If::parse("ifa {}").is_err());
        assert_eq!(If::parse("if a {} elseif").unwrap().0, "elseif");
        assert!(If::parse("if a {} else = 0").is_err());
        assert_eq!(If::parse("if a {} var = 0").unwrap().0, "var = 0");
    }
}
