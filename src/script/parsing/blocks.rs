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
    combinator::{map, success},
    error::context,
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, terminated, tuple},
};

use super::{parse_name, space0, MemberVariable, Span, Statement, VResult, VariableType};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct NamedBlock<S: Span> {
    pub name: S,
    pub parameter_span: S,
    pub parameters: Vec<MemberVariable<S>>,
    pub block: Block<S>,
}

impl<S: Span> NamedBlock<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        map(
            Self::parse_internal(success(())),
            |(block, _return_type)| block,
        )(input)
    }

    pub fn parse_with_return_type(input: S) -> VResult<S, (Self, VariableType<S>)> {
        Self::parse_internal(context(
            "Could not parse return type",
            preceded(pair(tag("->"), space0), VariableType::parse),
        ))(input)
    }

    fn parse_internal<T>(
        return_type_parser: impl FnMut(S) -> VResult<S, T>,
    ) -> impl FnMut(S) -> VResult<S, (Self, T)> {
        map(
            tuple((
                parse_name,
                context(
                    "Expected parameter list",
                    terminated(
                        pair(
                            preceded(space0, tag("(")),
                            separated_list0(
                                nom_char(','),
                                delimited(space0, MemberVariable::parse, space0),
                            ),
                        ),
                        pair(nom_char(')'), space0),
                    ),
                ),
                delimited(space0, return_type_parser, space0),
                Block::parse,
            )),
            |(name, (parameter_span, parameters), return_type, block)| {
                (
                    Self {
                        name,
                        parameter_span,
                        parameters,
                        block,
                    },
                    return_type,
                )
            },
        )
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum BlockStatement<S: Span> {
    Closed(Statement<S>),
    Open(Statement<S>),
    Blank(S),
}

impl<S: Span> BlockStatement<S> {
    fn parse(input: S) -> VResult<S, Self> {
        alt((
            map(
                delimited(space0, Statement::parse, pair(space0, nom_char(';'))),
                Self::Closed,
            ),
            map(preceded(space0, Statement::parse), Self::Open),
            map(preceded(space0, tag(";")), Self::Blank),
        ))(input)
    }

    pub fn get(&self) -> Option<&Statement<S>> {
        match self {
            BlockStatement::Closed(statement) | BlockStatement::Open(statement) => Some(statement),
            BlockStatement::Blank(_) => None,
        }
    }

    pub fn get_span(&self) -> &S {
        match self {
            BlockStatement::Closed(spanable) => spanable.get_span(),
            BlockStatement::Open(spanable) => spanable.get_span(),
            BlockStatement::Blank(spanable) => spanable,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Block<S: Span> {
    pub statements: Vec<BlockStatement<S>>,
}

impl<S: Span> Block<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        map(
            delimited(
                context("Block is missing opening bracket", nom_char('{')),
                many0(alt((delimited(space0, BlockStatement::parse, space0),))),
                context(
                    "Block is missing closing bracket",
                    pair(space0, nom_char('}')),
                ),
            ),
            |statements| Self { statements },
        )(input)
    }
}

#[cfg(test)]
mod test {
    use crate::script::parsing::{
        member_variable::{
            MemberVariableConstraint, MemberVariableConstraintList, MemberVariableType,
        },
        Assign, Assignable, AssignableVariable, Break, Litteral, Loop, Number, Return,
    };

    use super::*;

    #[test]
    fn block() {
        assert_eq!(Block::parse("{}"), Ok(("", Block { statements: vec![] })));
        assert_eq!(
            Block::parse("{;}"),
            Ok((
                "",
                Block {
                    statements: vec![BlockStatement::Blank(";")],
                }
            ))
        );

        assert_eq!(
            Block::parse("{ break; a = b; }"),
            Ok((
                "",
                Block {
                    statements: vec![
                        BlockStatement::Closed(Statement::Break(Break {
                            starting_span: "break",
                            loop_name: None,
                            expression: None
                        })),
                        BlockStatement::Closed(Statement::Assign(Assign {
                            starting_span: "a",
                            is_new: false,
                            to_assign: Assignable::Variable(AssignableVariable {
                                name: "a",
                                ty: None,
                            }),
                            statement: Box::new(Statement::parse("b").unwrap().1)
                        }))
                    ],
                }
            ))
        );

        assert_eq!(
            Block::parse("{ a = b }"),
            Ok((
                "",
                Block {
                    statements: vec![BlockStatement::Open(Statement::Assign(Assign {
                        starting_span: "a",
                        is_new: false,
                        to_assign: Assignable::Variable(AssignableVariable {
                            name: "a",
                            ty: None,
                        }),
                        statement: Box::new(Statement::parse("b").unwrap().1)
                    }))],
                }
            ))
        );

        assert_eq!(
            Block::parse("{ break; a = b }"),
            Ok((
                "",
                Block {
                    statements: vec![
                        BlockStatement::Closed(Statement::Break(Break {
                            starting_span: "break",
                            loop_name: None,
                            expression: None
                        })),
                        BlockStatement::Open(Statement::Assign(Assign {
                            starting_span: "a",
                            is_new: false,
                            to_assign: Assignable::Variable(AssignableVariable {
                                name: "a",
                                ty: None,
                            }),
                            statement: Box::new(Statement::parse("b").unwrap().1)
                        }))
                    ],
                }
            ))
        );

        assert_eq!(
            Block::parse("{ loop {} }"),
            Ok((
                "",
                Block {
                    statements: vec![BlockStatement::Open(Statement::Loop(Loop {
                        starting_span: "loop",
                        name: None,
                        block: Block { statements: vec![] }
                    }))],
                }
            ))
        );

        assert_eq!(
            Block::parse("{ loop {} return }"),
            Ok((
                "",
                Block {
                    statements: vec![
                        BlockStatement::Open(Statement::Loop(Loop {
                            starting_span: "loop",
                            name: None,
                            block: Block { statements: vec![] }
                        })),
                        BlockStatement::Open(Statement::Return(Return {
                            starting_span: "return",
                            expression: None
                        }))
                    ],
                }
            ))
        );

        assert!(Block::parse("{ break a = b }").is_err());
        assert!(Block::parse("{ break a = b; }").is_err());
    }

    #[test]
    fn named_block() {
        assert_eq!(
            NamedBlock::parse("my_thing() {}"),
            Ok((
                "",
                NamedBlock {
                    name: "my_thing",
                    parameter_span: "(",
                    parameters: vec![],
                    block: Block { statements: vec![] }
                }
            ))
        );

        assert_eq!(
            NamedBlock::parse("my_thing(one: Length) {}"),
            Ok((
                "",
                NamedBlock {
                    name: "my_thing",
                    parameter_span: "(",
                    parameters: vec![MemberVariable {
                        name: "one",
                        ty: MemberVariableType {
                            ty: VariableType::Measurement("Length"),
                            constraints: None,
                            default_value: None
                        }
                    }],
                    block: Block { statements: vec![] }
                }
            ))
        );

        assert_eq!(
            NamedBlock::parse("my_thing(one: Length, two: Angle = 2) {}"),
            Ok((
                "",
                NamedBlock {
                    name: "my_thing",
                    parameter_span: "(",
                    parameters: vec![
                        MemberVariable {
                            name: "one",
                            ty: MemberVariableType {
                                ty: VariableType::Measurement("Length"),
                                constraints: None,
                                default_value: None
                            }
                        },
                        MemberVariable {
                            name: "two",
                            ty: MemberVariableType {
                                ty: VariableType::Measurement("Angle"),
                                constraints: None,
                                default_value: Some(Litteral::Number(Number {
                                    integer: Some("2"),
                                    dot: None,
                                    fractional: None
                                }))
                            }
                        }
                    ],
                    block: Block { statements: vec![] }
                }
            ))
        );

        assert_eq!(
            NamedBlock::parse("my_thing(one: Length, two: Angle = 2, #[integer] three: Number) {}"),
            Ok((
                "",
                NamedBlock {
                    name: "my_thing",
                    parameter_span: "(",
                    parameters: vec![
                        MemberVariable {
                            name: "one",
                            ty: MemberVariableType {
                                ty: VariableType::Measurement("Length"),
                                constraints: None,
                                default_value: None
                            }
                        },
                        MemberVariable {
                            name: "two",
                            ty: MemberVariableType {
                                ty: VariableType::Measurement("Angle"),
                                constraints: None,
                                default_value: Some(Litteral::Number(Number {
                                    integer: Some("2"),
                                    dot: None,
                                    fractional: None
                                }))
                            }
                        },
                        MemberVariable {
                            name: "three",
                            ty: MemberVariableType {
                                ty: VariableType::Number,
                                constraints: Some(MemberVariableConstraintList {
                                    constraints: vec![MemberVariableConstraint::Integer]
                                }),
                                default_value: None,
                            }
                        }
                    ],
                    block: Block { statements: vec![] }
                }
            ))
        );

        assert_eq!(
            NamedBlock::parse_with_return_type("my_thing() -> struct T {}"),
            Ok((
                "",
                (
                    NamedBlock {
                        name: "my_thing",
                        parameter_span: "(",
                        parameters: vec![],
                        block: Block { statements: vec![] }
                    },
                    VariableType::Struct("T")
                )
            ))
        );
    }
}
