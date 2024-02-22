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
    combinator::{cut, map, opt, value},
    error::context,
    multi::separated_list0,
    sequence::{delimited, pair, preceded, terminated, tuple},
};

use super::{parse_name, space0, take_keyword, Litteral, Span, VResult, VariableType};

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum MemberVariableConstraint<S: Span> {
    Min(Litteral<S>),
    Max(Litteral<S>),
    Enum(Vec<Litteral<S>>),
    Integer,
    // TODO add the ability to constrain lists to a certain type, or set of types.
}

impl<S: Span> MemberVariableConstraint<S> {
    fn parse(input: S) -> VResult<S, Self> {
        alt((
            Self::parametric_constraint("min", map(Litteral::parse, Self::Min)),
            Self::parametric_constraint("max", map(Litteral::parse, Self::Max)),
            Self::parametric_constraint(
                "enum",
                map(
                    separated_list0(nom_char(','), delimited(space0, Litteral::parse, space0)),
                    Self::Enum,
                ),
            ),
            value(Self::Integer, take_keyword("integer")),
        ))(input)
    }

    fn parametric_constraint(
        name: &'static str,
        parameter_parser: impl FnMut(S) -> VResult<S, Self>,
    ) -> impl FnMut(S) -> VResult<S, Self> {
        preceded(
            take_keyword(name),
            delimited(
                pair(
                    space0,
                    context("Expected opening `(` to begin arguments", nom_char('(')),
                ),
                delimited(space0, parameter_parser, space0),
                context("Expected closing `)` to end arguments", nom_char(')')),
            ),
        )
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct MemberVariableConstraintList<S: Span> {
    pub constraints: Vec<MemberVariableConstraint<S>>,
}

impl<S: Span> MemberVariableConstraintList<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            delimited(
                pair(tag("#["), space0),
                separated_list0(
                    nom_char(','),
                    delimited(space0, MemberVariableConstraint::parse, space0),
                ),
                pair(
                    space0,
                    context("Missing closing `]` for constraint list", cut(tag("]"))),
                ),
            ),
            |constraints| Self { constraints },
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct MemberVariable<S: Span> {
    pub name: S,
    pub ty: VariableType<S>,
    pub constraints: Option<MemberVariableConstraintList<S>>,
    pub default_value: Option<Litteral<S>>,
}

impl<S: Span> MemberVariable<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        map(
            tuple((
                opt(terminated(MemberVariableConstraintList::parse, space0)),
                terminated(parse_name, space0),
                preceded(nom_char(':'), preceded(space0, VariableType::parse)),
                opt(preceded(
                    delimited(space0, nom_char('='), space0),
                    Litteral::parse,
                )),
            )),
            |(constraints, name, ty, default_value)| Self {
                constraints,
                name,
                ty,
                default_value,
            },
        )(input)
    }
}

#[cfg(test)]
mod test {
    use crate::script::parsing::Number;

    use super::*;

    #[test]
    fn member_variable_constraint() {
        assert_eq!(
            MemberVariableConstraint::parse("integer"),
            Ok(("", MemberVariableConstraint::Integer))
        );

        assert_eq!(
            MemberVariableConstraint::parse("min(0)"),
            Ok((
                "",
                MemberVariableConstraint::Min(Litteral::Number(Number {
                    integer: Some("0"),
                    dot: None,
                    fractional: None
                }))
            ))
        );
        assert_eq!(
            MemberVariableConstraint::parse("max(0)"),
            Ok((
                "",
                MemberVariableConstraint::Max(Litteral::Number(Number {
                    integer: Some("0"),
                    dot: None,
                    fractional: None
                }))
            ))
        );
        assert_eq!(
            MemberVariableConstraint::parse("enum(0, 1, 2)"),
            Ok((
                "",
                MemberVariableConstraint::Enum(vec![
                    Litteral::Number(Number {
                        integer: Some("0"),
                        dot: None,
                        fractional: None
                    }),
                    Litteral::Number(Number {
                        integer: Some("1"),
                        dot: None,
                        fractional: None
                    }),
                    Litteral::Number(Number {
                        integer: Some("2"),
                        dot: None,
                        fractional: None
                    })
                ])
            ))
        );
    }

    #[test]
    fn member_variable_constraint_list() {
        assert_eq!(
            MemberVariableConstraintList::parse("#[]"),
            Ok((
                "",
                MemberVariableConstraintList {
                    constraints: vec![]
                }
            ))
        );

        assert_eq!(
            MemberVariableConstraintList::parse("#[integer]"),
            Ok((
                "",
                MemberVariableConstraintList {
                    constraints: vec![MemberVariableConstraint::Integer]
                }
            ))
        );

        assert_eq!(
            MemberVariableConstraintList::parse("#[integer, integer]"),
            Ok((
                "",
                MemberVariableConstraintList {
                    constraints: vec![
                        MemberVariableConstraint::Integer,
                        MemberVariableConstraint::Integer
                    ]
                }
            ))
        );

        assert_eq!(
            MemberVariableConstraintList::parse("#[integer, integer, integer]"),
            Ok((
                "",
                MemberVariableConstraintList {
                    constraints: vec![
                        MemberVariableConstraint::Integer,
                        MemberVariableConstraint::Integer,
                        MemberVariableConstraint::Integer
                    ]
                }
            ))
        );
    }

    #[test]
    fn member_variable() {
        assert_eq!(
            MemberVariable::parse("variable: Number"),
            Ok((
                "",
                MemberVariable {
                    name: "variable",
                    ty: VariableType::Number,
                    constraints: None,
                    default_value: None,
                }
            ))
        );

        assert_eq!(
            MemberVariable::parse("variable: Number = 2"),
            Ok((
                "",
                MemberVariable {
                    name: "variable",
                    ty: VariableType::Number,
                    constraints: None,
                    default_value: Some(Litteral::Number(Number {
                        integer: Some("2"),
                        dot: None,
                        fractional: None
                    })),
                }
            ))
        );

        assert_eq!(
            MemberVariable::parse("#[integer] variable: Number = 2"),
            Ok((
                "",
                MemberVariable {
                    name: "variable",
                    ty: VariableType::Number,
                    constraints: Some(MemberVariableConstraintList {
                        constraints: vec![MemberVariableConstraint::Integer]
                    }),
                    default_value: Some(Litteral::Number(Number {
                        integer: Some("2"),
                        dot: None,
                        fractional: None
                    })),
                }
            ))
        );
    }
}