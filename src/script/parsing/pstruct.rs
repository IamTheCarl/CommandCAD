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
    multi::separated_list0,
    sequence::{delimited, pair, preceded},
};

use super::{parse_name, space0, take_keyword, MemberVariable, Span, VResult};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StructDefinition<S: Span> {
    pub name: S,
    pub members: Vec<MemberVariable<S>>,
}

impl<S: Span> StructDefinition<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        map(
            preceded(
                take_keyword("struct"),
                pair(
                    delimited(space0, parse_name, space0),
                    delimited(
                        pair(nom_char('{'), space0),
                        separated_list0(nom_char(','), preceded(space0, MemberVariable::parse)),
                        preceded(
                            pair(opt(pair(space0, nom_char(','))), space0),
                            nom_char('}'),
                        ),
                    ),
                ),
            ),
            |(name, assignments)| Self {
                name,
                members: assignments,
            },
        )(input)
    }
}

#[cfg(test)]
mod test {
    use crate::script::parsing::member_variable::{
        MemberVariableConstraint, MemberVariableConstraintList, MemberVariableType,
    };
    use crate::script::parsing::{Litteral, VariableType};

    use super::*;

    #[test]
    fn parse_struct() {
        assert_eq!(
            StructDefinition::parse("struct MyStruct {}"),
            Ok((
                "",
                StructDefinition {
                    name: "MyStruct",
                    members: vec![]
                }
            ))
        );

        assert_eq!(
            StructDefinition::parse("struct MyStruct { a: Length }"),
            Ok((
                "",
                StructDefinition {
                    name: "MyStruct",
                    members: vec![MemberVariable {
                        name: "a",
                        ty: MemberVariableType {
                            ty: VariableType::Scalar("Length"),
                            constraints: None,
                            default_value: None
                        }
                    }]
                }
            ))
        );

        assert_eq!(
            StructDefinition::parse("struct MyStruct { a: Length, #[integer] b: Angle = true }"),
            Ok((
                "",
                StructDefinition {
                    name: "MyStruct",
                    members: vec![
                        MemberVariable {
                            name: "a",
                            ty: MemberVariableType {
                                ty: VariableType::Scalar("Length"),
                                constraints: None,
                                default_value: None
                            }
                        },
                        MemberVariable {
                            name: "b",
                            ty: MemberVariableType {
                                ty: VariableType::Scalar("Angle"),
                                constraints: Some(MemberVariableConstraintList {
                                    constraints: vec![MemberVariableConstraint::Integer]
                                }),
                                default_value: Some(Litteral::Boolean("true", true))
                            }
                        }
                    ]
                }
            ))
        );

        assert_eq!(
            StructDefinition::parse("struct MyStruct { a: Length, #[integer] b: Angle = true, }"),
            Ok((
                "",
                StructDefinition {
                    name: "MyStruct",
                    members: vec![
                        MemberVariable {
                            name: "a",
                            ty: MemberVariableType {
                                ty: VariableType::Scalar("Length"),
                                constraints: None,
                                default_value: None
                            }
                        },
                        MemberVariable {
                            name: "b",
                            ty: MemberVariableType {
                                ty: VariableType::Scalar("Angle"),
                                constraints: Some(MemberVariableConstraintList {
                                    constraints: vec![MemberVariableConstraint::Integer]
                                }),
                                default_value: Some(Litteral::Boolean("true", true))
                            }
                        }
                    ]
                }
            ))
        );
    }
}
