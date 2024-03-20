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
use std::{borrow::Cow, fmt::Display};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::char as nom_char,
    combinator::map,
    error::context,
    multi::many0,
    sequence::{delimited, pair, preceded, terminated},
};

use crate::script::parsing::IteratorFormatter;

use super::{parse_name, space0, take_keyword, MemberVariableType, Span, VResult};

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum FunctionSignature<S: Span> {
    Function {
        return_type: Box<VariableType<S>>,
        arguments: Vec<MemberVariableType<S>>,
    },
    Task {
        return_type: Box<VariableType<S>>,
        arguments: Vec<MemberVariableType<S>>,
    },
    Sketch {
        arguments: Vec<MemberVariableType<S>>,
    },
    Solid {
        arguments: Vec<MemberVariableType<S>>,
    },
}

impl<S: Span> FunctionSignature<S> {
    fn parse(input: S) -> VResult<S, Self> {
        alt((
            Self::parse_private_function,
            Self::parse_task,
            Self::parse_sketch,
            Self::parse_solid,
        ))(input)
    }

    fn parse_private_function(input: S) -> VResult<S, Self> {
        map(
            preceded(
                pair(tag("function"), space0),
                pair(
                    terminated(Self::parse_argument_list, space0),
                    Self::parse_return_type,
                ),
            ),
            |(arguments, return_type)| Self::Function {
                arguments,
                return_type: Box::new(return_type),
            },
        )(input)
    }
    fn parse_task(input: S) -> VResult<S, Self> {
        map(
            preceded(
                pair(tag("task"), space0),
                pair(
                    terminated(Self::parse_argument_list, space0),
                    Self::parse_return_type,
                ),
            ),
            |(arguments, return_type)| Self::Task {
                arguments,
                return_type: Box::new(return_type),
            },
        )(input)
    }
    fn parse_sketch(input: S) -> VResult<S, Self> {
        map(
            preceded(
                pair(tag("sketch"), space0),
                terminated(Self::parse_argument_list, space0),
            ),
            |arguments| Self::Sketch { arguments },
        )(input)
    }
    fn parse_solid(input: S) -> VResult<S, Self> {
        map(
            preceded(
                pair(tag("solid"), space0),
                terminated(Self::parse_argument_list, space0),
            ),
            |arguments| Self::Sketch { arguments },
        )(input)
    }

    fn parse_argument_list(input: S) -> VResult<S, Vec<MemberVariableType<S>>> {
        delimited(
            nom_char('('),
            many0(MemberVariableType::parse),
            nom_char(')'),
        )(input)
    }

    fn parse_return_type(input: S) -> VResult<S, VariableType<S>> {
        preceded(pair(tag("->"), space0), VariableType::parse)(input)
    }
}

impl<S: Span> Display for FunctionSignature<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionSignature::Function {
                return_type,
                arguments,
            } => write!(
                f,
                "function({}) -> {}",
                IteratorFormatter(arguments.iter()),
                return_type
            ),
            FunctionSignature::Task {
                return_type,
                arguments,
            } => write!(
                f,
                "task({}) -> {}",
                IteratorFormatter(arguments.iter()),
                return_type
            ),
            FunctionSignature::Sketch { arguments } => {
                write!(f, "sketch({})", IteratorFormatter(arguments.iter()))
            }
            FunctionSignature::Solid { arguments } => {
                write!(f, "solid({})", IteratorFormatter(arguments.iter()))
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum VariableType<S: Span> {
    String,
    List,
    Boolean,
    Range,
    Struct(S),
    Scalar(S),
    Vector(u8, S),
    Transform(u8),
    Quaternion,
    Cycle,
    Region,
    Sketch,
    Surface,
    Solid,
    Shell,
    Face,
    Function(FunctionSignature<S>),
}

impl<S: Span> VariableType<S> {
    pub(super) fn parse(input: S) -> VResult<S, Self> {
        context(
            "Invalid type",
            alt((
                map(tag("String"), |_| Self::String),
                map(tag("List"), |_| Self::List),
                map(tag("Boolean"), |_| Self::Boolean),
                map(tag("Range"), |_| Self::Range),
                map(tag("Cycle"), |_| Self::Cycle),
                map(tag("Region"), |_| Self::Region),
                map(tag("Sketch"), |_| Self::Sketch),
                map(tag("Surface"), |_| Self::Surface),
                map(tag("Solid"), |_| Self::Solid),
                map(tag("Shell"), |_| Self::Shell),
                map(tag("Face"), |_| Self::Face),
                map(
                    preceded(pair(take_keyword("struct"), space0), parse_name),
                    Self::Struct,
                ),
                map(
                    preceded(
                        pair(take_keyword("Vector2"), space0),
                        delimited(
                            pair(nom_char('<'), space0),
                            parse_name,
                            pair(space0, nom_char('>')),
                        ),
                    ),
                    |name| Self::Vector(2, name),
                ),
                map(
                    preceded(
                        pair(take_keyword("Vector3"), space0),
                        delimited(
                            pair(nom_char('<'), space0),
                            parse_name,
                            pair(space0, nom_char('>')),
                        ),
                    ),
                    |name| Self::Vector(3, name),
                ),
                map(
                    preceded(
                        pair(take_keyword("Vector4"), space0),
                        delimited(
                            pair(nom_char('<'), space0),
                            parse_name,
                            pair(space0, nom_char('>')),
                        ),
                    ),
                    |name| Self::Vector(4, name),
                ),
                map(tag("Transform2D"), |_| Self::Transform(2)),
                map(tag("Transform3D"), |_| Self::Transform(3)),
                map(tag("Quaternion"), |_| Self::Quaternion),
                map(FunctionSignature::parse, Self::Function),
                map(parse_name, Self::Scalar),
            )),
        )(input)
    }

    pub fn name(&self) -> Cow<'static, str> {
        match self {
            Self::String => "String".into(),
            Self::List => "List".into(),
            Self::Boolean => "Boolean".into(),
            Self::Range => "Range".into(),
            Self::Struct(name) => format!("struct {}", name.as_str()).into(),
            Self::Scalar(name) => name.to_string().into(),
            Self::Vector(dimension, name) => {
                format!("Vector{}<{}>", dimension, name.as_str()).into()
            }
            Self::Transform(2) => "Transform2D".into(),
            Self::Transform(3) => "Transform3D".into(),
            Self::Transform(_) => unreachable!(),
            Self::Quaternion => "Quaternion".into(),
            Self::Cycle => "Cycle".into(),
            Self::Region => "Region".into(),
            Self::Sketch => "Sketch".into(),
            Self::Surface => "Surface".into(),
            Self::Solid => "Solid".into(),
            Self::Shell => "Shell".into(),
            Self::Face => "Face".into(),
            Self::Function(function) => format!("{}", function).into(),
        }
    }
}

impl<S: Span> Display for VariableType<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String => write!(f, "String"),
            Self::List => write!(f, "List"),
            Self::Boolean => write!(f, "Boolean"),
            Self::Range => write!(f, "Range"),
            Self::Struct(name) => write!(f, "struct {}", name.as_str()),
            Self::Scalar(name) => write!(f, "{}", name.as_str()),
            Self::Vector(dimension, name) => {
                write!(f, "Vector{}<{}>", dimension, name.as_str())
            }
            Self::Transform(2) => write!(f, "Transform2D"),
            Self::Transform(3) => write!(f, "Transform3D"),
            Self::Transform(_) => unreachable!(),
            Self::Quaternion => write!(f, "Quaternion"),
            Self::Cycle => write!(f, "Cycle"),
            Self::Region => write!(f, "Region"),
            Self::Sketch => write!(f, "Sketch"),
            Self::Surface => write!(f, "Surface"),
            Self::Solid => write!(f, "Solid"),
            Self::Shell => write!(f, "Shell"),
            Self::Face => write!(f, "Face"),
            Self::Function(function) => write!(f, "{}", function),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn variable_type() {
        assert_eq!(
            VariableType::parse("Length"),
            Ok(("", VariableType::Scalar("Length")))
        );
        assert_eq!(
            VariableType::parse("Angle"),
            Ok(("", VariableType::Scalar("Angle")))
        );
        assert_eq!(
            VariableType::parse("Vector2<Length>"),
            Ok(("", VariableType::Vector(2, "Length")))
        );
        assert_eq!(
            VariableType::parse("Vector3<Length>"),
            Ok(("", VariableType::Vector(3, "Length")))
        );
        assert_eq!(
            VariableType::parse("Vector4<Length>"),
            Ok(("", VariableType::Vector(4, "Length")))
        );
        assert_eq!(VariableType::parse("List"), Ok(("", VariableType::List)));
        assert_eq!(
            VariableType::parse("Boolean"),
            Ok(("", VariableType::Boolean))
        );
        assert_eq!(
            VariableType::parse("struct MyStruct"),
            Ok(("", VariableType::Struct("MyStruct")))
        );

        assert_eq!(
            VariableType::parse("function() -> Number"),
            Ok((
                "",
                VariableType::Function(FunctionSignature::Function {
                    return_type: Box::new(VariableType::Scalar("Number")),
                    arguments: vec![]
                })
            ))
        );

        assert_eq!(
            VariableType::parse("function() -> Number"),
            Ok((
                "",
                VariableType::Function(FunctionSignature::Function {
                    return_type: Box::new(VariableType::Scalar("Number")),
                    arguments: vec![]
                })
            ))
        );
    }
}
