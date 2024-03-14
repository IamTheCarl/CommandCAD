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
    Measurement(S),
    Vector(u8, S),
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
                map(FunctionSignature::parse, Self::Function),
                map(parse_name, Self::Measurement),
            )),
        )(input)
    }

    pub fn name(&self) -> Cow<'static, str> {
        match self {
            VariableType::String => "String".into(),
            VariableType::List => "List".into(),
            VariableType::Boolean => "Boolean".into(),
            VariableType::Range => "Range".into(),
            VariableType::Struct(name) => format!("struct {}", name.as_str()).into(),
            VariableType::Measurement(name) => name.to_string().into(),
            VariableType::Vector(dimension, name) => {
                format!("Vector{}<{}>", dimension, name.as_str()).into()
            }
            VariableType::Cycle => "Cycle".into(),
            VariableType::Region => "Region".into(),
            VariableType::Sketch => "Sketch".into(),
            VariableType::Surface => "Surface".into(),
            VariableType::Solid => "Solid".into(),
            VariableType::Shell => "Shell".into(),
            VariableType::Face => "Face".into(),
            VariableType::Function(function) => format!("{}", function).into(),
        }
    }
}

impl<S: Span> Display for VariableType<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VariableType::String => write!(f, "String"),
            VariableType::List => write!(f, "List"),
            VariableType::Boolean => write!(f, "Boolean"),
            VariableType::Range => write!(f, "Range"),
            VariableType::Struct(name) => write!(f, "struct {}", name.as_str()),
            VariableType::Measurement(name) => write!(f, "{}", name.as_str()),
            VariableType::Vector(dimension, name) => {
                write!(f, "Vector{}<{}>", dimension, name.as_str())
            }
            VariableType::Cycle => write!(f, "Cycle"),
            VariableType::Region => write!(f, "Region"),
            VariableType::Sketch => write!(f, "Sketch"),
            VariableType::Surface => write!(f, "Surface"),
            VariableType::Solid => write!(f, "Solid"),
            VariableType::Shell => write!(f, "Shell"),
            VariableType::Face => write!(f, "Face"),
            VariableType::Function(function) => write!(f, "{}", function),
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
            Ok(("", VariableType::Measurement("Length")))
        );
        assert_eq!(
            VariableType::parse("Angle"),
            Ok(("", VariableType::Measurement("Angle")))
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
                    return_type: Box::new(VariableType::Measurement("Number")),
                    arguments: vec![]
                })
            ))
        );

        assert_eq!(
            VariableType::parse("function() -> Number"),
            Ok((
                "",
                VariableType::Function(FunctionSignature::Function {
                    return_type: Box::new(VariableType::Measurement("Number")),
                    arguments: vec![]
                })
            ))
        );
    }
}
