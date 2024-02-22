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
use std::borrow::Cow;

use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map, value},
    error::context,
    sequence::{pair, preceded},
};

use super::{parse_name, space0, take_keyword, Span, VResult};

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum VariableType<S: Span> {
    Number,
    String,
    List,
    Boolean,
    Range,
    Struct(S),
    Measurement(S),
    Cycle,
    Region,
    Sketch,
    Surface,
    Solid,
}

impl<S: Span> VariableType<S> {
    pub(super) fn parse(input: S) -> VResult<S, Self> {
        context(
            "Invalid type",
            alt((
                value(Self::Number, tag("Number")),
                value(Self::String, tag("String")),
                value(Self::List, tag("List")),
                value(Self::Boolean, tag("Boolean")),
                value(Self::Range, tag("Range")),
                value(Self::Cycle, tag("Cycle")),
                value(Self::Region, tag("Region")),
                value(Self::Sketch, tag("Sketch")),
                value(Self::Surface, tag("Surface")),
                value(Self::Solid, tag("Solid")),
                map(
                    preceded(pair(take_keyword("struct"), space0), parse_name),
                    Self::Struct,
                ),
                map(parse_name, Self::Measurement),
            )),
        )(input)
    }

    pub fn name(&self) -> Cow<'static, str> {
        match self {
            VariableType::Number => "Number".into(),
            VariableType::String => "String".into(),
            VariableType::List => "List".into(),
            VariableType::Boolean => "Boolean".into(),
            VariableType::Range => "Range".into(),
            VariableType::Struct(name) => format!("struct {}", name.as_str()).into(),
            VariableType::Measurement(name) => name.to_string().into(),
            VariableType::Cycle => "Cycle".into(),
            VariableType::Region => "Region".into(),
            VariableType::Sketch => "Sketch".into(),
            VariableType::Surface => "Surface".into(),
            VariableType::Solid => "Solid".into(),
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
            VariableType::parse("Number"),
            Ok(("", VariableType::Number))
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
    }
}
