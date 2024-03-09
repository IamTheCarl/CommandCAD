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

use std::ops::{RangeFrom, RangeTo};

use nom_locate::LocatedSpan;

pub trait Span:
    nom::InputIter<Item = char>
    + for<'b> nom::Compare<&'b str>
    + nom::InputLength
    + nom::InputTake
    + nom::InputTakeAtPosition<Item = char>
    + nom::Slice<RangeFrom<usize>>
    + nom::Slice<RangeTo<usize>>
    + nom::Offset
    + for<'b> nom::FindSubstring<&'b str>
    + Clone
    + std::fmt::Debug
    + Eq
    + PartialEq
    + std::hash::Hash
    + ToString
    + AsStr
    + FromStr
    + FormatSpan
{
    fn chars(&self) -> impl Iterator<Item = char>;
}

pub trait AsStr {
    fn as_str(&self) -> &str;
}

pub trait FromStr {
    fn from_str(string: &'static str) -> Self;
}

impl<'a> AsStr for &'a str {
    fn as_str(&self) -> &str {
        self
    }
}

impl<'a> FromStr for &'a str {
    fn from_str(string: &'static str) -> Self {
        string
    }
}

impl<'a> AsStr for LocatedSpan<&'a str> {
    fn as_str(&self) -> &str {
        self.fragment()
    }
}

impl<'a> FromStr for LocatedSpan<&'a str> {
    fn from_str(string: &'static str) -> Self {
        LocatedSpan::new(string)
    }
}

impl AsStr for imstr::ImString {
    fn as_str(&self) -> &str {
        self.as_str()
    }
}

impl FromStr for imstr::ImString {
    fn from_str(string: &'static str) -> Self {
        imstr::ImString::from(string)
    }
}

impl AsStr for LocatedSpan<imstr::ImString> {
    fn as_str(&self) -> &str {
        self.fragment().as_str()
    }
}

impl FromStr for LocatedSpan<imstr::ImString> {
    fn from_str(string: &'static str) -> Self {
        LocatedSpan::new(imstr::ImString::from(string))
    }
}

impl<'a> Span for &'a str {
    fn chars(&self) -> impl Iterator<Item = char> {
        str::chars(self)
    }
}
impl<'a> Span for LocatedSpan<&'a str> {
    fn chars(&self) -> impl Iterator<Item = char> {
        self.fragment().chars()
    }
}
impl Span for imstr::ImString {
    fn chars(&self) -> impl Iterator<Item = char> {
        imstr::ImString::chars(self)
    }
}
impl Span for LocatedSpan<imstr::ImString> {
    fn chars(&self) -> impl Iterator<Item = char> {
        self.fragment().chars()
    }
}

pub trait FormatSpan {
    fn format_span(&self) -> String;
}

impl<'a> FormatSpan for &'a str {
    fn format_span(&self) -> String {
        format!("`{}`", self)
    }
}
impl<'a> FormatSpan for LocatedSpan<&'a str> {
    fn format_span(&self) -> String {
        format!("[{}:{}]", self.location_line(), self.get_column())
    }
}
impl FormatSpan for imstr::ImString {
    fn format_span(&self) -> String {
        format!("`{}`", self)
    }
}
impl FormatSpan for LocatedSpan<imstr::ImString> {
    fn format_span(&self) -> String {
        format!("[{}:{}]", self.location_line(), self.get_column())
    }
}

#[cfg(test)]
mod test {
    use crate::script::parsing::PString;

    use super::*;

    #[test]
    fn span_types() {
        assert_eq!(
            PString::parse(r#""test""#),
            Ok(("", PString { value: "test" }))
        );

        assert_eq!(
            PString::parse(LocatedSpan::new(r#""test""#))
                .unwrap()
                .1
                .value
                .fragment(),
            &"test"
        );
    }
}
