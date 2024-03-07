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

use std::{borrow::Cow, num::ParseFloatError};

use nom_locate::LocatedSpan;
use ordered_float::ParseNotNanError;

use crate::script::Span;

// TODO We need stack traces.
// TODO we should separate out the failure span and failure type.
#[derive(Debug, Eq, PartialEq)]
pub enum Failure<S> {
    UnclosedStatement(S),
    ReservedKeyword(S, &'static str),
    DuplicateGlobal(S, S),
    ExpectedGot(S, Cow<'static, str>, Cow<'static, str>),
    NumberConversion(S, ParseNotNanError<ParseFloatError>),
    VariableNotInScope(S, Cow<'static, str>),
    StructMissingAssignment(S, S),
    StructExcessAssignment(S),
    StructWrongInheritanceType(S, S, S),
    NoDefault(S, S),
    UnsupportedOperation(S, Cow<'static, str>, &'static str),
    UnknownAttribute(S),
    ResultIsNan(S),
    MissingArguments(S),
    IndexOutOfRange(S, isize),
    ListIsEmpty(S),
    UnknownUnitType(S, Cow<'static, str>),
    CannotConvertFromTo(S, Cow<'static, str>, Cow<'static, str>),
    InvalidCharIndex(S, isize),
    Formatting(S, std::fmt::Error),
    FormatArgumentIndexOutOfRange(S, isize),
    ParseFormatter(S),
    InvalidPrecision(S, isize),
    MissingUpperBound(S),
    ListLengthsDontMatch(S),
    DidNotMatch(S),
    BreakOutsideOfLoop(S),
    BreakLabelNotFound(S, S),
    ContinueOutsideOfLoop(S),
    ContinueLabelNotFound(S, S),
    BadArgumentTypes(S, Vec<Failure<S>>),
    StructConstruction(S, Vec<Failure<S>>),
    SliceOutOfRange(S, Option<isize>, &'static str, Option<isize>),
    TooManyArguments(S),
    ListWrongLength(S, usize, usize),
    ListElementFailure(S, usize, Box<Failure<S>>),
    ListContainsDuplicate(S, usize),
    ShellNotInSolid(S),
    FaceNotInShell(S),
    RegionNotInFace(S),
}

impl<S: Span + FormatSpan> std::fmt::Display for Failure<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnclosedStatement(span) => {
                write!(
                    f,
                    "{}: Non-tail expressions must be closed with a semicolon",
                    span.format_span()
                )
            }
            Self::ReservedKeyword(span, keyword) => {
                write!(
                    f,
                    "{}: Keyword `{}` is reserved",
                    span.format_span(),
                    keyword
                )
            }
            Self::DuplicateGlobal(first_instance, second_instance) => {
                write!(
                    f,
                    "Duplicate global {}:{}",
                    first_instance.format_span(),
                    second_instance.format_span()
                )
            }
            Self::ExpectedGot(span, expected, got) => {
                write!(
                    f,
                    "{}: Expected `{}`, got `{}`",
                    span.format_span(),
                    expected,
                    got
                )
            }
            Self::NumberConversion(span, error) => {
                write!(
                    f,
                    "{}: Failed to convert to number: {:?}",
                    span.format_span(),
                    error
                )
            }
            Self::VariableNotInScope(span, name) => {
                write!(
                    f,
                    "{}: Variable `{}` was not found in current scope",
                    span.format_span(),
                    name
                )
            }
            Self::StructMissingAssignment(span, name) => {
                write!(
                    f,
                    "{}: Missing assignment for struct: {}",
                    span.format_span(),
                    name.as_str()
                )
            }
            Self::StructExcessAssignment(assignment) => {
                write!(
                    f,
                    "{}: Excess value assigned to struct",
                    assignment.format_span()
                )
            }
            Self::StructWrongInheritanceType(span, expected_type, given_type) => {
                write!(
                    f,
                    "{}: Wrong type given for in inheritance. Expected `{}`, got `{}`",
                    span.format_span(),
                    expected_type.as_str(),
                    given_type.as_str()
                )
            }
            Self::NoDefault(span, name) => {
                write!(
                    f,
                    "{}: Default value not available for memeber `{}`",
                    span.format_span(),
                    name.as_str()
                )
            }
            Self::UnsupportedOperation(span, name, operation) => {
                write!(
                    f,
                    "{}: Type `{}` does not support {} operation",
                    span.format_span(),
                    name,
                    operation
                )
            }
            Self::UnknownAttribute(attribute) => write!(
                f,
                "{}: Unknown attribute: {}",
                attribute.format_span(),
                attribute.as_str()
            ),
            Self::ResultIsNan(span) => write!(
                f,
                "{}: Result of arithmetic operation is NaN",
                span.format_span()
            ),
            Self::MissingArguments(span) => write!(
                f,
                "{}: Not enough arguments for function",
                span.format_span()
            ),
            Self::IndexOutOfRange(span, index) => write!(
                f,
                "{}: Index `{}` is out of range",
                span.format_span(),
                index
            ),
            Self::ListIsEmpty(span) => write!(f, "{}: List is empty", span.format_span()),
            Self::UnknownUnitType(span, unit_type) => write!(
                f,
                "{}: Unknown unit type `{}`",
                span.format_span(),
                unit_type
            ),
            Self::CannotConvertFromTo(span, from, to) => {
                write!(
                    f,
                    "{}: Cannot convert from `{}` to `{}`",
                    span.format_span(),
                    from,
                    to
                )
            }
            Self::InvalidCharIndex(span, index) => {
                write!(
                    f,
                    "{}: Index `{}` does not lie on a character boundary",
                    span.format_span(),
                    index
                )
            }
            Self::Formatting(span, error) => {
                write!(
                    f,
                    "{}: Error formatting text: {:?}",
                    span.format_span(),
                    error
                )
            }
            Self::FormatArgumentIndexOutOfRange(span, index) => {
                write!(
                    f,
                    "{}: Format requests argument index `{}` which is not present",
                    span.format_span(),
                    *index,
                )
            }
            Self::ParseFormatter(span) => {
                write!(
                    f,
                    "{}: Failed to parse string as format",
                    span.format_span()
                )
            }
            Self::InvalidPrecision(span, precision) => {
                write!(
                    f,
                    "{}: Invalid precision `{}`. Precision must be greater than zero",
                    span.format_span(),
                    *precision
                )
            }
            Self::MissingUpperBound(span) => {
                write!(
                    f,
                    "{}: Upper bound of range be specified when inclusive",
                    span.format_span()
                )
            }
            Self::ListLengthsDontMatch(span) => {
                write!(
		    f,
		    "{}: Attempt to assign list from list of a different length. You can use range access `[..]` to trim an oversized list down to the expected size",
		    span.format_span()
		)
            }
            Self::DidNotMatch(span) => {
                write!(
                    f,
                    "{}: No branches in match statement matched input",
                    span.format_span()
                )
            }
            Self::BreakOutsideOfLoop(span) => {
                write!(f, "{}: Break outside of loop", span.format_span())
            }
            Self::BreakLabelNotFound(span, label) => {
                write!(
                    f,
                    "{}: Could not find loop with label `{}`",
                    span.format_span(),
                    label.as_str()
                )
            }
            Self::ContinueOutsideOfLoop(span) => {
                write!(f, "{}: Continue outside of loop", span.format_span())
            }
            Self::ContinueLabelNotFound(span, label) => {
                write!(
                    f,
                    "{}: Could not find loop with label `{}`",
                    span.format_span(),
                    label.as_str()
                )
            }
            Self::BadArgumentTypes(span, failures) => {
                writeln!(f, "{}: Bad arguments to function call:", span.format_span())?;

                for failure in failures.iter() {
                    writeln!(f, "\t{}", failure)?;
                }

                Ok(())
            }
            Self::StructConstruction(span, failures) => {
                writeln!(f, "{}: Faild to build struct:", span.format_span())?;

                for failure in failures.iter() {
                    writeln!(f, "\t{}", failure)?;
                }

                Ok(())
            }
            Self::SliceOutOfRange(span, lower, ty, upper) => match (lower, upper) {
                (None, None) => write!(f, "{}: Slice out of range [{}]", span.format_span(), ty,),
                (Some(lower), None) => write!(
                    f,
                    "{}: Slice out of range [{}{}]",
                    span.format_span(),
                    lower,
                    ty,
                ),
                (Some(lower), Some(upper)) => write!(
                    f,
                    "{}: Slice out of range [{}{}{}]",
                    span.format_span(),
                    lower,
                    ty,
                    upper
                ),
                (None, Some(upper)) => write!(
                    f,
                    "{}: Slice out of range [{}{}]",
                    span.format_span(),
                    ty,
                    upper
                ),
            },
            Self::TooManyArguments(span) => {
                write!(
                    f,
                    "{}: Too many arguemnts for function call",
                    span.format_span()
                )
            }
            Self::ListWrongLength(span, expected_length, actual_length) => write!(
                f,
                "{}: Expected list of length {}; got a length of {}",
                span.format_span(),
                expected_length,
                actual_length
            ),
            Self::ListElementFailure(span, index, failure) => {
                write!(
                    f,
                    "{}: Error with element {} of list: {}",
                    span.format_span(),
                    index,
                    failure
                )
            }
            Self::ListContainsDuplicate(span, index) => {
                write!(
                    f,
                    "{}: Element {} is a duplicate",
                    span.format_span(),
                    index
                )
            }
            Self::ShellNotInSolid(span) => {
                write!(f, "{}: Could not find shell in solid", span.format_span())
            }
            Self::FaceNotInShell(span) => {
                write!(f, "{}: Could not find face in shell", span.format_span())
            }
            Self::RegionNotInFace(span) => {
                write!(f, "{}: Could not find region in face", span.format_span())
            }
        }
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
