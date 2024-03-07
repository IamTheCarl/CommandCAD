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

use std::fmt::Display;

use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while, take_while1},
    character::complete::char as nom_char,
    combinator::{recognize, rest, value, verify},
    error::context,
    multi::{fold_many0, fold_many1},
    sequence::{delimited, preceded},
    CompareResult, IResult,
};

// TODO currently all comments are just discarded.
// I would like to start tracking them for auto-formatting and generation
// of documentation.

pub type VResult<I, O> = IResult<I, O, nom::error::VerboseError<I>>;

mod span;
pub use span::Span;

mod root;
pub use root::{FileAST, Import, RootElement, Sketch, Solid, Task};

mod pstruct;
pub use pstruct::StructDefinition;

mod function;
pub use function::Function;

mod member_variable;
pub use member_variable::{MemberVariable, MemberVariableType};

mod blocks;
pub use blocks::{Block, BlockStatement, CallableBlock, NamedBlock};

mod statements;
pub use statements::{
    ArithmeticExpression, Assign, Assignable, AssignableVariable, Break, Comparison, Continue,
    Else, Expression, Factor, For, If, Loop, Match, Return, Statement, Term, Trailer, While,
};

mod variable_type;
pub use variable_type::{FunctionSignature, VariableType};

mod range;
pub use range::Range;

mod struct_initalization;
pub use struct_initalization::StructInitialization;

mod litteral;
pub use litteral::Litteral;

mod list;
pub use list::List;

mod string;
pub use string::PString;

mod number;
pub use number::Number;

mod measurement;
pub use measurement::Measurement;

mod closure;
pub use closure::{CapturedVariable, Closure};

fn is_digit(c: char) -> bool {
    "0123456789".contains(c)
}

fn parse_integer<S: Span>(input: S) -> VResult<S, S> {
    take_while1(is_digit)(input)
}

fn parse_name<S: Span>(input: S) -> VResult<S, S> {
    fn is_alpha(c: char) -> bool {
        c.is_alphabetic() || matches!(c, '_')
    }

    fn is_alphanumeric(c: char) -> bool {
        is_alpha(c) || c.is_numeric()
    }

    context(
        "Failed to parse name",
        recognize(preceded(take_while1(is_alpha), take_while(is_alphanumeric))),
    )(input)
}

fn take_keyword<S: Span>(keyword: &'static str) -> impl FnMut(S) -> VResult<S, S> {
    verify(parse_name::<S>, move |name| {
        matches!(name.compare(keyword), CompareResult::Ok) && name.input_len() == keyword.len()
    })
}

fn is_space(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\r' | '\n')
}

fn consume_space_token<S: Span>(input: S) -> VResult<S, ()> {
    value(
        (),
        alt((
            take_while1(is_space),
            delimited(tag("//"), take_while(|c| c != '\n'), nom_char('\n')),
            delimited(tag("/*"), take_until("*/"), tag("*/")),
            preceded(tag("//"), rest),
        )),
    )(input)
}

fn space0<S: Span>(input: S) -> VResult<S, ()> {
    value((), fold_many0(consume_space_token, || (), |_, _| {}))(input)
}

fn space1<S: Span>(input: S) -> VResult<S, ()> {
    value((), fold_many1(consume_space_token, || (), |_, _| {}))(input)
}

/// Presents iterators in a comma separated format.
struct IteratorFormatter<I, D>(pub I)
where
    I: Iterator<Item = D> + Clone,
    D: Display;

impl<I, D> Display for IteratorFormatter<I, D>
where
    I: Iterator<Item = D> + Clone,
    D: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.clone().peekable();

        loop {
            match (iter.next(), iter.peek().is_some()) {
                (Some(next), true) => write!(f, "{}, ", next)?,
                (Some(next), false) => write!(f, "{}", next)?,
                (None, _) => break Ok(()),
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn integer() {
        assert_eq!(parse_integer("1234"), Ok(("", "1234")));
        assert_eq!(parse_integer("1234.5678"), Ok((".5678", "1234")));
    }

    #[test]
    fn name() {
        assert_eq!(parse_name("name"), Ok(("", "name")));
        assert_eq!(parse_name("name1"), Ok(("", "name1")));
        assert_eq!(parse_name("name_1"), Ok(("", "name_1")));
        assert_eq!(parse_name("_name"), Ok(("", "_name")));
        assert!(parse_name("1name").is_err());
    }

    #[test]
    fn space_and_comments() {
        assert_eq!(space0(" \t\r\n"), Ok(("", ())));
        assert_eq!(
            space0(" // A line comment. \n after_the_line"),
            Ok(("after_the_line", ()))
        );
        assert_eq!(
            space0(" /* a block comment. */ after_the_comment"),
            Ok(("after_the_comment", ()))
        );

        assert_eq!(
            space0(" /* a block comment. */ /* another comment */ after_the_comment"),
            Ok(("after_the_comment", ()))
        );
        assert_eq!(space0(""), Ok(("", ())));

        assert_eq!(space1(" \t\r\n"), Ok(("", ())));
        assert_eq!(
            space1(" // A line comment. \n after_the_line"),
            Ok(("after_the_line", ()))
        );
        assert_eq!(space1(" // A line comment with no newline"), Ok(("", ())));
        assert_eq!(
            space1(" /* a block comment. */ after_the_comment"),
            Ok(("after_the_comment", ()))
        );

        assert_eq!(
            space1(" /* a block comment. */ /* another comment */ after_the_comment"),
            Ok(("after_the_comment", ()))
        );
        assert!(space1("").is_err());
    }
}
