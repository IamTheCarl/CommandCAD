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
    bytes::complete::{escaped, take_till, take_while1},
    character::complete::{char as nom_char, one_of},
    sequence::delimited,
};

use super::{Span, VResult};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct PString<S: Span> {
    pub value: S,
}

impl<S: Span> PString<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        let (input, value) = delimited(
            nom_char('"'),
            alt((
                escaped(
                    take_while1(|c| !matches!(c, '\\' | '"')),
                    '\\',
                    one_of(r#""n\"#),
                ),
                take_till(|c| c == '"'),
            )),
            nom_char('"'),
        )(input)?;

        Ok((input, PString { value }))
    }

    pub fn get_span(&self) -> &S {
        &self.value
    }
}

impl<S: Span> ToString for PString<S> {
    fn to_string(&self) -> String {
        let mut char_iterator = self.value.chars().peekable();
        let iterator = std::iter::from_fn(|| {
            char_iterator
                .next()
                .map(|next| (next, char_iterator.peek().copied()))
        });
        let mut string = String::default();

        for (current, next) in iterator {
            if current == '\\' {
                match next {
                    Some('"') => string.push('"'),
                    Some('\n') => string.push('\n'),
                    Some('\\') => string.push('\\'),
                    _ => {} // Shouldn't be possible to get an unrecognized char. This could happen for a downslash at the end of the string though.
                }
            } else {
                string.push(current);
            }
        }

        string
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_string() {
        assert_eq!(
            PString::parse(r#""test""#),
            Ok(("", PString { value: "test" }))
        );
        assert_eq!(
            PString::parse(r#"" \n \\ \" ""#),
            Ok((
                "",
                PString {
                    value: r#" \n \\ \" "#,
                }
            ))
        );
        assert_eq!(PString::parse("\"\""), Ok(("", PString { value: "" })));
    }
}
