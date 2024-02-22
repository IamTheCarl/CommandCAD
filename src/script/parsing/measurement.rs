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
    bytes::complete::take_while1,
    character::complete::char as nom_char,
    combinator::{consumed, map, verify},
    multi::fold_many0,
    sequence::{delimited, separated_pair},
};

use super::{space0, Number, Span, VResult};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Measurement<S: Span> {
    pub number: Number<S>,
    pub ty: S,
}

impl<S: Span> Measurement<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        // TODO the unit type parsing is terribly under-tested.
        map(
            separated_pair(Number::parse, space0, Self::parse_type),
            |(number, ty)| Measurement { number, ty },
        )(input)
    }

    fn parse_type(input: S) -> VResult<S, S> {
        fn take_body<S: Span>(input: S) -> VResult<S, S> {
            take_while1(|c: char| {
                c.is_alphanumeric() || matches!(c, '^' | '/' | '_' | '.' | '%' | '<' | '-' | '\\')
            })(input)
        }

        verify(
            map(
                consumed(fold_many0(
                    alt((
                        // This can't handle nested () but none of our supported unit types need that.
                        take_body,
                        delimited(nom_char('('), take_body, nom_char(')')),
                    )),
                    || (),
                    |_, _| (),
                )),
                |(span, _)| span,
            ),
            |input: &S| {
                // The unit type must start with a letter or number.
                input
                    .as_str()
                    .chars()
                    .next()
                    .map(|input| input.is_alphanumeric())
                    .unwrap_or(false)
            },
        )(input)
    }

    pub fn get_span(&self) -> &S {
        self.number.get_span()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn measurement() {
        assert_eq!(
            Measurement::parse("22 m"),
            Ok((
                "",
                Measurement {
                    number: Number {
                        integer: Some("22"),
                        dot: None,
                        fractional: None
                    },
                    ty: "m"
                }
            ))
        );

        assert_eq!(
            Measurement::parse("22m"),
            Ok((
                "",
                Measurement {
                    number: Number {
                        integer: Some("22"),
                        dot: None,
                        fractional: None
                    },
                    ty: "m"
                }
            ))
        );

        assert_eq!(
            Measurement::parse("22.44m"),
            Ok((
                "",
                Measurement {
                    number: Number {
                        integer: Some("22"),
                        dot: Some("."),
                        fractional: Some("44"),
                    },
                    ty: "m"
                }
            ))
        );
    }
}
