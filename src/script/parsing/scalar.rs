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
    bytes::complete::{tag, take_while, take_while1},
    character::complete::char as nom_char,
    combinator::{consumed, map, verify},
    multi::fold_many0,
    sequence::{delimited, separated_pair},
};

use super::{space0, Number, Span, VResult};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Scalar<S: Span> {
    pub number: Number<S>,
    pub ty: S,
}

impl<S: Span> Scalar<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        // TODO the unit type parsing is terribly under-tested.
        map(
            separated_pair(Number::parse, space0, Self::parse_type),
            |(number, ty)| Scalar { number, ty },
        )(input)
    }

    fn parse_type(input: S) -> VResult<S, S> {
        fn parse_unit<S: Span>(input: S) -> VResult<S, S> {
            verify(
                map(
                    consumed(fold_many0(
                        alt((
                            take_while1(|c: char| {
                                c.is_alphanumeric() || matches!(c, '^' | '/' | '%' | '-' | '\\')
                            }),
                            tag("<-"),
                        )),
                        || (),
                        |_, _| (),
                    )),
                    |(span, _)| span,
                ),
                |input: &S| {
                    let input = input.as_str();
                    // If the unit type exists, it must start with a letter or number.
                    input.is_empty()
                        || input
                            .chars()
                            .next()
                            .map(|input| input.is_alphanumeric())
                            .unwrap_or(false)
                },
            )(input)
        }

        fn parse_unit_quoted<S: Span>(input: S) -> VResult<S, S> {
            delimited(nom_char('<'), take_while(|c| c != '>'), nom_char('>'))(input)
        }

        fn empty_unit<S: Span>(input: S) -> VResult<S, S> {
            let empty = input.slice(..0);
            Ok((input, empty))
        }

        alt((parse_unit_quoted, parse_unit, empty_unit))(input)
    }

    pub fn get_span(&self) -> &S {
        self.number.get_span()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_type() {
        assert_eq!(Scalar::parse_type("<cm^3(STP)>"), Ok(("", "cm^3(STP)")));
        assert_eq!(Scalar::parse_type("mm"), Ok(("", "mm")));
        assert_eq!(Scalar::parse_type(""), Ok(("", "")));

        assert_eq!(Scalar::parse_type("mm <"), Ok((" <", "mm")));
        assert_eq!(Scalar::parse_type("mm.floor()"), Ok((".floor()", "mm")));
        assert_eq!(Scalar::parse_type(".floor()"), Ok((".floor()", "")));
    }

    #[test]
    fn measurement() {
        assert_eq!(
            Scalar::parse("22 m"),
            Ok((
                "",
                Scalar {
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
            Scalar::parse("22m"),
            Ok((
                "",
                Scalar {
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
            Scalar::parse("22.44m"),
            Ok((
                "",
                Scalar {
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

    #[test]
    fn number() {
        assert_eq!(
            Scalar::parse("22"),
            Ok((
                "",
                Scalar {
                    number: Number {
                        integer: Some("22"),
                        dot: None,
                        fractional: None
                    },
                    ty: ""
                }
            ))
        );

        assert_eq!(
            Scalar::parse("22.44"),
            Ok((
                "",
                Scalar {
                    number: Number {
                        integer: Some("22"),
                        dot: Some("."),
                        fractional: Some("44"),
                    },
                    ty: ""
                }
            ))
        );

        assert_eq!(
            Scalar::parse("22.44 <"),
            Ok((
                "<",
                Scalar {
                    number: Number {
                        integer: Some("22"),
                        dot: Some("."),
                        fractional: Some("44"),
                    },
                    ty: ""
                }
            ))
        );

        assert_eq!(
            Scalar::parse("22.44.floor()"),
            Ok((
                ".floor()",
                Scalar {
                    number: Number {
                        integer: Some("22"),
                        dot: Some("."),
                        fractional: Some("44"),
                    },
                    ty: ""
                }
            ))
        );
    }
}
