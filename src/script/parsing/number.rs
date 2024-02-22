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
    combinator::{map, success, verify},
    sequence::{delimited, tuple},
};

use crate::script::parsing::{parse_integer, space0};

use super::{Span, VResult};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Number<S: Span> {
    pub integer: Option<S>,
    pub dot: Option<S>,
    pub fractional: Option<S>,
}

impl<S: Span> Number<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        fn take_dot<S: Span>(input: S) -> VResult<S, S> {
            verify(take_while1(|c| c == '.'), |output: &S| {
                output.as_str().len() == 1
            })(input)
        }

        let (input, (integer, dot, fractional)) = alt((
            tuple((
                map(parse_integer, Some),
                map(delimited(space0, take_dot, space0), Some),
                map(parse_integer, Some),
            )),
            tuple((
                success(None),
                map(delimited(space0, take_dot, space0), Some),
                map(parse_integer, Some),
            )),
            tuple((
                map(parse_integer, Some),
                map(delimited(space0, take_dot, space0), Some),
                success(None),
            )),
            tuple((map(parse_integer, Some), success(None), success(None))),
        ))(input)?;

        Ok((
            input,
            Self {
                integer,
                dot,
                fractional,
            },
        ))
    }

    pub fn get_span(&self) -> &S {
        // We accept '0.0', '.0', '0.', and '.', but not ''. It should not be possible for this to fail.
        self.integer.as_ref().unwrap_or_else(|| {
            self.dot
                .as_ref()
                .unwrap_or_else(|| self.fractional.as_ref().unwrap())
        })
    }

    pub fn to_float<P>(&self) -> Result<P, P::Err>
    where
        P: std::str::FromStr,
    {
        match (self.integer.as_ref(), self.fractional.as_ref()) {
            (None, None) => P::from_str("0.0"),
            (Some(integer), None) => P::from_str(&integer.to_string()),
            (None, Some(fractional)) => P::from_str(&format!(".{}", fractional.to_string())),
            (Some(integer), Some(fractional)) => P::from_str(&format!(
                "{}.{}",
                integer.to_string(),
                fractional.to_string()
            )),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn number() {
        assert_eq!(
            Number::parse("1234"),
            Ok((
                "",
                Number {
                    integer: Some("1234"),
                    dot: None,
                    fractional: None
                }
            ))
        );
        assert_eq!(
            Number::parse(".1234"),
            Ok((
                "",
                Number {
                    integer: None,
                    dot: Some("."),
                    fractional: Some("1234")
                }
            ))
        );
        assert_eq!(
            Number::parse("1234."),
            Ok((
                "",
                Number {
                    integer: Some("1234"),
                    dot: Some("."),
                    fractional: None
                }
            ))
        );
        assert_eq!(
            Number::parse("1234.5678"),
            Ok((
                "",
                Number {
                    integer: Some("1234"),
                    dot: Some("."),
                    fractional: Some("5678")
                }
            ))
        );
        assert_eq!(
            Number::parse("1234;"),
            Ok((
                ";",
                Number {
                    integer: Some("1234"),
                    dot: None,
                    fractional: None
                }
            ))
        );
        assert!(Number::parse("").is_err());
        assert!(Number::parse(".").is_err());
    }
}
