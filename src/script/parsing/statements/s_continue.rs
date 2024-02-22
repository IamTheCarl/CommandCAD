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
    character::complete::char as nom_char,
    combinator::{map, opt},
    sequence::{pair, preceded},
};

use crate::script::{
    parsing::{parse_name, space0, take_keyword, VResult},
    Span,
};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Continue<S: Span> {
    pub starting_span: S,
    pub loop_name: Option<S>,
}

impl<S: Span> Continue<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        map(
            pair(
                take_keyword("continue"),
                opt(preceded(pair(space0, nom_char('\'')), parse_name)),
            ),
            |(starting_span, loop_name)| Continue {
                starting_span,
                loop_name,
            },
        )(input)
    }

    pub fn get_span(&self) -> &S {
        &self.starting_span
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn statement_continue() {
        assert_eq!(
            Continue::parse("continue"),
            Ok((
                "",
                Continue {
                    starting_span: "continue",

                    loop_name: None
                }
            ))
        );
        assert_eq!(
            Continue::parse("continue 'my_loop"),
            Ok((
                "",
                Continue {
                    starting_span: "continue",
                    loop_name: Some("my_loop")
                }
            ))
        );
    }
}
