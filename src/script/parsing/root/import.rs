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
    bytes::complete::tag,
    character::complete::char as nom_char,
    combinator::{map, opt},
    multi::separated_list0,
    sequence::{delimited, pair, preceded},
};

use crate::script::{
    parsing::{parse_name, space0, take_keyword, VResult},
    Span,
};

#[derive(Debug, Eq, PartialEq)]
pub struct Import<S: Span> {
    pub path: Vec<S>,
    pub external: bool,
}

impl<S: Span> Import<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        map(
            preceded(
                take_keyword("import"),
                delimited(
                    space0,
                    pair(
                        opt(take_keyword("extern")),
                        separated_list0(tag("::"), delimited(space0, parse_name, space0)),
                    ),
                    pair(space0, nom_char(';')),
                ),
            ),
            |(external, path)| Import {
                path,
                external: external.is_some(),
            },
        )(input)
    }
}
