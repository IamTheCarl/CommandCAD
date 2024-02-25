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
    bytes::complete::tag,
    character::complete::char as nom_char,
    combinator::{map, opt, success},
    error::context,
    multi::separated_list0,
    sequence::{delimited, pair, preceded, separated_pair, terminated},
};

use super::{parse_name, space0, Expression, Span, Trailer, VResult};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StructInitialization<S: Span> {
    pub starting_span: S,
    pub assignments: Vec<(S, Expression<S>)>,
    pub inheritance: Option<Box<Trailer<S>>>,
}

impl<S: Span> StructInitialization<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        map(
            terminated(
                pair(
                    terminated(context("Missing opening bracket", tag("{")), space0),
                    alt((
                        pair(
                            success(vec![]),
                            map(
                                preceded(
                                    terminated(tag(".."), space0),
                                    map(Trailer::parse, Box::new),
                                ),
                                Some,
                            ),
                        ),
                        pair(
                            separated_list0(
                                nom_char(','),
                                delimited(
                                    space0,
                                    separated_pair(
                                        parse_name,
                                        delimited(space0, nom_char('='), space0),
                                        Expression::parse,
                                    ),
                                    space0,
                                ),
                            ),
                            opt(preceded(
                                pair(
                                    pair(space0, nom_char(',')),
                                    delimited(space0, tag(".."), space0),
                                ),
                                map(Trailer::parse, Box::new),
                            )),
                        ),
                    )),
                ),
                pair(space0, context("Missing closing bracket", nom_char('}'))),
            ),
            |(starting_span, (assignments, inheritance))| Self {
                starting_span,
                assignments,
                inheritance,
            },
        )(input)
    }

    pub fn get_span(&self) -> &S {
        &self.starting_span
    }
}

// The functionality of struct initalization is done in the tests for trailers.
