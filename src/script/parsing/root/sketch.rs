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
    combinator::{cut, map},
    sequence::{pair, terminated},
};

use crate::script::{
    parsing::{space1, take_keyword, NamedBlock, VResult},
    Span,
};

#[derive(Debug, Eq, PartialEq)]
pub struct Sketch<S: Span> {
    pub starting_span: S,
    pub named_block: NamedBlock<S>,
}

impl<S: Span> Sketch<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        map(
            pair(
                terminated(take_keyword("sketch"), space1),
                cut(NamedBlock::parse),
            ),
            |(starting_span, named_block)| Self {
                starting_span,
                named_block,
            },
        )(input)
    }
}

#[cfg(test)]
mod test {
    use crate::script::parsing::Block;

    use super::*;

    #[test]
    fn sketch() {
        assert!(Sketch::parse("sketch my_sketch() -> Length {}").is_err());
        assert_eq!(
            Sketch::parse("sketch my_sketch() {}"),
            Ok((
                "",
                Sketch {
                    starting_span: "sketch",
                    named_block: NamedBlock {
                        name: "my_sketch",
                        parameter_span: "(",
                        parameters: vec![],
                        block: Block { statements: vec![] }
                    },
                }
            ))
        );
    }
}
