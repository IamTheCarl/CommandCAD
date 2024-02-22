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

use super::{space1, take_keyword, NamedBlock, Span, VResult, VariableType};

#[derive(Debug, Eq, PartialEq)]
pub struct Function<S: Span> {
    pub starting_span: S,
    pub named_block: NamedBlock<S>,
    pub return_type: VariableType<S>,
}

impl<S: Span> Function<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        map(
            pair(
                terminated(take_keyword("function"), space1),
                cut(NamedBlock::parse_with_return_type),
            ),
            |(starting_span, (named_block, return_type))| Self {
                starting_span,
                named_block,
                return_type,
            },
        )(input)
    }
}

#[cfg(test)]
mod test {
    use crate::script::parsing::Block;

    use super::*;

    #[test]
    fn function() {
        assert!(Function::parse("function my_function() {}").is_err());
        assert_eq!(
            Function::parse("function my_function() -> Length {}"),
            Ok((
                "",
                Function {
                    starting_span: "function",
                    named_block: NamedBlock {
                        name: "my_function",
                        parameter_span: "(",
                        parameters: vec![],
                        block: Block { statements: vec![] }
                    },
                    return_type: VariableType::Measurement("Length")
                }
            ))
        );
    }
}
