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

use std::rc::Rc;

use nom::{
    combinator::{cut, map},
    sequence::{pair, terminated},
};

use crate::script::{
    parsing::{space1, take_keyword, FunctionSignature, NamedBlock, VResult},
    Span,
};

#[derive(Debug, Eq, PartialEq)]
pub struct Task<S: Span> {
    pub starting_span: S,
    pub named_block: NamedBlock<S>,
    pub signature: Rc<FunctionSignature<S>>,
}

impl<S: Span> Task<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        map(
            pair(
                terminated(take_keyword("task"), space1),
                cut(NamedBlock::parse_with_return_type),
            ),
            |(starting_span, (named_block, return_type))| Self {
                starting_span,
                signature: Rc::new(FunctionSignature::Task {
                    return_type: Box::new(return_type),
                    arguments: named_block
                        .callable
                        .parameters
                        .iter()
                        .map(|p| p.ty.clone())
                        .collect(),
                }),
                named_block,
            },
        )(input)
    }
}

#[cfg(test)]
mod test {
    use crate::script::parsing::{blocks::CallableBlock, Block, VariableType};

    use super::*;

    #[test]
    fn task() {
        assert!(Task::parse("task my_task() {}").is_err());
        assert_eq!(
            Task::parse("task my_task() -> Length {}"),
            Ok((
                "",
                Task {
                    starting_span: "task",
                    named_block: NamedBlock {
                        name: "my_task",
                        callable: CallableBlock {
                            parameter_span: "(",
                            parameters: vec![],
                            block: Block { statements: vec![] }
                        }
                    },
                    signature: Rc::new(FunctionSignature::Task {
                        return_type: Box::new(VariableType::Scalar("Length")),
                        arguments: vec![],
                    })
                }
            ))
        );
    }
}
