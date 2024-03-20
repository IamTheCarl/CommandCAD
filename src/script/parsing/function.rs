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

use super::{space1, take_keyword, FunctionSignature, NamedBlock, Span, VResult};

#[derive(Debug, Eq, PartialEq)]
pub struct Function<S: Span> {
    pub starting_span: S,
    pub named_block: NamedBlock<S>,
    pub signature: Rc<FunctionSignature<S>>,
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
                signature: Rc::new(FunctionSignature::Function {
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

    pub fn get_span(&self) -> &S {
        &self.starting_span
    }
}

#[cfg(test)]
mod test {
    use crate::script::parsing::{blocks::CallableBlock, Block, VariableType};

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
                        callable: CallableBlock {
                            parameter_span: "(",
                            parameters: vec![],
                            block: Block { statements: vec![] }
                        }
                    },
                    signature: Rc::new(FunctionSignature::Function {
                        return_type: Box::new(VariableType::Scalar("Length")),
                        arguments: vec![],
                    })
                }
            ))
        );
    }
}
