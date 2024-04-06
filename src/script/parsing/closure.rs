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
    character::complete::{char as nom_char, space0},
    combinator::map,
    multi::separated_list0,
    sequence::{delimited, pair, preceded, terminated},
};

use super::{parse_name, CallableBlock, FunctionSignature, Span, VResult};

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum CapturedVariable<S: Span> {
    Copy(S),
    Reference(S),
}

impl<S: Span> CapturedVariable<S> {
    fn parse(input: S) -> VResult<S, Self> {
        alt((
            map(
                preceded(pair(nom_char('&'), space0), parse_name),
                Self::Reference,
            ),
            map(parse_name, Self::Copy),
        ))(input)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Closure<S: Span> {
    pub starting_span: S,
    pub captured_variables: Vec<CapturedVariable<S>>,
    pub callable: CallableBlock<S>,
    pub signature: FunctionSignature<S>,
}

impl<S: Span> Closure<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        map(
            pair(
                pair(
                    terminated(tag("["), space0),
                    terminated(
                        separated_list0(
                            delimited(space0, nom_char(','), space0),
                            CapturedVariable::parse,
                        ),
                        pair(space0, tag("]")),
                    ),
                ),
                CallableBlock::parse_with_return_type,
            ),
            |((starting_span, captured_variables), (callable, return_type))| Self {
                signature: FunctionSignature::Function {
                    return_type: Box::new(return_type),
                    arguments: callable.parameters.iter().map(|p| p.ty.clone()).collect(),
                },
                starting_span,
                captured_variables,
                callable,
            },
        )(input)
    }

    pub fn get_span(&self) -> &S {
        &self.starting_span
    }
}

#[cfg(test)]
mod test {
    use crate::script::parsing::{Block, VariableType};

    use super::*;

    #[test]
    fn captured_variable() {
        assert_eq!(
            CapturedVariable::parse("&variable"),
            Ok(("", CapturedVariable::Reference("variable")))
        );
        assert_eq!(
            CapturedVariable::parse("variable"),
            Ok(("", CapturedVariable::Copy("variable")))
        );
    }

    #[test]
    fn closure() {
        assert_eq!(
            Closure::parse("[]() -> Number {}"),
            Ok((
                "",
                Closure {
                    starting_span: "[",
                    captured_variables: vec![],
                    callable: CallableBlock {
                        parameter_span: "(",
                        parameters: vec![],
                        block: Block { statements: vec![] }
                    },
                    signature: FunctionSignature::Function {
                        return_type: Box::new(VariableType::Scalar("Number")),
                        arguments: vec![]
                    }
                }
            ))
        );

        assert_eq!(
            Closure::parse("[a]() -> Number {}"),
            Ok((
                "",
                Closure {
                    starting_span: "[",
                    captured_variables: vec![CapturedVariable::Copy("a")],
                    callable: CallableBlock {
                        parameter_span: "(",
                        parameters: vec![],
                        block: Block { statements: vec![] }
                    },
                    signature: FunctionSignature::Function {
                        return_type: Box::new(VariableType::Scalar("Number")),
                        arguments: vec![]
                    }
                }
            ))
        );

        assert_eq!(
            Closure::parse("[a, &b]() -> Number {}"),
            Ok((
                "",
                Closure {
                    starting_span: "[",
                    captured_variables: vec![
                        CapturedVariable::Copy("a"),
                        CapturedVariable::Reference("b")
                    ],
                    callable: CallableBlock {
                        parameter_span: "(",
                        parameters: vec![],
                        block: Block { statements: vec![] }
                    },
                    signature: FunctionSignature::Function {
                        return_type: Box::new(VariableType::Scalar("Number")),
                        arguments: vec![]
                    }
                }
            ))
        );
    }
}
