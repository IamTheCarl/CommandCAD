use std::rc::Rc;

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
use nom::{branch::alt, combinator::map};

use super::{closure::Closure, take_keyword, List, PString, Scalar, Span, VResult};

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Litteral<S: Span> {
    Scalar(Scalar<S>),
    String(PString<S>),
    List(List<S>),
    Boolean(S, bool),
    Default(S),
    Closure(Rc<Closure<S>>),
}

impl<S: Span> Litteral<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        alt((
            map(Scalar::parse, Self::Scalar),
            map(PString::parse, Self::String),
            map(Closure::parse, |closure| Self::Closure(Rc::new(closure))),
            map(List::parse, Self::List),
            map(take_keyword("true"), |span| Self::Boolean(span, true)),
            map(take_keyword("false"), |span| Self::Boolean(span, false)),
            map(take_keyword("default"), Self::Default),
        ))(input)
    }

    pub fn get_span(&self) -> &S {
        match self {
            Litteral::Scalar(spanable) => spanable.get_span(),
            Litteral::String(spanable) => spanable.get_span(),
            Litteral::List(spanable) => spanable.get_span(),
            Litteral::Boolean(spanable, _) => spanable,
            Litteral::Default(spanable) => spanable,
            Litteral::Closure(spanable) => spanable.get_span(),
        }
    }
}

#[cfg(test)]
mod test {
    use std::rc::Rc;

    use crate::script::parsing::{Block, CallableBlock, FunctionSignature, Number, VariableType};

    use super::*;

    #[test]
    fn litteral() {
        // Scalar(Scalar<S>),
        assert_eq!(
            Litteral::parse("1234.5678m"),
            Ok((
                "",
                Litteral::Scalar(Scalar {
                    number: Number {
                        integer: Some("1234"),
                        dot: Some("."),
                        fractional: Some("5678")
                    },
                    ty: "m"
                })
            ))
        );

        // Scalar, but it's just a number
        assert_eq!(
            Litteral::parse("1234.5678"),
            Ok((
                "",
                Litteral::Scalar(Scalar {
                    number: Number {
                        integer: Some("1234"),
                        dot: Some("."),
                        fractional: Some("5678")
                    },
                    ty: "",
                })
            ))
        );

        // String(PString<S>),
        assert_eq!(
            Litteral::parse(r#""test""#),
            Ok(("", Litteral::String(PString { value: "test" })))
        );

        // List(List<S>),
        assert_eq!(
            Litteral::parse("[]"),
            Ok((
                "",
                Litteral::List(List {
                    starting_span: "[",
                    expressions: vec![]
                })
            ))
        );

        // Boolean(bool),
        assert_eq!(
            Litteral::parse("true"),
            Ok(("", Litteral::Boolean("true", true)))
        );
        assert_eq!(
            Litteral::parse("false"),
            Ok(("", Litteral::Boolean("false", false)))
        );

        // Default,
        assert_eq!(
            Litteral::parse("default"),
            Ok(("", Litteral::Default("default")))
        );

        // Closure
        assert_eq!(
            Litteral::parse("[]() -> Number {}"),
            Ok((
                "",
                Litteral::Closure(Rc::new(Closure {
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
                }))
            ))
        )
    }
}
