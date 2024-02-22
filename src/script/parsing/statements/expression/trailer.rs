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
    character::complete::char as nom_char,
    combinator::{flat_map, map},
    multi::{fold_many0, separated_list0},
    sequence::{delimited, pair, preceded},
};

use crate::script::{
    parsing::{parse_name, space0, VResult},
    Span,
};

use super::{factor::Factor, Expression};

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Trailer<S: Span> {
    None(Factor<S>),
    Attribute(Box<Trailer<S>>, S),
    Call(Box<Trailer<S>>, Vec<Expression<S>>),
    MethodCall(Box<Trailer<S>>, S, Vec<Expression<S>>),
    Index(Box<Trailer<S>>, Box<Expression<S>>),
}

impl<S: Span> Trailer<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        #[derive(Clone)]
        enum Operation<S: Span> {
            Attribute(S),
            Call(Vec<Expression<S>>),
            MethodCall(S, Vec<Expression<S>>),
            Index(Expression<S>),
        }

        alt((
            flat_map(Factor::parse, |first_factor| {
                fold_many0(
                    delimited(
                        space0,
                        alt((
                            map(
                                pair(
                                    preceded(pair(nom_char('.'), space0), parse_name),
                                    preceded(
                                        space0,
                                        delimited(
                                            nom_char('('),
                                            separated_list0(
                                                nom_char(','),
                                                delimited(space0, Expression::parse, space0),
                                            ),
                                            nom_char(')'),
                                        ),
                                    ),
                                ),
                                |(attribute, arguments)| {
                                    Operation::MethodCall(attribute, arguments)
                                },
                            ),
                            map(
                                preceded(pair(nom_char('.'), space0), parse_name),
                                Operation::Attribute,
                            ),
                            map(
                                delimited(
                                    nom_char('('),
                                    separated_list0(
                                        nom_char(','),
                                        delimited(space0, Expression::parse, space0),
                                    ),
                                    nom_char(')'),
                                ),
                                Operation::Call,
                            ),
                            map(
                                delimited(
                                    nom_char('['),
                                    delimited(space0, Expression::parse, space0),
                                    nom_char(']'),
                                ),
                                Operation::Index,
                            ),
                        )),
                        space0,
                    ),
                    move || Self::None(first_factor.clone()),
                    |trailer, operation| match operation {
                        Operation::Attribute(member_name) => {
                            Self::Attribute(Box::new(trailer), member_name)
                        }
                        Operation::Call(arguments) => Self::Call(Box::new(trailer), arguments),
                        Operation::MethodCall(member_name, arguments) => {
                            Self::MethodCall(Box::new(trailer), member_name, arguments)
                        }
                        Operation::Index(indexer) => {
                            Self::Index(Box::new(trailer), Box::new(indexer))
                        }
                    },
                )
            }),
            map(Factor::parse, Self::None),
        ))(input)
    }

    pub fn get_span(&self) -> &S {
        match self {
            Trailer::None(spanable) => spanable.get_span(),
            Trailer::Attribute(spanable, _) => spanable.get_span(),
            Trailer::Call(spanable, _) => spanable.get_span(),
            Trailer::MethodCall(spanable, _, _) => spanable.get_span(),
            Trailer::Index(spanable, _) => spanable.get_span(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn trailer_call() {
        assert_eq!(
            Trailer::parse("my_function()"),
            Ok((
                "",
                Trailer::Call(
                    Box::new(Trailer::None(Factor::Variable("my_function"))),
                    vec![]
                )
            ))
        );

        assert_eq!(
            Trailer::parse("my_function(a)"),
            Ok((
                "",
                Trailer::Call(
                    Box::new(Trailer::None(Factor::Variable("my_function"))),
                    vec![Expression::parse("a").unwrap().1]
                )
            ))
        );

        assert_eq!(
            Trailer::parse("my_function(a, b)"),
            Ok((
                "",
                Trailer::Call(
                    Box::new(Trailer::None(Factor::Variable("my_function"))),
                    vec![
                        Expression::parse("a").unwrap().1,
                        Expression::parse("b").unwrap().1
                    ]
                )
            ))
        );
    }

    #[test]
    fn trailer_attribute() {
        assert_eq!(
            Trailer::parse("variable"),
            Ok(("", Trailer::None(Factor::Variable("variable"))))
        );

        assert_eq!(
            Trailer::parse("variable.access"),
            Ok((
                "",
                Trailer::Attribute(
                    Box::new(Trailer::None(Factor::Variable("variable"))),
                    "access"
                )
            ))
        );

        assert_eq!(
            Trailer::parse("variable.access.sub_access"),
            Ok((
                "",
                Trailer::Attribute(
                    Box::new(Trailer::Attribute(
                        Box::new(Trailer::None(Factor::Variable("variable"))),
                        "access"
                    )),
                    "sub_access"
                )
            ))
        );
    }

    #[test]
    fn trailer_index() {
        assert_eq!(
            Trailer::parse("x[y]"),
            Ok((
                "",
                Trailer::Index(
                    Box::new(Trailer::None(Factor::Variable("x"))),
                    Box::new(Expression::parse("y").unwrap().1)
                )
            ))
        );

        assert_eq!(
            Trailer::parse("x[y][z]"),
            Ok((
                "",
                Trailer::Index(
                    Box::new(Trailer::Index(
                        Box::new(Trailer::None(Factor::Variable("x"))),
                        Box::new(Expression::parse("y").unwrap().1)
                    )),
                    Box::new(Expression::parse("z").unwrap().1)
                )
            ))
        );
    }

    #[test]
    fn trailer_method_call() {
        assert_eq!(
            Trailer::parse("test.sub_call()"),
            Ok((
                "",
                Trailer::MethodCall(
                    Box::new(Trailer::None(Factor::Variable("test"))),
                    "sub_call",
                    vec![]
                ),
            ))
        );

        assert_eq!(
            Trailer::parse("test.sub_call(a, b)"),
            Ok((
                "",
                Trailer::MethodCall(
                    Box::new(Trailer::None(Factor::Variable("test"))),
                    "sub_call",
                    vec![
                        Expression::parse("a").unwrap().1,
                        Expression::parse("b").unwrap().1
                    ]
                ),
            ))
        );

        assert_eq!(
            Trailer::parse("test.sub_call().returned()"),
            Ok((
                "",
                Trailer::MethodCall(
                    Box::new(Trailer::MethodCall(
                        Box::new(Trailer::None(Factor::Variable("test"))),
                        "sub_call",
                        vec![]
                    )),
                    "returned",
                    vec![]
                )
            ))
        );
    }
}
