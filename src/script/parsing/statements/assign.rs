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
    combinator::{map, opt},
    multi::separated_list0,
    sequence::{delimited, pair, preceded, terminated, tuple},
};

use crate::script::{
    parsing::{parse_name, space0, space1, take_keyword, VResult, VariableType},
    Span,
};

use super::Statement;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct AssignableVariable<S: Span> {
    pub name: S,
    pub ty: Option<VariableType<S>>,
}

impl<S: Span> AssignableVariable<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            pair(
                terminated(parse_name, space0),
                opt(preceded(pair(nom_char(':'), space0), VariableType::parse)),
            ),
            |(name, ty)| Self { name, ty },
        )(input)
    }

    pub fn get_span(&self) -> &S {
        &self.name
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Assignable<S: Span> {
    Variable(AssignableVariable<S>),
    List(S, Vec<AssignableVariable<S>>),
}

pub enum AssignableIter<
    'a,
    S: Span + 'a,
    A: Iterator<Item = &'a AssignableVariable<S>>,
    B: Iterator<Item = &'a AssignableVariable<S>>,
> {
    A(A),
    B(B),
}

impl<
        'a,
        S: Span + 'a,
        A: Iterator<Item = &'a AssignableVariable<S>>,
        B: Iterator<Item = &'a AssignableVariable<S>>,
    > std::iter::Iterator for AssignableIter<'a, S, A, B>
{
    type Item = &'a AssignableVariable<S>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            AssignableIter::A(iter) => iter.next(),
            AssignableIter::B(iter) => iter.next(),
        }
    }
}

impl<S: Span> Assignable<S> {
    pub(super) fn parse(input: S) -> VResult<S, Self> {
        alt((
            map(AssignableVariable::parse, Self::Variable),
            map(Self::parse_list, |(starting_span, list)| {
                Self::List(starting_span, list)
            }),
        ))(input)
    }

    fn parse_list(input: S) -> VResult<S, (S, Vec<AssignableVariable<S>>)> {
        terminated(
            pair(
                tag("["),
                separated_list0(
                    delimited(space0, nom_char(','), space0),
                    AssignableVariable::parse,
                ),
            ),
            nom_char(']'),
        )(input)
    }

    pub fn iter(
        &self,
    ) -> AssignableIter<
        '_,
        S,
        std::iter::Once<&AssignableVariable<S>>,
        std::slice::Iter<AssignableVariable<S>>,
    > {
        match self {
            Assignable::Variable(single) => AssignableIter::A(std::iter::once(single)),
            Assignable::List(_span, list) => AssignableIter::B(list.iter()),
        }
    }

    pub fn get_span(&self) -> &S {
        match self {
            Assignable::Variable(spanable) => spanable.get_span(),
            Assignable::List(spanable, _) => spanable,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Assign<S: Span> {
    pub starting_span: S,
    pub is_new: bool,
    pub to_assign: Assignable<S>,
    pub statement: Box<Statement<S>>,
}

impl<S: Span> Assign<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        map(
            tuple((
                opt(terminated(take_keyword::<S>("let"), space1)),
                Assignable::parse,
                preceded(delimited(space0, nom_char('='), space0), Statement::parse),
            )),
            |(let_keyword, to_assign, statement)| {
                let is_new = let_keyword.is_some();
                Self {
                    starting_span: let_keyword.unwrap_or_else(|| to_assign.get_span().clone()),
                    is_new,
                    to_assign,
                    statement: Box::new(statement),
                }
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
    fn assignable_variable() {
        assert_eq!(
            AssignableVariable::parse("variable"),
            Ok((
                "",
                AssignableVariable {
                    name: "variable",
                    ty: None
                }
            ))
        );

        assert_eq!(
            AssignableVariable::parse("variable: Length"),
            Ok((
                "",
                AssignableVariable {
                    name: "variable",
                    ty: Some(VariableType::Scalar("Length")),
                }
            ))
        );
    }

    #[test]
    fn assignable() {
        assert_eq!(
            Assignable::parse("variable"),
            Ok((
                "",
                Assignable::Variable(AssignableVariable {
                    name: "variable",
                    ty: None
                })
            ))
        );

        assert_eq!(
            Assignable::parse("[a, b]"),
            Ok((
                "",
                Assignable::List(
                    "[",
                    vec![
                        AssignableVariable {
                            name: "a",
                            ty: None
                        },
                        AssignableVariable {
                            name: "b",
                            ty: None
                        }
                    ]
                )
            ))
        );

        assert_eq!(
            Assignable::parse("[a: Length, b: Angle]"),
            Ok((
                "",
                Assignable::List(
                    "[",
                    vec![
                        AssignableVariable {
                            name: "a",
                            ty: Some(VariableType::Scalar("Length"))
                        },
                        AssignableVariable {
                            name: "b",
                            ty: Some(VariableType::Scalar("Angle"))
                        }
                    ]
                )
            ))
        );
    }

    #[test]
    fn statement_assign() {
        assert_eq!(
            Assign::parse("a = b"),
            Ok((
                "",
                Assign {
                    starting_span: "a",
                    is_new: false,
                    to_assign: Assignable::Variable(AssignableVariable {
                        name: "a",
                        ty: None,
                    }),
                    statement: Box::new(Statement::parse("b").unwrap().1)
                }
            ))
        );

        assert_eq!(
            Assign::parse("a: Length = b"),
            Ok((
                "",
                Assign {
                    starting_span: "a",
                    is_new: false,
                    to_assign: Assignable::Variable(AssignableVariable {
                        name: "a",
                        ty: Some(VariableType::Scalar("Length")),
                    }),
                    statement: Box::new(Statement::parse("b").unwrap().1)
                }
            ))
        );

        assert_eq!(
            Assign::parse("a = loop {}"),
            Ok((
                "",
                Assign {
                    starting_span: "a",
                    is_new: false,
                    to_assign: Assignable::Variable(AssignableVariable {
                        name: "a",
                        ty: None,
                    }),
                    statement: Box::new(Statement::parse("loop {}").unwrap().1),
                }
            ))
        );

        assert_eq!(
            Assign::parse("let a = b"),
            Ok((
                "",
                Assign {
                    starting_span: "let",
                    is_new: true,
                    to_assign: Assignable::Variable(AssignableVariable {
                        name: "a",
                        ty: None,
                    }),
                    statement: Box::new(Statement::parse("b").unwrap().1)
                }
            ))
        );
    }
}
