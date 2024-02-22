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
    combinator::map,
    sequence::{delimited, pair, preceded},
};

use crate::script::{
    parsing::{parse_name, space0, Litteral, StructInitialization, VResult},
    Span,
};

use super::Expression;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Factor<S: Span> {
    Litteral(Litteral<S>),
    Variable(S),
    Parenthesis(Box<Expression<S>>),
    UnaryPlus(Box<Factor<S>>),
    UnaryMinus(Box<Factor<S>>),
    UnaryLogicalNot(Box<Factor<S>>),
    StructInitalization(StructInitialization<S>),
}

impl<S: Span> Factor<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        alt((
            map(
                preceded(pair(nom_char('+'), space0), Self::parse),
                |factor| Self::UnaryPlus(Box::new(factor)),
            ),
            map(
                preceded(pair(nom_char('-'), space0), Self::parse),
                |factor| Self::UnaryMinus(Box::new(factor)),
            ),
            map(
                preceded(pair(nom_char('!'), space0), Self::parse),
                |factor| Self::UnaryLogicalNot(Box::new(factor)),
            ),
            map(Litteral::parse, Self::Litteral),
            map(
                delimited(nom_char('('), Expression::parse, nom_char(')')),
                |expression| Self::Parenthesis(Box::new(expression)),
            ),
            map(StructInitialization::parse, Self::StructInitalization),
            map(parse_name, Self::Variable),
        ))(input)
    }

    pub fn get_span(&self) -> &S {
        match self {
            Factor::Litteral(spanable) => spanable.get_span(),
            Factor::Variable(spanable) => spanable,
            Factor::Parenthesis(spanable) => spanable.get_span(),
            Factor::UnaryPlus(spanable) => spanable.get_span(),
            Factor::UnaryMinus(spanable) => spanable.get_span(),
            Factor::UnaryLogicalNot(spanable) => spanable.get_span(),
            Factor::StructInitalization(spanable) => spanable.get_span(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::script::parsing::{
        statements::expression::{
            arithmetic::ArithmeticExpression, comparison::Comparison, term::Term, trailer::Trailer,
        },
        Number,
    };

    use super::*;

    #[test]
    fn factor() {
        // Const(Litteral<S>),
        assert_eq!(
            Factor::parse("22"),
            Ok((
                "",
                Factor::Litteral(Litteral::Number(Number {
                    integer: Some("22"),
                    dot: None,
                    fractional: None
                }))
            ))
        );
        // Variable(S),
        assert_eq!(
            Factor::parse("my_variable"),
            Ok(("", Factor::Variable("my_variable")))
        );
        // Parenthesis(Box<Expression<S>>),
        assert_eq!(
            Factor::parse("(my_variable)"),
            Ok((
                "",
                Factor::Parenthesis(Box::new(Expression::Buffer(Comparison::None(
                    ArithmeticExpression::Term(Term::Trailer(Trailer::None(Factor::Variable(
                        "my_variable"
                    ))))
                ))))
            ))
        );
        // UnaryPlus(Term<S>),
        assert_eq!(
            Factor::parse("+my_variable"),
            Ok((
                "",
                Factor::UnaryPlus(Box::new(Factor::Variable("my_variable")))
            ))
        );
        // UnaryMinus(Term<S>),
        assert_eq!(
            Factor::parse("-my_variable"),
            Ok((
                "",
                Factor::UnaryMinus(Box::new(Factor::Variable("my_variable")))
            ))
        );
        // UnaryLogicalNot(Term<S>),
        assert_eq!(
            Factor::parse("!my_variable"),
            Ok((
                "",
                Factor::UnaryLogicalNot(Box::new(Factor::Variable("my_variable")))
            ))
        );
        // StructInitalization(StructInitalization<S>),
        assert_eq!(
            Factor::parse("struct MyStruct {}"),
            Ok((
                "",
                Factor::StructInitalization(StructInitialization {
                    starting_span: "struct",
                    name: "MyStruct",
                    assignments: vec![],
                    inheritance: None
                })
            ))
        );
        assert_eq!(
            Factor::parse("struct MyStruct { a = b, c = d }"),
            Ok((
                "",
                Factor::StructInitalization(StructInitialization {
                    starting_span: "struct",
                    name: "MyStruct",
                    assignments: vec![
                        ("a", Expression::parse("b").unwrap().1),
                        ("c", Expression::parse("d").unwrap().1)
                    ],
                    inheritance: None
                })
            ))
        );
        assert_eq!(
            Factor::parse("struct MyStruct { a = b, c = d, ..default }"),
            Ok((
                "",
                Factor::StructInitalization(StructInitialization {
                    starting_span: "struct",
                    name: "MyStruct",
                    assignments: vec![
                        ("a", Expression::parse("b").unwrap().1),
                        ("c", Expression::parse("d").unwrap().1)
                    ],
                    inheritance: Some(Box::new(Trailer::None(Factor::Litteral(
                        Litteral::Default("default")
                    ))))
                })
            ))
        );
        assert_eq!(
            Factor::parse("struct MyStruct { ..default }"),
            Ok((
                "",
                Factor::StructInitalization(StructInitialization {
                    starting_span: "struct",
                    name: "MyStruct",
                    assignments: vec![],
                    inheritance: Some(Box::new(Trailer::None(Factor::Litteral(
                        Litteral::Default("default")
                    ))))
                })
            ))
        );
    }
}
