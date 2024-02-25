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
use nom::{branch::alt, combinator::map, error::context};

pub use self::{
    assign::{Assign, Assignable, AssignableVariable},
    expression::{ArithmeticExpression, Comparison, Expression, Factor, Term, Trailer},
    s_break::Break,
    s_continue::Continue,
    s_for::For,
    s_if::{Else, If},
    s_loop::Loop,
    s_match::Match,
    s_return::Return,
    s_while::While,
};

use super::{Function, Span, StructDefinition, VResult};

mod assign;
mod expression;
mod s_break;
mod s_continue;
mod s_for;
mod s_if;
mod s_loop;
mod s_match;
mod s_return;
mod s_while;

#[derive(Debug, Eq, PartialEq)]
pub enum Statement<S: Span> {
    Expression(Expression<S>),
    Assign(Assign<S>),
    Return(Return<S>),
    If(If<S>),
    Match(Match<S>),
    For(For<S>),
    While(While<S>),
    Loop(Loop<S>),
    Break(Break<S>),
    Continue(Continue<S>),
    DefineFunction(Function<S>),
    DefineStruct(StructDefinition<S>),
}

impl<S: Span> Statement<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        context(
            "Failed to parse statement",
            alt((
                map(Assign::parse, Self::Assign),
                map(Return::parse, Self::Return),
                map(If::parse, Self::If),
                map(Match::parse, Self::Match),
                map(For::parse, Self::For),
                map(While::parse, Self::While),
                map(Loop::parse, Self::Loop),
                map(Break::parse, Self::Break),
                map(Continue::parse, Self::Continue),
                map(Function::parse, Self::DefineFunction),
                map(StructDefinition::parse, Self::DefineStruct),
                map(Expression::parse, Self::Expression),
            )),
        )(input)
    }

    pub fn get_span(&self) -> &S {
        match self {
            Statement::Expression(spanable) => spanable.get_span(),
            Statement::Assign(spanable) => spanable.get_span(),
            Statement::Return(spanable) => spanable.get_span(),
            Statement::If(spanable) => spanable.get_span(),
            Statement::Match(spanable) => spanable.get_span(),
            Statement::For(spanable) => spanable.get_span(),
            Statement::While(spanable) => spanable.get_span(),
            Statement::Loop(spanable) => spanable.get_span(),
            Statement::Break(spanable) => spanable.get_span(),
            Statement::Continue(spanable) => spanable.get_span(),
            Statement::DefineFunction(spanable) => spanable.get_span(),
            Statement::DefineStruct(spanable) => &spanable.name,
        }
    }
}

#[cfg(test)]
mod test {
    use std::rc::Rc;

    use crate::script::parsing::{
        statements::assign::{Assignable, AssignableVariable},
        Block, FunctionSignature, NamedBlock, VariableType,
    };

    use super::*;

    #[test]
    fn statement() {
        assert_eq!(
            Statement::parse("a = b"),
            Ok((
                "",
                Statement::Assign(Assign {
                    starting_span: "a",
                    is_new: false,
                    to_assign: Assignable::Variable(AssignableVariable {
                        name: "a",
                        ty: None,
                    }),
                    statement: Box::new(Statement::parse("b").unwrap().1)
                })
            ))
        );

        assert_eq!(
            Statement::parse("return"),
            Ok((
                "",
                Statement::Return(Return {
                    starting_span: "return",
                    expression: None
                })
            ))
        );

        assert_eq!(
            Statement::parse("match a {}"),
            Ok((
                "",
                Statement::Match(Match {
                    starting_span: "match",
                    expression: Expression::parse("a").unwrap().1,
                    branches: vec![]
                })
            ))
        );

        assert_eq!(
            Statement::parse("for a in b {}"),
            Ok((
                "",
                Statement::For(For {
                    starting_span: "for",
                    name: None,
                    variable_assignment: Assignable::parse("a").unwrap().1,
                    iterator_expression: Expression::parse("b").unwrap().1,
                    block: Block { statements: vec![] }
                })
            ))
        );

        assert_eq!(
            Statement::parse("while a {}"),
            Ok((
                "",
                Statement::While(While {
                    starting_span: "while",
                    name: None,
                    expression: Expression::parse("a").unwrap().1,
                    block: Block { statements: vec![] }
                })
            ))
        );

        assert_eq!(
            Statement::parse("loop {}"),
            Ok((
                "",
                Statement::Loop(Loop {
                    starting_span: "loop",
                    name: None,
                    block: Block { statements: vec![] }
                })
            ))
        );

        assert_eq!(
            Statement::parse("break"),
            Ok((
                "",
                Statement::Break(Break {
                    starting_span: "break",
                    loop_name: None,
                    expression: None
                })
            ))
        );

        assert_eq!(
            Statement::parse("continue"),
            Ok((
                "",
                Statement::Continue(Continue {
                    starting_span: "continue",

                    loop_name: None
                })
            ))
        );

        assert_eq!(
            Statement::parse("function my_function() -> Number {}"),
            Ok((
                "",
                Statement::DefineFunction(Function {
                    starting_span: "function",
                    named_block: NamedBlock {
                        name: "my_function",
                        parameter_span: "(",
                        parameters: vec![],
                        block: Block { statements: vec![] }
                    },
                    signature: Rc::new(FunctionSignature::Function {
                        return_type: Box::new(VariableType::Number),
                        arguments: vec![]
                    })
                })
            ))
        );

        assert_eq!(
            Statement::parse("struct MyStruct {}"),
            Ok((
                "",
                Statement::DefineStruct(StructDefinition {
                    name: "MyStruct",
                    members: vec![]
                })
            ))
        );
    }
}
