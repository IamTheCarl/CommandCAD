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

use std::{
    borrow::Cow,
    ops::{RangeFrom, RangeTo},
};

use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, take_until, take_while, take_while1},
    character::complete::{char as nom_char, one_of},
    combinator::{
        all_consuming, consumed, cut, flat_map, map, opt, recognize, success, value, verify,
    },
    error::context,
    multi::{fold_many0, fold_many1, many0, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    CompareResult, IResult,
};
use nom_locate::LocatedSpan;

// TODO currently all comments are just discarded.
// I would like to start tracking them for auto-formatting and generation
// of documentation.

pub type VResult<I, O> = IResult<I, O, nom::error::VerboseError<I>>;

pub trait Span:
    nom::InputIter<Item = char>
    + for<'b> nom::Compare<&'b str>
    + nom::InputLength
    + nom::InputTake
    + nom::InputTakeAtPosition<Item = char>
    + nom::Slice<RangeFrom<usize>>
    + nom::Slice<RangeTo<usize>>
    + nom::Offset
    + for<'b> nom::FindSubstring<&'b str>
    + Clone
    + std::fmt::Debug
    + Eq
    + PartialEq
    + std::hash::Hash
    + ToString
    + AsStr
    + FromStr
{
    fn chars(&self) -> impl Iterator<Item = char>;
}

pub trait AsStr {
    fn as_str(&self) -> &str;
}

pub trait FromStr {
    fn from_str(string: &'static str) -> Self;
}

impl<'a> AsStr for &'a str {
    fn as_str(&self) -> &str {
        self
    }
}

impl<'a> FromStr for &'a str {
    fn from_str(string: &'static str) -> Self {
        string
    }
}

impl<'a> AsStr for LocatedSpan<&'a str> {
    fn as_str(&self) -> &str {
        self.fragment()
    }
}

impl<'a> FromStr for LocatedSpan<&'a str> {
    fn from_str(string: &'static str) -> Self {
        LocatedSpan::new(string)
    }
}

impl AsStr for imstr::ImString {
    fn as_str(&self) -> &str {
        self.as_str()
    }
}

impl FromStr for imstr::ImString {
    fn from_str(string: &'static str) -> Self {
        imstr::ImString::from(string)
    }
}

impl AsStr for LocatedSpan<imstr::ImString> {
    fn as_str(&self) -> &str {
        self.fragment().as_str()
    }
}

impl FromStr for LocatedSpan<imstr::ImString> {
    fn from_str(string: &'static str) -> Self {
        LocatedSpan::new(imstr::ImString::from(string))
    }
}

impl<'a> Span for &'a str {
    fn chars(&self) -> impl Iterator<Item = char> {
        str::chars(self)
    }
}
impl<'a> Span for LocatedSpan<&'a str> {
    fn chars(&self) -> impl Iterator<Item = char> {
        self.fragment().chars()
    }
}
impl Span for imstr::ImString {
    fn chars(&self) -> impl Iterator<Item = char> {
        imstr::ImString::chars(self)
    }
}
impl Span for LocatedSpan<imstr::ImString> {
    fn chars(&self) -> impl Iterator<Item = char> {
        self.fragment().chars()
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct FileAST<S: Span> {
    pub root_elements: Vec<RootElement<S>>,
}

impl<S: Span> FileAST<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        all_consuming(map(
            many0(delimited(space0, RootElement::parse, space0)),
            |root_elements| FileAST { root_elements },
        ))(input)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum RootElement<S: Span> {
    Import(Import<S>),
    Struct(Struct<S>),
    Sketch(Sketch<S>),
    Solid(Solid<S>),
    Function(Function<S>),
}

impl<S: Span> RootElement<S> {
    fn parse(input: S) -> VResult<S, Self> {
        alt((
            map(Import::parse, Self::Import),
            map(Struct::parse, Self::Struct),
            map(Sketch::parse, Self::Sketch),
            map(Solid::parse, Self::Solid),
            map(Function::parse, Self::Function),
        ))(input)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Import<S: Span> {
    pub path: Vec<S>,
    pub external: bool,
}

impl<S: Span> Import<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            preceded(
                take_keyword("import"),
                delimited(
                    space0,
                    pair(
                        opt(take_keyword("extern")),
                        separated_list0(tag("::"), delimited(space0, parse_name, space0)),
                    ),
                    pair(space0, nom_char(';')),
                ),
            ),
            |(external, path)| Import {
                path,
                external: external.is_some(),
            },
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Struct<S: Span> {
    pub name: S,
    pub members: Vec<MemberVariable<S>>,
}

impl<S: Span> Struct<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            preceded(
                take_keyword("struct"),
                pair(
                    delimited(space0, parse_name, space0),
                    delimited(
                        pair(nom_char('{'), space0),
                        separated_list0(nom_char(','), preceded(space0, MemberVariable::parse)),
                        preceded(space0, nom_char('}')),
                    ),
                ),
            ),
            |(name, assignments)| Self {
                name,
                members: assignments,
            },
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Sketch<S: Span> {
    pub starting_span: S,
    pub named_block: NamedBlock<S>,
}

impl<S: Span> Sketch<S> {
    fn parse(input: S) -> VResult<S, Self> {
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

#[derive(Debug, Eq, PartialEq)]
pub struct Solid<S: Span> {
    pub starting_span: S,
    pub named_block: NamedBlock<S>,
}

impl<S: Span> Solid<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            pair(
                terminated(take_keyword("solid"), space1),
                cut(NamedBlock::parse),
            ),
            |(starting_span, named_block)| Self {
                starting_span,
                named_block,
            },
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Function<S: Span> {
    pub starting_span: S,
    pub named_block: NamedBlock<S>,
    pub return_type: VariableType<S>,
}

impl<S: Span> Function<S> {
    fn parse(input: S) -> VResult<S, Self> {
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum MemberVariableConstraint<S: Span> {
    Min(Litteral<S>),
    Max(Litteral<S>),
    Enum(Vec<Litteral<S>>),
    Integer,
    // TODO add the ability to constrain lists to a certain type, or set of types.
}

impl<S: Span> MemberVariableConstraint<S> {
    fn parse(input: S) -> VResult<S, Self> {
        alt((
            Self::parametric_constraint("min", map(Litteral::parse, Self::Min)),
            Self::parametric_constraint("max", map(Litteral::parse, Self::Max)),
            Self::parametric_constraint(
                "enum",
                map(
                    separated_list0(nom_char(','), delimited(space0, Litteral::parse, space0)),
                    Self::Enum,
                ),
            ),
            value(Self::Integer, take_keyword("integer")),
        ))(input)
    }

    fn parametric_constraint(
        name: &'static str,
        parameter_parser: impl FnMut(S) -> VResult<S, Self>,
    ) -> impl FnMut(S) -> VResult<S, Self> {
        preceded(
            take_keyword(name),
            delimited(
                pair(
                    space0,
                    context("Expected opening `(` to begin arguments", nom_char('(')),
                ),
                delimited(space0, parameter_parser, space0),
                context("Expected closing `)` to end arguments", nom_char(')')),
            ),
        )
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct MemberVariableConstraintList<S: Span> {
    constraints: Vec<MemberVariableConstraint<S>>,
}

impl<S: Span> MemberVariableConstraintList<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            delimited(
                pair(tag("#["), space0),
                separated_list0(
                    nom_char(','),
                    delimited(space0, MemberVariableConstraint::parse, space0),
                ),
                pair(
                    space0,
                    context("Missing closing `]` for constraint list", cut(tag("]"))),
                ),
            ),
            |constraints| Self { constraints },
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct MemberVariable<S: Span> {
    pub name: S,
    pub ty: VariableType<S>,
    pub constraints: Option<MemberVariableConstraintList<S>>,
    pub default_value: Option<Litteral<S>>,
}

impl<S: Span> MemberVariable<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            tuple((
                opt(terminated(MemberVariableConstraintList::parse, space0)),
                terminated(parse_name, space0),
                preceded(nom_char(':'), preceded(space0, VariableType::parse)),
                opt(preceded(
                    delimited(space0, nom_char('='), space0),
                    Litteral::parse,
                )),
            )),
            |(constraints, name, ty, default_value)| Self {
                constraints,
                name,
                ty,
                default_value,
            },
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct NamedBlock<S: Span> {
    pub name: S,
    pub parameter_span: S,
    pub parameters: Vec<MemberVariable<S>>,
    pub block: Block<S>,
}

impl<S: Span> NamedBlock<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            Self::parse_internal(success(())),
            |(block, _return_type)| block,
        )(input)
    }

    fn parse_with_return_type(input: S) -> VResult<S, (Self, VariableType<S>)> {
        Self::parse_internal(context(
            "Could not parse return type",
            preceded(pair(tag("->"), space0), VariableType::parse),
        ))(input)
    }

    fn parse_internal<T>(
        return_type_parser: impl FnMut(S) -> VResult<S, T>,
    ) -> impl FnMut(S) -> VResult<S, (Self, T)> {
        map(
            tuple((
                parse_name,
                context(
                    "Expected parameter list",
                    terminated(
                        pair(
                            preceded(space0, tag("(")),
                            separated_list0(
                                nom_char(','),
                                delimited(space0, MemberVariable::parse, space0),
                            ),
                        ),
                        pair(nom_char(')'), space0),
                    ),
                ),
                delimited(space0, return_type_parser, space0),
                Block::parse,
            )),
            |(name, (parameter_span, parameters), return_type, block)| {
                (
                    Self {
                        name,
                        parameter_span,
                        parameters,
                        block,
                    },
                    return_type,
                )
            },
        )
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum BlockStatement<S: Span> {
    Closed(Statement<S>),
    Open(Statement<S>),
    Blank(S),
}

impl<S: Span> BlockStatement<S> {
    fn parse(input: S) -> VResult<S, Self> {
        alt((
            map(
                delimited(space0, Statement::parse, pair(space0, nom_char(';'))),
                Self::Closed,
            ),
            map(preceded(space0, Statement::parse), Self::Open),
            map(preceded(space0, tag(";")), Self::Blank),
        ))(input)
    }

    pub fn get(&self) -> Option<&Statement<S>> {
        match self {
            BlockStatement::Closed(statement) | BlockStatement::Open(statement) => Some(statement),
            BlockStatement::Blank(_) => None,
        }
    }

    pub fn get_span(&self) -> &S {
        match self {
            BlockStatement::Closed(spanable) => spanable.get_span(),
            BlockStatement::Open(spanable) => spanable.get_span(),
            BlockStatement::Blank(spanable) => spanable,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Block<S: Span> {
    pub statements: Vec<BlockStatement<S>>,
}

impl<S: Span> Block<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        map(
            delimited(
                context("Block is missing opening bracket", nom_char('{')),
                many0(alt((delimited(space0, BlockStatement::parse, space0),))),
                context(
                    "Block is missing closing bracket",
                    pair(space0, nom_char('}')),
                ),
            ),
            |statements| Self { statements },
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
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
        }
    }
}

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
    fn parse(input: S) -> VResult<S, Self> {
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Assign<S: Span> {
    pub starting_span: S,
    pub is_new: bool,
    pub to_assign: Assignable<S>,
    pub statement: Box<Statement<S>>,
}

impl<S: Span> Assign<S> {
    fn parse(input: S) -> VResult<S, Self> {
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Return<S: Span> {
    pub starting_span: S,
    pub expression: Option<Expression<S>>,
}

impl<S: Span> Return<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            pair(
                take_keyword("return"),
                opt(preceded(space1, Expression::parse)),
            ),
            |(starting_span, expression)| Self {
                starting_span,
                expression,
            },
        )(input)
    }

    pub fn get_span(&self) -> &S {
        &self.starting_span
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct If<S: Span> {
    pub starting_span: S,
    pub expression: Expression<S>,
    pub block: Block<S>,
    pub else_statement: Option<Else<S>>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Else<S: Span> {
    Else(Block<S>),
    IfElse(Box<If<S>>),
}

impl<S: Span> If<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            pair(
                terminated(take_keyword("if"), space1),
                tuple((
                    context(
                        "An expression is required for the if condition",
                        Expression::parse,
                    ),
                    delimited(
                        space0,
                        context("Expected a code block for if statement", Block::parse),
                        space0,
                    ),
                    opt(preceded(
                        take_keyword("else"),
                        context(
                            "Expected a code block or another if statement after `else` keyword",
                            cut(alt((
                                map(preceded(space0, Block::parse), |block| Else::Else(block)),
                                map(preceded(space1, Self::parse), |if_statement| {
                                    Else::IfElse(Box::new(if_statement))
                                }),
                            ))),
                        ),
                    )),
                )),
            ),
            |(starting_span, (expression, block, else_statement))| Self {
                starting_span,
                expression,
                block,
                else_statement,
            },
        )(input)
    }

    pub fn get_span(&self) -> &S {
        &self.starting_span
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Match<S: Span> {
    pub starting_span: S,
    pub expression: Expression<S>,
    pub branches: Vec<MatchBranch<S>>,
}

impl<S: Span> Match<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            pair(
                terminated(take_keyword("match"), space1),
                pair(
                    Expression::parse,
                    delimited(
                        pair(space0, nom_char('{')),
                        separated_list0(
                            nom_char(','),
                            delimited(space0, MatchBranch::parse, space0),
                        ),
                        pair(space0, nom_char('}')),
                    ),
                ),
            ),
            |(starting_span, (expression, branches))| Self {
                starting_span,
                expression,
                branches,
            },
        )(input)
    }

    pub fn get_span(&self) -> &S {
        &self.starting_span
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct MatchBranch<S: Span> {
    pub litteral: Litteral<S>,
    pub block: Block<S>,
}

impl<S: Span> MatchBranch<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            separated_pair(
                context("Match branch must start with a litteral", Litteral::parse),
                delimited(
                    space0,
                    context("Match branch missing `=>` token", tag("=>")),
                    space0,
                ),
                alt((
                    Block::parse,
                    map(Expression::parse, |expression| Block {
                        statements: vec![BlockStatement::Open(Statement::Expression(expression))],
                    }),
                )),
            ),
            |(litteral, block)| Self { litteral, block },
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct For<S: Span> {
    pub starting_span: S,
    pub name: Option<S>,
    pub variable_assignment: Assignable<S>,
    pub iterator_expression: Expression<S>,
    pub block: Block<S>,
}

impl<S: Span> For<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            tuple((
                opt(delimited(
                    nom_char('\''),
                    parse_name,
                    pair(nom_char(':'), space0),
                )),
                pair(
                    take_keyword("for"),
                    cut(tuple((
                        delimited(
                            space0,
                            context("Missing variable assignment", Assignable::parse),
                            space0,
                        ),
                        context("Missing `in` keyword", take_keyword("in")),
                        delimited(
                            space0,
                            context("Missing iterator expression", Expression::parse),
                            space0,
                        ),
                        Block::parse,
                    ))),
                ),
            )),
            |(name, (starting_span, (variable_assignment, _in, iterator_expression, block)))| {
                Self {
                    starting_span,
                    name,
                    variable_assignment,
                    iterator_expression,
                    block,
                }
            },
        )(input)
    }

    pub fn get_span(&self) -> &S {
        &self.starting_span
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct While<S: Span> {
    pub starting_span: S,
    pub name: Option<S>,
    pub expression: Expression<S>,
    pub block: Block<S>,
}

impl<S: Span> While<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            tuple((
                opt(delimited(
                    nom_char('\''),
                    parse_name,
                    pair(nom_char(':'), space0),
                )),
                pair(
                    take_keyword("while"),
                    cut(pair(
                        delimited(
                            space0,
                            context("Missing while loop condition expression", Expression::parse),
                            space0,
                        ),
                        context("While loop is missing block", Block::parse),
                    )),
                ),
            )),
            |(name, (starting_span, (expression, block)))| Self {
                starting_span,
                name,
                expression,
                block,
            },
        )(input)
    }

    pub fn get_span(&self) -> &S {
        &self.starting_span
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Loop<S: Span> {
    pub starting_span: S,
    pub name: Option<S>,
    pub block: Block<S>,
}

impl<S: Span> Loop<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            tuple((
                opt(delimited(
                    nom_char('\''),
                    parse_name,
                    pair(nom_char(':'), space0),
                )),
                pair(
                    take_keyword("loop"),
                    cut(preceded(
                        space0,
                        context("Loop is missing its block", Block::parse),
                    )),
                ),
            )),
            |(name, (starting_span, block))| Self {
                starting_span,
                name,
                block,
            },
        )(input)
    }

    pub fn get_span(&self) -> &S {
        &self.starting_span
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Break<S: Span> {
    pub starting_span: S,
    pub loop_name: Option<S>,
    pub expression: Option<Expression<S>>,
}

impl<S: Span> Break<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            pair(
                take_keyword("break"),
                pair(
                    opt(preceded(pair(space0, nom_char('\'')), parse_name)),
                    opt(preceded(space1, Expression::parse)),
                ),
            ),
            |(starting_span, (loop_name, expression))| Break {
                starting_span,
                loop_name,
                expression,
            },
        )(input)
    }

    pub fn get_span(&self) -> &S {
        &self.starting_span
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Continue<S: Span> {
    pub starting_span: S,
    pub loop_name: Option<S>,
}

impl<S: Span> Continue<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            pair(
                take_keyword("continue"),
                opt(preceded(pair(space0, nom_char('\'')), parse_name)),
            ),
            |(starting_span, loop_name)| Continue {
                starting_span,
                loop_name,
            },
        )(input)
    }

    pub fn get_span(&self) -> &S {
        &self.starting_span
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum VariableType<S: Span> {
    Number,
    String,
    List,
    Boolean,
    Range,
    Struct(S),
    Measurement(S),
    Cycle,
    Region,
    Sketch,
    Surface,
    Solid,
}

impl<S: Span> VariableType<S> {
    fn parse(input: S) -> VResult<S, Self> {
        context(
            "Invalid type",
            alt((
                value(Self::Number, tag("Number")),
                value(Self::String, tag("String")),
                value(Self::List, tag("List")),
                value(Self::Boolean, tag("Boolean")),
                value(Self::Range, tag("Range")),
                value(Self::Cycle, tag("Cycle")),
                value(Self::Region, tag("Region")),
                value(Self::Sketch, tag("Sketch")),
                value(Self::Surface, tag("Surface")),
                value(Self::Solid, tag("Solid")),
                map(
                    preceded(pair(take_keyword("struct"), space0), parse_name),
                    Self::Struct,
                ),
                map(parse_name, Self::Measurement),
            )),
        )(input)
    }

    pub fn name(&self) -> Cow<'static, str> {
        match self {
            VariableType::Number => "Number".into(),
            VariableType::String => "String".into(),
            VariableType::List => "List".into(),
            VariableType::Boolean => "Boolean".into(),
            VariableType::Range => "Range".into(),
            VariableType::Struct(name) => format!("struct {}", name.as_str()).into(),
            VariableType::Measurement(name) => name.to_string().into(),
            VariableType::Cycle => "Cycle".into(),
            VariableType::Region => "Region".into(),
            VariableType::Sketch => "Sketch".into(),
            VariableType::Surface => "Surface".into(),
            VariableType::Solid => "Solid".into(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression<S: Span> {
    And(Box<Self>, Comparison<S>),
    Or(Box<Self>, Comparison<S>),
    Buffer(Comparison<S>),
}

impl<S: Span> Expression<S> {
    pub(super) fn parse(input: S) -> VResult<S, Self> {
        #[derive(Clone)]
        enum Operator {
            And,
            Or,
        }

        alt((
            flat_map(Comparison::parse, |first_comp| {
                fold_many0(
                    pair(
                        delimited(
                            space0,
                            alt((
                                value(Operator::And, tag("&&")),
                                value(Operator::Or, tag("||")),
                            )),
                            space0,
                        ),
                        context(
                            "Expected right side comparison or arithmetic expression",
                            cut(Comparison::parse),
                        ),
                    ),
                    move || Self::Buffer(first_comp.clone()),
                    |expression, (operator, comparison)| match operator {
                        Operator::And => Self::And(Box::new(expression), comparison),
                        Operator::Or => Self::Or(Box::new(expression), comparison),
                    },
                )
            }),
            map(Comparison::parse, Self::Buffer),
        ))(input)
    }

    pub fn get_span(&self) -> &S {
        match self {
            Expression::And(spanable, _) => spanable.get_span(),
            Expression::Or(spanable, _) => spanable.get_span(),
            Expression::Buffer(spanable) => spanable.get_span(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Comparison<S: Span> {
    LessThan(Box<Self>, ArithmeticExpression<S>),
    LessThanEqual(Box<Self>, ArithmeticExpression<S>),
    Equal(Box<Self>, ArithmeticExpression<S>),
    GreaterThanEqual(Box<Self>, ArithmeticExpression<S>),
    GreaterThan(Box<Self>, ArithmeticExpression<S>),
    None(ArithmeticExpression<S>),
}

impl<S: Span> Comparison<S> {
    fn parse(input: S) -> VResult<S, Self> {
        #[derive(Clone)]
        enum Operator {
            LessThan,
            LessThanEqual,
            Equal,
            GreaterThanEqual,
            GreaterThan,
        }

        alt((
            flat_map(ArithmeticExpression::parse, |first_expression| {
                fold_many0(
                    pair(
                        delimited(
                            space0,
                            alt((
                                value(Operator::LessThanEqual, tag("<=")),
                                value(Operator::Equal, tag("==")),
                                value(Operator::GreaterThanEqual, tag(">=")),
                                value(Operator::LessThan, nom_char('<')),
                                value(Operator::GreaterThan, nom_char('>')),
                            )),
                            space0,
                        ),
                        context(
                            "Expected right side arithmetic expression.",
                            cut(ArithmeticExpression::parse),
                        ),
                    ),
                    move || Comparison::None(first_expression.clone()),
                    |comparison, (operator, expression)| match operator {
                        Operator::LessThan => Self::LessThan(Box::new(comparison), expression),
                        Operator::LessThanEqual => {
                            Self::LessThanEqual(Box::new(comparison), expression)
                        }
                        Operator::Equal => Self::Equal(Box::new(comparison), expression),
                        Operator::GreaterThanEqual => {
                            Self::GreaterThanEqual(Box::new(comparison), expression)
                        }
                        Operator::GreaterThan => {
                            Self::GreaterThan(Box::new(comparison), expression)
                        }
                    },
                )
            }),
            map(ArithmeticExpression::parse, Self::None),
        ))(input)
    }

    pub fn get_span(&self) -> &S {
        match self {
            Comparison::LessThan(spanable, _) => spanable.get_span(),
            Comparison::LessThanEqual(spanable, _) => spanable.get_span(),
            Comparison::Equal(spanable, _) => spanable.get_span(),
            Comparison::GreaterThanEqual(spanable, _) => spanable.get_span(),
            Comparison::GreaterThan(spanable, _) => spanable.get_span(),
            Comparison::None(spanable) => spanable.get_span(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ArithmeticExpression<S: Span> {
    Addition(Box<Self>, Term<S>),
    Subtraction(Box<Self>, Term<S>),
    Term(Term<S>),
}

impl<S: Span> ArithmeticExpression<S> {
    fn parse(input: S) -> VResult<S, Self> {
        #[derive(Clone)]
        enum Operator {
            Addition,
            Subtraction,
        }

        alt((
            flat_map(Term::parse, |first_term| {
                fold_many0(
                    pair(
                        delimited(
                            space0,
                            alt((
                                value(Operator::Addition, nom_char('+')),
                                value(Operator::Subtraction, nom_char('-')),
                            )),
                            space0,
                        ),
                        context("Expected right side term", cut(Term::parse)),
                    ),
                    move || ArithmeticExpression::Term(first_term.clone()),
                    |expression, (operator, factor)| match operator {
                        Operator::Addition => Self::Addition(Box::new(expression), factor),
                        Operator::Subtraction => Self::Subtraction(Box::new(expression), factor),
                    },
                )
            }),
            map(Term::parse, Self::Term),
        ))(input)
    }

    pub fn get_span(&self) -> &S {
        match self {
            ArithmeticExpression::Addition(spanable, _) => spanable.get_span(),
            ArithmeticExpression::Subtraction(spanable, _) => spanable.get_span(),
            ArithmeticExpression::Term(spanable) => spanable.get_span(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Term<S: Span> {
    Multiply(Box<Self>, Trailer<S>),
    Divide(Box<Self>, Trailer<S>),
    Range(Range<S>),
    Trailer(Trailer<S>),
}

impl<S: Span> Term<S> {
    fn parse(input: S) -> VResult<S, Self> {
        #[derive(Clone)]
        enum Operator {
            Multiplication,
            Division,
        }

        alt((
            map(Range::parse, Self::Range),
            flat_map(Trailer::parse, |first_trailer| {
                fold_many0(
                    pair(
                        delimited(
                            space0,
                            alt((
                                value(Operator::Multiplication, nom_char('*')),
                                value(Operator::Division, nom_char('/')),
                            )),
                            space0,
                        ),
                        context("Expected right side term.", cut(Trailer::parse)),
                    ),
                    move || Self::Trailer(first_trailer.clone()),
                    |term, (operator, accessor)| match operator {
                        Operator::Multiplication => Self::Multiply(Box::new(term), accessor),
                        Operator::Division => Self::Divide(Box::new(term), accessor),
                    },
                )
            }),
            map(Trailer::parse, Self::Trailer),
        ))(input)
    }

    pub fn get_span(&self) -> &S {
        match self {
            Term::Multiply(spanable, _) => spanable.get_span(),
            Term::Divide(spanable, _) => spanable.get_span(),
            Term::Range(spannable) => spannable.get_span(),
            Term::Trailer(spanable) => spanable.get_span(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Range<S: Span> {
    comparison_operator: S,
    pub lower_bound: Option<Trailer<S>>,
    pub upper_bound_is_inclusive: bool,
    pub upper_bound: Option<Trailer<S>>,
}

impl<S: Span> Range<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        enum RangeType<S> {
            Exclusive(S),
            Inclusive(S),
        }

        map(
            tuple((
                opt(Trailer::parse),
                delimited(
                    space0,
                    alt((
                        map(tag("..="), RangeType::Inclusive),
                        map(tag(".."), RangeType::Exclusive),
                    )),
                    space0,
                ),
                opt(Trailer::parse),
            )),
            |(lower_bound, range_type, upper_bound)| {
                let (comparison_operator, upper_bound_is_inclusive) = match range_type {
                    RangeType::Inclusive(operator) => (operator, true),
                    RangeType::Exclusive(operator) => (operator, false),
                };

                Self {
                    comparison_operator,
                    upper_bound,
                    upper_bound_is_inclusive,
                    lower_bound,
                }
            },
        )(input)
    }

    pub fn get_span(&self) -> &S {
        self.lower_bound
            .as_ref()
            .map(|expression| expression.get_span())
            .unwrap_or(&self.comparison_operator)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Trailer<S: Span> {
    None(Factor<S>),
    Attribute(Box<Trailer<S>>, S),
    Call(Box<Trailer<S>>, Vec<Expression<S>>),
    MethodCall(Box<Trailer<S>>, S, Vec<Expression<S>>),
    Index(Box<Trailer<S>>, Box<Expression<S>>),
}

impl<S: Span> Trailer<S> {
    fn parse(input: S) -> VResult<S, Self> {
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
    fn parse(input: S) -> VResult<S, Self> {
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StructInitialization<S: Span> {
    pub starting_span: S,
    pub name: S,
    pub assignments: Vec<(S, Expression<S>)>,
    pub inheritance: Option<Box<Trailer<S>>>,
}

impl<S: Span> StructInitialization<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            pair(
                take_keyword("struct"),
                cut(pair(
                    delimited(space0, parse_name, space0),
                    delimited(
                        pair(context("Missing opening bracket", nom_char('{')), space0),
                        alt((
                            pair(
                                success(vec![]),
                                map(
                                    preceded(
                                        terminated(tag(".."), space0),
                                        map(Trailer::parse, Box::new),
                                    ),
                                    Some,
                                ),
                            ),
                            pair(
                                separated_list0(
                                    nom_char(','),
                                    delimited(
                                        space0,
                                        separated_pair(
                                            parse_name,
                                            delimited(space0, nom_char('='), space0),
                                            Expression::parse,
                                        ),
                                        space0,
                                    ),
                                ),
                                opt(preceded(
                                    pair(
                                        pair(space0, nom_char(',')),
                                        delimited(space0, tag(".."), space0),
                                    ),
                                    map(Trailer::parse, Box::new),
                                )),
                            ),
                        )),
                        pair(space0, context("Missing closing bracket", nom_char('}'))),
                    ),
                )),
            ),
            |(starting_span, (name, (assignments, inheritance)))| Self {
                starting_span,
                name,
                assignments,
                inheritance,
            },
        )(input)
    }

    pub fn get_span(&self) -> &S {
        &self.starting_span
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Litteral<S: Span> {
    Measurement(Measurement<S>),
    Number(Number<S>),
    String(PString<S>),
    List(List<S>),
    Boolean(S, bool),
    Default(S),
}

impl<S: Span> Litteral<S> {
    pub(super) fn parse(input: S) -> VResult<S, Self> {
        alt((
            map(Measurement::parse, Self::Measurement),
            map(Number::parse, Self::Number),
            map(PString::parse, Self::String),
            map(List::parse, Self::List),
            map(take_keyword("true"), |span| Self::Boolean(span, true)),
            map(take_keyword("false"), |span| Self::Boolean(span, false)),
            map(take_keyword("default"), Self::Default),
        ))(input)
    }

    pub fn get_span(&self) -> &S {
        match self {
            Litteral::Measurement(spanable) => spanable.get_span(),
            Litteral::Number(spanable) => spanable.get_span(),
            Litteral::String(spanable) => spanable.get_span(),
            Litteral::List(spanable) => spanable.get_span(),
            Litteral::Boolean(spanable, _) => spanable,
            Litteral::Default(spanable) => spanable,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct List<S: Span> {
    pub starting_span: S,
    pub expressions: Vec<Expression<S>>,
}

impl<S: Span> List<S> {
    fn parse(input: S) -> VResult<S, Self> {
        let (input, (starting_span, expressions)) = terminated(
            pair(
                tag("["),
                separated_list0(nom_char(','), delimited(space0, Expression::parse, space0)),
            ),
            nom_char(']'),
        )(input)?;

        Ok((
            input,
            Self {
                starting_span,
                expressions,
            },
        ))
    }

    pub fn get_span(&self) -> &S {
        &self.starting_span
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct PString<S: Span> {
    pub value: S,
}

impl<S: Span> PString<S> {
    fn parse(input: S) -> VResult<S, Self> {
        let (input, value) = delimited(
            nom_char('"'),
            escaped(
                take_while1(|c| !matches!(c, '\\' | '"')),
                '\\',
                one_of(r#""n\"#),
            ),
            nom_char('"'),
        )(input)?;

        Ok((input, PString { value }))
    }

    pub fn get_span(&self) -> &S {
        &self.value
    }
}

impl<S: Span> ToString for PString<S> {
    fn to_string(&self) -> String {
        let mut char_iterator = self.value.chars().peekable();
        let iterator = std::iter::from_fn(|| {
            char_iterator
                .next()
                .map(|next| (next, char_iterator.peek().copied()))
        });
        let mut string = String::default();

        for (current, next) in iterator {
            if current == '\\' {
                match next {
                    Some('"') => string.push('"'),
                    Some('\n') => string.push('\n'),
                    Some('\\') => string.push('\\'),
                    _ => {} // Shouldn't be possible to get an unrecognized char. This could happen for a downslash at the end of the string though.
                }
            } else {
                string.push(current);
            }
        }

        string
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Measurement<S: Span> {
    pub number: Number<S>,
    pub ty: S,
}

impl<S: Span> Measurement<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        // TODO the unit type parsing is terribly under-tested.
        map(
            separated_pair(Number::parse, space0, Self::parse_type),
            |(number, ty)| Measurement { number, ty },
        )(input)
    }

    fn parse_type(input: S) -> VResult<S, S> {
        fn take_body<S: Span>(input: S) -> VResult<S, S> {
            take_while1(|c: char| {
                c.is_alphanumeric() || matches!(c, '^' | '/' | '_' | '.' | '%' | '<' | '-' | '\\')
            })(input)
        }

        verify(
            map(
                consumed(fold_many0(
                    alt((
                        // This can't handle nested () but none of our supported unit types need that.
                        take_body,
                        delimited(nom_char('('), take_body, nom_char(')')),
                    )),
                    || (),
                    |_, _| (),
                )),
                |(span, _)| span,
            ),
            |input: &S| {
                // The unit type must start with a letter or number.
                input
                    .as_str()
                    .chars()
                    .next()
                    .map(|input| input.is_alphanumeric())
                    .unwrap_or(false)
            },
        )(input)
    }

    pub fn get_span(&self) -> &S {
        self.number.get_span()
    }
}

fn is_digit(c: char) -> bool {
    "0123456789".contains(c)
}

fn parse_integer<S: Span>(input: S) -> VResult<S, S> {
    take_while1(is_digit)(input)
}

fn parse_name<S: Span>(input: S) -> VResult<S, S> {
    fn is_alpha(c: char) -> bool {
        c.is_alphabetic() || matches!(c, '_')
    }

    fn is_alphanumeric(c: char) -> bool {
        is_alpha(c) || c.is_numeric()
    }

    context(
        "Failed to parse name",
        recognize(preceded(take_while1(is_alpha), take_while(is_alphanumeric))),
    )(input)
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Number<S: Span> {
    integer: Option<S>,
    dot: Option<S>,
    fractional: Option<S>,
}

impl<S: Span> Number<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        fn take_dot<S: Span>(input: S) -> VResult<S, S> {
            verify(take_while1(|c| c == '.'), |output: &S| {
                output.as_str().len() == 1
            })(input)
        }

        let (input, (integer, dot, fractional)) = alt((
            tuple((
                map(parse_integer, Some),
                map(delimited(space0, take_dot, space0), Some),
                map(parse_integer, Some),
            )),
            tuple((
                success(None),
                map(delimited(space0, take_dot, space0), Some),
                map(parse_integer, Some),
            )),
            tuple((
                map(parse_integer, Some),
                map(delimited(space0, take_dot, space0), Some),
                success(None),
            )),
            tuple((map(parse_integer, Some), success(None), success(None))),
        ))(input)?;

        Ok((
            input,
            Self {
                integer,
                dot,
                fractional,
            },
        ))
    }

    pub fn get_span(&self) -> &S {
        // We accept '0.0', '.0', '0.', and '.', but not ''. It should not be possible for this to fail.
        self.integer.as_ref().unwrap_or_else(|| {
            self.dot
                .as_ref()
                .unwrap_or_else(|| self.fractional.as_ref().unwrap())
        })
    }

    pub fn to_float<P>(&self) -> Result<P, P::Err>
    where
        P: std::str::FromStr,
    {
        match (self.integer.as_ref(), self.fractional.as_ref()) {
            (None, None) => P::from_str("0.0"),
            (Some(integer), None) => P::from_str(&integer.to_string()),
            (None, Some(fractional)) => P::from_str(&format!(".{}", fractional.to_string())),
            (Some(integer), Some(fractional)) => P::from_str(&format!(
                "{}.{}",
                integer.to_string(),
                fractional.to_string()
            )),
        }
    }
}

fn take_keyword<S: Span>(keyword: &'static str) -> impl FnMut(S) -> VResult<S, S> {
    verify(parse_name::<S>, move |name| {
        matches!(name.compare(keyword), CompareResult::Ok) && name.input_len() == keyword.len()
    })
}

fn is_space(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\r' | '\n')
}

fn consume_space_token<S: Span>(input: S) -> VResult<S, ()> {
    value(
        (),
        alt((
            take_while1(is_space),
            delimited(tag("//"), take_while(|c| c != '\n'), nom_char('\n')),
            delimited(tag("/*"), take_until("*/"), tag("*/")),
        )),
    )(input)
}

fn space0<S: Span>(input: S) -> VResult<S, ()> {
    value((), fold_many0(consume_space_token, || (), |_, _| {}))(input)
}

fn space1<S: Span>(input: S) -> VResult<S, ()> {
    value((), fold_many1(consume_space_token, || (), |_, _| {}))(input)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn span_types() {
        assert_eq!(
            PString::parse(r#""test""#),
            Ok(("", PString { value: "test" }))
        );

        assert_eq!(
            PString::parse(LocatedSpan::new(r#""test""#))
                .unwrap()
                .1
                .value
                .fragment(),
            &"test"
        );
    }

    #[test]
    fn integer() {
        assert_eq!(parse_integer("1234"), Ok(("", "1234")));
        assert_eq!(parse_integer("1234.5678"), Ok((".5678", "1234")));
    }

    #[test]
    fn number() {
        assert_eq!(
            Number::parse("1234"),
            Ok((
                "",
                Number {
                    integer: Some("1234"),
                    dot: None,
                    fractional: None
                }
            ))
        );
        assert_eq!(
            Number::parse(".1234"),
            Ok((
                "",
                Number {
                    integer: None,
                    dot: Some("."),
                    fractional: Some("1234")
                }
            ))
        );
        assert_eq!(
            Number::parse("1234."),
            Ok((
                "",
                Number {
                    integer: Some("1234"),
                    dot: Some("."),
                    fractional: None
                }
            ))
        );
        assert_eq!(
            Number::parse("1234.5678"),
            Ok((
                "",
                Number {
                    integer: Some("1234"),
                    dot: Some("."),
                    fractional: Some("5678")
                }
            ))
        );
        assert_eq!(
            Number::parse("1234;"),
            Ok((
                ";",
                Number {
                    integer: Some("1234"),
                    dot: None,
                    fractional: None
                }
            ))
        );
        assert!(Number::parse("").is_err());
        assert!(Number::parse(".").is_err());
    }

    #[test]
    fn measurement() {
        assert_eq!(
            Measurement::parse("22 m"),
            Ok((
                "",
                Measurement {
                    number: Number {
                        integer: Some("22"),
                        dot: None,
                        fractional: None
                    },
                    ty: "m"
                }
            ))
        );

        assert_eq!(
            Measurement::parse("22m"),
            Ok((
                "",
                Measurement {
                    number: Number {
                        integer: Some("22"),
                        dot: None,
                        fractional: None
                    },
                    ty: "m"
                }
            ))
        );

        assert_eq!(
            Measurement::parse("22.44m"),
            Ok((
                "",
                Measurement {
                    number: Number {
                        integer: Some("22"),
                        dot: Some("."),
                        fractional: Some("44"),
                    },
                    ty: "m"
                }
            ))
        );
    }

    #[test]
    fn test_string() {
        assert_eq!(
            PString::parse(r#""test""#),
            Ok(("", PString { value: "test" }))
        );
        assert_eq!(
            PString::parse(r#"" \n \\ \" ""#),
            Ok((
                "",
                PString {
                    value: r#" \n \\ \" "#,
                }
            ))
        );
    }

    #[test]
    fn list() {
        assert_eq!(
            List::parse("[]"),
            Ok((
                "",
                List {
                    starting_span: "[",
                    expressions: vec![]
                }
            ))
        );
        assert_eq!(
            List::parse("[one]"),
            Ok((
                "",
                List {
                    starting_span: "[",
                    expressions: vec![Expression::Buffer(Comparison::None(
                        ArithmeticExpression::Term(Term::Trailer(Trailer::None(Factor::Variable(
                            "one"
                        ))))
                    ))]
                }
            ))
        );
        assert_eq!(
            List::parse("[one, two]"),
            Ok((
                "",
                List {
                    starting_span: "[",
                    expressions: vec![
                        Expression::Buffer(Comparison::None(ArithmeticExpression::Term(
                            Term::Trailer(Trailer::None(Factor::Variable("one"),))
                        ))),
                        Expression::Buffer(Comparison::None(ArithmeticExpression::Term(
                            Term::Trailer(Trailer::None(Factor::Variable("two")))
                        )))
                    ]
                }
            ))
        );
        assert_eq!(
            List::parse("[one, two, three]"),
            Ok((
                "",
                List {
                    starting_span: "[",
                    expressions: vec![
                        Expression::Buffer(Comparison::None(ArithmeticExpression::Term(
                            Term::Trailer(Trailer::None(Factor::Variable("one"),))
                        ))),
                        Expression::Buffer(Comparison::None(ArithmeticExpression::Term(
                            Term::Trailer(Trailer::None(Factor::Variable("two"),))
                        ))),
                        Expression::Buffer(Comparison::None(ArithmeticExpression::Term(
                            Term::Trailer(Trailer::None(Factor::Variable("three"),))
                        )))
                    ]
                }
            ))
        );
        assert!(List::parse("[one, two, three, ]").is_err());
        assert!(List::parse("[one two]").is_err());
    }

    #[test]
    fn name() {
        assert_eq!(parse_name("name"), Ok(("", "name")));
        assert_eq!(parse_name("name1"), Ok(("", "name1")));
        assert_eq!(parse_name("name_1"), Ok(("", "name_1")));
        assert_eq!(parse_name("_name"), Ok(("", "_name")));
        assert!(parse_name("1name").is_err());
    }

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

    #[test]
    fn litteral() {
        // Measurement(Measurement<S>),
        assert_eq!(
            Litteral::parse("1234.5678m"),
            Ok((
                "",
                Litteral::Measurement(Measurement {
                    number: Number {
                        integer: Some("1234"),
                        dot: Some("."),
                        fractional: Some("5678")
                    },
                    ty: "m"
                })
            ))
        );

        // Number(Number<S>),
        assert_eq!(
            Litteral::parse("1234.5678"),
            Ok((
                "",
                Litteral::Number(Number {
                    integer: Some("1234"),
                    dot: Some("."),
                    fractional: Some("5678")
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
    }

    #[test]
    fn expression() {
        assert_eq!(
            Expression::parse("a"),
            Ok((
                "",
                Expression::Buffer(Comparison::None(ArithmeticExpression::Term(Term::Trailer(
                    Trailer::None(Factor::Variable("a"))
                ))),)
            ))
        );

        assert_eq!(
            Expression::parse("a || b"),
            Ok((
                "",
                Expression::Or(
                    Box::new(Expression::Buffer(Comparison::None(
                        ArithmeticExpression::Term(Term::Trailer(Trailer::None(Factor::Variable(
                            "a"
                        ))))
                    ))),
                    Comparison::None(ArithmeticExpression::Term(Term::Trailer(Trailer::None(
                        Factor::Variable("b")
                    ))))
                )
            ))
        );

        assert_eq!(
            Expression::parse("a && b"),
            Ok((
                "",
                Expression::And(
                    Box::new(Expression::Buffer(Comparison::None(
                        ArithmeticExpression::Term(Term::Trailer(Trailer::None(Factor::Variable(
                            "a"
                        ))))
                    ))),
                    Comparison::None(ArithmeticExpression::Term(Term::Trailer(Trailer::None(
                        Factor::Variable("b")
                    ))))
                )
            ))
        );
    }

    #[test]
    fn comparison() {
        assert_eq!(
            Comparison::parse("a"),
            Ok((
                "",
                Comparison::None(ArithmeticExpression::Term(Term::Trailer(Trailer::None(
                    Factor::Variable("a")
                ))))
            ))
        );

        assert_eq!(
            Comparison::parse("a < b"),
            Ok((
                "",
                Comparison::LessThan(
                    Box::new(Comparison::None(ArithmeticExpression::Term(Term::Trailer(
                        Trailer::None(Factor::Variable("a"))
                    )))),
                    ArithmeticExpression::Term(Term::Trailer(Trailer::None(Factor::Variable("b")))),
                )
            ))
        );

        assert_eq!(
            Comparison::parse("a <= b"),
            Ok((
                "",
                Comparison::LessThanEqual(
                    Box::new(Comparison::None(ArithmeticExpression::Term(Term::Trailer(
                        Trailer::None(Factor::Variable("a"))
                    )))),
                    ArithmeticExpression::Term(Term::Trailer(Trailer::None(Factor::Variable("b")))),
                )
            ))
        );

        assert_eq!(
            Comparison::parse("a == b"),
            Ok((
                "",
                Comparison::Equal(
                    Box::new(Comparison::None(ArithmeticExpression::Term(Term::Trailer(
                        Trailer::None(Factor::Variable("a"))
                    )))),
                    ArithmeticExpression::Term(Term::Trailer(Trailer::None(Factor::Variable("b")))),
                )
            ))
        );

        assert_eq!(
            Comparison::parse("a >= b"),
            Ok((
                "",
                Comparison::GreaterThanEqual(
                    Box::new(Comparison::None(ArithmeticExpression::Term(Term::Trailer(
                        Trailer::None(Factor::Variable("a"))
                    )))),
                    ArithmeticExpression::Term(Term::Trailer(Trailer::None(Factor::Variable("b")))),
                )
            ))
        );

        assert_eq!(
            Comparison::parse("a > b"),
            Ok((
                "",
                Comparison::GreaterThan(
                    Box::new(Comparison::None(ArithmeticExpression::Term(Term::Trailer(
                        Trailer::None(Factor::Variable("a"))
                    )))),
                    ArithmeticExpression::Term(Term::Trailer(Trailer::None(Factor::Variable("b")))),
                )
            ))
        );
    }

    #[test]
    fn arithmetic_expression() {
        // Addition(Box<Self>, Term<S>),
        assert_eq!(
            ArithmeticExpression::parse("a + b"),
            Ok((
                "",
                ArithmeticExpression::Addition(
                    Box::new(ArithmeticExpression::Term(Term::Trailer(Trailer::None(
                        Factor::Variable("a")
                    )))),
                    Term::Trailer(Trailer::None(Factor::Variable("b")))
                )
            ))
        );
        // Subtraction(Box<Self>, Term<S>),
        assert_eq!(
            ArithmeticExpression::parse("a - b"),
            Ok((
                "",
                ArithmeticExpression::Subtraction(
                    Box::new(ArithmeticExpression::Term(Term::Trailer(Trailer::None(
                        Factor::Variable("a")
                    )))),
                    Term::Trailer(Trailer::None(Factor::Variable("b")))
                )
            ))
        );
        // Term(Term<S>),
        assert_eq!(
            ArithmeticExpression::parse("a"),
            Ok((
                "",
                ArithmeticExpression::Term(Term::Trailer(Trailer::None(Factor::Variable("a"))))
            ))
        );

        // Order of operation tests.
        assert_eq!(
            ArithmeticExpression::parse("+a + b"),
            Ok((
                "",
                ArithmeticExpression::Addition(
                    Box::new(ArithmeticExpression::Term(Term::Trailer(Trailer::None(
                        Factor::UnaryPlus(Box::new(Factor::Variable("a")))
                    )))),
                    Term::Trailer(Trailer::None(Factor::Variable("b")))
                )
            ))
        );
    }

    #[test]
    fn term() {
        // Multiply(Box<Self>, Factor<S>),
        assert_eq!(
            Term::parse("x * y"),
            Ok((
                "",
                Term::Multiply(
                    Box::new(Term::Trailer(Trailer::None(Factor::Variable("x")))),
                    Trailer::None(Factor::Variable("y"))
                )
            ))
        );
        // Divide(Box<Self>, Factor<S>),
        assert_eq!(
            Term::parse("x / y"),
            Ok((
                "",
                Term::Divide(
                    Box::new(Term::Trailer(Trailer::None(Factor::Variable("x")))),
                    Trailer::None(Factor::Variable("y"))
                )
            ))
        );
        // Factor(Factor<S>),
        assert_eq!(
            Term::parse("x"),
            Ok(("", Term::Trailer(Trailer::None(Factor::Variable("x")))))
        );

        // Range(Range<S>),
        assert_eq!(
            Term::parse("a..b"),
            Ok((
                "",
                Term::Range(Range {
                    comparison_operator: "..",
                    upper_bound: Some(Trailer::None(Factor::Variable("b"))),
                    lower_bound: Some(Trailer::None(Factor::Variable("a"))),
                    upper_bound_is_inclusive: false,
                })
            ))
        );
        assert_eq!(
            Term::parse(".."),
            Ok((
                "",
                Term::Range(Range {
                    comparison_operator: "..",
                    upper_bound: None,
                    lower_bound: None,
                    upper_bound_is_inclusive: false,
                })
            ))
        );
    }

    #[test]
    fn range() {
        assert_eq!(
            Range::parse(".."),
            Ok((
                "",
                Range {
                    comparison_operator: "..",
                    upper_bound: None,
                    upper_bound_is_inclusive: false,
                    lower_bound: None,
                }
            ))
        );
        assert_eq!(
            Range::parse("..="),
            Ok((
                "",
                Range {
                    comparison_operator: "..=",
                    upper_bound: None,
                    upper_bound_is_inclusive: true,
                    lower_bound: None,
                }
            ))
        );
        assert_eq!(
            Range::parse("a..b"),
            Ok((
                "",
                Range {
                    comparison_operator: "..",
                    upper_bound: Some(Trailer::None(Factor::Variable("b"))),
                    lower_bound: Some(Trailer::None(Factor::Variable("a"))),
                    upper_bound_is_inclusive: false,
                }
            ))
        );

        assert_eq!(*Range::parse("..").unwrap().1.get_span(), "..");
        assert_eq!(*Range::parse("a..").unwrap().1.get_span(), "a");
    }

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

    #[test]
    fn variable_type() {
        assert_eq!(
            VariableType::parse("Length"),
            Ok(("", VariableType::Measurement("Length")))
        );
        assert_eq!(
            VariableType::parse("Angle"),
            Ok(("", VariableType::Measurement("Angle")))
        );
        assert_eq!(
            VariableType::parse("Number"),
            Ok(("", VariableType::Number))
        );
        assert_eq!(VariableType::parse("List"), Ok(("", VariableType::List)));
        assert_eq!(
            VariableType::parse("Boolean"),
            Ok(("", VariableType::Boolean))
        );
        assert_eq!(
            VariableType::parse("struct MyStruct"),
            Ok(("", VariableType::Struct("MyStruct")))
        );
    }

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
                    ty: Some(VariableType::Measurement("Length")),
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
                            ty: Some(VariableType::Measurement("Length"))
                        },
                        AssignableVariable {
                            name: "b",
                            ty: Some(VariableType::Measurement("Angle"))
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
                        ty: Some(VariableType::Measurement("Length")),
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

    #[test]
    fn statement_return() {
        assert_eq!(
            Return::parse("return"),
            Ok((
                "",
                Return {
                    starting_span: "return",
                    expression: None
                }
            ))
        );

        assert_eq!(
            Return::parse("return a"),
            Ok((
                "",
                Return {
                    starting_span: "return",
                    expression: Some(Expression::parse("a").unwrap().1)
                }
            ))
        );
        assert!(Return::parse("returna").is_err());
    }

    #[test]
    fn statement_if() {
        assert_eq!(
            If::parse("if a {}"),
            Ok((
                "",
                If {
                    starting_span: "if",
                    expression: Expression::parse("a").unwrap().1,
                    block: Block { statements: vec![] },
                    else_statement: None
                }
            ))
        );

        assert_eq!(
            If::parse("if a {} else {}"),
            Ok((
                "",
                If {
                    starting_span: "if",
                    expression: Expression::parse("a").unwrap().1,
                    block: Block { statements: vec![] },
                    else_statement: Some(Else::Else(Block { statements: vec![] }))
                }
            ))
        );

        assert_eq!(
            If::parse("if a {} else if b {}"),
            Ok((
                "",
                If {
                    starting_span: "if",
                    expression: Expression::parse("a").unwrap().1,
                    block: Block { statements: vec![] },
                    else_statement: Some(Else::IfElse(Box::new(If {
                        starting_span: "if",
                        expression: Expression::parse("b").unwrap().1,
                        block: Block { statements: vec![] },
                        else_statement: None,
                    })))
                }
            ))
        );

        assert!(If::parse("ifa {}").is_err());
        assert_eq!(If::parse("if a {} elseif").unwrap().0, "elseif");
        assert!(If::parse("if a {} else = 0").is_err());
        assert_eq!(If::parse("if a {} var = 0").unwrap().0, "var = 0");
    }

    #[test]
    fn match_branch() {
        assert_eq!(
            MatchBranch::parse("1 => {}"),
            Ok((
                "",
                MatchBranch {
                    litteral: Litteral::parse("1").unwrap().1,
                    block: Block { statements: vec![] }
                }
            ))
        );

        assert_eq!(
            MatchBranch::parse("1 => false"),
            Ok((
                "",
                MatchBranch {
                    litteral: Litteral::parse("1").unwrap().1,
                    block: Block {
                        statements: vec![BlockStatement::Open(Statement::Expression(
                            Expression::parse("false").unwrap().1
                        ))]
                    }
                }
            ))
        );
    }

    #[test]
    fn statement_match() {
        assert_eq!(
            Match::parse("match a {}"),
            Ok((
                "",
                Match {
                    starting_span: "match",
                    expression: Expression::parse("a").unwrap().1,
                    branches: vec![]
                }
            ))
        );

        assert_eq!(
            Match::parse("match a { 1 => {} }"),
            Ok((
                "",
                Match {
                    starting_span: "match",
                    expression: Expression::parse("a").unwrap().1,
                    branches: vec![MatchBranch {
                        litteral: Litteral::parse("1").unwrap().1,
                        block: Block { statements: vec![] }
                    }]
                }
            ))
        );

        assert_eq!(
            Match::parse("match a { 1 => {}, 2 => {}}"),
            Ok((
                "",
                Match {
                    starting_span: "match",
                    expression: Expression::parse("a").unwrap().1,
                    branches: vec![
                        MatchBranch {
                            litteral: Litteral::parse("1").unwrap().1,
                            block: Block { statements: vec![] }
                        },
                        MatchBranch {
                            litteral: Litteral::parse("2").unwrap().1,
                            block: Block { statements: vec![] }
                        }
                    ]
                }
            ))
        );

        assert!(Match::parse("match a { not a branch }").is_err(),);
    }

    #[test]
    fn statement_for() {
        assert_eq!(
            For::parse("for a in b {}"),
            Ok((
                "",
                For {
                    starting_span: "for",
                    name: None,
                    variable_assignment: Assignable::parse("a").unwrap().1,
                    iterator_expression: Expression::parse("b").unwrap().1,
                    block: Block { statements: vec![] }
                }
            ))
        );

        assert_eq!(
            For::parse("'my_for_loop: for a in b {}"),
            Ok((
                "",
                For {
                    starting_span: "for",
                    name: Some("my_for_loop"),
                    variable_assignment: Assignable::parse("a").unwrap().1,
                    iterator_expression: Expression::parse("b").unwrap().1,
                    block: Block { statements: vec![] }
                }
            ))
        );
    }

    #[test]
    fn statement_while() {
        assert_eq!(
            While::parse("while a {}"),
            Ok((
                "",
                While {
                    starting_span: "while",
                    name: None,
                    expression: Expression::parse("a").unwrap().1,
                    block: Block { statements: vec![] }
                }
            ))
        );

        assert_eq!(
            While::parse("'my_while_loop: while a {}"),
            Ok((
                "",
                While {
                    starting_span: "while",
                    name: Some("my_while_loop"),
                    expression: Expression::parse("a").unwrap().1,
                    block: Block { statements: vec![] }
                }
            ))
        );
    }

    #[test]
    fn statement_loop() {
        assert_eq!(
            Loop::parse("loop {}"),
            Ok((
                "",
                Loop {
                    starting_span: "loop",
                    name: None,
                    block: Block { statements: vec![] }
                }
            ))
        );

        assert_eq!(
            Loop::parse("'my_loop: loop {}"),
            Ok((
                "",
                Loop {
                    starting_span: "loop",
                    name: Some("my_loop"),
                    block: Block { statements: vec![] }
                }
            ))
        );
    }

    #[test]
    fn statement_break() {
        assert_eq!(
            Break::parse("break"),
            Ok((
                "",
                Break {
                    starting_span: "break",
                    loop_name: None,
                    expression: None
                }
            ))
        );

        assert_eq!(
            Break::parse("break a"),
            Ok((
                "",
                Break {
                    starting_span: "break",
                    loop_name: None,
                    expression: Some(Expression::parse("a").unwrap().1)
                }
            ))
        );

        assert_eq!(
            Break::parse("break 'my_loop"),
            Ok((
                "",
                Break {
                    starting_span: "break",
                    loop_name: Some("my_loop"),
                    expression: None
                }
            ))
        );

        assert_eq!(
            Break::parse("break 'my_loop a"),
            Ok((
                "",
                Break {
                    starting_span: "break",
                    loop_name: Some("my_loop"),
                    expression: Some(Expression::parse("a").unwrap().1)
                }
            ))
        );
    }

    #[test]
    fn statement_continue() {
        assert_eq!(
            Continue::parse("continue"),
            Ok((
                "",
                Continue {
                    starting_span: "continue",

                    loop_name: None
                }
            ))
        );
        assert_eq!(
            Continue::parse("continue 'my_loop"),
            Ok((
                "",
                Continue {
                    starting_span: "continue",
                    loop_name: Some("my_loop")
                }
            ))
        );
    }

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
    }

    #[test]
    fn block() {
        assert_eq!(Block::parse("{}"), Ok(("", Block { statements: vec![] })));
        assert_eq!(
            Block::parse("{;}"),
            Ok((
                "",
                Block {
                    statements: vec![BlockStatement::Blank(";")],
                }
            ))
        );

        assert_eq!(
            Block::parse("{ break; a = b; }"),
            Ok((
                "",
                Block {
                    statements: vec![
                        BlockStatement::Closed(Statement::Break(Break {
                            starting_span: "break",
                            loop_name: None,
                            expression: None
                        })),
                        BlockStatement::Closed(Statement::Assign(Assign {
                            starting_span: "a",
                            is_new: false,
                            to_assign: Assignable::Variable(AssignableVariable {
                                name: "a",
                                ty: None,
                            }),
                            statement: Box::new(Statement::parse("b").unwrap().1)
                        }))
                    ],
                }
            ))
        );

        assert_eq!(
            Block::parse("{ a = b }"),
            Ok((
                "",
                Block {
                    statements: vec![BlockStatement::Open(Statement::Assign(Assign {
                        starting_span: "a",
                        is_new: false,
                        to_assign: Assignable::Variable(AssignableVariable {
                            name: "a",
                            ty: None,
                        }),
                        statement: Box::new(Statement::parse("b").unwrap().1)
                    }))],
                }
            ))
        );

        assert_eq!(
            Block::parse("{ break; a = b }"),
            Ok((
                "",
                Block {
                    statements: vec![
                        BlockStatement::Closed(Statement::Break(Break {
                            starting_span: "break",
                            loop_name: None,
                            expression: None
                        })),
                        BlockStatement::Open(Statement::Assign(Assign {
                            starting_span: "a",
                            is_new: false,
                            to_assign: Assignable::Variable(AssignableVariable {
                                name: "a",
                                ty: None,
                            }),
                            statement: Box::new(Statement::parse("b").unwrap().1)
                        }))
                    ],
                }
            ))
        );

        assert_eq!(
            Block::parse("{ loop {} }"),
            Ok((
                "",
                Block {
                    statements: vec![BlockStatement::Open(Statement::Loop(Loop {
                        starting_span: "loop",
                        name: None,
                        block: Block { statements: vec![] }
                    }))],
                }
            ))
        );

        assert_eq!(
            Block::parse("{ loop {} return }"),
            Ok((
                "",
                Block {
                    statements: vec![
                        BlockStatement::Open(Statement::Loop(Loop {
                            starting_span: "loop",
                            name: None,
                            block: Block { statements: vec![] }
                        })),
                        BlockStatement::Open(Statement::Return(Return {
                            starting_span: "return",
                            expression: None
                        }))
                    ],
                }
            ))
        );

        assert!(Block::parse("{ break a = b }").is_err());
        assert!(Block::parse("{ break a = b; }").is_err());
    }

    #[test]
    fn member_variable_constraint() {
        assert_eq!(
            MemberVariableConstraint::parse("integer"),
            Ok(("", MemberVariableConstraint::Integer))
        );

        assert_eq!(
            MemberVariableConstraint::parse("min(0)"),
            Ok((
                "",
                MemberVariableConstraint::Min(Litteral::Number(Number {
                    integer: Some("0"),
                    dot: None,
                    fractional: None
                }))
            ))
        );
        assert_eq!(
            MemberVariableConstraint::parse("max(0)"),
            Ok((
                "",
                MemberVariableConstraint::Max(Litteral::Number(Number {
                    integer: Some("0"),
                    dot: None,
                    fractional: None
                }))
            ))
        );
        assert_eq!(
            MemberVariableConstraint::parse("enum(0, 1, 2)"),
            Ok((
                "",
                MemberVariableConstraint::Enum(vec![
                    Litteral::Number(Number {
                        integer: Some("0"),
                        dot: None,
                        fractional: None
                    }),
                    Litteral::Number(Number {
                        integer: Some("1"),
                        dot: None,
                        fractional: None
                    }),
                    Litteral::Number(Number {
                        integer: Some("2"),
                        dot: None,
                        fractional: None
                    })
                ])
            ))
        );
    }

    #[test]
    fn member_variable_constraint_list() {
        assert_eq!(
            MemberVariableConstraintList::parse("#[]"),
            Ok((
                "",
                MemberVariableConstraintList {
                    constraints: vec![]
                }
            ))
        );

        assert_eq!(
            MemberVariableConstraintList::parse("#[integer]"),
            Ok((
                "",
                MemberVariableConstraintList {
                    constraints: vec![MemberVariableConstraint::Integer]
                }
            ))
        );

        assert_eq!(
            MemberVariableConstraintList::parse("#[integer, integer]"),
            Ok((
                "",
                MemberVariableConstraintList {
                    constraints: vec![
                        MemberVariableConstraint::Integer,
                        MemberVariableConstraint::Integer
                    ]
                }
            ))
        );

        assert_eq!(
            MemberVariableConstraintList::parse("#[integer, integer, integer]"),
            Ok((
                "",
                MemberVariableConstraintList {
                    constraints: vec![
                        MemberVariableConstraint::Integer,
                        MemberVariableConstraint::Integer,
                        MemberVariableConstraint::Integer
                    ]
                }
            ))
        );
    }

    #[test]
    fn member_variable() {
        assert_eq!(
            MemberVariable::parse("variable: Number"),
            Ok((
                "",
                MemberVariable {
                    name: "variable",
                    ty: VariableType::Number,
                    constraints: None,
                    default_value: None,
                }
            ))
        );

        assert_eq!(
            MemberVariable::parse("variable: Number = 2"),
            Ok((
                "",
                MemberVariable {
                    name: "variable",
                    ty: VariableType::Number,
                    constraints: None,
                    default_value: Some(Litteral::Number(Number {
                        integer: Some("2"),
                        dot: None,
                        fractional: None
                    })),
                }
            ))
        );

        assert_eq!(
            MemberVariable::parse("#[integer] variable: Number = 2"),
            Ok((
                "",
                MemberVariable {
                    name: "variable",
                    ty: VariableType::Number,
                    constraints: Some(MemberVariableConstraintList {
                        constraints: vec![MemberVariableConstraint::Integer]
                    }),
                    default_value: Some(Litteral::Number(Number {
                        integer: Some("2"),
                        dot: None,
                        fractional: None
                    })),
                }
            ))
        );
    }

    #[test]
    fn named_block() {
        assert_eq!(
            NamedBlock::parse("my_thing() {}"),
            Ok((
                "",
                NamedBlock {
                    name: "my_thing",
                    parameter_span: "(",
                    parameters: vec![],
                    block: Block { statements: vec![] }
                }
            ))
        );

        assert_eq!(
            NamedBlock::parse("my_thing(one: Length) {}"),
            Ok((
                "",
                NamedBlock {
                    name: "my_thing",
                    parameter_span: "(",
                    parameters: vec![MemberVariable {
                        name: "one",
                        ty: VariableType::Measurement("Length"),
                        constraints: None,
                        default_value: None
                    }],
                    block: Block { statements: vec![] }
                }
            ))
        );

        assert_eq!(
            NamedBlock::parse("my_thing(one: Length, two: Angle = 2) {}"),
            Ok((
                "",
                NamedBlock {
                    name: "my_thing",
                    parameter_span: "(",
                    parameters: vec![
                        MemberVariable {
                            name: "one",
                            ty: VariableType::Measurement("Length"),
                            constraints: None,
                            default_value: None
                        },
                        MemberVariable {
                            name: "two",
                            ty: VariableType::Measurement("Angle"),
                            constraints: None,
                            default_value: Some(Litteral::Number(Number {
                                integer: Some("2"),
                                dot: None,
                                fractional: None
                            }))
                        }
                    ],
                    block: Block { statements: vec![] }
                }
            ))
        );

        assert_eq!(
            NamedBlock::parse("my_thing(one: Length, two: Angle = 2, #[integer] three: Number) {}"),
            Ok((
                "",
                NamedBlock {
                    name: "my_thing",
                    parameter_span: "(",
                    parameters: vec![
                        MemberVariable {
                            name: "one",
                            ty: VariableType::Measurement("Length"),
                            constraints: None,
                            default_value: None
                        },
                        MemberVariable {
                            name: "two",
                            ty: VariableType::Measurement("Angle"),
                            constraints: None,
                            default_value: Some(Litteral::Number(Number {
                                integer: Some("2"),
                                dot: None,
                                fractional: None
                            }))
                        },
                        MemberVariable {
                            name: "three",
                            ty: VariableType::Number,
                            constraints: Some(MemberVariableConstraintList {
                                constraints: vec![MemberVariableConstraint::Integer]
                            }),
                            default_value: None,
                        }
                    ],
                    block: Block { statements: vec![] }
                }
            ))
        );

        assert_eq!(
            NamedBlock::parse_with_return_type("my_thing() -> struct T {}"),
            Ok((
                "",
                (
                    NamedBlock {
                        name: "my_thing",
                        parameter_span: "(",
                        parameters: vec![],
                        block: Block { statements: vec![] }
                    },
                    VariableType::Struct("T")
                )
            ))
        );
    }

    #[test]
    fn parse_struct() {
        assert_eq!(
            Struct::parse("struct MyStruct {}"),
            Ok((
                "",
                Struct {
                    name: "MyStruct",
                    members: vec![]
                }
            ))
        );

        assert_eq!(
            Struct::parse("struct MyStruct { a: Length }"),
            Ok((
                "",
                Struct {
                    name: "MyStruct",
                    members: vec![MemberVariable {
                        name: "a",
                        ty: VariableType::Measurement("Length"),
                        constraints: None,
                        default_value: None
                    }]
                }
            ))
        );

        assert_eq!(
            Struct::parse("struct MyStruct { a: Length, #[integer] b: Angle = true }"),
            Ok((
                "",
                Struct {
                    name: "MyStruct",
                    members: vec![
                        MemberVariable {
                            name: "a",
                            ty: VariableType::Measurement("Length"),
                            constraints: None,
                            default_value: None
                        },
                        MemberVariable {
                            name: "b",
                            ty: VariableType::Measurement("Angle"),
                            constraints: Some(MemberVariableConstraintList {
                                constraints: vec![MemberVariableConstraint::Integer]
                            }),
                            default_value: Some(Litteral::Boolean("true", true))
                        }
                    ]
                }
            ))
        );
    }

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

    #[test]
    fn solid() {
        assert!(Solid::parse("solid my_solid() -> Length {}").is_err());
        assert_eq!(
            Solid::parse("solid my_solid() {}"),
            Ok((
                "",
                Solid {
                    starting_span: "solid",
                    named_block: NamedBlock {
                        name: "my_solid",
                        parameter_span: "(",
                        parameters: vec![],
                        block: Block { statements: vec![] }
                    },
                }
            ))
        );
    }

    #[test]
    fn space_and_comments() {
        assert_eq!(space0(" \t\r\n"), Ok(("", ())));
        assert_eq!(
            space0(" // A line comment. \n after_the_line"),
            Ok(("after_the_line", ()))
        );
        assert_eq!(
            space0(" /* a block comment. */ after_the_comment"),
            Ok(("after_the_comment", ()))
        );

        assert_eq!(
            space0(" /* a block comment. */ /* another comment */ after_the_comment"),
            Ok(("after_the_comment", ()))
        );
        assert_eq!(space0(""), Ok(("", ())));

        assert_eq!(space1(" \t\r\n"), Ok(("", ())));
        assert_eq!(
            space1(" // A line comment. \n after_the_line"),
            Ok(("after_the_line", ()))
        );
        assert_eq!(
            space1(" /* a block comment. */ after_the_comment"),
            Ok(("after_the_comment", ()))
        );

        assert_eq!(
            space1(" /* a block comment. */ /* another comment */ after_the_comment"),
            Ok(("after_the_comment", ()))
        );
        assert!(space1("").is_err());
    }

    #[test]
    fn import() {
        assert_eq!(
            Import::parse("import path::to::module;"),
            Ok((
                "",
                Import {
                    path: vec!["path", "to", "module"],
                    external: false
                }
            ))
        );
        assert_eq!(
            Import::parse("import extern path::to::module;"),
            Ok((
                "",
                Import {
                    path: vec!["path", "to", "module"],
                    external: true
                }
            ))
        );
    }

    #[test]
    fn root_element() {
        // Import(Import<S>),
        assert_eq!(
            RootElement::parse("import path::to::module;"),
            Ok((
                "",
                RootElement::Import(Import {
                    path: vec!["path", "to", "module"],
                    external: false
                })
            ))
        );

        // Struct(Struct<S>),
        assert_eq!(
            RootElement::parse("struct MyStruct {}"),
            Ok((
                "",
                RootElement::Struct(Struct {
                    name: "MyStruct",
                    members: vec![]
                })
            ))
        );

        // Sketch(Sketch<S>),
        assert_eq!(
            RootElement::parse("sketch my_sketch() {}"),
            Ok((
                "",
                RootElement::Sketch(Sketch {
                    starting_span: "sketch",
                    named_block: NamedBlock {
                        name: "my_sketch",
                        parameter_span: "(",
                        parameters: vec![],
                        block: Block { statements: vec![] }
                    },
                })
            ))
        );

        // Solid(Solid<S>),
        assert_eq!(
            RootElement::parse("solid my_solid() {}"),
            Ok((
                "",
                RootElement::Solid(Solid {
                    starting_span: "solid",
                    named_block: NamedBlock {
                        name: "my_solid",
                        parameter_span: "(",
                        parameters: vec![],
                        block: Block { statements: vec![] }
                    },
                })
            ))
        );

        // Function(NamedBlock<S>),
        assert_eq!(
            RootElement::parse("function my_function() -> Length {}"),
            Ok((
                "",
                RootElement::Function(Function {
                    starting_span: "function",
                    named_block: NamedBlock {
                        name: "my_function",
                        parameter_span: "(",
                        parameters: vec![],
                        block: Block { statements: vec![] }
                    },
                    return_type: VariableType::Measurement("Length")
                })
            ))
        );
    }

    #[test]
    fn file_ast() {
        assert_eq!(
            FileAST::parse(
                r#"
            /// My Struct
            struct MyStruct {}
    
            /* My Sketch */
            sketch my_sketch() {}
            solid my_solid() {}
            function my_function() -> Length {}
"#
            ),
            Ok((
                "",
                FileAST {
                    root_elements: vec![
                        RootElement::Struct(Struct {
                            name: "MyStruct",
                            members: vec![]
                        }),
                        RootElement::Sketch(Sketch {
                            starting_span: "sketch",
                            named_block: NamedBlock {
                                name: "my_sketch",
                                parameter_span: "(",
                                parameters: vec![],
                                block: Block { statements: vec![] }
                            },
                        }),
                        RootElement::Solid(Solid {
                            starting_span: "solid",
                            named_block: NamedBlock {
                                name: "my_solid",
                                parameter_span: "(",
                                parameters: vec![],
                                block: Block { statements: vec![] }
                            },
                        }),
                        RootElement::Function(Function {
                            starting_span: "function",
                            named_block: NamedBlock {
                                name: "my_function",
                                parameter_span: "(",
                                parameters: vec![],
                                block: Block { statements: vec![] }
                            },
                            return_type: VariableType::Measurement("Length")
                        }),
                    ]
                }
            ))
        );
    }
}
