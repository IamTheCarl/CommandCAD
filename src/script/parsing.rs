use std::ops::{RangeFrom, RangeTo};

use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, take_until, take_while, take_while1},
    character::complete::{char as nom_char, one_of},
    combinator::{all_consuming, cut, flat_map, map, opt, recognize, success, value, verify},
    error::context,
    multi::{fold_many0, fold_many1, many0, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    CompareResult, IResult,
};
use nom_locate::LocatedSpan;

// TODO currently add comments are just discarded.
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
{
}

impl<'a> Span for &'a str {}
impl<'a> Span for LocatedSpan<&'a str> {}
impl Span for imstr::ImString {}
impl Span for LocatedSpan<imstr::ImString> {}

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
    Widget(Widget<S>),
    Function(Function<S>),
}

impl<S: Span> RootElement<S> {
    fn parse(input: S) -> VResult<S, Self> {
        alt((
            map(Import::parse, Self::Import),
            map(Struct::parse, Self::Struct),
            map(Sketch::parse, Self::Sketch),
            map(Widget::parse, Self::Widget),
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

#[derive(Debug, Eq, PartialEq)]
pub struct Struct<S: Span> {
    pub name: S,
    pub assignments: Vec<MemberVariable<S>>,
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
                        separated_list0(
                            nom_char(','),
                            delimited(space0, MemberVariable::parse, space0),
                        ),
                        nom_char('}'),
                    ),
                ),
            ),
            |(name, assignments)| Self { name, assignments },
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Sketch<S: Span> {
    pub named_block: NamedBlock<S>,
}

impl<S: Span> Sketch<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            preceded(pair(take_keyword("sketch"), space1), NamedBlock::parse),
            |named_block| Self { named_block },
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Widget<S: Span> {
    pub named_block: NamedBlock<S>,
}

impl<S: Span> Widget<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            preceded(pair(take_keyword("widget"), space1), NamedBlock::parse),
            |named_block| Self { named_block },
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Function<S: Span> {
    pub named_block: NamedBlock<S>,
    pub return_type: VariableType<S>,
}

impl<S: Span> Function<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            preceded(
                pair(take_keyword("function"), space1),
                NamedBlock::parse_with_return_type,
            ),
            |(named_block, return_type)| Self {
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

#[derive(Debug, Eq, PartialEq)]
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

#[derive(Debug, Eq, PartialEq)]
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

#[derive(Debug, Eq, PartialEq)]
pub struct NamedBlock<S: Span> {
    pub name: S,
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
                    delimited(
                        pair(space0, nom_char('(')),
                        separated_list0(
                            nom_char(','),
                            delimited(space0, MemberVariable::parse, space0),
                        ),
                        pair(nom_char(')'), space0),
                    ),
                ),
                delimited(space0, return_type_parser, space0),
                Block::parse,
            )),
            |(name, parameters, return_type, block)| {
                (
                    Self {
                        name,
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
    Blank,
}

impl<S: Span> BlockStatement<S> {
    fn parse(input: S) -> VResult<S, Self> {
        alt((
            map(
                delimited(space0, Statement::parse, pair(space0, nom_char(';'))),
                Self::Closed,
            ),
            map(preceded(space0, Statement::parse), Self::Open),
            value(Self::Blank, pair(space0, nom_char(';'))),
        ))(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Block<S: Span> {
    pub statements: Vec<BlockStatement<S>>,
}

impl<S: Span> Block<S> {
    fn parse(input: S) -> VResult<S, Self> {
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
    fn parse(input: S) -> VResult<S, Self> {
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
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct AssignableVariable<S: Span> {
    path: VariablePath<S>,
    ty: Option<VariableType<S>>,
}

impl<S: Span> AssignableVariable<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            pair(
                terminated(VariablePath::parse, space0),
                opt(preceded(pair(nom_char(':'), space0), VariableType::parse)),
            ),
            |(path, ty)| Self { path, ty },
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Assignable<S: Span> {
    Variable(AssignableVariable<S>),
    List(Vec<AssignableVariable<S>>),
}

impl<S: Span> Assignable<S> {
    fn parse(input: S) -> VResult<S, Self> {
        alt((
            map(AssignableVariable::parse, Self::Variable),
            map(Self::parse_list, Self::List),
        ))(input)
    }

    fn parse_list(input: S) -> VResult<S, Vec<AssignableVariable<S>>> {
        delimited(
            nom_char('['),
            separated_list0(
                delimited(space0, nom_char(','), space0),
                AssignableVariable::parse,
            ),
            nom_char(']'),
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Assign<S: Span> {
    pub is_new: bool,
    pub to_assign: Assignable<S>,
    pub statement: Box<Statement<S>>,
}

impl<S: Span> Assign<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            tuple((
                opt(terminated(take_keyword("let"), space1)),
                Assignable::parse,
                preceded(delimited(space0, nom_char('='), space0), Statement::parse),
            )),
            |(let_keyword, to_assign, statement)| Self {
                is_new: let_keyword.is_some(),
                to_assign,
                statement: Box::new(statement),
            },
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Return<S: Span> {
    pub expression: Option<Expression<S>>,
}

impl<S: Span> Return<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            preceded(
                take_keyword("return"),
                opt(preceded(space1, Expression::parse)),
            ),
            |expression| Self { expression },
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct If<S: Span> {
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
            preceded(
                pair(take_keyword("if"), space1),
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
            |(expression, block, else_statement)| Self {
                expression,
                block,
                else_statement,
            },
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Match<S: Span> {
    pub expression: Expression<S>,
    pub branches: Vec<MatchBranch<S>>,
}

impl<S: Span> Match<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            preceded(
                pair(take_keyword("match"), space1),
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
            |(expression, branches)| Self {
                expression,
                branches,
            },
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct MatchBranch<S: Span> {
    expression: Expression<S>,
    block: Block<S>,
}

impl<S: Span> MatchBranch<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            separated_pair(
                context(
                    "Match branch must start with an expression",
                    Expression::parse,
                ),
                delimited(
                    space0,
                    context("Match branch missing `=>` token", tag("=>")),
                    space0,
                ),
                Block::parse,
            ),
            |(expression, block)| Self { expression, block },
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct For<S: Span> {
    pub name: Option<S>,
    pub variable_expression: Expression<S>,
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
                preceded(
                    take_keyword("for"),
                    cut(tuple((
                        delimited(
                            space0,
                            context("Missing variable expression", Expression::parse),
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
            |(name, (variable_expression, _in, iterator_expression, block))| Self {
                name,
                variable_expression,
                iterator_expression,
                block,
            },
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct While<S: Span> {
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
                preceded(
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
            |(name, (expression, block))| Self {
                name,
                expression,
                block,
            },
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Loop<S: Span> {
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
                preceded(
                    take_keyword("loop"),
                    cut(preceded(
                        space0,
                        context("Loop is missing its block", Block::parse),
                    )),
                ),
            )),
            |(name, block)| Self { name, block },
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Break<S: Span> {
    pub loop_name: Option<S>,
    pub expression: Option<Expression<S>>,
}

impl<S: Span> Break<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            preceded(
                take_keyword("break"),
                pair(
                    opt(preceded(pair(space0, nom_char('\'')), parse_name)),
                    opt(preceded(space1, Expression::parse)),
                ),
            ),
            |(loop_name, expression)| Break {
                loop_name,
                expression,
            },
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Continue<S: Span> {
    pub loop_name: Option<S>,
}

impl<S: Span> Continue<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            preceded(
                take_keyword("continue"),
                opt(preceded(pair(space0, nom_char('\'')), parse_name)),
            ),
            |loop_name| Continue { loop_name },
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum VariableType<S: Span> {
    Length,
    Angle,
    Number,
    List,
    Boolean,
    Struct(S),
}

impl<S: Span> VariableType<S> {
    fn parse(input: S) -> VResult<S, Self> {
        context(
            "Invalid type",
            alt((
                value(Self::Length, tag("Length")),
                value(Self::Angle, tag("Angle")),
                value(Self::Number, tag("Number")),
                value(Self::List, tag("List")),
                value(Self::Boolean, tag("Boolean")),
                map(
                    preceded(pair(take_keyword("struct"), space0), parse_name),
                    Self::Struct,
                ),
            )),
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum UnitType {
    Millimeter,
    Centimeter,
    Meter,
    Inch,
    Mil,
    Foot,
    Yard,
    Degree,
    Radian,
}

impl UnitType {
    fn parse<S: Span>(input: S) -> VResult<S, Self> {
        alt((
            value(Self::Millimeter, tag("mm")),
            value(Self::Centimeter, tag("cm")),
            value(Self::Inch, tag("in")),
            value(Self::Mil, tag("mil")),
            value(Self::Meter, tag("m")),
            value(Self::Foot, tag("ft")),
            value(Self::Yard, tag("yd")),
            value(Self::Degree, tag("deg")),
            value(Self::Radian, tag("rad")),
        ))(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression<S: Span> {
    And(Box<Self>, Comparison<S>),
    Or(Box<Self>, Comparison<S>),
    Buffer(Comparison<S>),
}

impl<S: Span> Expression<S> {
    fn parse(input: S) -> VResult<S, Self> {
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
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Term<S: Span> {
    Multiply(Box<Self>, Trailer<S>),
    Divide(Box<Self>, Trailer<S>),
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
            flat_map(Trailer::parse, |first_accessor| {
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
                        Trailer::parse,
                    ),
                    move || Term::Trailer(first_accessor.clone()),
                    |term, (operator, accessor)| match operator {
                        Operator::Multiplication => Self::Multiply(Box::new(term), accessor),
                        Operator::Division => Self::Divide(Box::new(term), accessor),
                    },
                )
            }),
            map(Trailer::parse, Self::Trailer),
        ))(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Trailer<S: Span> {
    None(Factor<S>),
    Attribute(Box<Trailer<S>>, S),
    Call(Box<Trailer<S>>, Vec<Expression<S>>),
    Index(Box<Trailer<S>>, Box<Expression<S>>),
}

impl<S: Span> Trailer<S> {
    fn parse(input: S) -> VResult<S, Self> {
        #[derive(Clone)]
        enum Operation<S: Span> {
            Attribute(S),
            Call(Vec<Expression<S>>),
            Index(Expression<S>),
        }

        alt((
            flat_map(Factor::parse, |first_factor| {
                fold_many0(
                    delimited(
                        space0,
                        alt((
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
                        Operation::Index(indexer) => {
                            Self::Index(Box::new(trailer), Box::new(indexer))
                        }
                    },
                )
            }),
            map(Factor::parse, Self::None),
        ))(input)
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
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StructInitialization<S: Span> {
    name: S,
    assignments: Vec<(S, Expression<S>)>,
    inheritance: Option<Box<Trailer<S>>>,
}

impl<S: Span> StructInitialization<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            preceded(
                take_keyword("struct"),
                cut(pair(
                    delimited(space0, parse_name, space0),
                    delimited(
                        pair(context("Missing opening bracket", nom_char('{')), space0),
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
                        pair(space0, context("Missing closing bracket", nom_char('}'))),
                    ),
                )),
            ),
            |(name, (assignments, inheritance))| Self {
                name,
                assignments,
                inheritance,
            },
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Litteral<S: Span> {
    Measurement(Measurement<S>),
    Number(Number<S>),
    String(PString<S>),
    List(List<S>),
    Vector2(Box<Vector2<S>>),
    Vector3(Box<Vector3<S>>),
    Boolean(bool),
    Default,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Vector2<S: Span> {
    x: Expression<S>,
    y: Expression<S>,
}

impl<S: Span> Vector2<S> {
    fn parse(input: S) -> VResult<S, Self> {
        preceded(
            pair(tag("V2"), space0),
            delimited(
                nom_char('('),
                alt((
                    map(
                        separated_pair(
                            delimited(space0, Expression::parse, space0),
                            nom_char(','),
                            delimited(space0, Expression::parse, space0),
                        ),
                        |(x, y)| Self { x, y },
                    ),
                    map(delimited(space0, Expression::parse, space0), |splat| Self {
                        x: splat.clone(),
                        y: splat,
                    }),
                )),
                nom_char(')'),
            ),
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Vector3<S: Span> {
    x: Expression<S>,
    y: Expression<S>,
    z: Expression<S>,
}

impl<S: Span> Vector3<S> {
    fn parse(input: S) -> VResult<S, Self> {
        preceded(
            pair(tag("V3"), space0),
            delimited(
                nom_char('('),
                alt((
                    map(
                        separated_pair(
                            separated_pair(
                                delimited(space0, Expression::parse, space0),
                                nom_char(','),
                                delimited(space0, Expression::parse, space0),
                            ),
                            nom_char(','),
                            delimited(space0, Expression::parse, space0),
                        ),
                        |((x, y), z)| Self { x, y, z },
                    ),
                    map(delimited(space0, Expression::parse, space0), |splat| Self {
                        x: splat.clone(),
                        y: splat.clone(),
                        z: splat,
                    }),
                )),
                nom_char(')'),
            ),
        )(input)
    }
}

impl<S: Span> Litteral<S> {
    fn parse(input: S) -> VResult<S, Self> {
        alt((
            map(Measurement::parse, Self::Measurement),
            map(Number::parse, Self::Number),
            map(PString::parse, Self::String),
            map(List::parse, Self::List),
            map(Vector2::parse, |v| Self::Vector2(Box::new(v))),
            map(Vector3::parse, |v| Self::Vector3(Box::new(v))),
            value(Self::Boolean(true), take_keyword("true")),
            value(Self::Boolean(false), take_keyword("false")),
            value(Self::Default, take_keyword("default")),
        ))(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct List<S: Span> {
    expressions: Vec<Expression<S>>,
}

impl<S: Span> List<S> {
    fn parse(input: S) -> VResult<S, Self> {
        let (input, expressions) = delimited(
            nom_char('['),
            separated_list0(nom_char(','), delimited(space0, Expression::parse, space0)),
            nom_char(']'),
        )(input)?;

        Ok((input, Self { expressions }))
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct PString<S: Span> {
    value: S,
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
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Measurement<S: Span> {
    pub number: Number<S>,
    pub ty: UnitType,
}

impl<S: Span> Measurement<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            separated_pair(Number::parse, space0, UnitType::parse),
            |(number, ty)| Measurement { number, ty },
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct VariablePath<S: Span> {
    parts: Vec<S>,
}

impl<S: Span> VariablePath<S> {
    fn parse(input: S) -> VResult<S, Self> {
        map(
            separated_list1(delimited(space0, nom_char('.'), space0), parse_name),
            |parts| Self { parts },
        )(input)
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
    fractional: Option<S>,
}

impl<S: Span> Number<S> {
    fn parse(input: S) -> VResult<S, Self> {
        let (input, (integer, fractional)) = alt((
            separated_pair(
                map(parse_integer, Some),
                delimited(space0, nom_char('.'), space0),
                map(parse_integer, Some),
            ),
            separated_pair(
                success(None),
                delimited(space0, nom_char('.'), space0),
                map(parse_integer, Some),
            ),
            separated_pair(
                map(parse_integer, Some),
                delimited(space0, nom_char('.'), space0),
                success(None),
            ),
            pair(map(parse_integer, Some), success(None)),
        ))(input)?;

        Ok((
            input,
            Self {
                integer,
                fractional,
            },
        ))
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
    fn unit_type() {
        assert_eq!(UnitType::parse("mm"), Ok(("", UnitType::Millimeter)));
        assert_eq!(UnitType::parse("cm"), Ok(("", UnitType::Centimeter)));
        assert_eq!(UnitType::parse("m"), Ok(("", UnitType::Meter)));
        assert_eq!(UnitType::parse("in"), Ok(("", UnitType::Inch)));
        assert_eq!(UnitType::parse("mil"), Ok(("", UnitType::Mil)));
        assert_eq!(UnitType::parse("ft"), Ok(("", UnitType::Foot)));
        assert_eq!(UnitType::parse("yd"), Ok(("", UnitType::Yard)));
        assert_eq!(UnitType::parse("deg"), Ok(("", UnitType::Degree)));
        assert_eq!(UnitType::parse("rad"), Ok(("", UnitType::Radian)));
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
                        fractional: None
                    },
                    ty: UnitType::Meter
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
                        fractional: None
                    },
                    ty: UnitType::Meter
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
                        fractional: Some("44"),
                    },
                    ty: UnitType::Meter
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
                    expressions: vec![]
                }
            ))
        );
        assert_eq!(
            List::parse("[one]"),
            Ok((
                "",
                List {
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

        assert_eq!(
            Trailer::parse("test.sub_call()"),
            Ok((
                "",
                Trailer::Call(
                    Box::new(Trailer::Attribute(
                        Box::new(Trailer::None(Factor::Variable("test"))),
                        "sub_call"
                    )),
                    vec![]
                )
            ))
        );

        assert_eq!(
            Trailer::parse("test.sub_call().returned()"),
            Ok((
                "",
                Trailer::Call(
                    Box::new(Trailer::Attribute(
                        Box::new(Trailer::Call(
                            Box::new(Trailer::Attribute(
                                Box::new(Trailer::None(Factor::Variable("test"))),
                                "sub_call"
                            )),
                            vec![]
                        )),
                        "returned"
                    )),
                    vec![]
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
    fn litteral() {
        // Measurement(Measurement<S>),
        assert_eq!(
            Litteral::parse("1234.5678m"),
            Ok((
                "",
                Litteral::Measurement(Measurement {
                    number: Number {
                        integer: Some("1234"),
                        fractional: Some("5678")
                    },
                    ty: UnitType::Meter
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
                    expressions: vec![]
                })
            ))
        );

        // Vector2(Box<Vector2<S>>),
        assert_eq!(
            Litteral::parse("V2(a, b)"),
            Ok((
                "",
                Litteral::Vector2(Box::new(Vector2 {
                    x: Expression::parse("a").unwrap().1,
                    y: Expression::parse("b").unwrap().1,
                }))
            ))
        );

        // Vector3(Box<Vector3<S>>),
        assert_eq!(
            Litteral::parse("V3(a, b, c)"),
            Ok((
                "",
                Litteral::Vector3(Box::new(Vector3 {
                    x: Expression::parse("a").unwrap().1,
                    y: Expression::parse("b").unwrap().1,
                    z: Expression::parse("c").unwrap().1,
                }))
            ))
        );

        // Boolean(bool),
        assert_eq!(Litteral::parse("true"), Ok(("", Litteral::Boolean(true))));
        assert_eq!(Litteral::parse("false"), Ok(("", Litteral::Boolean(false))));

        // Default,
        assert_eq!(Litteral::parse("default"), Ok(("", Litteral::Default)));
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
                    name: "MyStruct",
                    assignments: vec![
                        ("a", Expression::parse("b").unwrap().1),
                        ("c", Expression::parse("d").unwrap().1)
                    ],
                    inheritance: Some(Box::new(Trailer::None(Factor::Litteral(Litteral::Default))))
                })
            ))
        );
    }

    #[test]
    fn vector2() {
        assert_eq!(
            Vector2::parse("V2(22m, 44m)"),
            Ok((
                "",
                Vector2 {
                    x: Expression::parse("22m").unwrap().1,
                    y: Expression::parse("44m").unwrap().1,
                }
            ))
        );

        assert_eq!(
            Vector2::parse("V2(22m)"),
            Ok((
                "",
                Vector2 {
                    x: Expression::parse("22m").unwrap().1,
                    y: Expression::parse("22m").unwrap().1,
                }
            ))
        );
    }

    #[test]
    fn vector3() {
        assert_eq!(
            Vector3::parse("V3(22m, 44m, 66m)"),
            Ok((
                "",
                Vector3 {
                    x: Expression::parse("22m").unwrap().1,
                    y: Expression::parse("44m").unwrap().1,
                    z: Expression::parse("66m").unwrap().1,
                }
            ))
        );

        assert_eq!(
            Vector3::parse("V3(22m)"),
            Ok((
                "",
                Vector3 {
                    x: Expression::parse("22m").unwrap().1,
                    y: Expression::parse("22m").unwrap().1,
                    z: Expression::parse("22m").unwrap().1,
                }
            ))
        );

        assert!(Vector2::parse("V3(22m 44m)").is_err());
    }

    #[test]
    fn variable_type() {
        assert_eq!(
            VariableType::parse("Length"),
            Ok(("", VariableType::Length))
        );
        assert_eq!(VariableType::parse("Angle"), Ok(("", VariableType::Angle)));
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
                    path: VariablePath {
                        parts: vec!["variable"]
                    },
                    ty: None
                }
            ))
        );

        assert_eq!(
            AssignableVariable::parse("variable: Length"),
            Ok((
                "",
                AssignableVariable {
                    path: VariablePath {
                        parts: vec!["variable"]
                    },
                    ty: Some(VariableType::Length),
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
                    path: VariablePath {
                        parts: vec!["variable"]
                    },
                    ty: None
                })
            ))
        );

        assert_eq!(
            Assignable::parse("[a, b]"),
            Ok((
                "",
                Assignable::List(vec![
                    AssignableVariable {
                        path: VariablePath { parts: vec!["a"] },
                        ty: None
                    },
                    AssignableVariable {
                        path: VariablePath { parts: vec!["b"] },
                        ty: None
                    }
                ])
            ))
        );

        assert_eq!(
            Assignable::parse("[a: Length, b: Angle]"),
            Ok((
                "",
                Assignable::List(vec![
                    AssignableVariable {
                        path: VariablePath { parts: vec!["a"] },
                        ty: Some(VariableType::Length)
                    },
                    AssignableVariable {
                        path: VariablePath { parts: vec!["b"] },
                        ty: Some(VariableType::Angle)
                    }
                ])
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
                    is_new: false,
                    to_assign: Assignable::Variable(AssignableVariable {
                        path: VariablePath { parts: vec!["a"] },
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
                    is_new: false,
                    to_assign: Assignable::Variable(AssignableVariable {
                        path: VariablePath { parts: vec!["a"] },
                        ty: Some(VariableType::Length),
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
                    is_new: false,
                    to_assign: Assignable::Variable(AssignableVariable {
                        path: VariablePath { parts: vec!["a"] },
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
                    is_new: true,
                    to_assign: Assignable::Variable(AssignableVariable {
                        path: VariablePath { parts: vec!["a"] },
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
            Ok(("", Return { expression: None }))
        );

        assert_eq!(
            Return::parse("return a"),
            Ok((
                "",
                Return {
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
                    expression: Expression::parse("a").unwrap().1,
                    block: Block { statements: vec![] },
                    else_statement: Some(Else::IfElse(Box::new(If {
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
            MatchBranch::parse("a => {}"),
            Ok((
                "",
                MatchBranch {
                    expression: Expression::parse("a").unwrap().1,
                    block: Block { statements: vec![] }
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
                    expression: Expression::parse("a").unwrap().1,
                    branches: vec![]
                }
            ))
        );

        assert_eq!(
            Match::parse("match a { b => {} }"),
            Ok((
                "",
                Match {
                    expression: Expression::parse("a").unwrap().1,
                    branches: vec![MatchBranch {
                        expression: Expression::parse("b").unwrap().1,
                        block: Block { statements: vec![] }
                    }]
                }
            ))
        );

        assert_eq!(
            Match::parse("match a { b => {}, c => {}}"),
            Ok((
                "",
                Match {
                    expression: Expression::parse("a").unwrap().1,
                    branches: vec![
                        MatchBranch {
                            expression: Expression::parse("b").unwrap().1,
                            block: Block { statements: vec![] }
                        },
                        MatchBranch {
                            expression: Expression::parse("c").unwrap().1,
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
                    name: None,
                    variable_expression: Expression::parse("a").unwrap().1,
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
                    name: Some("my_for_loop"),
                    variable_expression: Expression::parse("a").unwrap().1,
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
            Ok(("", Continue { loop_name: None }))
        );
        assert_eq!(
            Continue::parse("continue 'my_loop"),
            Ok((
                "",
                Continue {
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
                    is_new: false,
                    to_assign: Assignable::Variable(AssignableVariable {
                        path: VariablePath { parts: vec!["a"] },
                        ty: None,
                    }),
                    statement: Box::new(Statement::parse("b").unwrap().1)
                })
            ))
        );

        assert_eq!(
            Statement::parse("return"),
            Ok(("", Statement::Return(Return { expression: None })))
        );

        assert_eq!(
            Statement::parse("match a {}"),
            Ok((
                "",
                Statement::Match(Match {
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
                    name: None,
                    variable_expression: Expression::parse("a").unwrap().1,
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
                    loop_name: None,
                    expression: None
                })
            ))
        );

        assert_eq!(
            Statement::parse("continue"),
            Ok(("", Statement::Continue(Continue { loop_name: None })))
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
                    statements: vec![BlockStatement::Blank],
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
                            loop_name: None,
                            expression: None
                        })),
                        BlockStatement::Closed(Statement::Assign(Assign {
                            is_new: false,
                            to_assign: Assignable::Variable(AssignableVariable {
                                path: VariablePath { parts: vec!["a"] },
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
                        is_new: false,
                        to_assign: Assignable::Variable(AssignableVariable {
                            path: VariablePath { parts: vec!["a"] },
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
                            loop_name: None,
                            expression: None
                        })),
                        BlockStatement::Open(Statement::Assign(Assign {
                            is_new: false,
                            to_assign: Assignable::Variable(AssignableVariable {
                                path: VariablePath { parts: vec!["a"] },
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
                            name: None,
                            block: Block { statements: vec![] }
                        })),
                        BlockStatement::Open(Statement::Return(Return { expression: None }))
                    ],
                }
            ))
        );

        assert!(Block::parse("{ break a = b }").is_err());
        assert!(Block::parse("{ break a = b; }").is_err());
    }

    #[test]
    fn variable_path() {
        assert_eq!(
            VariablePath::parse("name"),
            Ok((
                "",
                VariablePath {
                    parts: vec!["name"]
                }
            ))
        );

        assert_eq!(
            VariablePath::parse("name.subname"),
            Ok((
                "",
                VariablePath {
                    parts: vec!["name", "subname"]
                }
            ))
        );

        assert!(VariablePath::parse("").is_err());
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
                        fractional: None
                    }),
                    Litteral::Number(Number {
                        integer: Some("1"),
                        fractional: None
                    }),
                    Litteral::Number(Number {
                        integer: Some("2"),
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
                    parameters: vec![MemberVariable {
                        name: "one",
                        ty: VariableType::Length,
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
                    parameters: vec![
                        MemberVariable {
                            name: "one",
                            ty: VariableType::Length,
                            constraints: None,
                            default_value: None
                        },
                        MemberVariable {
                            name: "two",
                            ty: VariableType::Angle,
                            constraints: None,
                            default_value: Some(Litteral::Number(Number {
                                integer: Some("2"),
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
                    parameters: vec![
                        MemberVariable {
                            name: "one",
                            ty: VariableType::Length,
                            constraints: None,
                            default_value: None
                        },
                        MemberVariable {
                            name: "two",
                            ty: VariableType::Angle,
                            constraints: None,
                            default_value: Some(Litteral::Number(Number {
                                integer: Some("2"),
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
                    assignments: vec![]
                }
            ))
        );

        assert_eq!(
            Struct::parse("struct MyStruct { a: Length }"),
            Ok((
                "",
                Struct {
                    name: "MyStruct",
                    assignments: vec![MemberVariable {
                        name: "a",
                        ty: VariableType::Length,
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
                    assignments: vec![
                        MemberVariable {
                            name: "a",
                            ty: VariableType::Length,
                            constraints: None,
                            default_value: None
                        },
                        MemberVariable {
                            name: "b",
                            ty: VariableType::Angle,
                            constraints: Some(MemberVariableConstraintList {
                                constraints: vec![MemberVariableConstraint::Integer]
                            }),
                            default_value: Some(Litteral::Boolean(true))
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
                    named_block: NamedBlock {
                        name: "my_function",
                        parameters: vec![],
                        block: Block { statements: vec![] }
                    },
                    return_type: VariableType::Length
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
                    named_block: NamedBlock {
                        name: "my_sketch",
                        parameters: vec![],
                        block: Block { statements: vec![] }
                    },
                }
            ))
        );
    }

    #[test]
    fn widget() {
        assert!(Widget::parse("widget my_widget() -> Length {}").is_err());
        assert_eq!(
            Widget::parse("widget my_widget() {}"),
            Ok((
                "",
                Widget {
                    named_block: NamedBlock {
                        name: "my_widget",
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
                    assignments: vec![]
                })
            ))
        );

        // Sketch(Sketch<S>),
        assert_eq!(
            RootElement::parse("sketch my_sketch() {}"),
            Ok((
                "",
                RootElement::Sketch(Sketch {
                    named_block: NamedBlock {
                        name: "my_sketch",
                        parameters: vec![],
                        block: Block { statements: vec![] }
                    },
                })
            ))
        );

        // Widget(Widget<S>),
        assert_eq!(
            RootElement::parse("widget my_widget() {}"),
            Ok((
                "",
                RootElement::Widget(Widget {
                    named_block: NamedBlock {
                        name: "my_widget",
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
                    named_block: NamedBlock {
                        name: "my_function",
                        parameters: vec![],
                        block: Block { statements: vec![] }
                    },
                    return_type: VariableType::Length
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
            widget my_widget() {}
            function my_function() -> Length {}
"#
            ),
            Ok((
                "",
                FileAST {
                    root_elements: vec![
                        RootElement::Struct(Struct {
                            name: "MyStruct",
                            assignments: vec![]
                        }),
                        RootElement::Sketch(Sketch {
                            named_block: NamedBlock {
                                name: "my_sketch",
                                parameters: vec![],
                                block: Block { statements: vec![] }
                            },
                        }),
                        RootElement::Widget(Widget {
                            named_block: NamedBlock {
                                name: "my_widget",
                                parameters: vec![],
                                block: Block { statements: vec![] }
                            },
                        }),
                        RootElement::Function(Function {
                            named_block: NamedBlock {
                                name: "my_function",
                                parameters: vec![],
                                block: Block { statements: vec![] }
                            },
                            return_type: VariableType::Length
                        }),
                    ]
                }
            ))
        );
    }
}
