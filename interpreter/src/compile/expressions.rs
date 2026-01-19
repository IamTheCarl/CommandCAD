use std::{cmp::Ordering, collections::HashSet, path::PathBuf, sync::Arc};

use hashable_map::HashableSet;
use imstr::ImString;
use type_sitter::{HasChild, Node};
use unwrap_enum::EnumAs;

use crate::{
    compile::{formula::Formula, unwrap_missing, Scalar},
    execution::find_all_variable_accesses_in_expression,
};

use super::{nodes, AstNode, Error, Parse};

/// Used for sorting operations that have dependencies on other operations for parallel execution.
trait DependentOperation {
    fn original_index(&self) -> usize;
    fn name(&self) -> &ImString;
    fn dependencies(&self) -> &HashableSet<ImString>;
}

fn sort_and_group_dependencies<D>(deps: &mut Vec<D>) -> Vec<std::ops::Range<usize>>
where
    D: DependentOperation,
{
    if !deps.is_empty() {
        deps.sort_by(|a, b| {
            let a_name = a.name();
            let b_name = b.name();
            let a_index = a.original_index();
            let b_index = b.original_index();
            let a = a.dependencies();
            let b = b.dependencies();

            // Dependency takes president.
            if a.contains(b_name) && a_index > b_index {
                Ordering::Greater
            } else if b.contains(a_name) && b_index > a_index {
                Ordering::Less
            } else {
                // If they have no dependency on each other, put the ones with fewer dependencies
                // as a higher priority.
                a.len().cmp(&b.len())
            }
        });

        let mut compute_groups = Vec::new();
        let mut start = 0;
        let mut iterator = deps.iter().enumerate().peekable();

        while let (Some((a_index, a)), Some((b_index, b))) = (iterator.next(), iterator.peek()) {
            // Every transition indicates the end of a group.
            if a.dependencies() != b.dependencies() {
                compute_groups.push(start..a_index + 1);
                start = *b_index;
            }
        }

        // Whatever remains is its own group
        compute_groups.push(start..deps.len());

        compute_groups
    } else {
        // Nothing to compute.
        Vec::new()
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct InvalidUnitError<'t, 'i> {
    pub name: &'i str,
    pub node: nodes::anon_unions::Identifier_UnitQuote<'t>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct ParseIntError<'t> {
    pub error: std::num::ParseIntError,
    pub node: nodes::Integer<'t>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct ParseNumberError<'t> {
    pub error: std::num::ParseIntError,
    pub node: nodes::Number<'t>,
}

#[derive(Debug, Hash, Eq, PartialEq, EnumAs)]
pub enum Expression {
    BinaryExpression(AstNode<Box<BinaryExpression>>),
    Boolean(AstNode<bool>),
    ClosureDefinition(AstNode<Box<ClosureDefinition>>),
    DictionaryConstruction(AstNode<DictionaryConstruction>),
    If(AstNode<Box<IfExpression>>),
    List(AstNode<Vec<AstNode<Expression>>>),
    Parenthesis(Box<AstNode<Expression>>),
    MemberAccess(Box<AstNode<MemberAccess>>),
    Self_(AstNode<Self_>),
    Identifier(AstNode<ImString>),
    Scalar(AstNode<Scalar>),
    Vector2(AstNode<Box<Vector2>>),
    Vector3(AstNode<Box<Vector3>>),
    Vector4(AstNode<Box<Vector4>>),
    SignedInteger(AstNode<i64>),
    String(AstNode<String>),
    StructDefinition(AstNode<StructDefinition>),
    UnaryExpression(AstNode<Box<UnaryExpression>>),
    UnsignedInteger(AstNode<u64>),
    FunctionCall(AstNode<Box<FunctionCall>>),
    MethodCall(AstNode<Box<MethodCall>>),
    LetIn(AstNode<Box<LetIn>>),
    Formula(AstNode<Formula>),
    Malformed(ImString),
}

impl<'t> Parse<'t, nodes::BinaryExpression<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::BinaryExpression<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

        Ok(AstNode::new(
            file,
            &value,
            Self::BinaryExpression(
                BinaryExpression::parse(file, input, value).map(|o| o.into_box())?,
            ),
        ))
    }
}

impl<'t> Parse<'t, nodes::Formula<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::Formula<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

        Ok(AstNode::new(
            file,
            &value,
            Self::Formula(Formula::parse(file, input, value)?),
        ))
    }
}

impl<'t> Parse<'t, nodes::Boolean<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::Boolean<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

        Ok(AstNode::new(
            file,
            &value,
            Self::Boolean(bool::parse(file, input, value)?),
        ))
    }
}

impl<'t> Parse<'t, nodes::ClosureDefinition<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::ClosureDefinition<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

        Ok(AstNode::new(
            file,
            &value,
            Self::ClosureDefinition(ClosureDefinition::parse(file, input, value)?.into_box()),
        ))
    }
}

impl<'t> Parse<'t, nodes::DictionaryConstruction<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::DictionaryConstruction<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

        Ok(AstNode::new(
            file,
            &value,
            Self::DictionaryConstruction(DictionaryConstruction::parse(file, input, value)?),
        ))
    }
}

impl<'t> Parse<'t, nodes::If<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::If<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

        Ok(AstNode::new(
            file,
            &value,
            Self::If(IfExpression::parse(file, input, value)?.into_box()),
        ))
    }
}

impl<'t> Parse<'t, nodes::List<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::List<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

        let mut cursor = value.walk();

        let mut list = Vec::new();
        for expression in value.expressions(&mut cursor) {
            let expression = expression?;
            let expression = Self::parse(file, input, expression)?;
            list.push(expression);
        }

        Ok(AstNode::new(
            file,
            &value,
            Self::List(AstNode::new(file, &value, list)),
        ))
    }
}

impl<'t> Parse<'t, nodes::Parenthesis<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::Parenthesis<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

        let expression = value.expression()?;
        Ok(AstNode::new(
            file,
            &value,
            Self::Parenthesis(Box::new(Self::parse(file, input, expression)?)),
        ))
    }
}

impl<'t> Parse<'t, nodes::MemberAccess<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::MemberAccess<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

        Ok(AstNode::new(
            file,
            &value,
            Self::MemberAccess(Box::new(MemberAccess::parse(file, input, value)?)),
        ))
    }
}

impl<'t> Parse<'t, nodes::Self_<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::Self_<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        Ok(AstNode::new(
            file,
            &value,
            Self::Self_(Self_::parse(file, input, value)?),
        ))
    }
}

impl<'t> Parse<'t, nodes::Scalar<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::Scalar<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

        Ok(AstNode::new(
            file,
            &value,
            Self::Scalar(Scalar::parse(file, input, value)?),
        ))
    }
}

impl<'t> Parse<'t, nodes::Vector2<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::Vector2<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

        Ok(AstNode::new(
            file,
            &value,
            Self::Vector2(Vector2::parse(file, input, value)?.into_box()),
        ))
    }
}

impl<'t> Parse<'t, nodes::Vector3<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::Vector3<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

        Ok(AstNode::new(
            file,
            &value,
            Self::Vector3(Vector3::parse(file, input, value)?.into_box()),
        ))
    }
}

impl<'t> Parse<'t, nodes::Vector4<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::Vector4<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

        Ok(AstNode::new(
            file,
            &value,
            Self::Vector4(Vector4::parse(file, input, value)?.into_box()),
        ))
    }
}

impl<'t> Parse<'t, nodes::SignedInteger<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::SignedInteger<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

        Ok(AstNode::new(
            file,
            &value,
            Self::SignedInteger(i64::parse(file, input, value)?),
        ))
    }
}

impl<'t> Parse<'t, nodes::UnsignedInteger<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::UnsignedInteger<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

        Ok(AstNode::new(
            file,
            &value,
            Self::UnsignedInteger(u64::parse(file, input, value)?),
        ))
    }
}

impl<'t> Parse<'t, nodes::String<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::String<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        static ESCAPE_SEQUENCES: &[(&str, &str); 3] =
            &[("\"", "\\\""), ("\n", "\\n"), ("\\", "\\\\")];
        unwrap_missing(&value)?;

        let raw_text = &input[value.byte_range()];
        let raw_text = &raw_text[1..raw_text.len() - 1];

        let mut sequence_iter = ESCAPE_SEQUENCES.iter();
        let (replace, find) = sequence_iter.next().unwrap(); // Should never fail since we static initalized that array.

        let mut processed_text = raw_text.replace(find, replace);

        for (replace, find) in sequence_iter {
            processed_text = processed_text.replace(find, replace);
        }

        Ok(AstNode::new(
            file,
            &value,
            Self::String(AstNode::new(file, &value, processed_text)),
        ))
    }
}

impl<'t> Parse<'t, nodes::StructDefinition<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::StructDefinition<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

        Ok(AstNode::new(
            file,
            &value,
            Self::StructDefinition(StructDefinition::parse(file, input, value)?),
        ))
    }
}

impl<'t> Parse<'t, nodes::UnaryExpression<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::UnaryExpression<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

        Ok(AstNode::new(
            file,
            &value,
            Self::UnaryExpression(
                UnaryExpression::parse(file, input, value).map(|n| n.into_box())?,
            ),
        ))
    }
}

impl<'t> Parse<'t, nodes::FunctionCall<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::FunctionCall<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

        Ok(AstNode::new(
            file,
            &value,
            Self::FunctionCall(FunctionCall::parse(file, input, value)?.into_box()),
        ))
    }
}

impl<'t> Parse<'t, nodes::MethodCall<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::MethodCall<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

        Ok(AstNode::new(
            file,
            &value,
            Self::MethodCall(MethodCall::parse(file, input, value)?.into_box()),
        ))
    }
}

impl<'t> Parse<'t, nodes::LetIn<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        node: nodes::LetIn<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&node)?;

        Ok(AstNode::new(
            file,
            &node,
            Self::LetIn(LetIn::parse(file, input, node)?.into_box()),
        ))
    }
}

impl<'t> Parse<'t, nodes::Expression<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::Expression<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        type ChildType<'t> = <nodes::Expression<'t> as HasChild<'t>>::Child;

        let result = match value.child()? {
            ChildType::BinaryExpression(binary_expression) => {
                Self::parse(file, input, binary_expression)
            }
            ChildType::Boolean(value) => Self::parse(file, input, value),
            ChildType::ClosureDefinition(closure_definition) => {
                Self::parse(file, input, closure_definition)
            }
            ChildType::DictionaryConstruction(dictionary_construction) => {
                Self::parse(file, input, dictionary_construction)
            }
            ChildType::If(if_expression) => Self::parse(file, input, if_expression),
            ChildType::List(list) => Self::parse(file, input, list),
            ChildType::Parenthesis(parenthesis) => Self::parse(file, input, parenthesis),
            ChildType::MemberAccess(path) => Self::parse(file, input, path),
            ChildType::Self_(path) => Self::parse(file, input, path),
            ChildType::Identifier(ident) => Ok(AstNode::new(
                file,
                &value,
                Self::Identifier(ImString::parse(file, input, ident)?),
            )),
            ChildType::Scalar(scalar) => Self::parse(file, input, scalar),
            ChildType::Vector2(vector) => Self::parse(file, input, vector),
            ChildType::Vector3(vector) => Self::parse(file, input, vector),
            ChildType::Vector4(vector) => Self::parse(file, input, vector),
            ChildType::SignedInteger(signed_integer) => Self::parse(file, input, signed_integer),
            ChildType::String(string) => Self::parse(file, input, string),
            ChildType::StructDefinition(struct_definition) => {
                Self::parse(file, input, struct_definition)
            }
            ChildType::UnaryExpression(unary_expression) => {
                Self::parse(file, input, unary_expression)
            }
            ChildType::UnsignedInteger(unsigned_integer) => {
                Self::parse(file, input, unsigned_integer)
            }
            ChildType::FunctionCall(function_call) => Self::parse(file, input, function_call),
            ChildType::MethodCall(method_call) => Self::parse(file, input, method_call),
            ChildType::Formula(formula) => Self::parse(file, input, formula),
            ChildType::LetIn(let_in) => Self::parse(file, input, let_in),
        };

        match result {
            Ok(expression) => Ok(expression),
            Err(Error::Malformed(kind)) => Ok(AstNode::new(
                file,
                &value,
                Self::Malformed(ImString::from(kind)),
            )),
            Err(error) => Err(error),
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum UnaryExpressionOperation {
    Add,
    Sub,
    Not,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct UnaryExpression {
    pub operation: AstNode<UnaryExpressionOperation>,
    pub expression: AstNode<Expression>,
}

impl<'t> Parse<'t, nodes::UnaryExpression<'t>> for UnaryExpression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::UnaryExpression<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let operation = value.op()?;

        let operation = match operation {
            nodes::anon_unions::Not_Add_Sub::Not(not) => {
                AstNode::new(file, &not, UnaryExpressionOperation::Not)
            }
            nodes::anon_unions::Not_Add_Sub::Add(add) => {
                AstNode::new(file, &add, UnaryExpressionOperation::Add)
            }
            nodes::anon_unions::Not_Add_Sub::Sub(sub) => {
                AstNode::new(file, &sub, UnaryExpressionOperation::Sub)
            }
        };

        let expression = Expression::parse(file, input, value.expression()?)?;

        Ok(AstNode::new(
            file,
            &value,
            Self {
                operation,
                expression,
            },
        ))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum BinaryExpressionOperation {
    NotEq,
    And,
    AndAnd,
    Mul,
    MulMul,
    Add,
    Sub,
    Div,
    Lt,
    LtLt,
    LtEq,
    EqEq,
    Gt,
    GtEq,
    GtGt,
    BitXor,
    Xor,
    Or,
    OrOr,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct BinaryExpression {
    pub operation: AstNode<BinaryExpressionOperation>,
    pub a: AstNode<Expression>,
    pub b: AstNode<Expression>,
}

impl<'t> Parse<'t, nodes::BinaryExpression<'t>> for BinaryExpression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::BinaryExpression<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        type Operation<'t> = nodes::anon_unions::NotEq_And_AndAnd_Mul_MulMul_Add_Sub_Div_Lt_LtLt_LtEq_EqEq_Gt_GtEq_GtGt_BitXor_BitXorBitXor_Or_OrOr<'t>;

        let operation = value.op()?;

        let operation = match operation {
            Operation::NotEq(not_eq) => {
                AstNode::new(file, &not_eq, BinaryExpressionOperation::NotEq)
            }
            Operation::And(and) => AstNode::new(file, &and, BinaryExpressionOperation::And),
            Operation::AndAnd(and_and) => {
                AstNode::new(file, &and_and, BinaryExpressionOperation::AndAnd)
            }
            Operation::Mul(mul) => AstNode::new(file, &mul, BinaryExpressionOperation::Mul),
            Operation::MulMul(mul_mul) => {
                AstNode::new(file, &mul_mul, BinaryExpressionOperation::MulMul)
            }
            Operation::Add(add) => AstNode::new(file, &add, BinaryExpressionOperation::Add),
            Operation::Sub(sub) => AstNode::new(file, &sub, BinaryExpressionOperation::Sub),
            Operation::Div(div) => AstNode::new(file, &div, BinaryExpressionOperation::Div),
            Operation::Lt(lt) => AstNode::new(file, &lt, BinaryExpressionOperation::Lt),
            Operation::LtLt(lt_lt) => AstNode::new(file, &lt_lt, BinaryExpressionOperation::LtLt),
            Operation::LtEq(lt_eq) => AstNode::new(file, &lt_eq, BinaryExpressionOperation::LtEq),
            Operation::EqEq(eq_eq) => AstNode::new(file, &eq_eq, BinaryExpressionOperation::EqEq),
            Operation::Gt(gt) => AstNode::new(file, &gt, BinaryExpressionOperation::Gt),
            Operation::GtEq(gt_eq) => AstNode::new(file, &gt_eq, BinaryExpressionOperation::GtEq),
            Operation::GtGt(gt_gt) => AstNode::new(file, &gt_gt, BinaryExpressionOperation::GtGt),
            Operation::BitXor(bit_xor) => {
                AstNode::new(file, &bit_xor, BinaryExpressionOperation::BitXor)
            }
            Operation::BitXorBitXor(xor_xor) => {
                AstNode::new(file, &xor_xor, BinaryExpressionOperation::Xor)
            }
            Operation::Or(or) => AstNode::new(file, &or, BinaryExpressionOperation::Or),
            Operation::OrOr(or_or) => AstNode::new(file, &or_or, BinaryExpressionOperation::OrOr),
        };

        let a = value.a()?;
        let a = Expression::parse(file, input, a)?;

        let b = value.b()?;
        let b = Expression::parse(file, input, b)?;

        Ok(AstNode::new(file, &value, Self { operation, a, b }))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct MemberAccess {
    pub base: AstNode<Expression>,
    pub member: AstNode<ImString>,
}

impl<'t> Parse<'t, nodes::MemberAccess<'t>> for MemberAccess {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::MemberAccess<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let base = Expression::parse(file, input, value.base()?)?;
        let member = ImString::parse(file, input, value.member()?)?;

        Ok(AstNode::new(file, &value, Self { base, member }))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct Self_;

impl<'t> Parse<'t, nodes::Self_<'t>> for Self_ {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        _input: &'i str,
        value: nodes::Self_<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        Ok(AstNode::new(file, &value, Self))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct Vector2 {
    pub x: AstNode<Expression>,
    pub y: AstNode<Expression>,
}

impl<'t> Parse<'t, nodes::Vector2<'t>> for Vector2 {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::Vector2<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let x = value.x()?;
        let x = Expression::parse(file, input, x)?;

        let y = value.y()?;
        let y = Expression::parse(file, input, y)?;

        Ok(AstNode::new(file, &value, Self { x, y }))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct Vector3 {
    pub x: AstNode<Expression>,
    pub y: AstNode<Expression>,
    pub z: AstNode<Expression>,
}

impl<'t> Parse<'t, nodes::Vector3<'t>> for Vector3 {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::Vector3<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let x = value.x()?;
        let x = Expression::parse(file, input, x)?;

        let y = value.y()?;
        let y = Expression::parse(file, input, y)?;

        let z = value.z()?;
        let z = Expression::parse(file, input, z)?;

        Ok(AstNode::new(file, &value, Self { x, y, z }))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct Vector4 {
    pub x: AstNode<Expression>,
    pub y: AstNode<Expression>,
    pub z: AstNode<Expression>,
    pub w: AstNode<Expression>,
}

impl<'t> Parse<'t, nodes::Vector4<'t>> for Vector4 {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::Vector4<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let x = value.x()?;
        let x = Expression::parse(file, input, x)?;

        let y = value.y()?;
        let y = Expression::parse(file, input, y)?;

        let z = value.z()?;
        let z = Expression::parse(file, input, z)?;

        let w = value.w()?;
        let w = Expression::parse(file, input, w)?;

        Ok(AstNode::new(file, &value, Self { x, y, z, w }))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct IfExpression {
    pub condition: AstNode<Expression>,
    pub on_true: AstNode<Expression>,
    pub on_false: AstNode<Expression>,
}

impl<'t> Parse<'t, nodes::If<'t>> for IfExpression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::If<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let condition = value.condition()?;
        let condition = Expression::parse(file, input, condition)?;

        let on_true = Expression::parse(file, input, value.on_true()?)?;
        let on_false = Expression::parse(file, input, value.on_false()?)?;

        Ok(AstNode::new(
            file,
            &value,
            IfExpression {
                condition,
                on_true,
                on_false,
            },
        ))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct DictionaryMemberAssignment {
    pub index: usize,
    pub dependencies: HashableSet<ImString>,
    pub name: AstNode<ImString>,
    pub assignment: AstNode<Expression>,
}

impl<'t> Parse<'t, nodes::DictionaryMemberAssignment<'t>> for DictionaryMemberAssignment {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::DictionaryMemberAssignment<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let name = value.name()?;
        let name = ImString::parse(file, input, name)?;
        let assignment = Expression::parse(file, input, value.assignment()?)?;

        let mut dependencies = HashableSet::new();

        // Because our access collector never returns errors, this will never fail.
        find_all_variable_accesses_in_expression(&assignment.node, &mut |name| {
            dependencies.insert(name.node.clone());
            Ok(())
        })
        .ok();

        Ok(AstNode::new(
            file,
            &value,
            Self {
                index: 0,
                dependencies,
                name,
                assignment,
            },
        ))
    }
}

impl DependentOperation for AstNode<DictionaryMemberAssignment> {
    fn original_index(&self) -> usize {
        self.node.index
    }

    fn name(&self) -> &ImString {
        &self.node.name.node
    }

    fn dependencies(&self) -> &HashableSet<ImString> {
        &self.node.dependencies
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct DictionaryConstruction {
    pub assignments: Vec<AstNode<DictionaryMemberAssignment>>,
    pub compute_groups: Vec<std::ops::Range<usize>>,
}

impl DictionaryConstruction {
    pub fn compute_groups(&self) -> impl Iterator<Item = &[AstNode<DictionaryMemberAssignment>]> {
        self.compute_groups
            .iter()
            .map(|range| &self.assignments[range.clone()])
    }
}

impl<'t> Parse<'t, nodes::DictionaryConstruction<'t>> for DictionaryConstruction {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::DictionaryConstruction<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let mut assignments = Vec::new();
        let mut cursor = value.walk();
        let assignments_iter = value.assignmentss(&mut cursor);

        let mut variable_names = HashSet::new();
        let mut index = 0;

        for assignment in assignments_iter {
            let assignment = assignment?;

            // Skip the commas.
            if let Some(assignment) = assignment.as_dictionary_member_assignment() {
                let mut assignment = DictionaryMemberAssignment::parse(file, input, assignment)?;
                assignment
                    .node
                    .dependencies
                    .retain(|name| variable_names.contains(name));
                assignment.node.index = index;
                index += 1;

                variable_names.insert(assignment.node.name.node.clone());
                assignments.push(assignment);
            }
        }

        let compute_groups = sort_and_group_dependencies(&mut assignments);

        let node = Self {
            assignments,
            compute_groups,
        };
        Ok(AstNode::new(file, &value, node))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct ClosureDefinition {
    pub argument_type: AstNode<StructDefinition>,
    pub return_type: AstNode<Expression>,
    pub expression: Arc<AstNode<Expression>>,
}

impl<'t> Parse<'t, nodes::ClosureDefinition<'t>> for ClosureDefinition {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::ClosureDefinition<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let argument = value.argument()?;
        let argument = StructDefinition::parse(file, input, argument)?;

        let returns = value.result()?;
        let returns = Expression::parse(file, input, returns)?;

        let expression = value.expression()?;
        let expression = Expression::parse(file, input, expression)?;
        let expression = Arc::new(AstNode {
            reference: expression.reference,
            node: expression.node,
        });

        Ok(AstNode::new(
            file,
            &value,
            Self {
                argument_type: argument,
                return_type: returns,
                expression,
            },
        ))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct StructMember {
    pub name: AstNode<ImString>,
    pub ty: AstNode<Expression>,
    pub default: Option<AstNode<Expression>>,
}

impl<'t> Parse<'t, nodes::StructMember<'t>> for StructMember {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::StructMember<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let name = value.name()?;
        let name = ImString::parse(file, input, name)?;

        let ty = value.declaration_type()?.expression()?;
        let ty = Expression::parse(file, input, ty)?;

        let default = if let Some(default) = value.default() {
            let default = default?;
            Some(Expression::parse(file, input, default)?)
        } else {
            None
        };

        Ok(AstNode::new(file, &value, Self { name, ty, default }))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct StructDefinition {
    pub members: Vec<AstNode<StructMember>>,
    pub variadic: bool,
}

impl<'t> Parse<'t, nodes::StructDefinition<'t>> for StructDefinition {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::StructDefinition<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let mut cursor = value.walk();

        let mut members = Vec::new();
        for member in value.memberss(&mut cursor) {
            let member = member?;
            if let nodes::anon_unions::Comma_StructMember::StructMember(member) = member {
                let member = StructMember::parse(file, input, member)?;
                members.push(member);
            }
        }

        let mut cursor = value.walk();

        // There can only ever be one.
        let final_member = value.final_elements(&mut cursor).next();
        let variadic = match final_member {
            Option::Some(member) => {
                let member = member?;
                match member {
                    nodes::anon_unions::Comma_StructMember_VaradicDots::Comma(_comma) => false,
                    nodes::anon_unions::Comma_StructMember_VaradicDots::StructMember(member) => {
                        let member = StructMember::parse(file, input, member)?;
                        members.push(member);
                        false
                    }
                    nodes::anon_unions::Comma_StructMember_VaradicDots::VaradicDots(_) => true,
                }
            }
            Option::None => false,
        };

        Ok(AstNode::new(file, &value, Self { members, variadic }))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct FunctionCall {
    pub to_call: AstNode<Expression>,
    pub argument: AstNode<DictionaryConstruction>,
}

impl<'t> Parse<'t, nodes::FunctionCall<'t>> for FunctionCall {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::FunctionCall<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let to_call = Expression::parse(file, input, value.to_call()?)?;
        let argument = DictionaryConstruction::parse(file, input, value.argument()?)?;

        Ok(AstNode::new(file, &value, Self { to_call, argument }))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct MethodCall {
    pub self_dictionary: AstNode<Expression>,
    pub to_call: AstNode<ImString>,
    pub argument: AstNode<DictionaryConstruction>,
}

impl<'t> Parse<'t, nodes::MethodCall<'t>> for MethodCall {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::MethodCall<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let self_dictionary = Expression::parse(file, input, value.self_dictionary()?)?;
        let to_call = ImString::parse(file, input, value.to_call()?)?;
        let argument = DictionaryConstruction::parse(file, input, value.argument()?)?;

        Ok(AstNode::new(
            file,
            &value,
            Self {
                self_dictionary,
                to_call,
                argument,
            },
        ))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct LetInAssignment {
    pub index: usize,
    pub ident: AstNode<ImString>,
    pub dependencies: HashableSet<ImString>,
    pub value: AstNode<Expression>,
}

impl DependentOperation for AstNode<LetInAssignment> {
    fn original_index(&self) -> usize {
        self.node.index
    }

    fn name(&self) -> &ImString {
        &self.node.ident.node
    }

    fn dependencies(&self) -> &HashableSet<ImString> {
        &self.node.dependencies
    }
}

impl<'t> Parse<'t, nodes::LetInAssignment<'t>> for LetInAssignment {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        node: nodes::LetInAssignment<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let ident = ImString::parse(file, input, node.ident()?)?;

        let value = Expression::parse(file, input, node.value()?)?;

        let mut dependencies = HashableSet::new();

        // Because our access collector never returns errors, this will never fail.
        find_all_variable_accesses_in_expression(&value.node, &mut |name| {
            dependencies.insert(name.node.clone());
            Ok(())
        })
        .ok();

        Ok(AstNode::new(
            file,
            &node,
            Self {
                index: 0,
                ident,
                dependencies,
                value,
            },
        ))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct LetIn {
    pub assignments: Vec<AstNode<LetInAssignment>>,
    pub compute_groups: Vec<std::ops::Range<usize>>,
    pub expression: AstNode<Expression>,
}

impl LetIn {
    pub fn compute_groups(&self) -> impl Iterator<Item = &[AstNode<LetInAssignment>]> {
        self.compute_groups
            .iter()
            .map(|range| &self.assignments[range.clone()])
    }
}

impl<'t> Parse<'t, nodes::LetIn<'t>> for LetIn {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        node: nodes::LetIn<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let mut cursor = node.walk();
        let mut assignments = Vec::new();

        let mut variable_names = HashSet::new();

        for (index, assignment) in node.assignments(&mut cursor).enumerate() {
            let assignment = assignment?;
            let mut assignment = LetInAssignment::parse(file, input, assignment)?;
            // Restrict all dependencies to names within our dependency set, that we already know
            // of.
            assignment
                .node
                .dependencies
                .retain(|name| variable_names.contains(name));
            assignment.node.index = index;

            variable_names.insert(assignment.node.ident.node.clone());
            assignments.push(assignment);
        }

        let compute_groups = sort_and_group_dependencies(&mut assignments);

        let expression = Expression::parse(file, input, node.expression()?)?;

        Ok(AstNode::new(
            file,
            &node,
            Self {
                assignments,
                compute_groups,
                expression,
            },
        ))
    }
}

pub type Parser = type_sitter::Parser<nodes::SourceFile<'static>>;
pub fn new_parser() -> Parser {
    type_sitter::Parser::new(&tree_sitter_command_cad_model::language())
        .expect("Error loading CommandCadModel grammar")
}

#[cfg(test)]
mod test {
    use crate::compile::full_compile;
    use common_data_types::{Dimension, Float};
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn binary_expression() {
        let root = full_compile("2i + 3i");

        let binary_expression = root.node.as_binaryexpression().unwrap();
        let operation_reference = binary_expression.node.operation.reference.clone();

        let a = &binary_expression.node.a;
        let a_reference = a.reference.clone();

        let b = &binary_expression.node.b;
        let b_reference = b.reference.clone();

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::BinaryExpression(AstNode {
                    reference: binary_expression.reference.clone(),
                    node: Box::new(BinaryExpression {
                        operation: AstNode {
                            reference: operation_reference,
                            node: BinaryExpressionOperation::Add
                        },
                        a: AstNode {
                            reference: a_reference,
                            node: Expression::SignedInteger(AstNode {
                                reference: a.node.as_signedinteger().unwrap().reference.clone(),
                                node: 2
                            },)
                        },
                        b: AstNode {
                            reference: b_reference,
                            node: Expression::SignedInteger(AstNode {
                                reference: b.node.as_signedinteger().unwrap().reference.clone(),
                                node: 3
                            })
                        }
                    })
                })
            }
        );
    }

    #[test]
    fn boolean_true() {
        let root = full_compile("true");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::Boolean(AstNode {
                    reference: root.node.as_boolean().unwrap().reference.clone(),
                    node: true
                })
            }
        );
    }

    #[test]
    fn boolean_false() {
        let root = full_compile("false");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::Boolean(AstNode {
                    reference: root.node.as_boolean().unwrap().reference.clone(),
                    node: false
                })
            }
        );
    }

    #[test]
    fn closure_definition() {
        let root = full_compile("() -> thing: \"\"");
        let closure = root.node.as_closuredefinition().unwrap();
        let closure_reference = closure.reference.clone();
        let argument = &closure.node.argument_type;
        let argument_reference = argument.reference.clone();

        let returns = &closure.node.return_type;
        let returns_reference = returns.reference.clone();
        let returns_path = returns.node.as_identifier().unwrap();
        let returns_path_reference = returns_path.reference.clone();

        let expression = &closure.node.expression;
        let expression_reference = expression.reference.clone();
        let string_reference = expression.node.as_string().unwrap().reference.clone();

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::ClosureDefinition(AstNode {
                    reference: closure_reference,
                    node: Box::new(ClosureDefinition {
                        argument_type: AstNode {
                            reference: argument_reference,
                            node: StructDefinition {
                                members: vec![],
                                variadic: false
                            }
                        },
                        return_type: AstNode {
                            reference: returns_reference,
                            node: Expression::Identifier(AstNode {
                                reference: returns_path_reference,
                                node: ImString::from("thing")
                            })
                        },
                        expression: Arc::new(AstNode {
                            reference: expression_reference,
                            node: Expression::String(AstNode {
                                reference: string_reference,
                                node: String::new()
                            })
                        })
                    })
                })
            }
        );
    }

    #[test]
    fn dictionary_construction() {
        let root = full_compile("(a = true, b = false)");
        let construction_expression = root.node.as_dictionaryconstruction().unwrap();
        assert_eq!(construction_expression.node.assignments.len(), 2);
        let a = &construction_expression.node.assignments[0];
        let b = &construction_expression.node.assignments[1];

        assert_eq!(
            a.node.name,
            AstNode {
                reference: a.node.name.reference.clone(),
                node: "a".into()
            }
        );
        assert_eq!(
            a.node.assignment,
            AstNode {
                reference: a.node.assignment.reference.clone(),
                node: Expression::Boolean(AstNode {
                    reference: a.node.assignment.reference.clone(),
                    node: true
                })
            }
        );

        assert_eq!(
            b.node.name,
            AstNode {
                reference: b.node.name.reference.clone(),
                node: "b".into()
            }
        );
        assert_eq!(
            b.node.assignment,
            AstNode {
                reference: b.node.assignment.reference.clone(),
                node: Expression::Boolean(AstNode {
                    reference: b.node.assignment.reference.clone(),
                    node: false
                })
            }
        );
    }

    #[test]
    fn if_else_expression() {
        let root = full_compile("if true then \"true\" else \"false\"");
        let if_expression = root.node.as_if().unwrap();
        let if_reference = if_expression.reference.clone();
        let condition = &if_expression.node.condition;
        let condition_reference = condition.reference.clone();
        let boolean_reference = condition.node.as_boolean().unwrap().reference.clone();
        let true_expression_reference = if_expression.node.on_true.reference.clone();
        let true_string_reference = if_expression
            .node
            .on_true
            .node
            .as_string()
            .unwrap()
            .reference
            .clone();
        let false_expression_reference = if_expression.node.on_false.reference.clone();
        let false_string_reference = if_expression
            .node
            .on_false
            .node
            .as_string()
            .unwrap()
            .reference
            .clone();

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::If(AstNode {
                    reference: if_reference,
                    node: Box::new(IfExpression {
                        condition: AstNode {
                            reference: condition_reference,
                            node: Expression::Boolean(AstNode {
                                reference: boolean_reference,
                                node: true
                            })
                        },
                        on_true: AstNode {
                            reference: true_expression_reference,
                            node: Expression::String(AstNode {
                                reference: true_string_reference,
                                node: String::from("true")
                            })
                        },
                        on_false: AstNode {
                            reference: false_expression_reference,
                            node: Expression::String(AstNode {
                                reference: false_string_reference,
                                node: String::from("false")
                            })
                        },
                    })
                })
            }
        );
    }

    #[test]
    fn list() {
        let root = full_compile("[1i, 2i, 3i, 4i, 5i]");

        let list = root.node.as_list().unwrap();
        let list_reference = list.reference.clone();
        let element_1_reference = list.node[0].reference.clone();
        let integer_1_reference = list.node[0]
            .node
            .as_signedinteger()
            .unwrap()
            .reference
            .clone();
        let element_2_reference = list.node[1].reference.clone();
        let integer_2_reference = list.node[1]
            .node
            .as_signedinteger()
            .unwrap()
            .reference
            .clone();
        let element_3_reference = list.node[2].reference.clone();
        let integer_3_reference = list.node[2]
            .node
            .as_signedinteger()
            .unwrap()
            .reference
            .clone();
        let element_4_reference = list.node[3].reference.clone();
        let integer_4_reference = list.node[3]
            .node
            .as_signedinteger()
            .unwrap()
            .reference
            .clone();
        let element_5_reference = list.node[4].reference.clone();
        let integer_5_reference = list.node[4]
            .node
            .as_signedinteger()
            .unwrap()
            .reference
            .clone();

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::List(AstNode {
                    reference: list_reference,
                    node: vec![
                        AstNode {
                            reference: element_1_reference,
                            node: Expression::SignedInteger(AstNode {
                                reference: integer_1_reference,
                                node: 1,
                            })
                        },
                        AstNode {
                            reference: element_2_reference,
                            node: Expression::SignedInteger(AstNode {
                                reference: integer_2_reference,
                                node: 2,
                            })
                        },
                        AstNode {
                            reference: element_3_reference,
                            node: Expression::SignedInteger(AstNode {
                                reference: integer_3_reference,
                                node: 3,
                            })
                        },
                        AstNode {
                            reference: element_4_reference,
                            node: Expression::SignedInteger(AstNode {
                                reference: integer_4_reference,
                                node: 4,
                            })
                        },
                        AstNode {
                            reference: element_5_reference,
                            node: Expression::SignedInteger(AstNode {
                                reference: integer_5_reference,
                                node: 5,
                            })
                        },
                    ]
                })
            }
        );
    }

    #[test]
    fn parenthesis() {
        let root = full_compile("(5i)");
        let parenthesis = root.node.as_parenthesis().unwrap();
        let parenthesis_reference = parenthesis.reference.clone();
        let integer = parenthesis.node.as_signedinteger().unwrap();
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::Parenthesis(Box::new(AstNode {
                    reference: parenthesis_reference,
                    node: Expression::SignedInteger(AstNode {
                        reference: integer.reference.clone(),
                        node: 5
                    })
                }))
            }
        );
    }

    #[test]
    fn member_access() {
        let root = full_compile("this.thang");
        let path = root.node.as_memberaccess().unwrap();
        let base = &path.node.base;
        let this = base.node.as_identifier().unwrap();
        let member = &path.node.member;

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::MemberAccess(Box::new(AstNode {
                    reference: path.reference.clone(),
                    node: MemberAccess {
                        base: AstNode {
                            reference: base.reference.clone(),
                            node: Expression::Identifier(AstNode {
                                reference: this.reference.clone(),
                                node: ImString::from("this")
                            })
                        },
                        member: AstNode {
                            reference: member.reference.clone(),
                            node: ImString::from("thang")
                        }
                    }
                }))
            }
        );
    }

    #[test]
    fn self_path() {
        let root = full_compile("self.this.thang");
        let thang = root.node.as_memberaccess().unwrap();
        let this_expr = &thang.node.base;
        let this = this_expr.node.as_memberaccess().unwrap();
        let self_expr = &this.node.base;
        let self_ = self_expr.node.as_self_().unwrap();

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::MemberAccess(Box::new(AstNode {
                    reference: thang.reference.clone(),
                    node: MemberAccess {
                        base: AstNode {
                            reference: this.reference.clone(),
                            node: Expression::MemberAccess(Box::new(AstNode {
                                reference: this.reference.clone(),
                                node: MemberAccess {
                                    base: AstNode {
                                        reference: self_expr.reference.clone(),
                                        node: Expression::Self_(AstNode {
                                            reference: self_.reference.clone(),
                                            node: Self_
                                        })
                                    },
                                    member: AstNode {
                                        reference: this.node.member.reference.clone(),
                                        node: ImString::from("this")
                                    }
                                }
                            }))
                        },
                        member: AstNode {
                            reference: thang.node.member.reference.clone(),
                            node: ImString::from("thang")
                        }
                    }
                }))
            }
        );
    }

    #[test]
    fn scalar_no_decimal() {
        let root = full_compile("0");

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::Scalar(AstNode {
                    reference: root.node.as_scalar().unwrap().reference.clone(),
                    node: Scalar {
                        dimension: Dimension::zero(),
                        value: Float::new(0.0).unwrap()
                    }
                })
            }
        );
    }

    #[test]
    fn scalar_no_unit() {
        let root = full_compile("0.0");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::Scalar(AstNode {
                    reference: root.node.as_scalar().unwrap().reference.clone(),
                    node: Scalar {
                        dimension: Dimension::zero(),
                        value: Float::new(0.0).unwrap()
                    }
                })
            }
        );
    }

    #[test]
    fn scalar_with_unit() {
        let root = full_compile("0.0mm");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::Scalar(AstNode {
                    reference: root.node.as_scalar().unwrap().reference.clone(),
                    node: Scalar {
                        dimension: Dimension::length(),
                        value: Float::new(0.0).unwrap()
                    }
                })
            }
        );
    }

    #[test]
    fn scalar_unit_conversion() {
        // Test conversion factor
        let root = full_compile("1cm");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::Scalar(AstNode {
                    reference: root.node.as_scalar().unwrap().reference.clone(),
                    node: Scalar {
                        dimension: Dimension::length(),
                        value: Float::new(0.01).unwrap()
                    }
                })
            }
        );
    }

    #[test]
    fn scalar_quoted_unit() {
        // Test conversion factor
        let root = full_compile("1'm^2'");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::Scalar(AstNode {
                    reference: root.node.as_scalar().unwrap().reference.clone(),
                    node: Scalar {
                        dimension: Dimension::area(),
                        value: Float::new(1.0).unwrap()
                    }
                })
            }
        );
    }

    #[test]
    fn signed_integer() {
        let root = full_compile("5i");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::SignedInteger(AstNode {
                    reference: root.node.as_signedinteger().unwrap().reference.clone(),
                    node: 5
                })
            }
        );
    }

    #[test]
    fn signed_integer_hex() {
        let root = full_compile("0x5i");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::SignedInteger(AstNode {
                    reference: root.node.as_signedinteger().unwrap().reference.clone(),
                    node: 0x5
                })
            }
        );
    }

    #[test]
    fn signed_integer_octal() {
        let root = full_compile("0o5i");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::SignedInteger(AstNode {
                    reference: root.node.as_signedinteger().unwrap().reference.clone(),
                    node: 0o5
                })
            }
        );
    }

    #[test]
    fn signed_integer_binary() {
        let root = full_compile("0b1010i");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::SignedInteger(AstNode {
                    reference: root.node.as_signedinteger().unwrap().reference.clone(),
                    node: 0b1010
                })
            }
        );
    }

    #[test]
    fn string() {
        let root = full_compile("\"Some text\\n\"");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::String(AstNode {
                    reference: root.node.as_string().unwrap().reference.clone(),
                    node: "Some text\n".into()
                })
            }
        );
    }

    #[test]
    fn struct_definition() {
        let root = full_compile("( one: One, two: Two = a, ... )");
        let struct_definition = root.node.as_structdefinition().unwrap();
        let members = &struct_definition.node.members;
        let one = &members[0];
        let one_ty = one.node.ty.node.as_identifier().unwrap();
        let two = &members[1];
        let two_ty = two.node.ty.node.as_identifier().unwrap();
        let two_default = two.node.default.as_ref().unwrap();
        let two_default_path = two_default.node.as_identifier().unwrap();

        assert!(struct_definition.node.variadic);
        assert_eq!(members.len(), 2);

        assert_eq!(
            *one,
            AstNode {
                reference: one.reference.clone(),
                node: StructMember {
                    name: AstNode {
                        reference: one.node.name.reference.clone(),
                        node: "one".into()
                    },
                    ty: AstNode {
                        reference: one.node.ty.reference.clone(),
                        node: Expression::Identifier(AstNode {
                            reference: one_ty.reference.clone(),
                            node: "One".into()
                        })
                    },
                    default: None
                }
            }
        );
        assert_eq!(
            *two,
            AstNode {
                reference: two.reference.clone(),
                node: StructMember {
                    name: AstNode {
                        reference: two.node.name.reference.clone(),
                        node: "two".into()
                    },
                    ty: AstNode {
                        reference: two.node.ty.reference.clone(),
                        node: Expression::Identifier(AstNode {
                            reference: two_ty.reference.clone(),
                            node: "Two".into()
                        })
                    },
                    default: Some(AstNode {
                        reference: two_default.reference.clone(),
                        node: Expression::Identifier(AstNode {
                            reference: two_default_path.reference.clone(),
                            node: "a".into()
                        })
                    })
                }
            }
        );
    }

    #[test]
    fn unary_expression() {
        let root = full_compile("-5i");
        let unary_expression = root.node.as_unaryexpression().unwrap();
        let unary_expression_reference = unary_expression.reference.clone();
        let expression_reference = unary_expression.node.expression.reference.clone();
        let integer_reference = unary_expression
            .node
            .expression
            .node
            .as_signedinteger()
            .unwrap()
            .reference
            .clone();
        let operation_reference = unary_expression.node.operation.reference.clone();

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::UnaryExpression(AstNode {
                    reference: unary_expression_reference,
                    node: Box::new(UnaryExpression {
                        expression: AstNode {
                            reference: expression_reference,
                            node: Expression::SignedInteger(AstNode {
                                reference: integer_reference,
                                node: 5
                            })
                        },
                        operation: AstNode {
                            reference: operation_reference,
                            node: UnaryExpressionOperation::Sub
                        }
                    })
                })
            }
        );
    }

    #[test]
    fn unsigned_integer() {
        let root = full_compile("5u");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::UnsignedInteger(AstNode {
                    reference: root.node.as_unsignedinteger().unwrap().reference.clone(),
                    node: 5
                })
            }
        );
    }

    #[test]
    fn unsigned_integer_hex() {
        let root = full_compile("0x5u");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::UnsignedInteger(AstNode {
                    reference: root.node.as_unsignedinteger().unwrap().reference.clone(),
                    node: 0x5
                })
            }
        );
    }

    #[test]
    fn unsigned_integer_octal() {
        let root = full_compile("0o5u");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::UnsignedInteger(AstNode {
                    reference: root.node.as_unsignedinteger().unwrap().reference.clone(),
                    node: 0o5
                })
            }
        );
    }

    #[test]
    fn unsigned_integer_binary() {
        let root = full_compile("0b1010u");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::UnsignedInteger(AstNode {
                    reference: root.node.as_unsignedinteger().unwrap().reference.clone(),
                    node: 0b1010
                })
            }
        );
    }

    #[test]
    fn function_call_no_arguments() {
        let root = full_compile("a()");
        let call = root.node.as_functioncall().unwrap();
        let to_call = call.node.to_call.node.as_identifier().unwrap();
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::FunctionCall(AstNode {
                    reference: call.reference.clone(),
                    node: Box::new(FunctionCall {
                        to_call: AstNode {
                            reference: call.node.to_call.reference.clone(),
                            node: Expression::Identifier(AstNode {
                                reference: to_call.reference.clone(),
                                node: "a".into()
                            })
                        },
                        argument: AstNode {
                            reference: call.node.argument.reference.clone(),
                            node: DictionaryConstruction {
                                assignments: vec![],
                                compute_groups: vec![]
                            }
                        }
                    })
                })
            }
        );
    }

    #[test]
    fn function_call_with_arguments() {
        let root = full_compile("a(value = 24u)");
        let call = root.node.as_functioncall().unwrap();
        let to_call = call.node.to_call.node.as_identifier().unwrap();
        let dict = &call.node.argument.node;
        let dict_assignment = &dict.assignments[0];
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::FunctionCall(AstNode {
                    reference: call.reference.clone(),
                    node: Box::new(FunctionCall {
                        to_call: AstNode {
                            reference: call.node.to_call.reference.clone(),
                            node: Expression::Identifier(AstNode {
                                reference: to_call.reference.clone(),
                                node: "a".into()
                            })
                        },
                        argument: AstNode {
                            reference: call.node.argument.reference.clone(),
                            node: DictionaryConstruction {
                                assignments: vec![AstNode {
                                    reference: dict_assignment.reference.clone(),
                                    node: DictionaryMemberAssignment {
                                        index: 0,
                                        name: AstNode {
                                            reference: dict_assignment.node.name.reference.clone(),
                                            node: "value".into()
                                        },
                                        dependencies: HashableSet::from(HashSet::from_iter([])),
                                        assignment: AstNode {
                                            reference: dict_assignment
                                                .node
                                                .assignment
                                                .reference
                                                .clone(),
                                            node: Expression::UnsignedInteger(AstNode {
                                                reference: dict_assignment
                                                    .node
                                                    .assignment
                                                    .node
                                                    .as_unsignedinteger()
                                                    .unwrap()
                                                    .reference
                                                    .clone(),
                                                node: 24u64
                                            })
                                        }
                                    }
                                }],
                                compute_groups: vec![0..1]
                            }
                        }
                    })
                })
            }
        );
    }

    #[test]
    fn method_call_no_arguments() {
        let root = full_compile("5u::c()");
        let call = root.node.as_methodcall().unwrap();
        let to_call = &call.node.to_call;
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::MethodCall(AstNode {
                    reference: call.reference.clone(),
                    node: Box::new(MethodCall {
                        self_dictionary: AstNode {
                            reference: call.node.self_dictionary.reference.clone(),
                            node: Expression::UnsignedInteger(AstNode {
                                reference: call
                                    .node
                                    .self_dictionary
                                    .node
                                    .as_unsignedinteger()
                                    .unwrap()
                                    .reference
                                    .clone(),
                                node: 5u64
                            })
                        },
                        to_call: AstNode {
                            reference: to_call.reference.clone(),
                            node: "c".into()
                        },
                        argument: AstNode {
                            reference: call.node.argument.reference.clone(),
                            node: DictionaryConstruction {
                                assignments: vec![],
                                compute_groups: vec![],
                            }
                        }
                    })
                })
            }
        );
    }

    #[test]
    fn method_call_with_arguments() {
        let root = full_compile("83u::c(value = 95u)");
        let call = root.node.as_methodcall().unwrap();
        let to_call = &call.node.to_call;
        let dict = &call.node.argument.node;
        let dict_assignment = &dict.assignments[0];
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::MethodCall(AstNode {
                    reference: call.reference.clone(),
                    node: Box::new(MethodCall {
                        self_dictionary: AstNode {
                            reference: call.node.self_dictionary.reference.clone(),
                            node: Expression::UnsignedInteger(AstNode {
                                reference: call
                                    .node
                                    .self_dictionary
                                    .node
                                    .as_unsignedinteger()
                                    .unwrap()
                                    .reference
                                    .clone(),
                                node: 83u64
                            })
                        },
                        to_call: AstNode {
                            reference: to_call.reference.clone(),
                            node: "c".into()
                        },
                        argument: AstNode {
                            reference: call.node.argument.reference.clone(),
                            node: DictionaryConstruction {
                                assignments: vec![AstNode {
                                    reference: dict_assignment.reference.clone(),
                                    node: DictionaryMemberAssignment {
                                        index: 0,
                                        dependencies: HashableSet::from(HashSet::from_iter([])),
                                        name: AstNode {
                                            reference: dict_assignment.node.name.reference.clone(),
                                            node: "value".into()
                                        },
                                        assignment: AstNode {
                                            reference: dict_assignment
                                                .node
                                                .assignment
                                                .reference
                                                .clone(),
                                            node: Expression::UnsignedInteger(AstNode {
                                                reference: dict_assignment
                                                    .node
                                                    .assignment
                                                    .node
                                                    .as_unsignedinteger()
                                                    .unwrap()
                                                    .reference
                                                    .clone(),
                                                node: 95u64
                                            })
                                        }
                                    }
                                }],
                                compute_groups: vec![0..1]
                            }
                        }
                    })
                })
            }
        );
    }

    #[test]
    fn let_in() {
        let root = full_compile("let value1 = 1u; value2 = 2u; in 3u");

        let let_in = root.node.as_letin().unwrap();

        let value1 = &let_in.node.assignments[0];
        let value1_ident = &value1.node.ident;
        let value1_value = &value1.node.value;
        let value1_value_uint = &value1.node.value.node.as_unsignedinteger().unwrap();

        let value2 = &let_in.node.assignments[1];
        let value2_ident = &value2.node.ident;
        let value2_value = &value2.node.value;
        let value2_value_uint = &value2.node.value.node.as_unsignedinteger().unwrap();

        let expression = &let_in.node.expression;
        let expression_uint = &let_in.node.expression.node.as_unsignedinteger().unwrap();

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::LetIn(
                    AstNode {
                        reference: let_in.reference.clone(),
                        node: LetIn {
                            assignments: vec![
                                AstNode {
                                    reference: value1.reference.clone(),
                                    node: LetInAssignment {
                                        ident: AstNode {
                                            reference: value1_ident.reference.clone(),
                                            node: "value1".into(),
                                        },
                                        index: 0,
                                        dependencies: HashableSet::new(),
                                        value: AstNode {
                                            reference: value1_value.reference.clone(),
                                            node: Expression::UnsignedInteger(AstNode {
                                                reference: value1_value_uint.reference.clone(),
                                                node: 1
                                            }),
                                        }
                                    }
                                },
                                AstNode {
                                    reference: value2.reference.clone(),
                                    node: LetInAssignment {
                                        ident: AstNode {
                                            reference: value2_ident.reference.clone(),
                                            node: "value2".into(),
                                        },
                                        index: 1,
                                        dependencies: HashableSet::new(),
                                        value: AstNode {
                                            reference: value2_value.reference.clone(),
                                            node: Expression::UnsignedInteger(AstNode {
                                                reference: value2_value_uint.reference.clone(),
                                                node: 2
                                            }),
                                        }
                                    }
                                }
                            ],
                            compute_groups: vec![0..2],
                            expression: AstNode {
                                reference: expression.reference.clone(),
                                node: Expression::UnsignedInteger(AstNode {
                                    reference: expression_uint.reference.clone(),
                                    node: 3
                                })
                            }
                        }
                    }
                    .into_box()
                )
            }
        );
    }

    #[derive(Debug, PartialEq, Eq)]
    struct TestDependency {
        name: ImString,
        index: usize,
        dependencies: HashableSet<ImString>,
    }

    impl TestDependency {
        fn new(
            index: usize,
            name: impl Into<ImString>,
            dependencies: impl IntoIterator<Item = &'static str>,
        ) -> Self {
            Self {
                index,
                name: name.into(),
                dependencies: HashableSet::from(HashSet::from_iter(
                    dependencies.into_iter().map(|name| ImString::from(name)),
                )),
            }
        }
    }

    impl DependentOperation for TestDependency {
        fn original_index(&self) -> usize {
            self.index
        }

        fn name(&self) -> &ImString {
            &self.name
        }

        fn dependencies(&self) -> &HashableSet<ImString> {
            &self.dependencies
        }
    }

    #[test]
    fn let_in_ordering() {
        let mut dependencies = vec![
            TestDependency::new(0, "a", []),
            TestDependency::new(1, "c", ["a"]),
            TestDependency::new(2, "d", ["c"]),
            TestDependency::new(3, "b", []),
        ];

        let groups = sort_and_group_dependencies(&mut dependencies);

        assert_eq!(
            dependencies,
            vec![
                TestDependency::new(0, "a", []),
                TestDependency::new(3, "b", []),
                TestDependency::new(1, "c", ["a"]),
                TestDependency::new(2, "d", ["c"]),
            ]
        );

        assert_eq!(groups, vec![0..2, 2..3, 3..4]);
    }

    #[test]
    fn let_in_broken_ordering() {
        // This would be a case of user error, but we need to keep the user's mistakes in the new
        // ordering to make the error messages make sense.
        let mut dependencies = vec![
            TestDependency::new(0, "a", []),
            TestDependency::new(1, "d", ["c"]),
            TestDependency::new(2, "c", ["a"]),
            TestDependency::new(3, "b", []),
        ];

        let groups = sort_and_group_dependencies(&mut dependencies);

        assert_eq!(
            dependencies,
            vec![
                TestDependency::new(0, "a", []),
                TestDependency::new(3, "b", []),
                TestDependency::new(1, "d", ["c"]),
                TestDependency::new(2, "c", ["a"]),
            ]
        );

        assert_eq!(groups, vec![0..2, 2..3, 3..4]);
    }

    #[test]
    fn let_in_circular_dependency() {
        let mut dependencies = vec![
            TestDependency::new(0, "a", ["b"]),
            TestDependency::new(1, "b", ["a"]),
        ];

        let groups = sort_and_group_dependencies(&mut dependencies);

        assert_eq!(
            dependencies,
            vec![
                TestDependency::new(0, "a", ["b"]),
                TestDependency::new(1, "b", ["a"]),
            ]
        );

        assert_eq!(groups, vec![0..1, 1..2]);
    }

    #[test]
    fn vector2() {
        let root = full_compile("<(1.0, 2.0)>");
        let vector = root.node.as_vector2().unwrap();
        let x = &vector.node.x;
        let x_scalar = x.node.as_scalar().unwrap();
        let y = &vector.node.y;
        let y_scalar = y.node.as_scalar().unwrap();

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::Vector2(AstNode {
                    reference: vector.reference.clone(),
                    node: Box::new(Vector2 {
                        x: AstNode {
                            reference: x.reference.clone(),
                            node: Expression::Scalar(AstNode {
                                reference: x_scalar.reference.clone(),
                                node: Scalar {
                                    dimension: Dimension::zero(),
                                    value: Float::new(1.0).unwrap()
                                }
                            })
                        },
                        y: AstNode {
                            reference: y.reference.clone(),
                            node: Expression::Scalar(AstNode {
                                reference: y_scalar.reference.clone(),
                                node: Scalar {
                                    dimension: Dimension::zero(),
                                    value: Float::new(2.0).unwrap()
                                }
                            })
                        }
                    })
                })
            }
        );
    }

    #[test]
    fn vector3() {
        let root = full_compile("<(1.0, 2.0, 3.0)>");
        let vector = root.node.as_vector3().unwrap();
        let x = &vector.node.x;
        let x_scalar = x.node.as_scalar().unwrap();
        let y = &vector.node.y;
        let y_scalar = y.node.as_scalar().unwrap();
        let z = &vector.node.z;
        let z_scalar = z.node.as_scalar().unwrap();

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::Vector3(AstNode {
                    reference: vector.reference.clone(),
                    node: Box::new(Vector3 {
                        x: AstNode {
                            reference: x.reference.clone(),
                            node: Expression::Scalar(AstNode {
                                reference: x_scalar.reference.clone(),
                                node: Scalar {
                                    dimension: Dimension::zero(),
                                    value: Float::new(1.0).unwrap()
                                }
                            })
                        },
                        y: AstNode {
                            reference: y.reference.clone(),
                            node: Expression::Scalar(AstNode {
                                reference: y_scalar.reference.clone(),
                                node: Scalar {
                                    dimension: Dimension::zero(),
                                    value: Float::new(2.0).unwrap()
                                }
                            })
                        },
                        z: AstNode {
                            reference: z.reference.clone(),
                            node: Expression::Scalar(AstNode {
                                reference: z_scalar.reference.clone(),
                                node: Scalar {
                                    dimension: Dimension::zero(),
                                    value: Float::new(3.0).unwrap()
                                }
                            })
                        }
                    })
                })
            }
        );
    }

    #[test]
    fn vector4() {
        let root = full_compile("<(1.0, 2.0, 3.0, 4.0)>");
        let vector = root.node.as_vector4().unwrap();
        let x = &vector.node.x;
        let x_scalar = x.node.as_scalar().unwrap();
        let y = &vector.node.y;
        let y_scalar = y.node.as_scalar().unwrap();
        let z = &vector.node.z;
        let z_scalar = z.node.as_scalar().unwrap();
        let w = &vector.node.w;
        let w_scalar = w.node.as_scalar().unwrap();

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::Vector4(AstNode {
                    reference: vector.reference.clone(),
                    node: Box::new(Vector4 {
                        x: AstNode {
                            reference: x.reference.clone(),
                            node: Expression::Scalar(AstNode {
                                reference: x_scalar.reference.clone(),
                                node: Scalar {
                                    dimension: Dimension::zero(),
                                    value: Float::new(1.0).unwrap()
                                }
                            })
                        },
                        y: AstNode {
                            reference: y.reference.clone(),
                            node: Expression::Scalar(AstNode {
                                reference: y_scalar.reference.clone(),
                                node: Scalar {
                                    dimension: Dimension::zero(),
                                    value: Float::new(2.0).unwrap()
                                }
                            })
                        },
                        z: AstNode {
                            reference: z.reference.clone(),
                            node: Expression::Scalar(AstNode {
                                reference: z_scalar.reference.clone(),
                                node: Scalar {
                                    dimension: Dimension::zero(),
                                    value: Float::new(3.0).unwrap()
                                }
                            })
                        },
                        w: AstNode {
                            reference: w.reference.clone(),
                            node: Expression::Scalar(AstNode {
                                reference: w_scalar.reference.clone(),
                                node: Scalar {
                                    dimension: Dimension::zero(),
                                    value: Float::new(4.0).unwrap()
                                }
                            })
                        }
                    })
                })
            }
        );
    }
}
