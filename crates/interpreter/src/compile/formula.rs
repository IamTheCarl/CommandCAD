/*
 * Copyright 2026 James Carl
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

use std::{path::PathBuf, sync::Arc};

use crate::compile::{nodes, Error, Parse};

use super::{AstNode, Scalar};
use imstr::ImString;
use type_sitter::HasChild;

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct Formula {
    left: AstNode<Expression>,
    right: AstNode<Expression>,
    relation: Relation,
}

impl<'t> Parse<'t, nodes::Formula<'t>> for Formula {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::Formula<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let left = Expression::parse(file, input, value.lhs()?)?;
        let right = Expression::parse(file, input, value.rhs()?)?;

        type Operator<'t> = nodes::anon_unions::NotEq_Lt_LtEq_EqEq_Gt_GtEq<'t>;
        let relation = match value.relation()? {
            Operator::Lt(_) => Relation::Less,
            Operator::LtEq(_) => Relation::LessEqual,
            Operator::EqEq(_) => Relation::Equal,
            Operator::GtEq(_) => Relation::GreaterEqual,
            Operator::Gt(_) => Relation::Greater,
            Operator::NotEq(_) => Relation::NotEqual,
        };

        Ok(AstNode::new(
            file,
            &value,
            Self {
                left,
                right,
                relation,
            },
        ))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum Relation {
    Less,
    LessEqual,
    Equal,
    GreaterEqual,
    Greater,
    NotEqual,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum Expression {
    Parenthesis(Box<AstNode<Expression>>),
    SignedInteger(AstNode<i64>),
    UnsignedInteger(AstNode<u64>),
    Boolean(AstNode<bool>),
    Scalar(AstNode<Scalar>),
    Vector2(AstNode<Box<Vector2>>),
    Vector3(AstNode<Box<Vector3>>),
    Vector4(AstNode<Box<Vector4>>),
    Identifier(AstNode<ImString>),
    UnaryExpression(Box<AstNode<UnaryExpression>>),
    BinaryExpression(Box<AstNode<BinaryExpression>>),
    FunctionCall(Box<AstNode<FunctionCall>>),
    MethodCall(Box<AstNode<MethodCall>>),
}

impl<'t> Parse<'t, nodes::FormulaExpression<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::FormulaExpression<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        type ChildType<'t> = <nodes::FormulaExpression<'t> as HasChild<'t>>::Child;

        match value.child()? {
            ChildType::FormulaBinaryExpression(binary_expression) => {
                Self::parse(file, input, binary_expression)
            }
            ChildType::Boolean(value) => Self::parse(file, input, value),
            ChildType::FormulaParenthesis(parenthesis) => Self::parse(file, input, parenthesis),
            ChildType::Identifier(ident) => Ok(AstNode::new(
                file,
                &value,
                Self::Identifier(ImString::parse(file, input, ident)?),
            )),
            ChildType::Scalar(scalar) => Self::parse(file, input, scalar),
            ChildType::FormulaVector2(vector) => Self::parse(file, input, vector),
            ChildType::FormulaVector3(vector) => Self::parse(file, input, vector),
            ChildType::FormulaVector4(vector) => Self::parse(file, input, vector),
            ChildType::SignedInteger(signed_integer) => Self::parse(file, input, signed_integer),
            ChildType::FormulaUnaryExpression(unary_expression) => {
                Self::parse(file, input, unary_expression)
            }
            ChildType::UnsignedInteger(unsigned_integer) => {
                Self::parse(file, input, unsigned_integer)
            }
            ChildType::FormulaFunctionCall(function_call) => {
                Self::parse(file, input, function_call)
            }
            ChildType::FormulaMethodCall(method_call) => Self::parse(file, input, method_call),
        }
    }
}

impl<'t> Parse<'t, nodes::FormulaBinaryExpression<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::FormulaBinaryExpression<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        Ok(AstNode::new(
            file,
            &value,
            Self::BinaryExpression(Box::new(BinaryExpression::parse(file, input, value)?)),
        ))
    }
}

impl<'t> Parse<'t, nodes::Boolean<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::Boolean<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        Ok(AstNode::new(
            file,
            &value,
            Self::Boolean(bool::parse(file, input, value)?),
        ))
    }
}

impl<'t> Parse<'t, nodes::FormulaParenthesis<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::FormulaParenthesis<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let expression = value.formula_expression()?;
        Ok(AstNode::new(
            file,
            &value,
            Self::Parenthesis(Box::new(Self::parse(file, input, expression)?)),
        ))
    }
}

impl<'t> Parse<'t, nodes::Scalar<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::Scalar<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        Ok(AstNode::new(
            file,
            &value,
            Self::Scalar(Scalar::parse(file, input, value)?),
        ))
    }
}

impl<'t> Parse<'t, nodes::FormulaVector2<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::FormulaVector2<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        Ok(AstNode::new(
            file,
            &value,
            Self::Vector2(Vector2::parse(file, input, value)?.into_box()),
        ))
    }
}

impl<'t> Parse<'t, nodes::FormulaVector3<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::FormulaVector3<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        Ok(AstNode::new(
            file,
            &value,
            Self::Vector3(Vector3::parse(file, input, value)?.into_box()),
        ))
    }
}

impl<'t> Parse<'t, nodes::FormulaVector4<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::FormulaVector4<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
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
        Ok(AstNode::new(
            file,
            &value,
            Self::UnsignedInteger(u64::parse(file, input, value)?),
        ))
    }
}

impl<'t> Parse<'t, nodes::FormulaUnaryExpression<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::FormulaUnaryExpression<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        Ok(AstNode::new(
            file,
            &value,
            Self::UnaryExpression(Box::new(UnaryExpression::parse(file, input, value)?)),
        ))
    }
}

impl<'t> Parse<'t, nodes::FormulaFunctionCall<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::FormulaFunctionCall<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        Ok(AstNode::new(
            file,
            &value,
            Self::FunctionCall(Box::new(FunctionCall::parse(file, input, value)?)),
        ))
    }
}

impl<'t> Parse<'t, nodes::FormulaMethodCall<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::FormulaMethodCall<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        Ok(AstNode::new(
            file,
            &value,
            Self::MethodCall(Box::new(MethodCall::parse(file, input, value)?)),
        ))
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

impl<'t> Parse<'t, nodes::FormulaUnaryExpression<'t>> for UnaryExpression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::FormulaUnaryExpression<'t>,
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

        let expression = Expression::parse(file, input, value.formula_expression()?)?;

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
    AndAnd,
    Mul,
    MulMul,
    Add,
    Sub,
    Div,
    Xor,
    OrOr,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct BinaryExpression {
    pub operation: AstNode<BinaryExpressionOperation>,
    pub a: AstNode<Expression>,
    pub b: AstNode<Expression>,
}

impl<'t> Parse<'t, nodes::FormulaBinaryExpression<'t>> for BinaryExpression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::FormulaBinaryExpression<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        type Operation<'t> =
            nodes::anon_unions::AndAnd_Mul_MulMul_Add_Sub_Div_BitXorBitXor_OrOr<'t>;

        let operation = value.op()?;

        let operation = match operation {
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
            Operation::BitXorBitXor(xor_xor) => {
                AstNode::new(file, &xor_xor, BinaryExpressionOperation::Xor)
            }
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
pub struct Vector2 {
    pub x: AstNode<Expression>,
    pub y: AstNode<Expression>,
}

impl<'t> Parse<'t, nodes::FormulaVector2<'t>> for Vector2 {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::FormulaVector2<'t>,
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

impl<'t> Parse<'t, nodes::FormulaVector3<'t>> for Vector3 {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::FormulaVector3<'t>,
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

impl<'t> Parse<'t, nodes::FormulaVector4<'t>> for Vector4 {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::FormulaVector4<'t>,
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
pub struct FunctionCall {
    pub to_call: AstNode<Expression>,
    pub argument: AstNode<Expression>,
}

impl<'t> Parse<'t, nodes::FormulaFunctionCall<'t>> for FunctionCall {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::FormulaFunctionCall<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let to_call = Expression::parse(file, input, value.to_call()?)?;
        let argument = Expression::parse(file, input, value.argument()?)?;

        Ok(AstNode::new(file, &value, Self { to_call, argument }))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct MethodCall {
    pub self_dictionary: AstNode<Expression>,
    pub to_call: AstNode<ImString>,
    pub argument: AstNode<Expression>,
}

impl<'t> Parse<'t, nodes::FormulaMethodCall<'t>> for MethodCall {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::FormulaMethodCall<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let self_dictionary = Expression::parse(file, input, value.self_dictionary()?)?;
        let to_call = ImString::parse(file, input, value.to_call()?)?;
        let argument = Expression::parse(file, input, value.argument()?)?;

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
