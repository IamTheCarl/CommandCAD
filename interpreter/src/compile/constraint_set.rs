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

use crate::compile::{nodes, unwrap_missing, Error, Parse};

use super::{AstNode, Scalar};
use imstr::ImString;
use type_sitter::{HasChild, Node};
use unwrap_enum::EnumAs;

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct ConstraintSet {
    pub variables: Vec<AstNode<ImString>>,
    pub left: AstNode<ConstraintSetExpression>,
    pub right: AstNode<ConstraintSetExpression>,
    pub relation: Relation,
}

impl<'t> Parse<'t, nodes::ConstraintSet<'t>> for ConstraintSet {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::ConstraintSet<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

        let variables = {
            let variables_node = value.variables()?;
            let mut cursor = variables_node.walk();

            let mut variables = Vec::new();
            for field in variables_node.identifiers(&mut cursor) {
                variables.push(ImString::parse(file, input, field)?);
            }

            variables
        };

        let left = ConstraintSetExpression::parse(file, input, value.lhs()?)?;
        let right = ConstraintSetExpression::parse(file, input, value.rhs()?)?;

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
                variables,
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

#[derive(Debug, Hash, Eq, PartialEq, EnumAs)]
pub enum ConstraintSetExpression {
    Parenthesis(Box<AstNode<ConstraintSetExpression>>),
    Scalar(AstNode<Scalar>),
    Identifier(AstNode<ImString>),
    UnaryExpression(Box<AstNode<UnaryExpression>>),
    BinaryExpression(Box<AstNode<BinaryExpression>>),
    MethodCall(Box<AstNode<MethodCall>>),
}

impl<'t> Parse<'t, nodes::ConstraintSetExpression<'t>> for ConstraintSetExpression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::ConstraintSetExpression<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        type ChildType<'t> = <nodes::ConstraintSetExpression<'t> as HasChild<'t>>::Child;

        unwrap_missing(&value)?;

        match value.child()? {
            ChildType::ConstraintSetBinaryExpression(binary_expression) => {
                Self::parse(file, input, binary_expression)
            }
            ChildType::ConstraintSetParenthesis(parenthesis) => {
                Self::parse(file, input, parenthesis)
            }
            ChildType::Identifier(ident) => Ok(AstNode::new(
                file,
                &value,
                Self::Identifier(ImString::parse(file, input, ident)?),
            )),
            ChildType::Scalar(scalar) => Self::parse(file, input, scalar),
            ChildType::ConstraintSetUnaryExpression(unary_expression) => {
                Self::parse(file, input, unary_expression)
            }
            ChildType::ConstraintSetMethodCall(method_call) => {
                Self::parse(file, input, method_call)
            }
        }
    }
}

impl<'t> Parse<'t, nodes::ConstraintSetBinaryExpression<'t>> for ConstraintSetExpression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::ConstraintSetBinaryExpression<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

        Ok(AstNode::new(
            file,
            &value,
            Self::BinaryExpression(Box::new(BinaryExpression::parse(file, input, value)?)),
        ))
    }
}

impl<'t> Parse<'t, nodes::ConstraintSetParenthesis<'t>> for ConstraintSetExpression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::ConstraintSetParenthesis<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

        let expression = value.constraint_set_expression()?;
        Ok(AstNode::new(
            file,
            &value,
            Self::Parenthesis(Box::new(Self::parse(file, input, expression)?)),
        ))
    }
}

impl<'t> Parse<'t, nodes::Scalar<'t>> for ConstraintSetExpression {
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

impl<'t> Parse<'t, nodes::ConstraintSetUnaryExpression<'t>> for ConstraintSetExpression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::ConstraintSetUnaryExpression<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

        Ok(AstNode::new(
            file,
            &value,
            Self::UnaryExpression(Box::new(UnaryExpression::parse(file, input, value)?)),
        ))
    }
}

impl<'t> Parse<'t, nodes::ConstraintSetMethodCall<'t>> for ConstraintSetExpression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::ConstraintSetMethodCall<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

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
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct UnaryExpression {
    pub operation: AstNode<UnaryExpressionOperation>,
    pub expression: AstNode<ConstraintSetExpression>,
}

impl<'t> Parse<'t, nodes::ConstraintSetUnaryExpression<'t>> for UnaryExpression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::ConstraintSetUnaryExpression<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

        let operation = value.op()?;

        let operation = match operation {
            nodes::anon_unions::Add_Sub::Add(add) => {
                AstNode::new(file, &add, UnaryExpressionOperation::Add)
            }
            nodes::anon_unions::Add_Sub::Sub(sub) => {
                AstNode::new(file, &sub, UnaryExpressionOperation::Sub)
            }
        };

        let expression =
            ConstraintSetExpression::parse(file, input, value.constraint_set_expression()?)?;

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
    Mul,
    Add,
    Sub,
    Div,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct BinaryExpression {
    pub operation: AstNode<BinaryExpressionOperation>,
    pub a: AstNode<ConstraintSetExpression>,
    pub b: AstNode<ConstraintSetExpression>,
}

impl<'t> Parse<'t, nodes::ConstraintSetBinaryExpression<'t>> for BinaryExpression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::ConstraintSetBinaryExpression<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

        type Operation<'t> = nodes::anon_unions::Mul_Add_Sub_Div<'t>;

        let operation = value.op()?;

        let operation = match operation {
            Operation::Mul(mul) => AstNode::new(file, &mul, BinaryExpressionOperation::Mul),
            Operation::Add(add) => AstNode::new(file, &add, BinaryExpressionOperation::Add),
            Operation::Sub(sub) => AstNode::new(file, &sub, BinaryExpressionOperation::Sub),
            Operation::Div(div) => AstNode::new(file, &div, BinaryExpressionOperation::Div),
        };

        let a = value.a()?;
        let a = ConstraintSetExpression::parse(file, input, a)?;

        let b = value.b()?;
        let b = ConstraintSetExpression::parse(file, input, b)?;

        Ok(AstNode::new(file, &value, Self { operation, a, b }))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct Vector2 {
    pub x: AstNode<ConstraintSetExpression>,
    pub y: AstNode<ConstraintSetExpression>,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct Vector3 {
    pub x: AstNode<ConstraintSetExpression>,
    pub y: AstNode<ConstraintSetExpression>,
    pub z: AstNode<ConstraintSetExpression>,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct MethodCall {
    pub self_dictionary: AstNode<ConstraintSetExpression>,
    pub to_call: AstNode<ImString>,
    pub argument: AstNode<ConstraintSetExpression>,
}

impl<'t> Parse<'t, nodes::ConstraintSetMethodCall<'t>> for MethodCall {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::ConstraintSetMethodCall<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

        let self_dictionary =
            ConstraintSetExpression::parse(file, input, value.self_dictionary()?)?;
        let to_call = ImString::parse(file, input, value.to_call()?)?;
        let argument = ConstraintSetExpression::parse(file, input, value.argument()?)?;

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

#[cfg(test)]
mod test {
    use common_data_types::{Dimension, Float};
    use pretty_assertions::assert_eq;

    use crate::compile::{full_compile, Expression};

    use super::*;

    fn comparison_test(input: &str, relation: Relation) {
        let root = full_compile(input);
        let formula = root.node.as_constraintset().unwrap();
        let a = &formula.node.variables[0];
        let left = &formula.node.left;
        let left_ident = left.node.as_identifier().unwrap();
        let right = &formula.node.right;
        let right_ident = right.node.as_identifier().unwrap();

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::ConstraintSet(AstNode {
                    reference: formula.reference.clone(),
                    node: Arc::new(ConstraintSet {
                        variables: vec![AstNode {
                            reference: a.reference.clone(),
                            node: ImString::from("a")
                        }],
                        left: AstNode {
                            reference: left.reference.clone(),
                            node: ConstraintSetExpression::Identifier(AstNode {
                                reference: left_ident.reference.clone(),
                                node: ImString::from("a")
                            })
                        },
                        right: AstNode {
                            reference: right.reference.clone(),
                            node: ConstraintSetExpression::Identifier(AstNode {
                                reference: right_ident.reference.clone(),
                                node: ImString::from("a")
                            })
                        },
                        relation
                    })
                })
            }
        );
    }

    #[test]
    fn less_than() {
        comparison_test("<<<a: a < a>>>", Relation::Less);
    }
    #[test]
    fn less_than_equel() {
        comparison_test("<<<a: a <= a>>>", Relation::LessEqual);
    }
    #[test]
    fn equel() {
        comparison_test("<<<a: a == a>>>", Relation::Equal);
    }
    #[test]
    fn greater_than_equel() {
        comparison_test("<<<a: a >= a>>>", Relation::GreaterEqual);
    }
    #[test]
    fn greater_than() {
        comparison_test("<<<a: a > a>>>", Relation::Greater);
    }
    #[test]
    fn not_equel() {
        comparison_test("<<<a: a != a>>>", Relation::NotEqual);
    }

    fn expression_test(
        input: &str,
        expression_builder: impl FnOnce(&ConstraintSetExpression) -> ConstraintSetExpression,
    ) {
        let root = full_compile(input);
        let formula = root.node.as_constraintset().unwrap();
        let a = &formula.node.variables[0];
        let left = &formula.node.left;
        let right = &formula.node.right;
        let right_ident = right.node.as_identifier().unwrap();

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::ConstraintSet(AstNode {
                    reference: formula.reference.clone(),
                    node: Arc::new(ConstraintSet {
                        variables: vec![AstNode {
                            reference: a.reference.clone(),
                            node: ImString::from("a")
                        }],
                        left: AstNode {
                            reference: left.reference.clone(),
                            node: expression_builder(&left.node)
                        },
                        right: AstNode {
                            reference: right.reference.clone(),
                            node: ConstraintSetExpression::Identifier(AstNode {
                                reference: right_ident.reference.clone(),
                                node: ImString::from("a")
                            })
                        },
                        relation: Relation::Equal
                    })
                })
            }
        );
    }

    #[test]
    fn parenthesis() {
        expression_test("<<<a: (a) == a>>>", |expression| {
            let paren = expression.as_parenthesis().unwrap();
            let ident = paren.node.as_identifier().unwrap();
            ConstraintSetExpression::Parenthesis(Box::new(AstNode {
                reference: paren.reference.clone(),
                node: ConstraintSetExpression::Identifier(AstNode {
                    reference: ident.reference.clone(),
                    node: ImString::from("a"),
                }),
            }))
        });
    }

    #[test]
    fn scalar() {
        expression_test("<<<a: 5m == a>>>", |expression| {
            let value = expression.as_scalar().unwrap();
            ConstraintSetExpression::Scalar(AstNode {
                reference: value.reference.clone(),
                node: Scalar {
                    dimension: Dimension::length(),
                    value: Float::new(5.0).unwrap(),
                },
            })
        });
    }

    fn unary_test(input: &str, operation: UnaryExpressionOperation) {
        expression_test(input, |expression| {
            let unary = expression.as_unaryexpression().unwrap();
            let operation_ref = &unary.node.operation.reference;
            let ident = unary.node.expression.node.as_identifier().unwrap();
            ConstraintSetExpression::UnaryExpression(Box::new(AstNode {
                reference: unary.reference.clone(),
                node: UnaryExpression {
                    operation: AstNode {
                        reference: operation_ref.clone(),
                        node: operation,
                    },
                    expression: AstNode {
                        reference: unary.node.expression.reference.clone(),
                        node: ConstraintSetExpression::Identifier(AstNode {
                            reference: ident.reference.clone(),
                            node: ImString::from("a"),
                        }),
                    },
                },
            }))
        });
    }

    #[test]
    fn unary_plus() {
        unary_test("<<<a: +a == a>>>", UnaryExpressionOperation::Add);
    }

    #[test]
    fn unary_minus() {
        unary_test("<<<a: -a == a>>>", UnaryExpressionOperation::Sub);
    }

    fn binary_test(input: &str, operation: BinaryExpressionOperation) {
        expression_test(input, |expression| {
            let binary = expression.as_binaryexpression().unwrap();
            let operation_ref = &binary.node.operation.reference;
            let ident_a = binary.node.a.node.as_identifier().unwrap();
            let ident_b = binary.node.b.node.as_identifier().unwrap();
            ConstraintSetExpression::BinaryExpression(Box::new(AstNode {
                reference: binary.reference.clone(),
                node: BinaryExpression {
                    operation: AstNode {
                        reference: operation_ref.clone(),
                        node: operation,
                    },
                    a: AstNode {
                        reference: binary.node.a.reference.clone(),
                        node: ConstraintSetExpression::Identifier(AstNode {
                            reference: ident_a.reference.clone(),
                            node: ImString::from("c"),
                        }),
                    },
                    b: AstNode {
                        reference: binary.node.b.reference.clone(),
                        node: ConstraintSetExpression::Identifier(AstNode {
                            reference: ident_b.reference.clone(),
                            node: ImString::from("b"),
                        }),
                    },
                },
            }))
        });
    }

    #[test]
    fn multiply() {
        binary_test("<<<a: c * b == a>>>", BinaryExpressionOperation::Mul);
    }

    #[test]
    fn add() {
        binary_test("<<<a: c + b == a>>>", BinaryExpressionOperation::Add);
    }

    #[test]
    fn sub() {
        binary_test("<<<a: c - b == a>>>", BinaryExpressionOperation::Sub);
    }

    #[test]
    fn divide() {
        binary_test("<<<a: c / b == a>>>", BinaryExpressionOperation::Div);
    }

    #[test]
    fn method_call() {
        expression_test("<<<a: b::method(c) == a>>>", |expression| {
            let method_call = expression.as_methodcall().unwrap();
            let self_dictionary = &method_call
                .node
                .self_dictionary
                .node
                .as_identifier()
                .unwrap();
            let to_call = &method_call.node.to_call;
            let argument = method_call.node.argument.node.as_identifier().unwrap();
            ConstraintSetExpression::MethodCall(Box::new(AstNode {
                reference: method_call.reference.clone(),
                node: MethodCall {
                    self_dictionary: AstNode {
                        reference: method_call.node.self_dictionary.reference.clone(),
                        node: ConstraintSetExpression::Identifier(AstNode {
                            reference: self_dictionary.reference.clone(),
                            node: ImString::from("b"),
                        }),
                    },
                    to_call: AstNode {
                        reference: to_call.reference.clone(),
                        node: ImString::from("method"),
                    },
                    argument: AstNode {
                        reference: method_call.node.argument.reference.clone(),
                        node: ConstraintSetExpression::Identifier(AstNode {
                            reference: argument.reference.clone(),
                            node: ImString::from("c"),
                        }),
                    },
                },
            }))
        });
    }

    #[test]
    fn multiple_variables() {
        let root = full_compile("<<<a, b: a == b>>>");
        let formula = root.node.as_constraintset().unwrap();
        let variables = &formula.node.variables;
        let left = &formula.node.left;
        let left_ident = left.node.as_identifier().unwrap();
        let right = &formula.node.right;
        let right_ident = right.node.as_identifier().unwrap();

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::ConstraintSet(AstNode {
                    reference: formula.reference.clone(),
                    node: Arc::new(ConstraintSet {
                        variables: vec![
                            AstNode {
                                reference: variables[0].reference.clone(),
                                node: ImString::from("a")
                            },
                            AstNode {
                                reference: variables[1].reference.clone(),
                                node: ImString::from("b")
                            }
                        ],
                        left: AstNode {
                            reference: left.reference.clone(),
                            node: ConstraintSetExpression::Identifier(AstNode {
                                reference: left_ident.reference.clone(),
                                node: ImString::from("a")
                            })
                        },
                        right: AstNode {
                            reference: right.reference.clone(),
                            node: ConstraintSetExpression::Identifier(AstNode {
                                reference: right_ident.reference.clone(),
                                node: ImString::from("b")
                            })
                        },
                        relation: Relation::Equal
                    })
                })
            }
        );
    }
}
