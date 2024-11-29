use std::{path::PathBuf, sync::Arc};

use type_sitter::HasChild;
use unwrap_enum::EnumAs;

use super::{nodes, AstNode, Expression, IdentityPath, Parse, ProceduralBlock};

#[derive(Debug, Hash, Eq, PartialEq, EnumAs)]
pub enum Statement {
    Assign(AstNode<Assign>),
    Let(AstNode<Let>),
    For(AstNode<For>),
    Expression(AstNode<Expression>),
    ClosedExpression(AstNode<ClosedExpression>),
}

impl<'t> Parse<'t, nodes::Statement<'t>> for Statement {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::Statement<'t>,
    ) -> Result<AstNode<Self>, super::Error<'t, 'i>> {
        type ChildType<'t> = <nodes::Statement<'t> as HasChild<'t>>::Child;
        let statement = match value.child()? {
            ChildType::Assign(value) => Self::Assign(Assign::parse(file, input, value)?),
            ChildType::ClosedExpression(value) => {
                Self::ClosedExpression(ClosedExpression::parse(file, input, value)?)
            }
            ChildType::Expression(value) => {
                Self::Expression(Expression::parse(file, input, value)?)
            }
            ChildType::For(value) => Self::For(For::parse(file, input, value)?),
            ChildType::Let(value) => Self::Let(Let::parse(file, input, value)?),
        };

        Ok(AstNode::new(file, &value, statement))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct Assign {
    pub to_assign: AstNode<IdentityPath>,
    pub assignment_type: AstNode<AssignmentType>,
    pub value: AstNode<Expression>,
}

impl<'t> Parse<'t, nodes::Assign<'t>> for Assign {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::Assign<'t>,
    ) -> Result<AstNode<Self>, super::Error<'t, 'i>> {
        let to_assign = IdentityPath::parse(file, input, value.to_assign()?)?;
        let assignment_type = AssignmentType::parse(file, input, value.assignment_operator()?)?;
        let assigned_value = Expression::parse(file, input, value.value()?)?;

        Ok(AstNode::new(
            file,
            &value,
            Self {
                to_assign,
                assignment_type,
                value: assigned_value,
            },
        ))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct Let {
    pub to_assign: AstNode<String>,
    pub assignment_type: AstNode<AssignmentType>,
    pub value: AstNode<Expression>,
}

impl<'t> Parse<'t, nodes::Let<'t>> for Let {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::Let<'t>,
    ) -> Result<AstNode<Self>, super::Error<'t, 'i>> {
        let to_assign = String::parse(file, input, value.to_assign()?)?;
        let assignment_type = AssignmentType::parse(file, input, value.assignment_operator()?)?;
        let assigned_value = Expression::parse(file, input, value.value()?)?;

        Ok(AstNode::new(
            file,
            &value,
            Self {
                to_assign,
                assignment_type,
                value: assigned_value,
            },
        ))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum AssignmentType {
    /// =
    Direct,
    /// &=
    BitAnd,
    /// |=
    BitOr,
    /// ^=
    BitXor,
    /// &&=
    LogicAnd,
    /// ||=
    LogicOr,
    /// ^^=
    LogicXor,
    /// +=
    Add,
    /// -=
    Sub,
    /// **=
    Exponent,
    /// *=
    Multiply,
    /// //=
    IntegerDivision,
    /// /=
    Division,
    /// <<=
    LeftShift,
    /// >>=
    RightShift,
}

impl<'t> Parse<'t, nodes::AssignmentOperator<'t>> for AssignmentType {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        _input: &'i str,
        value: nodes::AssignmentOperator<'t>,
    ) -> Result<AstNode<Self>, super::Error<'t, 'i>> {
        use nodes::anon_unions::AndAndEq_AndEq_MulMulEq_MulEq_AddEq_SubEq_DivDivEq_DivEq_LtLtEq_Eq_GtGtEq_BitXorEq_BitXorBitXorEq_OrEq_OrOrEq as Operators;
        let operator = value.op()?;

        let operation = match operator {
            Operators::AndAndEq(_) => Self::LogicAnd,
            Operators::AndEq(_) => Self::BitAnd,
            Operators::MulMulEq(_) => Self::Exponent,
            Operators::MulEq(_) => Self::Multiply,
            Operators::AddEq(_) => Self::Add,
            Operators::SubEq(_) => Self::Sub,
            Operators::DivDivEq(_) => Self::IntegerDivision,
            Operators::DivEq(_) => Self::Division,
            Operators::LtLtEq(_) => Self::LeftShift,
            Operators::Eq(_) => Self::Direct,
            Operators::GtGtEq(_) => Self::RightShift,
            Operators::BitXorEq(_) => Self::BitXor,
            Operators::BitXorBitXorEq(_) => Self::LogicXor,
            Operators::OrEq(_) => Self::BitOr,
            Operators::OrOrEq(_) => Self::LogicOr,
        };

        Ok(AstNode::new(file, &operator, operation))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct For {
    pub to_assign: AstNode<String>,
    pub to_iterate: AstNode<Expression>,
    pub to_run: AstNode<ProceduralBlock>,
}

impl<'t> Parse<'t, nodes::For<'t>> for For {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::For<'t>,
    ) -> Result<AstNode<Self>, super::Error<'t, 'i>> {
        let to_assign = String::parse(file, input, value.to_assign()?)?;
        let to_iterate = Expression::parse(file, input, value.to_iterate()?)?;
        let to_run = ProceduralBlock::parse(file, input, value.to_run()?)?;

        Ok(AstNode::new(
            file,
            &value,
            Self {
                to_assign,
                to_iterate,
                to_run,
            },
        ))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct ClosedExpression {
    pub expression: AstNode<Expression>,
}

impl<'t> Parse<'t, nodes::ClosedExpression<'t>> for ClosedExpression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::ClosedExpression<'t>,
    ) -> Result<AstNode<Self>, super::Error<'t, 'i>> {
        let expression = Expression::parse(file, input, value.expression()?)?;

        Ok(AstNode::new(file, &value, Self { expression }))
    }
}

#[cfg(test)]
mod test {
    use crate::{compile::PathType, new_parser};

    use super::*;

    fn full_compile(input: &str) -> AstNode<Expression> {
        let test_file = Arc::new(PathBuf::from("test_file.ccm"));
        let mut tree = new_parser();
        let root = tree.parse(input, None).unwrap();
        crate::compile(&test_file, input, &root).unwrap()
    }

    #[test]
    fn assign() {
        let root = full_compile("{ my_value.sub_value = (); }");
        let block = root.node.as_proceduralblock().unwrap();
        let statement = &block.node.statements[0];
        let assign = statement.node.as_assign().unwrap();
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::ProceduralBlock(AstNode {
                    reference: block.reference.clone(),
                    node: ProceduralBlock {
                        statements: vec![AstNode {
                            reference: statement.reference.clone(),
                            node: Statement::Assign(AstNode {
                                reference: assign.reference.clone(),
                                node: Assign {
                                    to_assign: AstNode {
                                        reference: assign.node.to_assign.reference.clone(),
                                        node: IdentityPath {
                                            ty: PathType::Local,
                                            path: vec![
                                                AstNode {
                                                    reference: assign.node.to_assign.node.path[0]
                                                        .reference
                                                        .clone(),
                                                    node: "my_value".to_string()
                                                },
                                                AstNode {
                                                    reference: assign.node.to_assign.node.path[1]
                                                        .reference
                                                        .clone(),
                                                    node: "sub_value".to_string()
                                                }
                                            ]
                                        }
                                    },
                                    assignment_type: AstNode {
                                        reference: assign.node.assignment_type.reference.clone(),
                                        node: AssignmentType::Direct
                                    },
                                    value: AstNode {
                                        reference: assign.node.value.reference.clone(),
                                        node: Expression::Void(AstNode {
                                            reference: assign
                                                .node
                                                .value
                                                .node
                                                .as_void()
                                                .unwrap()
                                                .reference
                                                .clone(),
                                            node: ()
                                        })
                                    }
                                }
                            })
                        }]
                    }
                })
            }
        );
    }

    #[test]
    fn assign_let() {
        let root = full_compile("{ let my_value = (); }");
        let block = root.node.as_proceduralblock().unwrap();
        let statement = &block.node.statements[0];
        let let_assign = statement.node.as_let().unwrap();
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::ProceduralBlock(AstNode {
                    reference: block.reference.clone(),
                    node: ProceduralBlock {
                        statements: vec![AstNode {
                            reference: statement.reference.clone(),
                            node: Statement::Let(AstNode {
                                reference: let_assign.reference.clone(),
                                node: Let {
                                    to_assign: AstNode {
                                        reference: let_assign.node.to_assign.reference.clone(),
                                        node: "my_value".to_string()
                                    },
                                    assignment_type: AstNode {
                                        reference: let_assign
                                            .node
                                            .assignment_type
                                            .reference
                                            .clone(),
                                        node: AssignmentType::Direct
                                    },
                                    value: AstNode {
                                        reference: let_assign.node.value.reference.clone(),
                                        node: Expression::Void(AstNode {
                                            reference: let_assign
                                                .node
                                                .value
                                                .node
                                                .as_void()
                                                .unwrap()
                                                .reference
                                                .clone(),
                                            node: ()
                                        })
                                    }
                                }
                            })
                        }]
                    }
                })
            }
        );
    }

    #[test]
    fn for_statement() {
        let root = full_compile("{ for i in () {} }");
        let block = root.node.as_proceduralblock().unwrap();
        let statement = &block.node.statements[0];
        let for_loop = statement.node.as_for().unwrap();
        let to_assign = &for_loop.node.to_assign;
        let to_iterate = &for_loop.node.to_iterate;
        let to_run = &for_loop.node.to_run;

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::ProceduralBlock(AstNode {
                    reference: block.reference.clone(),
                    node: ProceduralBlock {
                        statements: vec![AstNode {
                            reference: statement.reference.clone(),
                            node: Statement::For(AstNode {
                                reference: for_loop.reference.clone(),
                                node: For {
                                    to_assign: AstNode {
                                        reference: to_assign.reference.clone(),
                                        node: "i".to_string()
                                    },
                                    to_iterate: AstNode {
                                        reference: to_iterate.reference.clone(),
                                        node: Expression::Void(AstNode {
                                            reference: to_iterate
                                                .node
                                                .as_void()
                                                .unwrap()
                                                .reference
                                                .clone(),
                                            node: ()
                                        })
                                    },
                                    to_run: AstNode {
                                        reference: to_run.reference.clone(),
                                        node: ProceduralBlock { statements: vec![] }
                                    }
                                }
                            })
                        }]
                    }
                })
            }
        );
    }

    #[test]
    fn expression() {
        let root = full_compile("{ () }");
        let block = root.node.as_proceduralblock().unwrap();
        let statement = &block.node.statements[0];
        let expression = statement.node.as_expression().unwrap();

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::ProceduralBlock(AstNode {
                    reference: block.reference.clone(),
                    node: ProceduralBlock {
                        statements: vec![AstNode {
                            reference: statement.reference.clone(),
                            node: Statement::Expression(AstNode {
                                reference: expression.reference.clone(),
                                node: Expression::Void(AstNode {
                                    reference: expression.node.as_void().unwrap().reference.clone(),
                                    node: ()
                                })
                            })
                        }]
                    }
                })
            }
        );
    }

    #[test]
    fn closed_expression() {
        let root = full_compile("{ (); }");
        let block = root.node.as_proceduralblock().unwrap();
        let statement = &block.node.statements[0];
        let closed_expression = statement.node.as_closedexpression().unwrap();
        let expression = &closed_expression.node.expression;

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::ProceduralBlock(AstNode {
                    reference: block.reference.clone(),
                    node: ProceduralBlock {
                        statements: vec![AstNode {
                            reference: statement.reference.clone(),
                            node: Statement::ClosedExpression(AstNode {
                                reference: closed_expression.reference.clone(),
                                node: ClosedExpression {
                                    expression: AstNode {
                                        reference: expression.reference.clone(),
                                        node: Expression::Void(AstNode {
                                            reference: expression
                                                .node
                                                .as_void()
                                                .unwrap()
                                                .reference
                                                .clone(),
                                            node: ()
                                        })
                                    }
                                }
                            })
                        }]
                    }
                })
            }
        );
    }
}
