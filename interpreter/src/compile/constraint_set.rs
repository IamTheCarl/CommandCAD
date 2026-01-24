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

use crate::compile::{nodes, unwrap_missing, Error, Expression, Parse};

use super::AstNode;
use imstr::ImString;
use type_sitter::Node;

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct ConstraintSet {
    pub variables: Vec<AstNode<ImString>>,
    pub constraints: Vec<AstNode<Constraint>>,
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

        let constraints = {
            let constraints_node = value.constraints()?;
            let mut cursor = constraints_node.walk();

            let mut constraints = Vec::new();
            for constraint in constraints_node.constraints(&mut cursor) {
                constraints.push(Constraint::parse(file, input, constraint?)?);
            }

            constraints
        };

        Ok(AstNode::new(
            file,
            &value,
            Self {
                variables,
                constraints,
            },
        ))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct Constraint {
    pub left: AstNode<Expression>,
    pub right: AstNode<Expression>,
    pub relation: Relation,
}

impl<'t> Parse<'t, nodes::Constraint<'t>> for Constraint {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::Constraint<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        unwrap_missing(&value)?;

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

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;

    use crate::compile::{full_compile, Expression};

    use super::*;

    fn comparison_test(input: &str, relation: Relation) {
        let root = full_compile(input);
        let constraint_set = root.node.as_constraintset().unwrap();
        let a = &constraint_set.node.variables[0];
        let constraint = &constraint_set.node.constraints[0];
        let left = &constraint.node.left;
        let left_ident = left.node.as_identifier().unwrap();
        let right = &constraint.node.right;
        let right_ident = right.node.as_identifier().unwrap();

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::ConstraintSet(AstNode {
                    reference: constraint_set.reference.clone(),
                    node: Arc::new(ConstraintSet {
                        variables: vec![AstNode {
                            reference: a.reference.clone(),
                            node: ImString::from("a")
                        }],
                        constraints: vec![AstNode {
                            reference: constraint.reference.clone(),
                            node: Constraint {
                                left: AstNode {
                                    reference: left.reference.clone(),
                                    node: Expression::Identifier(AstNode {
                                        reference: left_ident.reference.clone(),
                                        node: ImString::from("a")
                                    })
                                },
                                right: AstNode {
                                    reference: right.reference.clone(),
                                    node: Expression::Identifier(AstNode {
                                        reference: right_ident.reference.clone(),
                                        node: ImString::from("a")
                                    })
                                },
                                relation
                            }
                        }]
                    })
                })
            }
        );
    }

    #[test]
    fn less_than() {
        comparison_test("<<<a: a :<: a>>>", Relation::Less);
    }
    #[test]
    fn less_than_equel() {
        comparison_test("<<<a: a :<=: a>>>", Relation::LessEqual);
    }
    #[test]
    fn equel() {
        comparison_test("<<<a: a :==: a>>>", Relation::Equal);
    }
    #[test]
    fn greater_than_equel() {
        comparison_test("<<<a: a :>=: a>>>", Relation::GreaterEqual);
    }
    #[test]
    fn greater_than() {
        comparison_test("<<<a: a :>: a>>>", Relation::Greater);
    }
    #[test]
    fn not_equel() {
        comparison_test("<<<a: a :!=: a>>>", Relation::NotEqual);
    }

    #[test]
    fn multiple_variables() {
        let root = full_compile("<<<a, b: a :==: b>>>");
        let constraint_set = root.node.as_constraintset().unwrap();
        let variables = &constraint_set.node.variables;
        let constraint = &constraint_set.node.constraints[0];
        let left = &constraint.node.left;
        let left_ident = left.node.as_identifier().unwrap();
        let right = &constraint.node.right;
        let right_ident = right.node.as_identifier().unwrap();

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::ConstraintSet(AstNode {
                    reference: constraint_set.reference.clone(),
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
                        constraints: vec![AstNode {
                            reference: constraint.reference.clone(),
                            node: Constraint {
                                left: AstNode {
                                    reference: left.reference.clone(),
                                    node: Expression::Identifier(AstNode {
                                        reference: left_ident.reference.clone(),
                                        node: ImString::from("a")
                                    })
                                },
                                right: AstNode {
                                    reference: right.reference.clone(),
                                    node: Expression::Identifier(AstNode {
                                        reference: right_ident.reference.clone(),
                                        node: ImString::from("b")
                                    })
                                },
                                relation: Relation::Equal
                            }
                        }]
                    })
                })
            }
        );
    }
}
