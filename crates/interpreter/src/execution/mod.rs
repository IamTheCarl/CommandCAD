/*
 * Copyright 2025 James Carl
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

use std::{cmp::Ordering, collections::HashMap};

use crate::{
    compile::{
        self, AstNode, BinaryExpressionOperation, Expression, SourceReference,
        UnaryExpressionOperation,
    },
    execution::{
        stack::{ScopeType, StackScope},
        values::BuiltinCallableDatabase,
    },
};

use rayon::prelude::*;

mod errors;
mod formatting;
mod logging;
mod stack;
mod standard_environment;
pub mod values;
use errors::ExpressionResult;
use imstr::ImString;
use logging::{LocatedStr, RuntimeLog, StackTrace};
use values::{
    closure::find_all_variable_accesses_in_closure_capture,
    dictionary::find_all_variable_accesses_in_dictionary_construction, Object, Value, ValueType,
};

pub use standard_environment::build_prelude;

pub fn find_value<'p, 's>(
    context: &ExecutionContext,
    path_iter: impl IntoIterator<Item = &'p compile::AstNode<ImString>>,
) -> ExpressionResult<Value> {
    let mut path_iter = path_iter.into_iter().peekable();
    let root = path_iter.next().expect("Path is empty");

    let stack_value = context.get_variable(LocatedStr {
        location: root.reference.clone(),
        string: &root.node,
    })?;

    if let Some(sub_path) = path_iter.next() {
        // We need the value off the heap.

        let mut value = stack_value.get_attribute(
            context,
            &LocatedStr {
                location: sub_path.reference.clone(),
                string: &sub_path.node,
            },
        )?;

        // Follow the chain of elements to evaluate the whole path, up to the last element.
        while let Some(sub_path) = path_iter.next() {
            if path_iter.peek().is_none() {
                // That's the last element of the path. We break out early because the
                // last one needs to be a mutable borrow.

                let final_value = value.get_attribute(
                    context,
                    &LocatedStr {
                        location: sub_path.reference.clone(),
                        string: &sub_path.node,
                    },
                )?;

                return Ok(final_value.clone());
            } else {
                value = value.get_attribute(
                    context,
                    &LocatedStr {
                        location: sub_path.reference.clone(),
                        string: &sub_path.node,
                    },
                )?;
            }
        }

        Ok(value.clone())
    } else {
        // We just needed the value off the stack.
        Ok(stack_value.clone())
    }
}

pub fn find_all_variable_accesses_in_expression(
    expression: &Expression,
    access_collector: &mut dyn FnMut(&AstNode<ImString>) -> ExpressionResult<()>,
) -> ExpressionResult<()> {
    match expression {
        Expression::BinaryExpression(ast_node) => {
            find_all_variable_accesses_in_expression(&ast_node.node.a.node, access_collector)?;
            find_all_variable_accesses_in_expression(&ast_node.node.b.node, access_collector)?;

            Ok(())
        }
        Expression::ClosureDefinition(ast_node) => {
            find_all_variable_accesses_in_expression(
                &ast_node.node.return_type.node,
                access_collector,
            )?;
            find_all_variable_accesses_in_closure_capture(&ast_node.node, access_collector)?;

            Ok(())
        }
        Expression::DictionaryConstruction(ast_node) => {
            find_all_variable_accesses_in_dictionary_construction(&ast_node.node, access_collector)
        }
        Expression::If(ast_node) => {
            find_all_variable_accesses_in_expression(
                &ast_node.node.condition.node,
                access_collector,
            )?;
            find_all_variable_accesses_in_expression(
                &ast_node.node.on_true.node,
                access_collector,
            )?;
            find_all_variable_accesses_in_expression(
                &ast_node.node.on_false.node,
                access_collector,
            )?;

            Ok(())
        }
        Expression::List(ast_node) => {
            for expression in ast_node.node.iter() {
                find_all_variable_accesses_in_expression(&expression.node, access_collector)?;
            }

            Ok(())
        }
        Expression::Parenthesis(ast_node) => {
            find_all_variable_accesses_in_expression(&ast_node.node, access_collector)
        }
        Expression::IdentityPath(ast_node) => {
            // Only the top most parent matters.
            access_collector(&ast_node.node.path[0])
        }
        Expression::StructDefinition(ast_node) => {
            for member in ast_node.node.members.iter() {
                find_all_variable_accesses_in_expression(&member.node.ty.node, access_collector)?;
                if let Some(default) = member.node.default.as_ref() {
                    find_all_variable_accesses_in_expression(&default.node, access_collector)?;
                }
            }

            Ok(())
        }
        Expression::UnaryExpression(ast_node) => find_all_variable_accesses_in_expression(
            &ast_node.node.expression.node,
            access_collector,
        ),
        Expression::FunctionCall(ast_node) => {
            find_all_variable_accesses_in_expression(
                &ast_node.node.to_call.node,
                access_collector,
            )?;
            find_all_variable_accesses_in_dictionary_construction(
                &ast_node.node.argument.node,
                access_collector,
            )?;

            Ok(())
        }
        Expression::MethodCall(ast_node) => {
            find_all_variable_accesses_in_expression(
                &ast_node.node.self_dictionary.node,
                access_collector,
            )?;
            find_all_variable_accesses_in_dictionary_construction(
                &ast_node.node.argument.node,
                access_collector,
            )?;

            Ok(())
        }
        Expression::LetIn(ast_node) => {
            for assignment in ast_node.node.assignments.iter() {
                find_all_variable_accesses_in_expression(
                    &assignment.node.value.node,
                    access_collector,
                )?;
            }

            find_all_variable_accesses_in_expression(
                &ast_node.node.expression.node,
                access_collector,
            )?;

            Ok(())
        }
        Expression::Boolean(_)
        | Expression::Scalar(_)
        | Expression::Vector2(_)
        | Expression::Vector3(_)
        | Expression::Vector4(_)
        | Expression::SignedInteger(_)
        | Expression::String(_)
        | Expression::UnsignedInteger(_)
        | Expression::SelfPath(_) => Ok(()),
    }
}

#[derive(Debug)]
pub struct ExecutionContext<'c> {
    pub log: &'c dyn RuntimeLog,
    pub stack_trace: &'c StackTrace<'c>,
    pub stack: &'c StackScope<'c>,
    pub database: &'c BuiltinCallableDatabase,
}

impl<'c> ExecutionContext<'c> {
    pub fn trace_scope<F, R>(&'c self, reference: impl Into<SourceReference>, code: F) -> R
    where
        F: FnOnce(&ExecutionContext<'_>) -> R,
    {
        self.stack_trace.trace_scope(reference, move |stack_trace| {
            let context = ExecutionContext {
                log: self.log,
                stack_trace: &stack_trace,
                stack: self.stack,
                database: self.database,
            };

            code(&context)
        })
    }

    pub fn get_variable<'s, S: Into<LocatedStr<'s>>>(&self, name: S) -> ExpressionResult<&Value> {
        self.stack.get_variable(self.stack_trace, name)
    }

    pub fn stack_scope<B, R>(
        &self,
        mode: ScopeType,
        variables: HashMap<ImString, Value>,
        block: B,
    ) -> ExpressionResult<R>
    where
        B: FnOnce(&ExecutionContext) -> R,
    {
        self.stack
            .scope(self.stack_trace, mode, variables, |stack, stack_trace| {
                let context = ExecutionContext {
                    log: self.log,
                    stack_trace: &stack_trace,
                    stack: &stack,
                    database: self.database,
                };

                block(&context)
            })
    }
}

pub fn execute_expression(
    context: &ExecutionContext,
    expression: &compile::AstNode<compile::Expression>,
) -> ExpressionResult<Value> {
    context.trace_scope(expression.reference.clone(), |context| {
        match &expression.node {
            compile::Expression::BinaryExpression(ast_node) => {
                execute_binary_expression(context, ast_node)
            }
            compile::Expression::Boolean(ast_node) => Ok(values::Boolean(ast_node.node).into()),
            compile::Expression::ClosureDefinition(ast_node) => {
                Ok(values::UserClosure::from_ast(context, ast_node)?.into())
            }
            compile::Expression::DictionaryConstruction(ast_node) => {
                Ok(values::Dictionary::from_ast(context, ast_node)?.into())
            }
            compile::Expression::If(ast_node) => execute_if_expression(context, ast_node),
            compile::Expression::List(ast_node) => {
                Ok(values::List::from_ast(context, ast_node)?.into())
            }
            compile::Expression::Parenthesis(ast_node) => execute_expression(context, &ast_node),
            compile::Expression::IdentityPath(ast_node) => {
                let path_iter = ast_node.node.path.iter();
                Ok(find_value(context, path_iter)?)
            }
            compile::Expression::SelfPath(ast_node) => {
                let self_code = AstNode {
                    reference: ast_node.reference.clone(),
                    node: ImString::from("self"),
                };
                let path_iter = [&self_code].into_iter().chain(ast_node.node.path.iter());
                Ok(find_value(context, path_iter)?)
            }

            compile::Expression::Scalar(ast_node) => Ok(values::Scalar {
                dimension: ast_node.node.dimension,
                value: ast_node.node.value,
            }
            .into()),
            compile::Expression::Vector2(vector) => {
                Ok(values::Vector2::from_ast(context, vector)?.into())
            }
            compile::Expression::Vector3(vector) => {
                Ok(values::Vector3::from_ast(context, vector)?.into())
            }
            compile::Expression::Vector4(vector) => {
                Ok(values::Vector4::from_ast(context, vector)?.into())
            }
            compile::Expression::SignedInteger(ast_node) => {
                Ok(values::SignedInteger::from(ast_node.node).into())
            }
            compile::Expression::String(ast_node) => {
                Ok(values::IString::from(ast_node.node.clone()).into())
            }
            compile::Expression::StructDefinition(ast_node) => {
                Ok(ValueType::from(values::StructDefinition::new(context, ast_node)?).into())
            }
            compile::Expression::UnaryExpression(ast_node) => {
                execute_unary_expression(context, ast_node)
            }
            compile::Expression::UnsignedInteger(ast_node) => {
                Ok(values::UnsignedInteger::from(ast_node.node).into())
            }
            compile::Expression::FunctionCall(ast_node) => execute_function_call(context, ast_node),
            compile::Expression::MethodCall(ast_node) => execute_method_call(context, ast_node),
            compile::Expression::LetIn(ast_node) => execute_let_in(context, ast_node),
        }
    })
}

fn execute_unary_expression(
    context: &ExecutionContext,
    expression: &compile::AstNode<Box<compile::UnaryExpression>>,
) -> ExpressionResult<Value> {
    context.trace_scope(expression.reference.clone(), |context| {
        let node = &expression.node;
        let value = execute_expression(context, &node.expression)?;
        match node.operation.node {
            UnaryExpressionOperation::Add => value.unary_plus(context),
            UnaryExpressionOperation::Sub => value.unary_minus(context),
            UnaryExpressionOperation::Not => value.unary_not(context),
        }
    })
}

fn execute_function_call(
    context: &ExecutionContext,
    call: &compile::AstNode<Box<compile::FunctionCall>>,
) -> ExpressionResult<Value> {
    let to_call = execute_expression(context, &call.node.to_call)?;
    let argument = values::Dictionary::from_ast(context, &call.node.argument)?;

    context.stack_scope(ScopeType::Isolated, HashMap::new(), |context| {
        to_call.call(context, argument)
    })?
}

fn execute_method_call(
    context: &ExecutionContext,
    call: &compile::AstNode<Box<compile::MethodCall>>,
) -> ExpressionResult<Value> {
    let self_dictionary = execute_expression(context, &call.node.self_dictionary)?;
    let to_call = self_dictionary
        .get_attribute(context, &call.node.to_call.node)?
        .clone();
    let argument = values::Dictionary::from_ast(context, &call.node.argument)?;

    context.stack_scope(
        ScopeType::Isolated,
        HashMap::from_iter([(ImString::from("self"), self_dictionary.into())]),
        |context| to_call.call(context, argument),
    )?
}

fn execute_let_in(
    context: &ExecutionContext,
    expression: &compile::AstNode<Box<compile::LetIn>>,
) -> ExpressionResult<Value> {
    context.trace_scope(expression.reference.clone(), |context| {
        context.stack.scope_mut(
            context.stack_trace,
            ScopeType::Inherited,
            HashMap::with_capacity(expression.node.assignments.len()),
            |stack, stack_trace| {
                let mut buffer = Vec::new();
                for group in expression.node.compute_groups() {
                    let context = ExecutionContext {
                        log: context.log,
                        stack_trace,
                        stack,
                        database: context.database,
                    };

                    buffer.par_extend(group.par_iter().map(|assignment| {
                        (
                            assignment.node.ident.node.clone(),
                            execute_expression(&context, &assignment.node.value),
                        )
                    }));

                    for (name, result) in buffer.drain(..) {
                        let value = result?;
                        stack.insert_value(name, value);
                    }
                }

                let context = ExecutionContext {
                    log: context.log,
                    stack_trace,
                    stack,
                    database: context.database,
                };

                let node = &expression.node;
                execute_expression(&context, &node.expression)
            },
        )?
    })
}

fn execute_binary_expression(
    context: &ExecutionContext,
    expression: &compile::AstNode<Box<compile::BinaryExpression>>,
) -> ExpressionResult<Value> {
    context.trace_scope(expression.reference.clone(), |context| {
        let node = &expression.node;
        let value_a = execute_expression(context, &node.a)?;
        let value_b = execute_expression(context, &node.b)?;
        match node.operation.node {
            BinaryExpressionOperation::NotEq => Ok(values::Boolean(
                !value_a
                    .clone()
                    .cmp(context, value_b.clone())
                    .map(|ord| matches!(ord, Ordering::Equal))
                    .or_else(|_| value_a.eq(context, value_b))?,
            )
            .into()),
            BinaryExpressionOperation::And => value_a.bit_and(context, value_b),
            BinaryExpressionOperation::AndAnd => value_a.and(context, value_b),
            BinaryExpressionOperation::Mul => value_a.multiply(context, value_b),
            BinaryExpressionOperation::MulMul => value_a.exponent(context, value_b),
            BinaryExpressionOperation::Add => value_a.addition(context, value_b),
            BinaryExpressionOperation::Sub => value_a.subtraction(context, value_b),
            BinaryExpressionOperation::Div => value_a.divide(context, value_b),
            BinaryExpressionOperation::Lt => Ok(values::Boolean(matches!(
                value_a.cmp(context, value_b)?,
                Ordering::Less
            ))
            .into()),
            BinaryExpressionOperation::LtLt => value_a.left_shift(context, value_b),
            BinaryExpressionOperation::LtEq => Ok(values::Boolean(matches!(
                value_a.cmp(context, value_b)?,
                Ordering::Less | Ordering::Equal
            ))
            .into()),
            BinaryExpressionOperation::EqEq => Ok(values::Boolean(
                value_a
                    .clone()
                    .cmp(context, value_b.clone())
                    .map(|ord| matches!(ord, Ordering::Equal))
                    .or_else(|_| value_a.eq(context, value_b))?,
            )
            .into()),
            BinaryExpressionOperation::Gt => Ok(values::Boolean(matches!(
                value_a.cmp(context, value_b)?,
                Ordering::Greater
            ))
            .into()),
            BinaryExpressionOperation::GtEq => Ok(values::Boolean(matches!(
                value_a.cmp(context, value_b)?,
                Ordering::Equal | Ordering::Greater
            ))
            .into()),
            BinaryExpressionOperation::GtGt => value_a.right_shift(context, value_b),
            BinaryExpressionOperation::BitXor => value_a.bit_xor(context, value_b),
            BinaryExpressionOperation::Xor => value_a.xor(context, value_b),
            BinaryExpressionOperation::Or => value_a.bit_or(context, value_b),
            BinaryExpressionOperation::OrOr => value_a.or(context, value_b),
        }
    })
}

pub fn execute_if_expression(
    context: &ExecutionContext,
    expression: &compile::AstNode<Box<compile::IfExpression>>,
) -> ExpressionResult<Value> {
    let condition = execute_expression(context, &expression.node.condition)?
        .downcast::<values::Boolean>(context.stack_trace)?
        .0;

    let expression = if condition {
        &expression.node.on_true
    } else {
        &expression.node.on_false
    };

    execute_expression(context, expression)
}

#[cfg(test)]
pub(crate) fn test_run(input: &str) -> ExpressionResult<Value> {
    use standard_environment::build_prelude;
    use std::sync::Mutex;

    let root = compile::full_compile(input);
    let database = BuiltinCallableDatabase::new();
    let prelude = build_prelude(&database);

    let context = ExecutionContext {
        log: &Mutex::new(Vec::new()),
        stack_trace: &StackTrace::test(),
        stack: &StackScope::top(&prelude),
        database: &database,
    };

    execute_expression(&context, &root)
}

#[cfg(test)]
mod test {
    use hashable_map::HashableMap;
    use std::{collections::HashMap, sync::Arc};

    use super::*;

    #[test]
    fn boolean_type() {
        let product = test_run("true").unwrap();
        assert_eq!(product, values::Boolean(true).into());
    }

    #[test]
    fn signed_integer_type() {
        let product = test_run("5i").unwrap();
        assert_eq!(product, values::SignedInteger::from(5).into());
    }

    #[test]
    fn unsigned_integer_type() {
        let product = test_run("5u").unwrap();
        assert_eq!(product, values::UnsignedInteger::from(5).into());
    }

    #[test]
    fn parenthesis() {
        // Fails because of a type mismatch.
        let product = test_run("(1i + 2i) * 3i").unwrap();
        assert_eq!(product, values::SignedInteger::from(9).into());
    }

    #[test]
    fn struct_definition() {
        let product = test_run("(name: std.types.None = std.consts.None, ...)").unwrap();
        assert_eq!(
            product,
            values::ValueType::Dictionary(values::StructDefinition {
                members: Arc::new(HashableMap::from(HashMap::from([(
                    "name".into(),
                    values::StructMember {
                        ty: ValueType::TypeNone,
                        default: Some(Value::ValueNone(values::ValueNone))
                    }
                )]))),
                variadic: true
            })
            .into()
        );
    }

    #[test]
    fn nested_value_access() {
        let product = test_run("let dictionary = (a = (b = 23u)); in dictionary.a.b").unwrap();
        assert_eq!(product, values::UnsignedInteger::from(23).into());
    }

    #[test]
    fn let_in() {
        let product = test_run("let value = 23u; in value").unwrap();
        assert_eq!(product, values::UnsignedInteger::from(23).into());
    }

    #[test]
    fn let_in_self_ref() {
        let product = test_run("let value = 23u; value2 = value + 2u; in value2").unwrap();
        assert_eq!(product, values::UnsignedInteger::from(25).into());
    }

    #[test]
    fn string() {
        let product = test_run("\"a simple string of text\"").unwrap();
        assert_eq!(
            product,
            values::IString::from("a simple string of text").into()
        );
    }

    #[test]
    fn if_expression() {
        let product = test_run("if true then 1u else 2u").unwrap();
        assert_eq!(product, values::UnsignedInteger::from(1).into());

        let product = test_run("if false then 1u else 2u").unwrap();
        assert_eq!(product, values::UnsignedInteger::from(2).into());
    }
}
