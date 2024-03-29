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

use crate::script::{
    execution::Failure,
    parsing::{
        self, Assign, AssignableVariable, Break, Continue, For, Function, If, Loop, Match, Return,
        Statement, While,
    },
    Span,
};

use super::{
    expressions::{self, run_expression},
    run_block,
    types::{List, StructDefinition, UserFunction},
    types::{NoneType, Object, Value},
    ControlFlow, ExecutionContext, ExecutionResult,
};

pub fn run_statement<'a, S: Span>(
    context: &mut ExecutionContext<'a, S>,
    statement: &'a Statement<S>,
) -> ExecutionResult<'a, S, Value<'a, S>> {
    match statement {
        Statement::Expression(expression) => Ok(expressions::run_expression(context, expression)?),
        Statement::Assign(assignment) => run_assignment(context, assignment),
        Statement::Return(return_statement) => run_return(context, return_statement),
        Statement::If(if_statement) => run_if(context, if_statement),
        Statement::Match(match_statement) => run_match(context, match_statement),
        Statement::For(for_statement) => run_for(context, for_statement),
        Statement::While(while_statement) => run_while(context, while_statement),
        Statement::Loop(loop_statement) => run_loop(context, loop_statement),
        Statement::Break(break_statement) => run_break(context, break_statement),
        Statement::Continue(continue_statement) => run_continue(context, continue_statement),
        Statement::DefineFunction(function) => run_define_function(context, function),
        Statement::DefineStruct(structure) => run_define_structure(context, structure),
    }
}

fn assign_values<'a, S: Span>(
    to_assign: &parsing::Assignable<S>,
    assignment_span: &S,
    source_span: &S,
    value: Value<'a, S>,
    mut assign: impl FnMut(&S, Value<'a, S>) -> ExecutionResult<'a, S, Value<'a, S>>,
) -> ExecutionResult<'a, S, Value<'a, S>> {
    fn assign_single_value<'a, S: Span>(
        assignment_span: &S,
        value: Value<'a, S>,
        variable: &AssignableVariable<S>,
        assign: impl FnOnce(&S, Value<'a, S>) -> ExecutionResult<'a, S, Value<'a, S>>,
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        if let Some(ty) = &variable.ty {
            if value.matches_type(ty) {
                assign(&variable.name, value)
            } else {
                Err(ControlFlow::Failure(Failure::ExpectedGot(
                    assignment_span.clone(),
                    ty.name(),
                    value.type_name(),
                )))
            }
        } else {
            assign(&variable.name, value)
        }
    }

    match to_assign {
        parsing::Assignable::Variable(variable) => {
            assign_single_value(assignment_span, value, variable, &mut assign)
        }
        parsing::Assignable::List(_span, variables) => {
            let values = value.downcast::<List<S>>(source_span)?;

            if values.len() != variables.len() {
                return Err(ControlFlow::Failure(Failure::ListLengthsDontMatch(
                    source_span.clone(),
                )));
            }

            for (variable, value) in variables.iter().zip(values.iter().cloned()) {
                assign_single_value(assignment_span, value, variable, &mut assign)?;
            }

            Ok(NoneType.into())
        }
    }
}

fn run_assignment<'a, S: Span>(
    context: &mut ExecutionContext<'a, S>,
    assignment: &'a Assign<S>,
) -> ExecutionResult<'a, S, Value<'a, S>> {
    let value = run_statement(context, &assignment.statement)?;

    if assignment.is_new {
        let assign = |name: &S, value: Value<'a, S>| -> ExecutionResult<'a, S, Value<'a, S>> {
            context.stack.new_variable(name, value);

            Ok(NoneType.into())
        };

        assign_values(
            &assignment.to_assign,
            assignment.get_span(),
            assignment.statement.get_span(),
            value,
            assign,
        )
    } else {
        let stack = &mut context.stack;

        let assign = |name: &S, value: Value<'a, S>| -> ExecutionResult<'a, S, Value<'a, S>> {
            let variable = stack.get_variable_mut(name)?;
            *variable = value;

            Ok(NoneType.into())
        };

        assign_values(
            &assignment.to_assign,
            assignment.get_span(),
            assignment.statement.get_span(),
            value,
            assign,
        )
    }
}

fn run_return<'a, S: Span>(
    context: &mut ExecutionContext<'a, S>,
    return_statement: &'a Return<S>,
) -> ExecutionResult<'a, S, Value<'a, S>> {
    let value = if let Some(expression) = return_statement.expression.as_ref() {
        run_expression(context, expression)?
    } else {
        NoneType.into()
    };

    Err(ControlFlow::Return { value })
}

fn run_if<'a, S: Span>(
    context: &mut ExecutionContext<'a, S>,
    if_statement: &'a If<S>,
) -> ExecutionResult<'a, S, Value<'a, S>> {
    let condition = expressions::run_expression(context, &if_statement.expression)?;

    if condition.downcast::<bool>(if_statement.expression.get_span())? {
        context.new_scope(|context| run_block(context, &if_statement.block))
    } else {
        match &if_statement.else_statement {
            None => Ok(NoneType.into()),
            Some(parsing::Else::Else(block)) => {
                context.new_scope(|context| run_block(context, block))
            }
            Some(parsing::Else::IfElse(if_statement)) => run_if(context, if_statement), // FIXME an if else chain that's too deep can cause a stack overflow.
        }
    }
}

fn run_match<'a, S: Span>(
    context: &mut ExecutionContext<'a, S>,
    match_statement: &'a Match<S>,
) -> ExecutionResult<'a, S, Value<'a, S>> {
    fn check_branch_matches<'a, S: Span>(
        value: &Value<'a, S>,
        branch_value: &Value<'a, S>,
    ) -> bool {
        if value == branch_value || matches!(branch_value, &Value::Default(_)) {
            true
        } else if let (Value::List(value_list), Value::List(branch_value_list)) =
            (value, branch_value)
        {
            // These are lists. We need to check the individual components.
            if value_list.len() == branch_value_list.len() {
                for (value, branch_value) in value_list.iter().zip(branch_value_list.iter()) {
                    if !check_branch_matches(value, branch_value) {
                        return false;
                    }
                }

                true
            } else {
                false
            }
        } else {
            false
        }
    }

    let value = run_expression(context, &match_statement.expression)?;

    // TODO should we cache these values in a hash map for situations where we may run this match statement several times?
    for branch in match_statement.branches.iter() {
        let branch_value = Value::from_litteral(context, &branch.litteral)?;

        if check_branch_matches(&value, &branch_value) {
            return context.new_scope(|context| run_block(context, &branch.block));
        }
    }

    Err(ControlFlow::Failure(Failure::DidNotMatch(
        match_statement.expression.get_span().clone(),
    )))
}

fn loop_impl<'a, S, F>(
    mut loop_control: F,
    context: &mut ExecutionContext<'a, S>,
    name: Option<&S>,
    block: &'a parsing::Block<S>,
) -> ExecutionResult<'a, S, Value<'a, S>>
where
    S: Span,
    F: FnMut(&mut ExecutionContext<'a, S>) -> ExecutionResult<'a, S, bool>,
{
    context.new_scope(|context| {
        loop {
            if loop_control(context)? {
                let result = run_block(context, block);

                match result {
                    Ok(result) => {
                        // Loops must end with a none-type.
                        if !matches!(result, Value::NoneType(_)) {
                            // If it returned something that's not None, then there must have been a final statement.
                            let final_statement = block.statements.last().unwrap();

                            break Err(ControlFlow::Failure(Failure::ExpectedGot(
                                final_statement.get_span().clone(),
                                "None".into(),
                                result.type_name(),
                            )));
                        }
                    }
                    Err(exit_reason) => match exit_reason {
                        ControlFlow::Failure(failure) => break Err(ControlFlow::Failure(failure)),
                        ControlFlow::Return { value } => break Err(ControlFlow::Return { value }),
                        ControlFlow::Break { span, label, value } => match (label, name) {
                            (None, _) => break Ok(value),
                            (Some(label), Some(loop_name)) => {
                                let label_str = label.as_str();
                                let loop_name = loop_name.as_str();

                                if label_str != loop_name {
                                    // We are not the loop breaking.
                                    // Pass it to our caller.
                                    break Err(ControlFlow::Break {
                                        span,
                                        value,
                                        label: Some(label),
                                    });
                                } else {
                                    // We are the loop breaking.
                                    break Ok(value);
                                }
                            }
                            (Some(label), None) => {
                                // This is for a labelled loop, but we are not labeled. Not for us it seems.
                                break Err(ControlFlow::Break {
                                    span,
                                    value,
                                    label: Some(label),
                                });
                            }
                        },
                        ControlFlow::Continue { span, label } => match (label, name) {
                            (None, _) => break Ok(NoneType.into()),
                            (Some(label), Some(loop_name)) => {
                                let label_str = label.as_str();
                                let loop_name = loop_name.as_str();

                                if label_str != loop_name {
                                    // We are not the loop breaking.
                                    // Pass it to our caller.
                                    break Err(ControlFlow::Continue {
                                        span,
                                        label: Some(label),
                                    });
                                } else {
                                    // We are the loop breaking.
                                    break Ok(NoneType.into());
                                }
                            }
                            (Some(label), None) => {
                                // This is for a labelled loop, but we are not labeled. Not for us it seems.
                                break Err(ControlFlow::Continue {
                                    span,
                                    label: Some(label),
                                });
                            }
                        },
                    },
                }
            } else {
                break Ok(NoneType.into());
            }
        }
    })
}

fn run_for<'a, S: Span>(
    context: &mut ExecutionContext<'a, S>,
    for_statement: &'a For<S>,
) -> ExecutionResult<'a, S, Value<'a, S>> {
    let iter_expression = run_expression(context, &for_statement.iterator_expression)?;
    let mut iterator =
        iter_expression.iterate(context.log, for_statement.iterator_expression.get_span())?;

    loop_impl(
        |context| {
            if let Some(next_value) = iterator.next() {
                let assign =
                    |name: &S, value: Value<'a, S>| -> ExecutionResult<'a, S, Value<'a, S>> {
                        context.stack.new_variable(name, value);

                        Ok(NoneType.into())
                    };
                assign_values(
                    &for_statement.variable_assignment,
                    for_statement.variable_assignment.get_span(),
                    for_statement.iterator_expression.get_span(),
                    next_value,
                    assign,
                )?;

                Ok(true)
            } else {
                Ok(false)
            }
        },
        context,
        for_statement.name.as_ref(),
        &for_statement.block,
    )
}

fn run_while<'a, S: Span>(
    context: &mut ExecutionContext<'a, S>,
    while_statement: &'a While<S>,
) -> ExecutionResult<'a, S, Value<'a, S>> {
    loop_impl(
        |context| {
            Ok(run_expression(context, &while_statement.expression)?
                .downcast::<bool>(while_statement.expression.get_span())?)
        },
        context,
        while_statement.name.as_ref(),
        &while_statement.block,
    )
}

fn run_loop<'a, S: Span>(
    context: &mut ExecutionContext<'a, S>,
    loop_statement: &'a Loop<S>,
) -> ExecutionResult<'a, S, Value<'a, S>> {
    loop_impl(
        |_context| Ok(true),
        context,
        loop_statement.name.as_ref(),
        &loop_statement.block,
    )
}

fn run_break<'a, S: Span>(
    context: &mut ExecutionContext<'a, S>,
    break_statement: &'a Break<S>,
) -> ExecutionResult<'a, S, Value<'a, S>> {
    let value = if let Some(expression) = &break_statement.expression {
        run_expression(context, expression)?
    } else {
        NoneType.into()
    };

    Err(ControlFlow::Break {
        span: break_statement.get_span().clone(),
        label: break_statement.loop_name.clone(),
        value,
    })
}

fn run_continue<'a, S: Span>(
    _context: &mut ExecutionContext<S>,
    continue_statement: &'a Continue<S>,
) -> ExecutionResult<'a, S, Value<'a, S>> {
    Err(ControlFlow::Continue {
        span: continue_statement.get_span().clone(),
        label: continue_statement.loop_name.clone(),
    })
}

fn run_define_function<'a, S: Span>(
    context: &mut ExecutionContext<'a, S>,
    function: &'a Function<S>,
) -> ExecutionResult<'a, S, Value<'a, S>> {
    context.stack.new_variable(
        &function.named_block.name,
        UserFunction {
            block: &function.named_block,
            signature: function.signature.clone(),
        }
        .into(),
    );

    Ok(NoneType.into())
}

fn run_define_structure<'a, S: Span>(
    context: &mut ExecutionContext<'a, S>,
    definition: &'a parsing::StructDefinition<S>,
) -> ExecutionResult<'a, S, Value<'a, S>> {
    context
        .stack
        .new_variable(&definition.name, StructDefinition { definition }.into());

    Ok(NoneType.into())
}

#[cfg(test)]
mod test {
    use crate::script::execution::{ControlFlow, Failure};
    use common_data_types::Number;

    use super::*;

    #[test]
    fn assignment() {
        let mut context = ExecutionContext::default();

        let statement = parsing::Statement::parse("value = 1").unwrap().1;
        assert_eq!(
            run_statement(&mut context, Box::leak(Box::new(statement))),
            Err(ControlFlow::Failure(Failure::VariableNotInScope(
                "value",
                "value".into(),
            )))
        );
        assert!(context.stack.get_variable(&"value").is_err());

        let statement = parsing::Statement::parse("let value = 1").unwrap().1;
        assert_eq!(
            run_statement(&mut context, Box::leak(Box::new(statement))),
            Ok(NoneType.into())
        );
        assert_eq!(
            context.stack.get_variable(&"value"),
            Ok(&Number::new(1.0).unwrap().into())
        );

        let statement = parsing::Statement::parse("value = 2").unwrap().1;
        assert_eq!(
            run_statement(&mut context, Box::leak(Box::new(statement))),
            Ok(NoneType.into())
        );
        assert_eq!(
            context.stack.get_variable(&"value"),
            Ok(&Number::new(2.0).unwrap().into())
        );

        let statement = parsing::Statement::parse("let [one, two] = [1, 2]")
            .unwrap()
            .1;
        assert_eq!(
            run_statement(&mut context, Box::leak(Box::new(statement))),
            Ok(NoneType.into())
        );
        assert_eq!(
            context.stack.get_variable(&"one"),
            Ok(&Number::new(1.0).unwrap().into())
        );
        assert_eq!(
            context.stack.get_variable(&"two"),
            Ok(&Number::new(2.0).unwrap().into())
        );

        let statement = parsing::Statement::parse("[one, two] = [3, 4]").unwrap().1;
        assert_eq!(
            run_statement(&mut context, Box::leak(Box::new(statement))),
            Ok(NoneType.into())
        );
        assert_eq!(
            context.stack.get_variable(&"one"),
            Ok(&Number::new(3.0).unwrap().into())
        );
        assert_eq!(
            context.stack.get_variable(&"two"),
            Ok(&Number::new(4.0).unwrap().into())
        );

        let statement = parsing::Statement::parse("let [one, two] = [1, 2, 3]")
            .unwrap()
            .1;
        assert_eq!(
            run_statement(&mut context, Box::leak(Box::new(statement))),
            Err(ControlFlow::Failure(Failure::ListLengthsDontMatch("[")))
        );
        let statement = parsing::Statement::parse("let [one, two] = [1]").unwrap().1;
        assert_eq!(
            run_statement(&mut context, Box::leak(Box::new(statement))),
            Err(ControlFlow::Failure(Failure::ListLengthsDontMatch("[")))
        );
        let statement = parsing::Statement::parse("let [one, two] = 1").unwrap().1;
        assert_eq!(
            run_statement(&mut context, Box::leak(Box::new(statement))),
            Err(ControlFlow::Failure(Failure::ExpectedGot(
                "1",
                "List".into(),
                "Number".into()
            )))
        );
    }

    #[test]
    fn assign_scopes() {
        let mut context = ExecutionContext::default();
        context
            .stack
            .new_variable(&"one", Number::new(1.0).unwrap().into());
        context
            .stack
            .new_variable(&"value", Number::new(1.0).unwrap().into());

        context.new_scope(|context| {
            let statement = parsing::Statement::parse("let one = 2").unwrap().1;
            assert_eq!(
                run_statement(context, Box::leak(Box::new(statement))),
                Ok(NoneType.into())
            );

            assert_eq!(
                context.stack.get_variable(&"value"),
                Ok(&Number::new(1.0).unwrap().into())
            );
            let statement = parsing::Statement::parse("value = 2").unwrap().1;
            assert_eq!(
                run_statement(context, Box::leak(Box::new(statement))),
                Ok(NoneType.into())
            );
            assert_eq!(
                context.stack.get_variable(&"value"),
                Ok(&Number::new(2.0).unwrap().into())
            );
        });

        assert_eq!(
            context.stack.get_variable(&"value"),
            Ok(&Number::new(2.0).unwrap().into())
        );
        assert_eq!(
            context.stack.get_variable(&"one"),
            Ok(&Number::new(1.0).unwrap().into())
        );
    }

    #[test]
    fn loop_statement() {
        let mut context = ExecutionContext::default();

        let statement = parsing::Statement::parse("loop { break; }").unwrap().1;
        assert_eq!(
            run_statement(&mut context, Box::leak(Box::new(statement))),
            Ok(NoneType.into())
        );
    }

    #[test]
    fn if_statement() {
        let mut context = ExecutionContext::default();

        let statement = parsing::Statement::parse("if true { 1.0 }").unwrap().1;
        assert_eq!(
            run_statement(&mut context, Box::leak(Box::new(statement))),
            Ok(Number::new(1.0).unwrap().into())
        );

        let statement = parsing::Statement::parse("if false { 1.0 }").unwrap().1;
        assert_eq!(
            run_statement(&mut context, Box::leak(Box::new(statement))),
            Ok(NoneType.into())
        );

        let statement = parsing::Statement::parse("if true { 1.0 } else { 2.0 }")
            .unwrap()
            .1;
        assert_eq!(
            run_statement(&mut context, Box::leak(Box::new(statement))),
            Ok(Number::new(1.0).unwrap().into())
        );

        let statement = parsing::Statement::parse("if false { 1.0 } else { 2.0 }")
            .unwrap()
            .1;
        assert_eq!(
            run_statement(&mut context, Box::leak(Box::new(statement))),
            Ok(Number::new(2.0).unwrap().into())
        );

        let statement = parsing::Statement::parse("if false { 1.0 } else if true { 2.0 }")
            .unwrap()
            .1;
        assert_eq!(
            run_statement(&mut context, Box::leak(Box::new(statement))),
            Ok(Number::new(2.0).unwrap().into())
        );

        let statement = parsing::Statement::parse("if true { 1.0 } else if true { 2.0 }")
            .unwrap()
            .1;
        assert_eq!(
            run_statement(&mut context, Box::leak(Box::new(statement))),
            Ok(Number::new(1.0).unwrap().into())
        );

        let statement =
            parsing::Statement::parse("if false { 1.0 } else if false { 2.0 } else { 3.0 }")
                .unwrap()
                .1;
        assert_eq!(
            run_statement(&mut context, Box::leak(Box::new(statement))),
            Ok(Number::new(3.0).unwrap().into())
        );
    }

    #[test]
    fn break_statement() {
        let mut context = ExecutionContext::default();

        let statement = parsing::Statement::parse("loop { break; }").unwrap().1;
        assert_eq!(
            run_statement(&mut context, Box::leak(Box::new(statement))),
            Ok(NoneType.into())
        );

        let block = parsing::Block::parse(
            "{ let test_one = 0; loop { test_one = 1; break; test_one = 2 } }",
        )
        .unwrap()
        .1;
        assert_eq!(
            run_block(&mut context, Box::leak(Box::new(block))),
            Ok(NoneType.into())
        );
        assert_eq!(
            context.stack.get_variable(&"test_one"),
            Ok(&Number::new(1.0).unwrap().into())
        );

        let block = parsing::Block::parse(
            "{ let test_one = 0; loop { if test_one >= 5 { break; } test_one = test_one + 1; } }",
        )
        .unwrap()
        .1;
        assert_eq!(
            run_block(&mut context, Box::leak(Box::new(block))),
            Ok(NoneType.into())
        );
        assert_eq!(
            context.stack.get_variable(&"test_one"),
            Ok(&Number::new(5.0).unwrap().into())
        );

        let block = parsing::Block::parse(
            "{ let test_one = 0; 'parent: loop { loop { break 'parent; } test_one = 1 } }",
        )
        .unwrap()
        .1;
        assert_eq!(
            run_block(&mut context, Box::leak(Box::new(block))),
            Ok(NoneType.into())
        );
        assert_eq!(
            context.stack.get_variable(&"test_one"),
            Ok(&Number::new(0.0).unwrap().into())
        );

        let block = parsing::Block::parse("{ loop { break 1.0; } }").unwrap().1;
        assert_eq!(
            run_block(&mut context, Box::leak(Box::new(block))),
            Ok(Number::new(1.0).unwrap().into())
        );
    }

    #[test]
    fn continue_statement() {
        let mut context = ExecutionContext::default();

        let block = parsing::Block::parse(
            "{ let a = 0; let b = false; loop { a = a + 1; if a >= 2 { break; } continue; b = true; } }",
        )
        .unwrap()
        .1;
        assert_eq!(
            run_block(&mut context, Box::leak(Box::new(block))),
            Ok(NoneType.into())
        );
        assert_eq!(context.stack.get_variable(&"b"), Ok(&false.into()));

        let block = parsing::Block::parse(
            "{ let a = 0; let b = false; 'parent: loop { a = a + 1; if a >= 2 { break; } loop { continue 'parent; } b = true; } }",
        )
        .unwrap()
        .1;
        assert_eq!(
            run_block(&mut context, Box::leak(Box::new(block))),
            Ok(NoneType.into())
        );
        assert_eq!(context.stack.get_variable(&"b"), Ok(&false.into()));
    }

    #[test]
    fn while_statement() {
        let mut context = ExecutionContext::default();

        let block =
            parsing::Block::parse("{ let count = 0; while count < 5 { count = count + 1; } }")
                .unwrap()
                .1;
        assert_eq!(
            run_block(&mut context, Box::leak(Box::new(block))),
            Ok(NoneType.into())
        );
        assert_eq!(
            context.stack.get_variable(&"count"),
            Ok(&Number::new(5.0).unwrap().into())
        );
    }

    #[test]
    fn for_statement() {
        let mut context = ExecutionContext::default();

        let block =
            parsing::Block::parse("{ let count = 0; for i in 0..5 { count = count + 1; } }")
                .unwrap()
                .1;
        assert_eq!(
            run_block(&mut context, Box::leak(Box::new(block))),
            Ok(NoneType.into())
        );
        assert_eq!(
            context.stack.get_variable(&"count"),
            Ok(&Number::new(5.0).unwrap().into())
        );

        let block =
            parsing::Block::parse("{ let count = 0; for i in 0..5 { count = count + i; } }")
                .unwrap()
                .1;
        assert_eq!(
            run_block(&mut context, Box::leak(Box::new(block))),
            Ok(NoneType.into())
        );
        assert_eq!(
            context.stack.get_variable(&"count"),
            Ok(&Number::new(10.0).unwrap().into())
        );

        let block = parsing::Block::parse(
            "{ let a = 0; let b = 0; for [x, y] in [[1, 2], [3, 4]] { a = a + x; b = b + y; } }",
        )
        .unwrap()
        .1;
        assert_eq!(
            run_block(&mut context, Box::leak(Box::new(block))),
            Ok(NoneType.into())
        );
        assert_eq!(
            context.stack.get_variable(&"a"),
            Ok(&Number::new(4.0).unwrap().into())
        );
        assert_eq!(
            context.stack.get_variable(&"b"),
            Ok(&Number::new(6.0).unwrap().into())
        );
    }

    #[test]
    fn match_statement() {
        let mut context = ExecutionContext::default();

        let block = parsing::Block::parse("{ match 1 { 1 => true, default => false } }")
            .unwrap()
            .1;
        assert_eq!(
            run_block(&mut context, Box::leak(Box::new(block))),
            Ok(true.into())
        );

        let block = parsing::Block::parse("{ match 2 { 1 => true, default => false } }")
            .unwrap()
            .1;
        assert_eq!(
            run_block(&mut context, Box::leak(Box::new(block))),
            Ok(false.into())
        );

        let block =
            parsing::Block::parse("{ match [1, 2] { [1, 1] => 2, [1, 2] => 1, default => 0 } }")
                .unwrap()
                .1;
        assert_eq!(
            run_block(&mut context, Box::leak(Box::new(block))),
            Ok(Number::new(1.0).unwrap().into())
        );

        let block =
            parsing::Block::parse("{ match [1, 1] { [1, 1] => 2, [1, 2] => 1, default => 0 } }")
                .unwrap()
                .1;
        assert_eq!(
            run_block(&mut context, Box::leak(Box::new(block))),
            Ok(Number::new(2.0).unwrap().into())
        );

        let block = parsing::Block::parse("{ match [1, 2] { [1, default] => 1, default => 0 } }")
            .unwrap()
            .1;
        assert_eq!(
            run_block(&mut context, Box::leak(Box::new(block))),
            Ok(Number::new(1.0).unwrap().into())
        );

        let block = parsing::Block::parse("{ match [1, 1] { [1, default] => 1, default => 0 } }")
            .unwrap()
            .1;
        assert_eq!(
            run_block(&mut context, Box::leak(Box::new(block))),
            Ok(Number::new(1.0).unwrap().into())
        );
    }

    #[test]
    fn define_function() {
        let mut context = ExecutionContext::default();

        let statement = parsing::Statement::parse("function my_function() -> Number { 0 }")
            .unwrap()
            .1;
        assert_eq!(
            run_statement(&mut context, Box::leak(Box::new(statement))),
            Ok(NoneType.into())
        );

        assert!(matches!(
            context.stack.get_variable(&"my_function"),
            Ok(Value::UserFunction(_))
        ));
    }

    #[test]
    fn define_structure() {
        let mut context = ExecutionContext::default();

        let statement = parsing::Statement::parse("struct MyStruct {}").unwrap().1;
        assert_eq!(
            run_statement(&mut context, Box::leak(Box::new(statement))),
            Ok(NoneType.into())
        );

        assert!(matches!(
            context.stack.get_variable(&"MyStruct"),
            Ok(Value::StructDefinition(_))
        ));
    }
}
