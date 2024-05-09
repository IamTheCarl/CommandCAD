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

use std::rc::Rc;

use crate::script::{
    execution::{types::List, Failure},
    parsing::{
        self, Assign, AssignableVariable, Break, Continue, For, Function, If, Loop, Match, Return,
        Statement, While,
    },
    Span,
};

use super::{
    expressions::{self, run_expression},
    run_block,
    types::{NoneType, Object, /*List, */ StructDefinition, UserFunction, Value},
    ControlFlow, ExecutionContext, ExecutionResult,
};

pub fn run_statement<S: Span>(
    context: &mut ExecutionContext<S>,
    statement: &Statement<S>,
) -> ExecutionResult<S, Value<S>> {
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

fn assign_values<S: Span>(
    context: &mut ExecutionContext<S>,
    to_assign: &parsing::Assignable<S>,
    assignment_span: &S,
    source_span: &S,
    value: Value<S>,
    mut assign: impl FnMut(&mut ExecutionContext<S>, &S, Value<S>) -> ExecutionResult<S, Value<S>>,
) -> ExecutionResult<S, Value<S>> {
    fn assign_single_value<S: Span>(
        context: &mut ExecutionContext<S>,
        assignment_span: &S,
        value: Value<S>,
        variable: &AssignableVariable<S>,
        assign: impl FnOnce(&mut ExecutionContext<S>, &S, Value<S>) -> ExecutionResult<S, Value<S>>,
    ) -> ExecutionResult<S, Value<S>> {
        if let Some(ty) = &variable.ty {
            if value.matches_type(ty, context.log, assignment_span)? {
                assign(context, &variable.name, value)
            } else {
                Err(ControlFlow::Failure(Failure::ExpectedGot(
                    assignment_span.clone(),
                    ty.name(),
                    value.type_name(),
                )))
            }
        } else {
            assign(context, &variable.name, value)
        }
    }

    match to_assign {
        parsing::Assignable::Variable(variable) => {
            assign_single_value(context, assignment_span, value, variable, &mut assign)
        }
        parsing::Assignable::List(span, variables) => {
            let values = value.downcast::<List<S>>(source_span)?;

            if values.len(span)? != variables.len() {
                return Err(ControlFlow::Failure(Failure::ListLengthsDontMatch(
                    source_span.clone(),
                )));
            }

            let value_iter = values.iter(span)?;
            for (variable, value) in variables.iter().zip(value_iter) {
                assign_single_value(context, assignment_span, value, variable, &mut assign)?;
            }

            Ok(NoneType.into())
        }
    }
}

fn run_assignment<S: Span>(
    context: &mut ExecutionContext<S>,
    assignment: &Assign<S>,
) -> ExecutionResult<S, Value<S>> {
    let value = run_statement(context, &assignment.statement)?;

    if assignment.is_new {
        let assign = |context: &mut ExecutionContext<S>,
                      name: &S,
                      value: Value<S>|
         -> ExecutionResult<S, Value<S>> {
            context.stack.new_variable(name, value);

            Ok(NoneType.into())
        };

        assign_values(
            context,
            &assignment.to_assign,
            assignment.get_span(),
            assignment.statement.get_span(),
            value,
            assign,
        )
    } else {
        let assign = |context: &mut ExecutionContext<S>,
                      name: &S,
                      value: Value<S>|
         -> ExecutionResult<S, Value<S>> {
            let variable = context.stack.get_variable_mut(name)?;
            *variable = value;

            Ok(NoneType.into())
        };

        assign_values(
            context,
            &assignment.to_assign,
            assignment.get_span(),
            assignment.statement.get_span(),
            value,
            assign,
        )
    }
}

fn run_return<S: Span>(
    context: &mut ExecutionContext<S>,
    return_statement: &Return<S>,
) -> ExecutionResult<S, Value<S>> {
    let value = if let Some(expression) = return_statement.expression.as_ref() {
        run_expression(context, expression)?
    } else {
        NoneType.into()
    };

    Err(ControlFlow::Return { value })
}

fn run_if<S: Span>(
    context: &mut ExecutionContext<S>,
    if_statement: &If<S>,
) -> ExecutionResult<S, Value<S>> {
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

fn run_match<S: Span>(
    context: &mut ExecutionContext<S>,
    match_statement: &Match<S>,
) -> ExecutionResult<S, Value<S>> {
    fn check_branch_matches<S: Span>(
        span: &S,
        value: &Value<S>,
        branch_value: &Value<S>,
    ) -> ExecutionResult<S, bool> {
        if value == branch_value || matches!(branch_value, &Value::Default(_)) {
            Ok(true)
        } else if let (Value::List(value_list), Value::List(branch_value_list)) =
            (value, branch_value)
        {
            // These are lists. We need to check the individual components.
            if value_list.len(span)? == branch_value_list.len(span)? {
                let branch_value_list = branch_value_list.iter(span)?;
                for (value, branch_value) in value_list.iter(span)?.zip(branch_value_list) {
                    if !check_branch_matches(span, &value, &branch_value)? {
                        return Ok(false);
                    }
                }

                Ok(true)
            } else {
                Ok(false)
            }
        } else {
            Ok(false)
        }
    }

    let value = run_expression(context, &match_statement.expression)?;

    // TODO should we cache these values in a hash map for situations where we may run this match statement several times?
    for branch in match_statement.branches.iter() {
        let branch_value = Value::from_litteral(context, &branch.litteral)?;

        if check_branch_matches(branch.litteral.get_span(), &value, &branch_value)? {
            return context.new_scope(|context| run_block(context, &branch.block));
        }
    }

    Err(ControlFlow::Failure(Failure::DidNotMatch(
        match_statement.expression.get_span().clone(),
    )))
}

fn loop_impl<S, F>(
    mut loop_control: F,
    context: &mut ExecutionContext<S>,
    name: Option<&S>,
    block: &parsing::Block<S>,
) -> ExecutionResult<S, Value<S>>
where
    S: Span,
    F: FnMut(&mut ExecutionContext<S>) -> ExecutionResult<S, bool>,
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

fn run_for<S: Span>(
    context: &mut ExecutionContext<S>,
    for_statement: &For<S>,
) -> ExecutionResult<S, Value<S>> {
    let iter_expression = run_expression(context, &for_statement.iterator_expression)?;
    let mut iterator =
        iter_expression.iterate(context.log, for_statement.iterator_expression.get_span())?;

    loop_impl(
        |context| {
            if let Some(next_value) = iterator.next() {
                let assign = |context: &mut ExecutionContext<S>,
                              name: &S,
                              value: Value<S>|
                 -> ExecutionResult<S, Value<S>> {
                    context.stack.new_variable(name, value);

                    Ok(NoneType.into())
                };
                let next_value = next_value.clone();

                assign_values(
                    context,
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

fn run_while<S: Span>(
    context: &mut ExecutionContext<S>,
    while_statement: &While<S>,
) -> ExecutionResult<S, Value<S>> {
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

fn run_loop<S: Span>(
    context: &mut ExecutionContext<S>,
    loop_statement: &Loop<S>,
) -> ExecutionResult<S, Value<S>> {
    loop_impl(
        |_context| Ok(true),
        context,
        loop_statement.name.as_ref(),
        &loop_statement.block,
    )
}

fn run_break<S: Span>(
    context: &mut ExecutionContext<S>,
    break_statement: &Break<S>,
) -> ExecutionResult<S, Value<S>> {
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

fn run_continue<S: Span>(
    _context: &mut ExecutionContext<S>,
    continue_statement: &Continue<S>,
) -> ExecutionResult<S, Value<S>> {
    Err(ControlFlow::Continue {
        span: continue_statement.get_span().clone(),
        label: continue_statement.loop_name.clone(),
    })
}

fn run_define_function<S: Span>(
    context: &mut ExecutionContext<S>,
    function: &Rc<Function<S>>,
) -> ExecutionResult<S, Value<S>> {
    context.stack.new_variable(
        &function.named_block.name,
        UserFunction::from(function).into(),
    );

    Ok(NoneType.into())
}

fn run_define_structure<S: Span>(
    context: &mut ExecutionContext<S>,
    definition: &Rc<parsing::StructDefinition<S>>,
) -> ExecutionResult<S, Value<S>> {
    context.stack.new_variable(
        &definition.name,
        StructDefinition {
            definition: Rc::clone(definition),
        }
        .into(),
    );

    Ok(NoneType.into())
}

#[cfg(test)]
mod test {
    use crate::script::{
        execution::{ControlFlow, Failure},
        Runtime,
    };
    use common_data_types::Float;

    use super::*;

    #[test]
    fn assignment() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                run_statement(context, &parsing::Statement::parse("value = 1").unwrap().1),
                Err(ControlFlow::Failure(Failure::VariableNotInScope(
                    "value",
                    "value".into(),
                )))
            );
            assert!(context.stack.get_variable(&"value").is_err());

            assert_eq!(
                run_statement(
                    context,
                    &parsing::Statement::parse("let value = 1").unwrap().1
                ),
                Ok(NoneType.into())
            );
            assert_eq!(
                context.stack.get_variable(&"value"),
                Ok(&Float::new(1.0).unwrap().into())
            );

            assert_eq!(
                run_statement(context, &parsing::Statement::parse("value = 2").unwrap().1),
                Ok(NoneType.into())
            );
            assert_eq!(
                context.stack.get_variable(&"value"),
                Ok(&Float::new(2.0).unwrap().into())
            );

            assert_eq!(
                run_statement(
                    context,
                    &parsing::Statement::parse("let [one, two] = [1, 2]")
                        .unwrap()
                        .1
                ),
                Ok(NoneType.into())
            );
            assert_eq!(
                context.stack.get_variable(&"one"),
                Ok(&Float::new(1.0).unwrap().into())
            );
            assert_eq!(
                context.stack.get_variable(&"two"),
                Ok(&Float::new(2.0).unwrap().into())
            );

            assert_eq!(
                run_statement(
                    context,
                    &parsing::Statement::parse("[one, two] = [3, 4]").unwrap().1
                ),
                Ok(NoneType.into())
            );
            assert_eq!(
                context.stack.get_variable(&"one"),
                Ok(&Float::new(3.0).unwrap().into())
            );
            assert_eq!(
                context.stack.get_variable(&"two"),
                Ok(&Float::new(4.0).unwrap().into())
            );

            assert_eq!(
                run_statement(
                    context,
                    &parsing::Statement::parse("let [one, two] = [1, 2, 3]")
                        .unwrap()
                        .1
                ),
                Err(ControlFlow::Failure(Failure::ListLengthsDontMatch("[")))
            );
            assert_eq!(
                run_statement(
                    context,
                    &parsing::Statement::parse("let [one, two] = [1]").unwrap().1
                ),
                Err(ControlFlow::Failure(Failure::ListLengthsDontMatch("[")))
            );
            assert_eq!(
                run_statement(
                    context,
                    &parsing::Statement::parse("let [one, two] = 1").unwrap().1
                ),
                Err(ControlFlow::Failure(Failure::ExpectedGot(
                    "1",
                    "List".into(),
                    "Number".into()
                )))
            );
        });
    }

    #[test]
    fn assign_scopes() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            context
                .stack
                .new_variable(&"one", Float::new(1.0).unwrap().into());
            context
                .stack
                .new_variable(&"value", Float::new(1.0).unwrap().into());

            context.new_scope(|context| {
                assert_eq!(
                    run_statement(
                        context,
                        &parsing::Statement::parse("let one = 2").unwrap().1
                    ),
                    Ok(NoneType.into())
                );

                assert_eq!(
                    context.stack.get_variable(&"value"),
                    Ok(&Float::new(1.0).unwrap().into())
                );
                assert_eq!(
                    run_statement(context, &parsing::Statement::parse("value = 2").unwrap().1),
                    Ok(NoneType.into())
                );
                assert_eq!(
                    context.stack.get_variable(&"value"),
                    Ok(&Float::new(2.0).unwrap().into())
                );
            });

            assert_eq!(
                context.stack.get_variable(&"value"),
                Ok(&Float::new(2.0).unwrap().into())
            );
            assert_eq!(
                context.stack.get_variable(&"one"),
                Ok(&Float::new(1.0).unwrap().into())
            );
        });
    }

    #[test]
    fn loop_statement() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                run_statement(
                    context,
                    &parsing::Statement::parse("loop { break; }").unwrap().1
                ),
                Ok(NoneType.into())
            );
        });
    }

    #[test]
    fn if_statement() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                run_statement(
                    context,
                    &parsing::Statement::parse("if true { 1.0 }").unwrap().1
                ),
                Ok(Float::new(1.0).unwrap().into())
            );

            assert_eq!(
                run_statement(
                    context,
                    &parsing::Statement::parse("if false { 1.0 }").unwrap().1
                ),
                Ok(NoneType.into())
            );

            assert_eq!(
                run_statement(
                    context,
                    &parsing::Statement::parse("if true { 1.0 } else { 2.0 }")
                        .unwrap()
                        .1
                ),
                Ok(Float::new(1.0).unwrap().into())
            );

            assert_eq!(
                run_statement(
                    context,
                    &parsing::Statement::parse("if false { 1.0 } else { 2.0 }")
                        .unwrap()
                        .1
                ),
                Ok(Float::new(2.0).unwrap().into())
            );

            assert_eq!(
                run_statement(
                    context,
                    &parsing::Statement::parse("if false { 1.0 } else if true { 2.0 }")
                        .unwrap()
                        .1
                ),
                Ok(Float::new(2.0).unwrap().into())
            );

            assert_eq!(
                run_statement(
                    context,
                    &parsing::Statement::parse("if true { 1.0 } else if true { 2.0 }")
                        .unwrap()
                        .1
                ),
                Ok(Float::new(1.0).unwrap().into())
            );

            assert_eq!(
                run_statement(
                    context,
                    &parsing::Statement::parse(
                        "if false { 1.0 } else if false { 2.0 } else { 3.0 }"
                    )
                    .unwrap()
                    .1
                ),
                Ok(Float::new(3.0).unwrap().into())
            );
        });
    }

    #[test]
    fn break_statement() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                run_statement(
                    context,
                    &parsing::Statement::parse("loop { break; }").unwrap().1
                ),
                Ok(NoneType.into())
            );

            assert_eq!(
                run_block(
                    context,
                    &parsing::Block::parse(
                        "{ let test_one = 0; loop { test_one = 1; break; test_one = 2 } }",
                    )
                    .unwrap()
                    .1
                ),
                Ok(NoneType.into())
            );
            assert_eq!(
                context.stack.get_variable(&"test_one"),
                Ok(&Float::new(1.0).unwrap().into())
            );

            assert_eq!(
                run_block(context, &parsing::Block::parse(
            "{ let test_one = 0; loop { if test_one >= 5 { break; } test_one = test_one + 1; } }",
        )
        .unwrap()
        .1),
                Ok(NoneType.into())
            );
            assert_eq!(
                context.stack.get_variable(&"test_one"),
                Ok(&Float::new(5.0).unwrap().into())
            );

            assert_eq!(
                run_block(context, &parsing::Block::parse(
                "{ let test_one = 0; 'parent: loop { loop { break 'parent; } test_one = 1 } }",
            )
            .unwrap()
            .1),
                Ok(NoneType.into())
            );
            assert_eq!(
                context.stack.get_variable(&"test_one"),
                Ok(&Float::new(0.0).unwrap().into())
            );

            assert_eq!(
                run_block(
                    context,
                    &parsing::Block::parse("{ loop { break 1.0; } }").unwrap().1
                ),
                Ok(Float::new(1.0).unwrap().into())
            );
        });
    }

    #[test]
    fn continue_statement() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
		run_block(context, &parsing::Block::parse(
		    "{ let a = 0; let b = false; loop { a = a + 1; if a >= 2 { break; } continue; b = true; } }",
		)
			  .unwrap()
        .1),
            Ok(NoneType.into())
        );
            assert_eq!(context.stack.get_variable(&"b"), Ok(&false.into()));

            assert_eq!(
            run_block(context, &parsing::Block::parse(
            "{ let a = 0; let b = false; 'parent: loop { a = a + 1; if a >= 2 { break; } loop { continue 'parent; } b = true; } }",
        )
        .unwrap()
        .1),
            Ok(NoneType.into())
        );
            assert_eq!(context.stack.get_variable(&"b"), Ok(&false.into()));
        });
    }

    #[test]
    fn while_statement() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                run_block(
                    context,
                    &parsing::Block::parse(
                        "{ let count = 0; while count < 5 { count = count + 1; } }"
                    )
                    .unwrap()
                    .1
                ),
                Ok(NoneType.into())
            );
            assert_eq!(
                context.stack.get_variable(&"count"),
                Ok(&Float::new(5.0).unwrap().into())
            );
        });
    }

    #[test]
    fn for_statement() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                run_block(
                    context,
                    &parsing::Block::parse(
                        "{ let count = 0; for i in 0..5 { count = count + 1; } }"
                    )
                    .unwrap()
                    .1
                ),
                Ok(NoneType.into())
            );
            assert_eq!(
                context.stack.get_variable(&"count"),
                Ok(&Float::new(5.0).unwrap().into())
            );

            assert_eq!(
                run_block(
                    context,
                    &parsing::Block::parse(
                        "{ let count = 0; for i in 0..5 { count = count + i; } }"
                    )
                    .unwrap()
                    .1
                ),
                Ok(NoneType.into())
            );
            assert_eq!(
                context.stack.get_variable(&"count"),
                Ok(&Float::new(10.0).unwrap().into())
            );

            assert_eq!(
                run_block(context, &parsing::Block::parse(
            "{ let a = 0; let b = 0; for [x, y] in [[1, 2], [3, 4]] { a = a + x; b = b + y; } }",
        )
        .unwrap()
        .1),
                Ok(NoneType.into())
            );
            assert_eq!(
                context.stack.get_variable(&"a"),
                Ok(&Float::new(4.0).unwrap().into())
            );
            assert_eq!(
                context.stack.get_variable(&"b"),
                Ok(&Float::new(6.0).unwrap().into())
            );
        });
    }

    #[test]
    fn match_statement() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                run_block(
                    context,
                    &parsing::Block::parse("{ match 1 { 1 => true, default => false } }")
                        .unwrap()
                        .1
                ),
                Ok(true.into())
            );

            assert_eq!(
                run_block(
                    context,
                    &parsing::Block::parse("{ match 2 { 1 => true, default => false } }")
                        .unwrap()
                        .1
                ),
                Ok(false.into())
            );

            assert_eq!(
                run_block(
                    context,
                    &parsing::Block::parse(
                        "{ match [1, 2] { [1, 1] => 2, [1, 2] => 1, default => 0 } }"
                    )
                    .unwrap()
                    .1
                ),
                Ok(Float::new(1.0).unwrap().into())
            );

            assert_eq!(
                run_block(
                    context,
                    &parsing::Block::parse(
                        "{ match [1, 1] { [1, 1] => 2, [1, 2] => 1, default => 0 } }"
                    )
                    .unwrap()
                    .1
                ),
                Ok(Float::new(2.0).unwrap().into())
            );

            assert_eq!(
                run_block(
                    context,
                    &parsing::Block::parse("{ match [1, 2] { [1, default] => 1, default => 0 } }")
                        .unwrap()
                        .1
                ),
                Ok(Float::new(1.0).unwrap().into())
            );

            assert_eq!(
                run_block(
                    context,
                    &parsing::Block::parse("{ match [1, 1] { [1, default] => 1, default => 0 } }")
                        .unwrap()
                        .1
                ),
                Ok(Float::new(1.0).unwrap().into())
            );
        });
    }

    #[test]
    fn define_function() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                run_statement(
                    context,
                    &parsing::Statement::parse("function my_function() -> Number { 0 }")
                        .unwrap()
                        .1
                ),
                Ok(NoneType.into())
            );

            assert!(matches!(
                context.stack.get_variable(&"my_function"),
                Ok(Value::UserFunction(_))
            ));
        });
    }

    #[test]
    fn define_structure() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                run_statement(
                    context,
                    &parsing::Statement::parse("struct MyStruct {}").unwrap().1
                ),
                Ok(NoneType.into())
            );

            assert!(matches!(
                context.stack.get_variable(&"MyStruct"),
                Ok(Value::StructDefinition(_))
            ));
        });
    }
}
