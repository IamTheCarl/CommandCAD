use crate::script::{
    parsing::{
        self, Assign, AssignableVariable, Break, Continue, For, If, Loop, Match, Return, Statement,
        While,
    },
    LogMessage, RuntimeLog, Span,
};

use super::{
    expressions::{self, run_expression},
    run_block,
    types::List,
    types::{NoneType, Object, Value},
    ControlFlow, ExecutionContext, ExecutionResult,
};

pub fn run_statement<'a, S: Span>(
    context: &mut ExecutionContext<'a, S>,
    statement: &Statement<S>,
) -> ExecutionResult<'a, S, Value<'a, S>> {
    match statement {
        Statement::Expression(expression) => expressions::run_expression(context, expression),
        Statement::Assign(assignment) => run_assignment(context, assignment),
        Statement::Return(return_statement) => run_return(context, return_statement),
        Statement::If(if_statement) => run_if(context, if_statement),
        Statement::Match(match_statement) => run_match(context, match_statement),
        Statement::For(for_statement) => run_for(context, for_statement),
        Statement::While(while_statement) => run_while(context, while_statement),
        Statement::Loop(loop_statement) => run_loop(context, loop_statement),
        Statement::Break(break_statement) => run_break(context, break_statement),
        Statement::Continue(continue_statement) => run_continue(context, continue_statement),
    }
}

fn assign_values<'a, S: Span>(
    log: &mut RuntimeLog<S>,
    to_assign: &parsing::Assignable<S>,
    assignment_span: &S,
    source_span: &S,
    value: Value<'a, S>,
    mut assign: impl FnMut(&mut RuntimeLog<S>, &S, Value<'a, S>) -> ExecutionResult<'a, S, Value<'a, S>>,
) -> ExecutionResult<'a, S, Value<'a, S>> {
    fn assign_single_value<'a, S: Span>(
        log: &mut RuntimeLog<S>,
        assignment_span: &S,
        value: Value<'a, S>,
        variable: &AssignableVariable<S>,
        assign: impl FnOnce(
            &mut RuntimeLog<S>,
            &S,
            Value<'a, S>,
        ) -> ExecutionResult<'a, S, Value<'a, S>>,
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        if let Some(ty) = &variable.ty {
            if value.matches_type(ty) {
                assign(log, &variable.name, value)
            } else {
                log.push(LogMessage::ExpectedGot(
                    assignment_span.clone(),
                    ty.name(),
                    value.type_name(),
                ));
                Err(ControlFlow::Failure)
            }
        } else {
            assign(log, &variable.name, value)
        }
    }

    match to_assign {
        parsing::Assignable::Variable(variable) => {
            assign_single_value(log, assignment_span, value, variable, &mut assign)
        }
        parsing::Assignable::List(_span, variables) => {
            let values = value.downcast::<List<S>>(log, source_span)?;

            if values.len() != variables.len() {
                log.push(LogMessage::ListLengthsDontMatch(source_span.clone()));
                return Err(ControlFlow::Failure);
            }

            for (variable, value) in variables.iter().zip(values.iter().cloned()) {
                assign_single_value(log, assignment_span, value, variable, &mut assign)?;
            }

            Ok(NoneType.into())
        }
    }
}

fn run_assignment<'a, S: Span>(
    context: &mut ExecutionContext<'a, S>,
    assignment: &Assign<S>,
) -> ExecutionResult<'a, S, Value<'a, S>> {
    let value = run_statement(context, &assignment.statement)?;

    if assignment.is_new {
        let assign = |_log: &mut RuntimeLog<S>,
                      name: &S,
                      value: Value<'a, S>|
         -> ExecutionResult<'a, S, Value<'a, S>> {
            context.stack.new_variable(name, value);

            Ok(NoneType.into())
        };

        assign_values(
            &mut context.log,
            &assignment.to_assign,
            assignment.get_span(),
            assignment.statement.get_span(),
            value,
            assign,
        )
    } else {
        let stack = &mut context.stack;

        let assign = |log: &mut RuntimeLog<S>,
                      name: &S,
                      value: Value<'a, S>|
         -> ExecutionResult<'a, S, Value<'a, S>> {
            let variable = stack.get_variable_mut(log, name)?;
            *variable = value;

            Ok(NoneType.into())
        };

        assign_values(
            &mut context.log,
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
    return_statement: &Return<S>,
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
    if_statement: &If<S>,
) -> ExecutionResult<'a, S, Value<'a, S>> {
    let condition = expressions::run_expression(context, &if_statement.expression)?;

    if condition.downcast::<bool>(&mut context.log, if_statement.expression.get_span())? {
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
    match_statement: &Match<S>,
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

    context.log.push(LogMessage::DidNotMatch(
        match_statement.expression.get_span().clone(),
    ));
    Err(ControlFlow::Failure)
}

fn loop_impl<'a, S, F>(
    mut loop_control: F,
    context: &mut ExecutionContext<'a, S>,
    name: Option<&S>,
    block: &parsing::Block<S>,
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

                            context.log.push(LogMessage::ExpectedGot(
                                final_statement.get_span().clone(),
                                "None".into(),
                                result.type_name(),
                            ));
                            break Err(ControlFlow::Failure);
                        }
                    }
                    Err(exit_reason) => match exit_reason {
                        ControlFlow::Failure => break Err(ControlFlow::Failure),
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
    for_statement: &For<S>,
) -> ExecutionResult<'a, S, Value<'a, S>> {
    let iter_expression = run_expression(context, &for_statement.iterator_expression)?;
    let mut iterator = iter_expression.iterate(
        &mut context.log,
        for_statement.iterator_expression.get_span(),
    )?;

    loop_impl(
        |context| {
            if let Some(next_value) = iterator.next() {
                let assign = |_log: &mut RuntimeLog<S>,
                              name: &S,
                              value: Value<'a, S>|
                 -> ExecutionResult<'a, S, Value<'a, S>> {
                    context.stack.new_variable(name, value);

                    Ok(NoneType.into())
                };
                assign_values(
                    &mut context.log,
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
    while_statement: &While<S>,
) -> ExecutionResult<'a, S, Value<'a, S>> {
    loop_impl(
        |context| {
            run_expression(context, &while_statement.expression)?
                .downcast::<bool>(&mut context.log, while_statement.expression.get_span())
        },
        context,
        while_statement.name.as_ref(),
        &while_statement.block,
    )
}

fn run_loop<'a, S: Span>(
    context: &mut ExecutionContext<'a, S>,
    loop_statement: &Loop<S>,
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
    break_statement: &Break<S>,
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
    continue_statement: &Continue<S>,
) -> ExecutionResult<'a, S, Value<'a, S>> {
    Err(ControlFlow::Continue {
        span: continue_statement.get_span().clone(),
        label: continue_statement.loop_name.clone(),
    })
}

#[cfg(test)]
mod test {
    use crate::script::execution::{types::Number, ControlFlow, Stack};

    use super::*;

    #[test]
    fn assignment() {
        let mut context = ExecutionContext {
            log: RuntimeLog::default(),
            stack: Stack::default(),
        };

        let statement = parsing::Statement::parse("value = 1").unwrap().1;
        assert_eq!(
            run_statement(&mut context, &statement),
            Err(ControlFlow::Failure)
        );
        assert!(context
            .stack
            .get_variable(&mut context.log, &"value")
            .is_err());

        let statement = parsing::Statement::parse("let value = 1").unwrap().1;
        assert_eq!(run_statement(&mut context, &statement), Ok(NoneType.into()));
        assert_eq!(
            context.stack.get_variable(&mut context.log, &"value"),
            Ok(&Number::new(1.0).unwrap().into())
        );

        let statement = parsing::Statement::parse("value = 2").unwrap().1;
        assert_eq!(run_statement(&mut context, &statement), Ok(NoneType.into()));
        assert_eq!(
            context.stack.get_variable(&mut context.log, &"value"),
            Ok(&Number::new(2.0).unwrap().into())
        );

        let statement = parsing::Statement::parse("let [one, two] = [1, 2]")
            .unwrap()
            .1;
        assert_eq!(run_statement(&mut context, &statement), Ok(NoneType.into()));
        assert_eq!(
            context.stack.get_variable(&mut context.log, &"one"),
            Ok(&Number::new(1.0).unwrap().into())
        );
        assert_eq!(
            context.stack.get_variable(&mut context.log, &"two"),
            Ok(&Number::new(2.0).unwrap().into())
        );

        let statement = parsing::Statement::parse("[one, two] = [3, 4]").unwrap().1;
        assert_eq!(run_statement(&mut context, &statement), Ok(NoneType.into()));
        assert_eq!(
            context.stack.get_variable(&mut context.log, &"one"),
            Ok(&Number::new(3.0).unwrap().into())
        );
        assert_eq!(
            context.stack.get_variable(&mut context.log, &"two"),
            Ok(&Number::new(4.0).unwrap().into())
        );

        let statement = parsing::Statement::parse("let [one, two] = [1, 2, 3]")
            .unwrap()
            .1;
        assert_eq!(
            run_statement(&mut context, &statement),
            Err(ControlFlow::Failure)
        );
        let statement = parsing::Statement::parse("let [one, two] = [1]").unwrap().1;
        assert_eq!(
            run_statement(&mut context, &statement),
            Err(ControlFlow::Failure)
        );
        let statement = parsing::Statement::parse("let [one, two] = 1").unwrap().1;
        assert_eq!(
            run_statement(&mut context, &statement),
            Err(ControlFlow::Failure)
        );
    }

    #[test]
    fn assign_scopes() {
        let mut context = ExecutionContext {
            log: Default::default(),
            stack: Stack::default(),
        };
        context
            .stack
            .new_variable(&"one", Number::new(1.0).unwrap().into());
        context
            .stack
            .new_variable(&"value", Number::new(1.0).unwrap().into());

        context.new_scope(|context| {
            let statement = parsing::Statement::parse("let one = 2").unwrap().1;
            assert_eq!(run_statement(context, &statement), Ok(NoneType.into()));

            assert_eq!(
                context.stack.get_variable(&mut context.log, &"value"),
                Ok(&Number::new(1.0).unwrap().into())
            );
            let statement = parsing::Statement::parse("value = 2").unwrap().1;
            assert_eq!(run_statement(context, &statement), Ok(NoneType.into()));
            assert_eq!(
                context.stack.get_variable(&mut context.log, &"value"),
                Ok(&Number::new(2.0).unwrap().into())
            );
        });

        assert_eq!(
            context.stack.get_variable(&mut context.log, &"value"),
            Ok(&Number::new(2.0).unwrap().into())
        );
        assert_eq!(
            context.stack.get_variable(&mut context.log, &"one"),
            Ok(&Number::new(1.0).unwrap().into())
        );
    }

    #[test]
    fn loop_statement() {
        let mut context = ExecutionContext {
            log: Default::default(),
            stack: Stack::default(),
        };

        let statement = parsing::Statement::parse("loop { break; }").unwrap().1;
        assert_eq!(run_statement(&mut context, &statement), Ok(NoneType.into()));
    }

    #[test]
    fn if_statement() {
        let mut context = ExecutionContext {
            log: Default::default(),
            stack: Stack::default(),
        };

        let statement = parsing::Statement::parse("if true { 1.0 }").unwrap().1;
        assert_eq!(
            run_statement(&mut context, &statement),
            Ok(Number::new(1.0).unwrap().into())
        );

        let statement = parsing::Statement::parse("if false { 1.0 }").unwrap().1;
        assert_eq!(run_statement(&mut context, &statement), Ok(NoneType.into()));

        let statement = parsing::Statement::parse("if true { 1.0 } else { 2.0 }")
            .unwrap()
            .1;
        assert_eq!(
            run_statement(&mut context, &statement),
            Ok(Number::new(1.0).unwrap().into())
        );

        let statement = parsing::Statement::parse("if false { 1.0 } else { 2.0 }")
            .unwrap()
            .1;
        assert_eq!(
            run_statement(&mut context, &statement),
            Ok(Number::new(2.0).unwrap().into())
        );

        let statement = parsing::Statement::parse("if false { 1.0 } else if true { 2.0 }")
            .unwrap()
            .1;
        assert_eq!(
            run_statement(&mut context, &statement),
            Ok(Number::new(2.0).unwrap().into())
        );

        let statement = parsing::Statement::parse("if true { 1.0 } else if true { 2.0 }")
            .unwrap()
            .1;
        assert_eq!(
            run_statement(&mut context, &statement),
            Ok(Number::new(1.0).unwrap().into())
        );

        let statement =
            parsing::Statement::parse("if false { 1.0 } else if false { 2.0 } else { 3.0 }")
                .unwrap()
                .1;
        assert_eq!(
            run_statement(&mut context, &statement),
            Ok(Number::new(3.0).unwrap().into())
        );
    }

    #[test]
    fn break_statement() {
        let mut context = ExecutionContext {
            log: Default::default(),
            stack: Stack::default(),
        };

        let statement = parsing::Statement::parse("loop { break; }").unwrap().1;
        assert_eq!(run_statement(&mut context, &statement), Ok(NoneType.into()));

        let block = parsing::Block::parse(
            "{ let test_one = 0; loop { test_one = 1; break; test_one = 2 } }",
        )
        .unwrap()
        .1;
        assert_eq!(run_block(&mut context, &block), Ok(NoneType.into()));
        assert_eq!(
            context.stack.get_variable(&mut context.log, &"test_one"),
            Ok(&Number::new(1.0).unwrap().into())
        );

        let block = parsing::Block::parse(
            "{ let test_one = 0; loop { if test_one >= 5 { break; } test_one = test_one + 1; } }",
        )
        .unwrap()
        .1;
        assert_eq!(run_block(&mut context, &block), Ok(NoneType.into()));
        assert_eq!(
            context.stack.get_variable(&mut context.log, &"test_one"),
            Ok(&Number::new(5.0).unwrap().into())
        );

        let block = parsing::Block::parse(
            "{ let test_one = 0; 'parent: loop { loop { break 'parent; } test_one = 1 } }",
        )
        .unwrap()
        .1;
        assert_eq!(run_block(&mut context, &block), Ok(NoneType.into()));
        assert_eq!(
            context.stack.get_variable(&mut context.log, &"test_one"),
            Ok(&Number::new(0.0).unwrap().into())
        );

        let block = parsing::Block::parse("{ loop { break 1.0; } }").unwrap().1;
        assert_eq!(
            run_block(&mut context, &block),
            Ok(Number::new(1.0).unwrap().into())
        );
    }

    #[test]
    fn continue_statement() {
        let mut context = ExecutionContext {
            log: Default::default(),
            stack: Stack::default(),
        };

        let block = parsing::Block::parse(
            "{ let a = 0; let b = false; loop { a = a + 1; if a >= 2 { break; } continue; b = true; } }",
        )
        .unwrap()
        .1;
        assert_eq!(run_block(&mut context, &block), Ok(NoneType.into()));
        assert_eq!(
            context.stack.get_variable(&mut context.log, &"b"),
            Ok(&false.into())
        );

        let block = parsing::Block::parse(
            "{ let a = 0; let b = false; 'parent: loop { a = a + 1; if a >= 2 { break; } loop { continue 'parent; } b = true; } }",
        )
        .unwrap()
        .1;
        assert_eq!(run_block(&mut context, &block), Ok(NoneType.into()));
        assert_eq!(
            context.stack.get_variable(&mut context.log, &"b"),
            Ok(&false.into())
        );
    }

    #[test]
    fn while_statement() {
        let mut context = ExecutionContext {
            log: Default::default(),
            stack: Stack::default(),
        };

        let block =
            parsing::Block::parse("{ let count = 0; while count < 5 { count = count + 1; } }")
                .unwrap()
                .1;
        assert_eq!(run_block(&mut context, &block), Ok(NoneType.into()));
        assert_eq!(
            context.stack.get_variable(&mut context.log, &"count"),
            Ok(&Number::new(5.0).unwrap().into())
        );
    }

    #[test]
    fn for_statement() {
        let mut context = ExecutionContext {
            log: Default::default(),
            stack: Stack::default(),
        };

        let block =
            parsing::Block::parse("{ let count = 0; for i in 0..5 { count = count + 1; } }")
                .unwrap()
                .1;
        assert_eq!(run_block(&mut context, &block), Ok(NoneType.into()));
        assert_eq!(
            context.stack.get_variable(&mut context.log, &"count"),
            Ok(&Number::new(5.0).unwrap().into())
        );

        let block =
            parsing::Block::parse("{ let count = 0; for i in 0..5 { count = count + i; } }")
                .unwrap()
                .1;
        assert_eq!(run_block(&mut context, &block), Ok(NoneType.into()));
        assert_eq!(
            context.stack.get_variable(&mut context.log, &"count"),
            Ok(&Number::new(10.0).unwrap().into())
        );

        let block = parsing::Block::parse(
            "{ let a = 0; let b = 0; for [x, y] in [[1, 2], [3, 4]] { a = a + x; b = b + y; } }",
        )
        .unwrap()
        .1;
        assert_eq!(run_block(&mut context, &block), Ok(NoneType.into()));
        assert_eq!(
            context.stack.get_variable(&mut context.log, &"a"),
            Ok(&Number::new(4.0).unwrap().into())
        );
        assert_eq!(
            context.stack.get_variable(&mut context.log, &"b"),
            Ok(&Number::new(6.0).unwrap().into())
        );
    }

    #[test]
    fn match_statement() {
        let mut context = ExecutionContext {
            log: Default::default(),
            stack: Stack::default(),
        };

        let block = parsing::Block::parse("{ match 1 { 1 => true, default => false } }")
            .unwrap()
            .1;
        assert_eq!(run_block(&mut context, &block), Ok(true.into()));

        let block = parsing::Block::parse("{ match 2 { 1 => true, default => false } }")
            .unwrap()
            .1;
        assert_eq!(run_block(&mut context, &block), Ok(false.into()));

        let block =
            parsing::Block::parse("{ match [1, 2] { [1, 1] => 2, [1, 2] => 1, default => 0 } }")
                .unwrap()
                .1;
        assert_eq!(
            run_block(&mut context, &block),
            Ok(Number::new(1.0).unwrap().into())
        );

        let block =
            parsing::Block::parse("{ match [1, 1] { [1, 1] => 2, [1, 2] => 1, default => 0 } }")
                .unwrap()
                .1;
        assert_eq!(
            run_block(&mut context, &block),
            Ok(Number::new(2.0).unwrap().into())
        );

        let block = parsing::Block::parse("{ match [1, 2] { [1, default] => 1, default => 0 } }")
            .unwrap()
            .1;
        assert_eq!(
            run_block(&mut context, &block),
            Ok(Number::new(1.0).unwrap().into())
        );

        let block = parsing::Block::parse("{ match [1, 1] { [1, default] => 1, default => 0 } }")
            .unwrap()
            .1;
        assert_eq!(
            run_block(&mut context, &block),
            Ok(Number::new(1.0).unwrap().into())
        );
    }
}
