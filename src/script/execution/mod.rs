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

use std::collections::{HashMap, HashSet};

use crate::script::execution::types::NoneType;

use self::types::{validate_assignment_type, Measurement, OperatorResult};

use super::{
    parsing::{self, Block, CallableBlock},
    RuntimeLog, Span,
};

pub mod types;
use compact_str::CompactString;
use fj_core::Core;
use types::{StructDefinition, UserFunction, Value};

mod expressions;
mod failure_message;
mod module;
pub mod statements;
pub use module::Module;

pub use failure_message::Failure;

#[derive(Debug)]
enum ScopeType {
    Inherited,
    Isolated,
    Module,
    Closure {
        references: HashSet<compact_str::CompactString>,
    },
}

impl Default for ScopeType {
    fn default() -> Self {
        Self::Inherited
    }
}

#[derive(Debug)]
struct Scope<'a, S: Span> {
    ty: ScopeType,
    variables: HashMap<compact_str::CompactString, Value<'a, S>>,
}

impl<'a, S: Span> Default for Scope<'a, S> {
    fn default() -> Self {
        Self {
            ty: ScopeType::Module,
            variables: Default::default(),
        }
    }
}

macro_rules! optionally_mut_ref {
    (mutable $l:lifetime $ty:ty) => {
        & $l mut $ty
    };
    (immutable $l:lifetime $ty:ty) => {
        & $l $ty
    };
}

macro_rules! generate_variable_getter {
    ($self:ident, $span:ident, $name:ident, $iter:ident, $get:ident, $mutable:ident) => {
	{
            fn check_module_scope<'a, 'b, S: Span>(
                mut scope_iterator: impl Iterator<Item = optionally_mut_ref!($mutable 'b Scope<'a, S>)>,
                name: &str,
            ) -> Option<optionally_mut_ref!($mutable 'b Value<'a, S>)> {
                if let Some(value) = scope_iterator
                    .find(|scope| matches!(&scope.ty, ScopeType::Module))
                    .and_then(|scope| scope.variables.$get(name))
                {
                    Some(value)
                } else {
                    None
                }
            }

            // TODO we should refuse to provide module level scopes when doing an immutable access, since those need to be fully immutable.
            let mut scope_iterator = $self.scopes[..=$self.active_scope].$iter().rev();

            for scope in &mut scope_iterator {
                if let Some(value) = scope.variables.$get($name) {
                    return Ok(value);
                }

                match &scope.ty {
                    // This is the scope of a closure. If the variable we are looking for is referenced, keep searching up the stack for it.
                    ScopeType::Closure { references } => {
                        if references.contains($name) {
                            continue;
                        } else {
                            // Oh, well then let's skip to the module scope.
                            if let Some(value) = check_module_scope(scope_iterator, $name) {
                                return Ok(value);
                            }
                        }
                        break;
                    }
                    // If this scope is isolated, then we should skip to the module scope.
                    ScopeType::Isolated => {
                        if let Some(value) = check_module_scope(scope_iterator, $name) {
                            return Ok(value);
                        }
                        break;
                    }
                    _ => {}
                }
            }

            Err(Failure::VariableNotInScope(
                $span.clone(),
                $name.to_string().into(),
            ))
	}
    };
}

// TODO do we want to implement a stack limit?
#[derive(Debug)]
pub struct Stack<'a, S: Span> {
    scopes: Vec<Scope<'a, S>>,
    active_scope: usize,
}

impl<'a, S: Span> Default for Stack<'a, S> {
    fn default() -> Self {
        Self {
            scopes: vec![Scope {
                ty: ScopeType::Module,
                ..Default::default()
            }],
            active_scope: 0,
        }
    }
}

impl<'a, S: Span> Stack<'a, S> {
    pub fn new(module_scope: ModuleScope<'a, S>) -> Self {
        let mut root_scope = Scope {
            ty: ScopeType::Module,
            ..Default::default()
        };

        for (name, definition) in module_scope.structs {
            root_scope
                .variables
                .insert(name.into(), StructDefinition { definition }.into());
        }

        for (name, function) in module_scope.functions {
            root_scope.variables.insert(
                name.into(),
                UserFunction {
                    block: &function.named_block,
                    signature: function.signature.clone(),
                }
                .into(),
            );
        }

        for (name, task) in module_scope.tasks {
            root_scope.variables.insert(
                name.into(),
                UserFunction {
                    block: &task.named_block,
                    signature: task.signature.clone(),
                }
                .into(),
            );
        }

        for (name, sketch) in module_scope.sketches {
            root_scope.variables.insert(
                name.into(),
                UserFunction {
                    block: &sketch.named_block,
                    signature: sketch.signature.clone(),
                }
                .into(),
            );
        }

        for (name, solid) in module_scope.solids {
            root_scope.variables.insert(
                name.into(),
                UserFunction {
                    block: &solid.named_block,
                    signature: solid.signature.clone(),
                }
                .into(),
            );
        }

        Self {
            scopes: vec![root_scope],
            active_scope: 0,
        }
    }

    fn push_scope(
        &mut self,
        variables_to_copy: impl Iterator<Item = &'a S>,
        mode: ScopeType,
    ) -> OperatorResult<S, ()> {
        let next_scope_index = self.active_scope + 1;
        if next_scope_index >= self.scopes.len() {
            self.scopes.push(Scope::default());
        }

        self.scopes[next_scope_index].ty = mode;

        for variable in variables_to_copy {
            let value = self.get_variable(variable)?.clone();
            self.scopes[next_scope_index]
                .variables
                .insert(variable.as_str().into(), value);
        }

        self.active_scope = next_scope_index;

        Ok(())
    }

    fn pop_scope(&mut self) {
        self.scopes[self.active_scope].variables.clear();
        self.active_scope -= 1;
    }

    // TODO Recommending similar named variables would help users to notice typos.
    pub fn get_variable(&self, name: &S) -> std::result::Result<&Value<'a, S>, Failure<S>> {
        self.get_variable_str(name, name.as_str())
    }

    pub fn get_variable_mut(&mut self, name: &S) -> Result<&mut Value<'a, S>, Failure<S>> {
        self.get_variable_str_mut(name, name.as_str())
    }

    pub fn get_variable_str(&self, span: &S, name: &str) -> Result<&Value<'a, S>, Failure<S>> {
        generate_variable_getter!(self, span, name, iter, get, immutable)
    }
    pub fn get_variable_str_mut(
        &mut self,
        span: &S,
        name: &str,
    ) -> Result<&mut Value<'a, S>, Failure<S>> {
        generate_variable_getter!(self, span, name, iter_mut, get_mut, mutable)
    }

    pub fn new_variable(&mut self, name: &S, value: Value<'a, S>) {
        self.new_variable_str(name.as_str(), value)
    }

    pub fn new_variable_str(&mut self, name: impl Into<CompactString>, value: Value<'a, S>) {
        let current_scope = &mut self.scopes[self.active_scope];

        current_scope.variables.insert(name.into(), value);
    }
}

#[derive(Debug, Default)]
pub struct ModuleScope<'a, S: Span> {
    structs: HashMap<String, &'a parsing::StructDefinition<S>>,
    functions: HashMap<String, &'a parsing::Function<S>>,
    tasks: HashMap<String, &'a parsing::Task<S>>,
    sketches: HashMap<String, &'a parsing::Sketch<S>>,
    solids: HashMap<String, &'a parsing::Solid<S>>,
}

impl<'a, S: Span> ModuleScope<'a, S> {
    pub fn new(
        module: &'a Module<S>, /* TODO I want to take in the Runtime at some point so we can resolve imports */
    ) -> Self {
        let structs = module
            .root_elements
            .structs
            .iter()
            .map(|structure| (structure.name.to_string(), structure))
            .collect();

        let functions = module
            .root_elements
            .functions
            .iter()
            .map(|function| (function.named_block.name.to_string(), function))
            .collect();

        let tasks = module
            .root_elements
            .tasks
            .iter()
            .map(|task| (task.named_block.name.to_string(), task))
            .collect();

        let sketches = module
            .root_elements
            .sketches
            .iter()
            .map(|sketch| (sketch.named_block.name.to_string(), sketch))
            .collect();

        let solids = module
            .root_elements
            .solids
            .iter()
            .map(|wolid| (wolid.named_block.name.to_string(), wolid))
            .collect();

        Self {
            structs,
            functions,
            tasks,
            sketches,
            solids,
        }
    }
}

pub type ExecutionResult<'a, S, V> = std::result::Result<V, ControlFlow<'a, S>>;

#[derive(Debug, PartialEq)]
pub enum ControlFlow<'a, S: Span> {
    Failure(Failure<S>),
    Break {
        span: S,
        label: Option<S>,
        value: Value<'a, S>,
    },
    Continue {
        span: S,
        label: Option<S>,
    },
    Return {
        value: Value<'a, S>,
    },
}

impl<S: Span> From<Failure<S>> for ControlFlow<'_, S> {
    fn from(value: Failure<S>) -> Self {
        Self::Failure(value)
    }
}

pub struct GlobalResources {
    pub convert_to_fornjot_units: fn(&Measurement) -> Option<f64>,

    // FIXME we need to unwind the validation messages, otherwise this panics on drop.
    pub fornjot_core: Core,
}

impl Default for GlobalResources {
    fn default() -> Self {
        Self {
            convert_to_fornjot_units: Measurement::get_measurement_to_float_converter("mm")
                .unwrap(),
            fornjot_core: Default::default(),
        }
    }
}

pub struct ExecutionContext<'a, S: Span> {
    pub global_resources: GlobalResources,
    pub log: RuntimeLog<S>,
    pub stack: Stack<'a, S>,
}

impl<'a, S: Span> Default for ExecutionContext<'a, S> {
    fn default() -> Self {
        let mut context = Self {
            global_resources: Default::default(),
            log: RuntimeLog::new(),
            stack: Default::default(),
        };

        // FIXME this registers the global functions as part of the module,
        // which is not actually global. This is a bad way to do this because
        // other modules won't have access to the global functions.
        types::register_globals(&mut context);

        context
    }
}

impl<'a, S: Span> ExecutionContext<'a, S> {
    pub fn new(module_scope: ModuleScope<'a, S>) -> Self {
        let mut context = Self {
            global_resources: GlobalResources::default(),
            log: RuntimeLog::new(),
            stack: Stack::new(module_scope),
        };

        // FIXME this registers the global functions as part of the module,
        // which is not actually global. This is a bad way to do this because
        // other modules won't have access to the global functions.
        types::register_globals(&mut context);

        context
    }

    pub fn new_scope<R>(&mut self, scope: impl FnOnce(&mut Self) -> R) -> R {
        // This doesn't copy any variables, so it can't fail.
        self.stack
            .push_scope(std::iter::empty(), ScopeType::Inherited)
            .unwrap();
        let result = scope(self);
        self.stack.pop_scope();

        result
    }

    pub fn new_isolated_scope<R>(&mut self, scope: impl FnOnce(&mut Self) -> R) -> R {
        // This doesn't copy any variables, so it can't fail.
        self.stack
            .push_scope(std::iter::empty(), ScopeType::Isolated)
            .unwrap();
        let result = scope(self);
        self.stack.pop_scope();

        result
    }

    pub fn new_closure_scope<R>(
        &mut self,
        references: impl Iterator<Item = impl Into<compact_str::CompactString>>,
        copies: impl Iterator<Item = &'a S>,
        scope: impl FnOnce(&mut Self) -> R,
    ) -> OperatorResult<S, R> {
        self.stack.push_scope(
            copies,
            ScopeType::Closure {
                references: references.map(|s| s.into()).collect(),
            },
        )?;
        let result = scope(self);
        self.stack.pop_scope();

        Ok(result)
    }
}

fn run_block<'a, S: Span>(
    context: &mut ExecutionContext<'a, S>,
    block: &'a Block<S>,
) -> ExecutionResult<'a, S, Value<'a, S>> {
    let mut result = NoneType.into();

    for statement in block.statements.iter() {
        if let Some(statement) = statement.get() {
            result = statements::run_statement(context, statement)?;
        }
    }

    Ok(result)
}

fn run_callable_block<'a, S: Span>(
    context: &mut ExecutionContext<'a, S>,
    block: &'a CallableBlock<S>,
    arguments: Vec<Value<'a, S>>,
    spans: &[parsing::Expression<S>],
    default_span: &S,
) -> Result<Value<'a, S>, Failure<S>> {
    // We do not return a ControlFlow because control flow does not
    // pass through named blocks (we can't continue or break a for loop outside of this named block)
    match arguments.len().cmp(&block.parameters.len()) {
        std::cmp::Ordering::Equal => {
            let mut failures = Vec::new();

            // Validate the arguments and put them into scope..
            for (span, (argument, parameter)) in spans
                .iter()
                .map(|expression| expression.get_span())
                .chain(std::iter::repeat(default_span))
                .zip(arguments.into_iter().zip(&block.parameters))
            {
                match validate_assignment_type(context, parameter, span, argument) {
                    Ok(value) => {
                        context.stack.new_variable(&parameter.name, value);
                    }
                    Err(failure) => failures.push(failure),
                }
            }

            if failures.is_empty() {
                match run_block(context, &block.block) {
                    Ok(value) => Ok(value),
                    Err(control_flow) => match control_flow {
                        ControlFlow::Return { value } => Ok(value), // Oh that's normal behavior.
                        ControlFlow::Failure(failure) => Err(failure),
                        ControlFlow::Break {
                            span,
                            label: None,
                            value: _,
                        } => Err(Failure::BreakOutsideOfLoop(span)),
                        ControlFlow::Break {
                            span,
                            label: Some(label),
                            value: _,
                        } => Err(Failure::BreakLabelNotFound(span, label)),
                        ControlFlow::Continue { span, label: None } => {
                            Err(Failure::ContinueOutsideOfLoop(span))
                        }
                        ControlFlow::Continue {
                            span,
                            label: Some(label),
                        } => Err(Failure::ContinueLabelNotFound(span, label)),
                    },
                }
            } else {
                Err(Failure::BadArgumentTypes(
                    block.parameter_span.clone(),
                    failures,
                ))
            }
        }
        std::cmp::Ordering::Less => Err(Failure::MissingArguments(block.parameter_span.clone())),
        std::cmp::Ordering::Greater => Err(Failure::MissingArguments(block.parameter_span.clone())),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::script::{
        execution::{
            expressions::run_expression, run_block, types::Number, ExecutionContext, ModuleScope,
        },
        parsing::Expression,
    };

    #[test]
    fn functions() {
        let mut log = Vec::new();

        let module = Module::load(
            &mut log,
            "test_module.ccm",
            "function my_function(input: Number) -> Number { input }",
        )
        .unwrap();

        assert!(log.is_empty());

        let module_scope = ModuleScope::new(&module);

        let mut context = ExecutionContext::new(module_scope);
        let block = parsing::Block::parse("{ my_function(5) }").unwrap().1;

        let result = run_block(&mut context, Box::leak(Box::new(block)));
        assert_eq!(result, Ok(Number::new(5.0).unwrap().into()));
    }

    #[test]
    fn default_function() {
        let mut log = Vec::new();

        let module = Module::load(
            &mut log,
            "test_module.ccm",
            "function my_function(input: Number = 5) -> Number { input }",
        )
        .unwrap();

        assert!(log.is_empty());

        let module_scope = ModuleScope::new(&module);

        let mut context = ExecutionContext::new(module_scope);
        let block = parsing::Block::parse("{ my_function(default) }").unwrap().1;

        let result = run_block(&mut context, Box::leak(Box::new(block)));
        assert_eq!(result, Ok(Number::new(5.0).unwrap().into()));
    }

    #[test]
    fn function_scope() {
        let mut log = Vec::new();

        let module = Module::load(
            &mut log,
            "test_module.ccm",
            "function my_function(input: Number) -> Number { value = input; input }",
        )
        .unwrap();

        assert!(log.is_empty());

        let module_scope = ModuleScope::new(&module);

        let mut context = ExecutionContext::new(module_scope);
        let block = parsing::Block::parse("{ let value = 0; my_function(5); value }")
            .unwrap()
            .1;

        let result = context.new_scope(|context| run_block(context, Box::leak(Box::new(block))));
        assert_eq!(
            result,
            Err(ControlFlow::Failure(Failure::VariableNotInScope(
                "value",
                "value".into()
            )))
        );
    }

    #[test]
    fn function_hygene() {
        let mut log = Vec::new();

        let module = Module::load(
            &mut log,
            "test_module.ccm",
            "struct MyStruct {} function my_function() -> struct MyStruct { MyStruct {} }",
        )
        .unwrap();

        assert!(log.is_empty());

        let module_scope = ModuleScope::new(&module);

        let mut context = ExecutionContext::new(module_scope);
        let block = parsing::Block::parse("{ my_function() }").unwrap().1;

        let result = context.new_scope(|context| run_block(context, Box::leak(Box::new(block))));
        assert_eq!(
            result.unwrap(),
            run_expression(
                &mut context,
                Box::leak(Box::new(Expression::parse("MyStruct {}").unwrap().1))
            )
            .unwrap()
        );
    }
}
