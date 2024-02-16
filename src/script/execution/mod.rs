use std::collections::HashMap;

use crate::script::execution::types::NoneType;

use self::types::Object;

use super::{
    module::Module,
    parsing::{self, Block, NamedBlock},
    RuntimeLog, Span,
};

pub mod types;
use types::{StructDefinition, UserFunction, Value};

mod expressions;
mod failure_message;
pub mod statements;

pub use failure_message::Failure;

#[derive(Debug)]
enum ScopeType {
    Inherited,
    Isolated,
    Module,
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
                    return_type: &function.return_type,
                }
                .into(),
            );
        }

        Self {
            scopes: vec![root_scope],
            active_scope: 0,
        }
    }

    fn push_scope(&mut self, mode: ScopeType) {
        self.active_scope += 1;
        if self.active_scope >= self.scopes.len() {
            self.scopes.push(Scope::default());
        }

        self.scopes[self.active_scope].ty = mode;
    }

    fn pop_scope(&mut self) {
        self.scopes[self.active_scope].variables.clear();
        self.active_scope -= 1;
    }

    // TODO Recommending similar named variables would help users to notice typos.

    pub fn get_variable(&self, name: &S) -> std::result::Result<&Value<'a, S>, Failure<S>> {
        self.get_variable_str(name, name.as_str())
    }

    pub fn get_variable_str(&self, span: &S, name: &str) -> Result<&Value<'a, S>, Failure<S>> {
        let mut scope_iterator = self.scopes[..=self.active_scope].iter().rev();

        for scope in &mut scope_iterator {
            if let Some(value) = scope.variables.get(name) {
                return Ok(value);
            }

            // If this scope is isolated, then we should skip to the module scope.
            if matches!(scope.ty, ScopeType::Isolated) {
                if let Some(value) = scope_iterator
                    .find(|scope| matches!(&scope.ty, ScopeType::Module))
                    .and_then(|scope| scope.variables.get(name))
                {
                    return Ok(value);
                }
                break;
            }
        }

        Err(Failure::VariableNotInScope(
            span.clone(),
            name.to_string().into(),
        ))
    }

    pub fn get_variable_mut(&mut self, name: &S) -> Result<&mut Value<'a, S>, Failure<S>> {
        // TODO we should refuse to provide module level scopes, since those need to be fully immutable.
        let mut scope_iterator = self.scopes[..=self.active_scope].iter_mut().rev();

        for scope in &mut scope_iterator {
            if let Some(value) = scope.variables.get_mut(name.as_str()) {
                return Ok(value);
            }

            // If this scope is isolated, then we should skip to the module scope.
            if matches!(scope.ty, ScopeType::Isolated) {
                if let Some(value) = scope_iterator
                    .find(|scope| matches!(scope.ty, ScopeType::Module))
                    .and_then(|scope| scope.variables.get_mut(name.as_str()))
                {
                    return Ok(value);
                }
                break;
            }
        }

        Err(Failure::VariableNotInScope(
            name.clone(),
            name.to_string().into(),
        ))
    }

    pub fn new_variable(&mut self, name: &S, value: Value<'a, S>) {
        let current_scope = &mut self.scopes[self.active_scope];

        current_scope.variables.insert(name.as_str().into(), value);
    }
}

#[derive(Debug, Default)]
pub struct ModuleScope<'a, S: Span> {
    structs: HashMap<String, &'a parsing::Struct<S>>,
    functions: HashMap<String, &'a parsing::Function<S>>,
    sketches: HashMap<String, &'a parsing::Sketch<S>>,
    widgets: HashMap<String, &'a parsing::Widget<S>>,
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

        let sketches = module
            .root_elements
            .sketches
            .iter()
            .map(|sketch| (sketch.named_block.name.to_string(), sketch))
            .collect();

        let widgets = module
            .root_elements
            .widgets
            .iter()
            .map(|widget| (widget.named_block.name.to_string(), widget))
            .collect();

        Self {
            structs,
            functions,
            sketches,
            widgets,
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

#[derive(Default)]
pub struct ExecutionContext<'a, S: Span> {
    pub log: RuntimeLog<S>,
    pub stack: Stack<'a, S>,
}

impl<'a, S: Span> ExecutionContext<'a, S> {
    pub fn new_scope<R>(&mut self, scope: impl FnOnce(&mut Self) -> R) -> R {
        self.stack.push_scope(ScopeType::Inherited);
        let result = scope(self);
        self.stack.pop_scope();

        result
    }

    pub fn new_isolated_scope<R>(&mut self, scope: impl FnOnce(&mut Self) -> R) -> R {
        self.stack.push_scope(ScopeType::Isolated);
        let result = scope(self);
        self.stack.pop_scope();

        result
    }
}

fn run_block<'a, S: Span>(
    context: &mut ExecutionContext<'a, S>,
    block: &Block<S>,
) -> ExecutionResult<'a, S, Value<'a, S>> {
    let mut result = NoneType.into();

    for statement in block.statements.iter() {
        if let Some(statement) = statement.get() {
            result = statements::run_statement(context, statement)?;
        }
    }

    Ok(result)
}

fn run_named_block<'a, S: Span>(
    context: &mut ExecutionContext<'a, S>,
    block: &NamedBlock<S>,
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
            for (span, (argument, variable)) in spans
                .iter()
                .map(|expression| expression.get_span())
                .chain(std::iter::repeat(default_span))
                .zip(arguments.into_iter().zip(&block.parameters))
            {
                if argument.matches_type(&variable.ty) {
                    context.stack.new_variable(&variable.name, argument);
                } else {
                    failures.push(Failure::ExpectedGot(
                        span.clone(),
                        variable.ty.name(),
                        argument.type_name(),
                    ));
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
            Stack,
        },
        module::Module,
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

        let mut context = ExecutionContext {
            log: Default::default(),
            stack: Stack::new(module_scope),
        };

        let result = run_block(
            &mut context,
            &parsing::Block::parse("{ my_function(5) }").unwrap().1,
        );
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

        let mut context = ExecutionContext {
            log: Default::default(),
            stack: Stack::new(module_scope),
        };

        let result = context.new_scope(|context| {
            run_block(
                context,
                &parsing::Block::parse("{ let value = 0; my_function(5); value }")
                    .unwrap()
                    .1,
            )
        });
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
            "struct MyStruct {} function my_function() -> struct MyStruct { struct MyStruct {} }",
        )
        .unwrap();

        assert!(log.is_empty());

        let module_scope = ModuleScope::new(&module);

        let mut context = ExecutionContext {
            log: Default::default(),
            stack: Stack::new(module_scope),
        };
        let result = context.new_scope(|context| {
            run_block(
                context,
                &parsing::Block::parse("{ my_function() }").unwrap().1,
            )
        });
        assert_eq!(
            result.unwrap(),
            run_expression(
                &mut context,
                &Expression::parse("struct MyStruct {}").unwrap().1
            )
            .unwrap()
        );
    }
}
