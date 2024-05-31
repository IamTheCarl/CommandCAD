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

use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use compact_str::CompactString;

use crate::script::Span;

use super::{
    types::{OperatorResult, StructDefinition, UserFunction, Value},
    Failure, Module,
};

#[derive(Debug)]
pub(super) enum ScopeType {
    Inherited,
    Isolated,
    Module,
    Closure { references: HashSet<CompactString> },
}

impl Default for ScopeType {
    fn default() -> Self {
        Self::Inherited
    }
}

#[derive(Debug)]
struct Scope<S: Span> {
    ty: ScopeType,
    variables: HashMap<CompactString, Value<S>>,
}

impl<S: Span> Default for Scope<S> {
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
                mut scope_iterator: impl Iterator<Item = optionally_mut_ref!($mutable 'b Scope<S>)>,
                name: &str,
            ) -> Option<optionally_mut_ref!($mutable 'b Value<S>)> {
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
pub struct Stack<S: Span> {
    scopes: Vec<Scope<S>>,
    active_scope: usize,
}

impl<S: Span> Default for Stack<S> {
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

impl<S: Span> Stack<S> {
    pub fn new(module: Module<S>) -> Self {
        let mut root_scope = Scope {
            ty: ScopeType::Module,
            ..Default::default()
        };

        let structs = module
            .root_elements
            .structs
            .into_iter()
            .map(|structure| (structure.name.to_string(), structure));
        for (name, definition) in structs {
            root_scope.variables.insert(
                name.into(),
                StructDefinition {
                    definition: Rc::new(definition),
                }
                .into(),
            );
        }

        let functions = module
            .root_elements
            .functions
            .into_iter()
            .map(|function| (function.named_block.name.to_string(), function));
        for (name, function) in functions {
            root_scope
                .variables
                .insert(name.into(), UserFunction::new(function).into());
        }

        let tasks = module
            .root_elements
            .tasks
            .into_iter()
            .map(|task| (task.named_block.name.to_string(), task));
        for (name, task) in tasks {
            root_scope
                .variables
                .insert(name.into(), UserFunction::new(task).into());
        }

        let sketches = module
            .root_elements
            .sketches
            .into_iter()
            .map(|sketch| (sketch.named_block.name.to_string(), sketch));
        for (name, sketch) in sketches {
            root_scope
                .variables
                .insert(name.into(), UserFunction::new(sketch).into());
        }

        let solids = module
            .root_elements
            .solids
            .into_iter()
            .map(|wolid| (wolid.named_block.name.to_string(), wolid));
        for (name, solid) in solids {
            root_scope
                .variables
                .insert(name.into(), UserFunction::new(solid).into());
        }

        Self {
            scopes: vec![root_scope],
            active_scope: 0,
        }
    }

    pub(super) fn push_scope<'a>(
        &mut self,
        variables_to_copy: impl Iterator<Item = &'a S>,
        mode: ScopeType,
    ) -> OperatorResult<S, ()>
    where
        S: 'a,
    {
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

    pub(super) fn pop_scope(&mut self) {
        self.scopes[self.active_scope].variables.clear();
        self.active_scope -= 1;
    }

    // TODO Recommending similar named variables would help users to notice typos.
    pub fn get_variable(&self, name: &S) -> std::result::Result<&Value<S>, Failure<S>> {
        self.get_variable_str(name, name.as_str())
    }

    pub fn get_variable_mut(&mut self, name: &S) -> Result<&mut Value<S>, Failure<S>> {
        self.get_variable_str_mut(name, name.as_str())
    }

    pub fn get_variable_str(&self, span: &S, name: &str) -> Result<&Value<S>, Failure<S>> {
        generate_variable_getter!(self, span, name, iter, get, immutable)
    }
    pub fn get_variable_str_mut(
        &mut self,
        span: &S,
        name: &str,
    ) -> Result<&mut Value<S>, Failure<S>> {
        generate_variable_getter!(self, span, name, iter_mut, get_mut, mutable)
    }

    pub fn new_variable(&mut self, name: &S, value: Value<S>) {
        self.new_variable_str(name.as_str(), value)
    }

    pub fn new_variable_str(&mut self, name: impl Into<CompactString>, value: Value<S>) {
        let current_scope = &mut self.scopes[self.active_scope];

        current_scope.variables.insert(name.into(), value);
    }
}
