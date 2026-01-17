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

use crate::execution::logging::StackTrace;

use super::{
    errors::{ErrorType, ExpressionResult, Raise},
    logging::LocatedStr,
    values::Value,
};
use imstr::ImString;
use std::{collections::HashMap, fmt::Display};

#[derive(Debug, Clone, Copy)]
pub enum ScopeType {
    Isolated,
    Inherited,
}

#[derive(Debug)]
pub struct StackScope<'p> {
    prelude: &'p HashMap<ImString, Value>,
    parent: Option<&'p StackScope<'p>>,
    ty: ScopeType,
    variables: HashMap<ImString, Value>,
}

impl<'p> StackScope<'p> {
    pub fn top(prelude: &'p HashMap<ImString, Value>) -> Self {
        Self {
            prelude,
            parent: None,
            ty: ScopeType::Isolated,
            variables: HashMap::new(),
        }
    }

    pub fn scope<'s, B, R>(
        &'p self,
        stack_trace: &StackTrace,
        mode: ScopeType,
        variables: HashMap<ImString, Value>,
        block: B,
    ) -> ExpressionResult<R>
    where
        B: FnOnce(&Self, &StackTrace) -> R,
    {
        let scope = Self {
            prelude: self.prelude,
            parent: Some(self),
            ty: mode,
            variables,
        };

        let result = block(&scope, stack_trace);

        Ok(result)
    }

    pub fn scope_mut<'s, B, R>(
        &'p self,
        stack_trace: &StackTrace,
        mode: ScopeType,
        variables: HashMap<ImString, Value>,
        block: B,
    ) -> ExpressionResult<R>
    where
        B: FnOnce(&mut Self, &StackTrace) -> R,
    {
        let mut scope = Self {
            prelude: self.prelude,
            parent: Some(self),
            ty: mode,
            variables,
        };

        let result = block(&mut scope, stack_trace);

        Ok(result)
    }

    pub fn insert_value(&mut self, name: ImString, value: Value) {
        self.variables.insert(name, value);
    }

    fn iter(&'p self) -> StackScopeIter<'p> {
        StackScopeIter {
            current: Some(self),
        }
    }

    /// Gets a reference to a variable on the stack.
    // TODO Recommending similar named variables would help users to notice typos.
    // https://crates.io/crates/levenshtein
    pub fn get_variable<'s, S: Into<LocatedStr<'s>>>(
        &self,
        stack_trace: &StackTrace,
        name: S,
    ) -> ExpressionResult<&Value> {
        let name = name.into();

        let mut scope_iterator = self.iter();
        let mut value = None;

        // Search the stack for the thing.
        for scope in &mut scope_iterator {
            if let Some(local_value) = scope.variables.get(name.string) {
                value = Some(local_value);
                break;
            }

            match &scope.ty {
                // If this scope is isolated, then we should not continue searching up the stack.
                // Skip to the prelude.
                ScopeType::Isolated => {
                    break;
                }
                _ => {}
            }
        }

        if let Some(value) = value {
            Ok(value)
        } else {
            // See if we can find it in the prelude.
            if let Some(value) = self.prelude.get(name.string) {
                return Ok(value);
            }

            // We couldn't find it.
            Err(NotInScopeError {
                variable_name: name.string.to_string(),
            }
            .to_error(stack_trace.iter().chain([&name.location])))
        }
    }
}

pub struct StackScopeIter<'p> {
    current: Option<&'p StackScope<'p>>,
}

impl<'p> Iterator for StackScopeIter<'p> {
    type Item = &'p StackScope<'p>;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.current.take();
        if let Some(next) = next {
            self.current = next.parent;
        }
        next
    }
}

#[derive(Debug, Eq, PartialEq)]
struct NotInScopeError {
    variable_name: String,
}

impl ErrorType for NotInScopeError {}

impl Display for NotInScopeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "`{}` could not be found in the current scope",
            self.variable_name
        )
    }
}
