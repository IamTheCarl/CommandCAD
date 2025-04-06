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

use crate::compile::SourceReference;

use super::{
    errors::{ErrorType, ExpressionResult, Raise},
    logging::LocatedStr,
    values::Value,
};
use compact_str::CompactString;
use std::{collections::HashMap, fmt::Display};

#[derive(Debug, Clone, Copy)]
pub enum ScopeType {
    Isolated,
    Inherited,
}

struct Scope {
    ty: ScopeType,
    variables: HashMap<CompactString, Value>,
}

pub struct Stack {
    scopes: Vec<Scope>,
    active_scope: usize,
}

macro_rules! generate_variable_getter {
    ($self:ident, $stack_trace: ident, $name:ident, $iter:ident, $get:ident, $mutable:ident) => {{
        let mut scope_iterator = $self.scopes[..=$self.active_scope].$iter().rev();

        for scope in &mut scope_iterator {
            if let Some(value) = scope.variables.$get($name.string) {
                return Ok(value);
            }

            match &scope.ty {
                // If this scope is isolated, then we should not continue searching up the stack.
                ScopeType::Isolated => {
                    break;
                }
                _ => {}
            }
        }

        NotInScopeError {
            variable_name: $name.string.to_string(),
        }
        .raise_with_line($stack_trace, $name.location.clone())
    }};
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

impl Stack {
    pub fn push_scope<'s>(
        &mut self,
        variables_to_copy: impl Iterator<Item = LocatedStr<'s>>,
        stack_trace: &[SourceReference],
        mode: ScopeType,
    ) -> ExpressionResult<()> {
        let next_scope_index = self.active_scope + 1;
        if next_scope_index >= self.scopes.len() {
            self.scopes.push(Scope {
                variables: HashMap::new(),
                ty: mode,
            });
        }

        self.scopes[next_scope_index].ty = mode;

        for variable in variables_to_copy {
            let value = self.get_variable(stack_trace, &variable)?.clone();
            self.scopes[next_scope_index]
                .variables
                .insert(variable.string.into(), value);
        }

        self.active_scope = next_scope_index;

        Ok(())
    }

    pub(super) fn pop_scope(&mut self) {
        self.scopes[self.active_scope].variables.clear();
        self.active_scope -= 1;
    }

    // TODO Recommending similar named variables would help users to notice typos.
    // https://crates.io/crates/levenshtein
    pub fn get_variable<'s, S: Into<LocatedStr<'s>>>(
        &self,
        stack_trace: &[SourceReference],
        name: S,
    ) -> ExpressionResult<&Value> {
        let name = name.into();
        generate_variable_getter!(self, stack_trace, name, iter, get, immutable)
    }

    pub fn get_variable_mut<'s, S: Into<LocatedStr<'s>>>(
        &mut self,
        stack_trace: &[SourceReference],
        name: S,
    ) -> ExpressionResult<&mut Value> {
        let name = name.into();
        generate_variable_getter!(self, stack_trace, name, iter_mut, get_mut, mutable)
    }
}
