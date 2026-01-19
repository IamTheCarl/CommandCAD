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
use levenshtein::levenshtein;
use std::{cmp::Ordering, collections::HashMap, fmt::Display};

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

    fn iter_visible_scopes(&'p self) -> StackScopeIter<'p> {
        StackScopeIter {
            only_visible: true,
            next: Some(self),
        }
    }

    pub fn iter_visible_variables(&'p self) -> impl Iterator<Item = (&'p ImString, &'p Value)> {
        self.iter_visible_scopes()
            .flat_map(|scope| scope.variables.iter())
            .chain(self.prelude.iter())
    }

    pub fn suggest_similar_names(
        &self,
        local_variables: impl IntoIterator<Item = ImString>,
        name: &str,
    ) -> Vec<ImString> {
        let mut names: Vec<_> = self
            .iter_visible_variables()
            .map(|(name, _value)| name.clone())
            .chain(local_variables.into_iter())
            .collect();
        names.sort_by(|name_a, name_b| {
            match (name_a.starts_with(name), name_b.starts_with(name)) {
                (true, false) => Ordering::Less,
                _ => match levenshtein(name_a, name).cmp(&levenshtein(name_b, name)) {
                    Ordering::Equal => name_b.cmp(name_a), // They're equel, so just alphabetize
                    // them to insure consistent test
                    // results.
                    ord => ord,
                },
            }
        });

        names
    }

    /// Gets a reference to a variable on the stack.
    // TODO Recommending similar named variables would help users to notice typos.
    // https://crates.io/crates/levenshtein
    pub fn get_variable<'s, S: Into<LocatedStr<'s>>>(
        &self,
        stack_trace: &StackTrace,
        local_variables: impl IntoIterator<Item = ImString>,
        name: S,
    ) -> ExpressionResult<&Value> {
        let name = name.into();

        let mut scope_iterator = self.iter_visible_scopes();
        let mut value = None;

        // Search the stack for the thing.
        for scope in &mut scope_iterator {
            if let Some(local_value) = scope.variables.get(name.string) {
                value = Some(local_value);
                break;
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
                variable_name: ImString::from(name.string),
                suggestions: self.suggest_similar_names(local_variables, name.string),
            }
            .to_error(stack_trace.iter().chain([&name.location])))
        }
    }
}

pub struct StackScopeIter<'p> {
    only_visible: bool,
    next: Option<&'p StackScope<'p>>,
}

impl<'p> Iterator for StackScopeIter<'p> {
    type Item = &'p StackScope<'p>;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.next.take();
        if let Some(current) = current {
            if !self.only_visible || matches!(current.ty, ScopeType::Inherited) {
                self.next = current.parent;
            } else {
                self.next = None;
            }
        }
        current
    }
}

#[derive(Debug, Eq, PartialEq)]
struct NotInScopeError {
    variable_name: ImString,
    suggestions: Vec<ImString>,
}

impl ErrorType for NotInScopeError {}

impl Display for NotInScopeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "`{}` could not be found in the current scope. Possible alternatives: ",
            self.variable_name
        )?;

        let mut names = self.suggestions.iter().take(3).peekable();

        while let Some(name) = names.next() {
            if names.peek().is_some() {
                write!(f, "{name}, ")?;
            } else {
                write!(f, "{name}")?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;

    use crate::values::{UnsignedInteger, ValueNone};

    use super::*;

    #[test]
    fn iter_variables() {
        let stack_trace = StackTrace::test();
        let prelude = HashMap::from_iter([("a".into(), UnsignedInteger::from(1).into())]);
        let stack = StackScope::top(&prelude);

        stack
            .scope(
                &stack_trace,
                ScopeType::Inherited,
                HashMap::from_iter([("b".into(), UnsignedInteger::from(2).into())]),
                |stack, stack_trace| {
                    stack
                        .scope(
                            &stack_trace,
                            ScopeType::Inherited,
                            HashMap::from_iter([("c".into(), UnsignedInteger::from(3).into())]),
                            |stack, _stack_trace| {
                                let mut variables =
                                    stack.iter_visible_variables().collect::<Vec<_>>();
                                variables.sort_by(|(name_a, _value_a), (name_b, _value_b)| {
                                    name_a.cmp(name_b)
                                });
                                assert_eq!(
                                    variables,
                                    vec![
                                        (&"a".into(), &UnsignedInteger::from(1).into()),
                                        (&"b".into(), &UnsignedInteger::from(2).into()),
                                        (&"c".into(), &UnsignedInteger::from(3).into())
                                    ]
                                );
                            },
                        )
                        .unwrap();
                },
            )
            .unwrap();
    }

    #[test]
    fn iter_isolated_variables() {
        let stack_trace = StackTrace::test();
        let prelude = HashMap::from_iter([("a".into(), UnsignedInteger::from(1).into())]);
        let stack = StackScope::top(&prelude);

        stack
            .scope(
                &stack_trace,
                ScopeType::Inherited,
                HashMap::from_iter([("b".into(), UnsignedInteger::from(2).into())]),
                |stack, stack_trace| {
                    stack
                        .scope(
                            &stack_trace,
                            ScopeType::Isolated,
                            HashMap::from_iter([("c".into(), UnsignedInteger::from(3).into())]),
                            |stack, _stack_trace| {
                                let mut variables =
                                    stack.iter_visible_variables().collect::<Vec<_>>();
                                variables.sort_by(|(name_a, _value_a), (name_b, _value_b)| {
                                    name_a.cmp(name_b)
                                });

                                assert_eq!(
                                    variables,
                                    vec![
                                        (&"a".into(), &UnsignedInteger::from(1).into()),
                                        (&"c".into(), &UnsignedInteger::from(3).into())
                                    ]
                                );
                            },
                        )
                        .unwrap();
                },
            )
            .unwrap();
    }

    #[test]
    fn suggest_similar_names() {
        let prelude = HashMap::from_iter([
            ("abc".into(), ValueNone.into()),
            ("abcdef".into(), ValueNone.into()),
            ("123".into(), ValueNone.into()),
            ("12345".into(), ValueNone.into()),
        ]);
        let stack = StackScope::top(&prelude);

        assert_eq!(
            stack.suggest_similar_names([], "abc"),
            vec![
                ImString::from("abc"),
                ImString::from("abcdef"),
                ImString::from("123"),
                ImString::from("12345")
            ]
        );

        assert_eq!(
            stack.suggest_similar_names([], "abcde"),
            vec![
                ImString::from("abcdef"),
                ImString::from("abc"),
                ImString::from("12345"),
                ImString::from("123"),
            ]
        );

        assert_eq!(
            stack.suggest_similar_names([], "123"),
            vec![
                ImString::from("123"),
                ImString::from("12345"),
                ImString::from("abc"),
                ImString::from("abcdef")
            ]
        );
    }
}
