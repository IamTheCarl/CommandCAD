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

use std::{
    borrow::Cow,
    fmt::Display,
    ops::{Deref, DerefMut},
    path::PathBuf,
    sync::{Arc, Mutex},
};
use tree_sitter::{Point, Range};

use crate::compile::SourceReference;

pub trait RuntimeLog: std::fmt::Debug + Send + Sync {
    fn push_message(&self, message: LogMessage);
}

impl RuntimeLog for Mutex<Vec<LogMessage>> {
    fn push_message(&self, message: LogMessage) {
        self.lock().expect("Log was poisoned").push(message);
    }
}

#[derive(Debug, Clone)]
pub struct StackTrace<'p> {
    parent: Option<&'p StackTrace<'p>>,
    reference: SourceReference,
}

impl<'p> Display for StackTrace<'p> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(parent) = self.parent {
            write!(f, "{parent}")?;
        }

        writeln!(f, "{}", self.reference)
    }
}

impl StackTrace<'static> {
    pub fn top(reference: SourceReference) -> Self {
        Self {
            parent: None,
            reference,
        }
    }

    /// A special stack trace for building the prelude
    pub fn bootstrap() -> Self {
        Self {
            parent: None,
            reference: SourceReference {
                file: Arc::new(PathBuf::from("bootstrap.ccm")),
                range: Range {
                    start_byte: 0,
                    end_byte: 0,
                    start_point: Point { row: 0, column: 0 },
                    end_point: Point { row: 0, column: 0 },
                },
            },
        }
    }

    #[cfg(test)]
    pub(crate) fn test() -> Self {
        Self {
            parent: None,
            reference: SourceReference {
                file: Arc::new(PathBuf::from("test.ccm")),
                range: Range {
                    start_byte: 0,
                    end_byte: 0,
                    start_point: Point { row: 0, column: 0 },
                    end_point: Point { row: 0, column: 0 },
                },
            },
        }
    }
}

impl<'p> StackTrace<'p> {
    pub fn trace_scope<F, R>(&'p self, reference: impl Into<SourceReference>, code: F) -> R
    where
        F: FnOnce(StackTrace<'p>) -> R,
    {
        let scope = Self {
            parent: Some(self),
            reference: reference.into(),
        };

        code(scope)
    }

    pub fn bottom(&self) -> &SourceReference {
        &self.reference
    }

    pub fn iter(&'p self) -> StackTraceIter<'p> {
        StackTraceIter {
            current: Some(self),
        }
    }
}

impl<'p> IntoIterator for &'p StackTrace<'p> {
    type Item = &'p SourceReference;
    type IntoIter = StackTraceIter<'p>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub struct StackTraceIter<'p> {
    current: Option<&'p StackTrace<'p>>,
}

impl<'p> Iterator for StackTraceIter<'p> {
    type Item = &'p SourceReference;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.current.take();
        if let Some(next) = next {
            self.current = next.parent;
        }
        next.map(|next| &next.reference)
    }
}

impl Display for SourceReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}",
            self.file.to_string_lossy(),
            // Most IDEs expect the first line to be 1.
            self.range.start_point.row + 1,
            self.range.start_point.column + 1
        )
    }
}

/// A string that knows what file it came from.
pub struct LocatedString {
    pub location: SourceReference,
    pub string: String,
}

impl Deref for LocatedString {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.string
    }
}

impl DerefMut for LocatedString {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.string
    }
}

impl AsRef<str> for LocatedString {
    fn as_ref(&self) -> &str {
        self.string.as_ref()
    }
}

/// A str that knows what file it came from.
pub struct LocatedStr<'s> {
    pub location: SourceReference,
    pub string: &'s str,
}

impl<'s> Deref for LocatedStr<'s> {
    type Target = &'s str;

    fn deref(&self) -> &Self::Target {
        &self.string
    }
}

impl<'s> From<&'s LocatedString> for LocatedStr<'s> {
    fn from(value: &'s LocatedString) -> Self {
        Self {
            location: value.location.clone(),
            string: &value.string,
        }
    }
}

impl<'s> From<&'s LocatedStr<'s>> for LocatedStr<'s> {
    fn from(value: &'s LocatedStr<'s>) -> Self {
        Self {
            location: value.location.clone(),
            string: &value.string,
        }
    }
}

#[derive(Debug)]
pub struct LogMessage {
    pub origin: SourceReference,
    pub level: LogLevel,
    pub message: Cow<'static, str>,
}

impl std::fmt::Display for LogMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.origin, self.message)
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum LogLevel {
    Info,
    Warning,
}
