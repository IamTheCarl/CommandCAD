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

use std::{any::Any, borrow::Cow, fmt::Display};

use ariadne::{Label, Report, ReportKind};

use crate::{compile::SourceReference, StackTrace};

pub type ExecutionResult<R> = std::result::Result<R, Error>;

#[derive(Debug)]
pub struct Error {
    pub ty: Box<dyn ErrorType>,
    pub trace: Vec<SourceReference>,
    pub failure_chain: Vec<Cow<'static, str>>,
}

impl Error {
    pub fn report(&self) -> Report<'_, SourceReference> {
        let bottom = self.find_primary_source();

        let mut builder = Report::build(ReportKind::Error, bottom.clone());
        builder.set_message("Failed to evaluate");
        builder.add_label(Label::new(bottom).with_message(format!("{}", self.ty)));

        builder.with_helps(self.failure_chain.iter());

        builder.finish()
    }
}

impl Error {
    /// Find the most meaningful source reference for error reporting.
    /// Prefers real files with non-zero ranges over synthetic sources (like "solve").
    fn find_primary_source(&self) -> SourceReference {
        // First try to find a trace entry that points to a real file with a meaningful range
        for trace in self.trace.iter().rev() {
            let file_name = trace.file.to_string_lossy();
            let has_range = trace.range.start_byte != trace.range.end_byte;
            // Skip synthetic sources and entries with zero ranges
            if !file_name.starts_with('<')
                && file_name != "solve"
                && file_name != "repl.ccm"
                && has_range
            {
                return trace.clone();
            }
        }
        // Fall back to the first trace entry if no real file with range found
        self.trace.first().expect("Error has no trace").clone()
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.ty)?;
        writeln!(f, "Backtrace:")?;
        for layer in self.trace.iter() {
            writeln!(f, "\t{}", layer)?;
        }

        Ok(())
    }
}

impl std::error::Error for Error {}

/// A generic error that will just display a static message.
#[derive(Debug, Eq, PartialEq)]
pub struct StrError(pub &'static str);

impl std::error::Error for StrError {}

impl Display for StrError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A generic error that will just display a formatted message.
#[derive(Debug, Eq, PartialEq)]
pub struct StringError(pub String);

impl std::error::Error for StringError {}

impl Display for StringError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub trait ErrorType: std::error::Error + Send + Sync + Any {
    fn as_any(&self) -> &dyn Any;
}

impl<E> ErrorType for E
where
    E: std::error::Error + Send + Sync + Any,
{
    fn as_any(&self) -> &dyn Any {
        self as &dyn Any
    }
}

pub trait Raise {
    fn to_error<'s>(self, stack_trace: impl IntoIterator<Item = &'s StackTrace<'s>>) -> Error;
}

impl<E: ErrorType> Raise for E {
    fn to_error<'s>(self, stack_trace: impl IntoIterator<Item = &'s StackTrace<'s>>) -> Error {
        let mut trace = Vec::new();
        let mut failure_chain = Vec::new();

        for layer in stack_trace {
            trace.push(layer.reference.clone());
            if let Some(message) = layer.failure_message.clone() {
                failure_chain.push(message);
            }
        }

        Error {
            ty: Box::new(self),
            trace,
            failure_chain,
        }
    }
}
