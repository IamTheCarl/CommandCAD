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

use std::{any::Any, fmt::Display};

use ariadne::{Label, Report, ReportKind};

use crate::compile::SourceReference;

pub type ExecutionResult<R> = std::result::Result<R, Error>;

#[derive(Debug)]
pub struct Error {
    pub ty: Box<dyn ErrorType>,
    pub trace: Vec<SourceReference>,
}

impl Error {
    pub fn report(&self) -> Report<'_, SourceReference> {
        let bottom = self.trace.first().expect("Error has no trace").clone();

        let mut builder = Report::build(ReportKind::Error, bottom.clone());
        builder.set_message("Failed to evaluate");
        builder.add_label(Label::new(bottom).with_message(format!("{}", self.ty)));

        builder.finish()
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

pub trait ErrorType: std::error::Error + Send + Sync + Any {}

impl<E> ErrorType for E where E: std::error::Error + Send + Sync + 'static {}

pub trait Raise {
    fn to_error<'s>(self, stack_trace: impl IntoIterator<Item = &'s SourceReference>) -> Error;
}

impl<E: ErrorType> Raise for E {
    fn to_error<'s>(self, stack_trace: impl IntoIterator<Item = &'s SourceReference>) -> Error {
        Error {
            ty: Box::new(self),
            trace: stack_trace.into_iter().cloned().collect(),
        }
    }
}
