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

use super::logging::StackPoint;
use std::{any::Any, fmt::Display, iter::once};

pub type ExpressionResult<R> = std::result::Result<R, Error>;

#[derive(Debug)]
pub struct Error {
    pub ty: Box<dyn ErrorType>,
    pub trace: Vec<StackPoint>,
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

pub trait ErrorType: std::fmt::Debug + std::fmt::Display + Any {}

pub trait Raise {
    fn raise<'s, R>(
        self,
        stack_trace: impl IntoIterator<Item = &'s StackPoint>,
    ) -> ExpressionResult<R>;

    fn raise_with_line<'s, R>(
        self,
        stack_trace: impl IntoIterator<Item = &'s StackPoint>,
        current_line: StackPoint,
    ) -> ExpressionResult<R>;
}

impl<E: ErrorType> Raise for E {
    fn raise<'s, R>(
        self,
        stack_trace: impl IntoIterator<Item = &'s StackPoint>,
    ) -> ExpressionResult<R> {
        Err(Error {
            ty: Box::new(self),
            trace: stack_trace.into_iter().cloned().collect(),
        })
    }

    fn raise_with_line<'s, R>(
        self,
        stack_trace: impl IntoIterator<Item = &'s StackPoint>,
        current_line: StackPoint,
    ) -> ExpressionResult<R> {
        Err(Error {
            ty: Box::new(self),
            trace: stack_trace
                .into_iter()
                .cloned()
                .chain(once(current_line))
                .collect(),
        })
    }
}
