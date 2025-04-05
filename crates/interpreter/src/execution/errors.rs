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
use std::{any::Any, fmt::Display};

pub type OperatorResult<R> = std::result::Result<R, Error>;

#[derive(Debug, Eq)]
pub struct Error {
    pub ty: Box<dyn ErrorType>,
    pub trace: Vec<StackPoint>,
}

impl PartialEq for Error {
    fn eq(&self, other: &Self) -> bool {
        self.ty.as_ref() == other.ty.as_ref() && self.trace == other.trace
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

pub trait ErrorType: std::fmt::Debug + std::fmt::Display + Any {}

pub trait Raise {
    fn raise<R>(self, stack_trace: &[StackPoint]) -> OperatorResult<R>;
}

impl<E: ErrorType> Raise for E {
    fn raise<R>(self, stack_trace: &[StackPoint]) -> OperatorResult<R> {
        Err(Error {
            ty: Box::new(self),
            trace: stack_trace.into(),
        })
    }
}

trait AutoAny {
    fn as_any(&self) -> &dyn Any;
}

impl<A: Any> AutoAny for A {
    fn as_any(&self) -> &dyn Any {
        self as &dyn Any
    }
}

trait DynEq {
    fn dyn_eq(&self, other: &dyn AutoAny) -> bool;
}

impl<D: PartialEq + AutoAny + 'static> DynEq for D {
    fn dyn_eq(&self, other: &dyn AutoAny) -> bool {
        if let Some(other) = other.as_any().downcast_ref::<D>() {
            todo!()
        } else {
            false
        }
    }
}

impl PartialEq for dyn ErrorType {
    fn eq(&self, other: &Self) -> bool {
        todo!()
    }
}

impl Eq for dyn ErrorType {}
