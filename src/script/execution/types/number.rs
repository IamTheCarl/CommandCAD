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

use common_data_types::{FloatIsNan, Number};

use crate::script::{execution::Failure, parsing, Span};

use super::{Measurement, OperatorResult, Value};

pub trait UnwrapNotNan: Sized {
    fn unwrap_not_nan<S: Span>(self, span: &S) -> OperatorResult<S, Number>;
}

pub fn unwrap_float<S: Span>(span: S, number: &parsing::Number<S>) -> OperatorResult<S, Number> {
    match number.to_float::<Number>() {
        Ok(number) => Ok(number),
        Err(error) => Err(Failure::NumberConversion(span, error)),
    }
}

impl UnwrapNotNan for std::result::Result<Number, FloatIsNan> {
    fn unwrap_not_nan<S: Span>(self, span: &S) -> OperatorResult<S, Number> {
        match self {
            Ok(number) => Ok(number),
            Err(_float_is_nan) => Err(Failure::ResultIsNan(span.clone())),
        }
    }
}

impl<'a, S: Span> From<Number> for Value<'a, S> {
    fn from(value: Number) -> Self {
        let measurement: Measurement = value.into();
        measurement.into()
    }
}
