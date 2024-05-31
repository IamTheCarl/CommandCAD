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

use common_data_types::{Float, FloatIsNan};

use crate::script::{execution::Failure, parsing, Span};

use super::{OperatorResult, Scalar, Value};

pub trait UnwrapNotNan: Sized {
    fn unwrap_not_nan<S: Span>(self, span: &S) -> OperatorResult<S, Float>;
}

pub fn unwrap_float<S: Span>(span: S, number: &parsing::Number<S>) -> OperatorResult<S, Float> {
    match number.to_float::<Float>() {
        Ok(number) => Ok(number),
        Err(error) => Err(Failure::NumberConversion(span, error)),
    }
}

impl UnwrapNotNan for std::result::Result<Float, FloatIsNan> {
    fn unwrap_not_nan<S: Span>(self, span: &S) -> OperatorResult<S, Float> {
        match self {
            Ok(number) => Ok(number),
            Err(_float_is_nan) => Err(Failure::ResultIsNan(span.clone())),
        }
    }
}

impl<S: Span> From<Float> for Value<S> {
    fn from(value: Float) -> Self {
        let measurement: Scalar = value.into();
        measurement.into()
    }
}
