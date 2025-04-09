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
use std::{borrow::Cow, fmt::Display, sync::Arc};

use common_data_types::Dimension;

use super::{
    closure::Signature as ClosureSignature, Boolean, DefaultValue, SignedInteger, StaticTypeName,
    UnsignedInteger, Void,
};

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum ValueType {
    Void,
    Default,
    Boolean,
    SignedInteger,
    UnsignedInteger,
    Scalar(Dimension),
    Closure(Arc<ClosureSignature>),
}

impl ValueType {
    pub fn name(&self) -> Cow<'static, str> {
        match self {
            Self::Void => Void::static_type_name().into(),
            Self::Default => DefaultValue::static_type_name().into(),
            Self::Boolean => Boolean::static_type_name().into(),
            Self::SignedInteger => SignedInteger::static_type_name().into(),
            Self::UnsignedInteger => UnsignedInteger::static_type_name().into(),
            Self::Scalar(dimension) => units::get_dimension_name(dimension).into(),
            Self::Closure(signature) => format!("{}", signature).into(),
        }
    }
}

impl Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // We can avoid a copy operation if we write directly into the formatter.
            Self::Closure(signature) => write!(f, "{}", signature),
            _ => write!(f, "{}", self.name()),
        }
    }
}
