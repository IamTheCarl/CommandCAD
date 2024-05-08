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

use crate::script::{execution::ExecutionContext, Span};

// TODO I want a box type that can be a square or a rectangle.
mod circle;
pub mod curve;
pub mod cycle;
pub mod face;
pub mod half_edge;
pub mod object_set;
mod polygon;
pub mod region;
pub mod shell;
pub mod sketch;
pub mod solid;
pub mod surface;
pub mod vertex;

pub fn register_globals<S: Span>(context: &mut ExecutionContext<'_, S>) {
    circle::register_globals(context);
    polygon::register_globals(context);

    cycle::register_globals(context);
    face::register_globals(context);
    region::register_globals(context);
    shell::register_globals(context);
    sketch::register_globals(context);
    solid::register_globals(context);
    surface::register_globals(context);
    curve::register_globals(context);
    half_edge::register_globals(context);
    vertex::register_globals(context);
}

macro_rules! handle_wrapper {
    ($name:ident, $handle:ident) => {
        impl<S: Span> From<Handle<$handle>> for crate::script::execution::types::Value<S> {
            fn from(handle: Handle<$handle>) -> Self {
                $name::from(handle).into()
            }
        }

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                #[allow(unused)]
                use std::ops::Deref;
                f.debug_struct(stringify!($name))
                    .field("id", &self.handle.id())
                    .field("content", &self.handle.deref())
                    .finish()
            }
        }

        impl<S: Span> TryFrom<crate::script::execution::types::Value<S>> for Handle<$handle> {
            type Error = crate::script::execution::types::Value<S>;

            fn try_from(
                value: crate::script::execution::types::Value<S>,
            ) -> Result<Self, Self::Error> {
                use enum_downcast::EnumDowncast;
                let value = value.enum_downcast::<$name>()?;
                Ok(value.handle.into())
            }
        }

        impl From<Handle<$handle>> for $name {
            fn from(handle: Handle<$handle>) -> Self {
                Self {
                    handle: handle.into(),
                }
            }
        }

        impl From<$name> for Handle<$handle> {
            fn from(val: $name) -> Self {
                val.handle.into()
            }
        }

        impl crate::script::execution::types::TypedObject for $name {
            fn get_type<S: Span>() -> VariableType<S> {
                VariableType::$name
            }
        }

        impl crate::script::execution::types::NamedObject for $name {
            fn static_type_name() -> &'static str {
                stringify!($name)
            }
        }

        impl PartialEq for $name {
            fn eq(&self, other: &Self) -> bool {
                self.handle == other.handle
            }
        }
    };
}

pub(crate) use handle_wrapper;
