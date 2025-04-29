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

use std::{borrow::Cow, cmp::Ordering, fmt::Display};

use enum_dispatch::enum_dispatch;
use enum_downcast::{AsVariant, EnumDowncast, IntoVariant};
use unwrap_enum::EnumAs;

use crate::compile::SourceReference;

use super::{
    errors::{ErrorType, ExpressionResult, Raise as _},
    logging::RuntimeLog,
};

mod void;
pub use void::Void;

mod default;
pub use default::DefaultValue;

mod boolean;
pub use boolean::Boolean;

mod integer;
pub use integer::{SignedInteger, UnsignedInteger};

mod scalar;
pub use scalar::Scalar;

mod closure;
pub use closure::UserClosure;

mod dictionary;
pub use dictionary::Dictionary;

mod value_type;
pub use value_type::{StructDefinition, StructMember, ValueType};

pub trait StaticTypeName {
    /// Provides the type name without having an instance of the object.
    /// This is used for formatting error messages when failing to cast to an expected type.
    fn static_type_name() -> &'static str;
}

#[derive(Debug, Eq, PartialEq)]
struct UnsupportedOperationError {
    pub type_name: Cow<'static, str>,
    pub operation_name: &'static str,
}

impl ErrorType for UnsupportedOperationError {}

impl Display for UnsupportedOperationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Type `{}` does not support {} operation",
            self.type_name, self.operation_name
        )
    }
}

impl UnsupportedOperationError {
    fn raise<O: Object + Sized, R>(
        object: &O,
        stack_trace: &[SourceReference],
        operation_name: &'static str,
    ) -> ExpressionResult<R> {
        Err(Self {
            type_name: object.type_name(),
            operation_name,
        }
        .to_error(stack_trace))
    }
}

#[derive(Debug, Eq, PartialEq)]
struct MissingAttributeError {
    pub name: String,
}

impl ErrorType for MissingAttributeError {}

impl Display for MissingAttributeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Object does not contain the attribute `{}`", self.name)
    }
}

#[enum_dispatch]
pub trait Object: StaticTypeName + Sized + Eq + PartialEq + ObjectCopy {
    fn get_type(&self) -> ValueType;

    // fn format(
    //     &self,
    //     _log: &mut dyn RuntimeLog,
    //     stack_trace: stack_trace: &S[SourceReference],
    //     _f: &mut dyn Write,
    //     _style: Style,
    //     _precision: Option<u8>,
    // ) -> OperatorResult<S, ()> {
    //     UnsupportedOperationError::raise(self, stack_trace, "format")
    // }

    fn type_name(&self) -> Cow<'static, str> {
        Self::static_type_name().into()
    }

    fn and(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        _rhs: Value,
    ) -> ExpressionResult<Value> {
        UnsupportedOperationError::raise(&self, stack_trace, "logical and")
    }
    fn or(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        _rhs: Value,
    ) -> ExpressionResult<Value> {
        UnsupportedOperationError::raise(&self, stack_trace, "logical or")
    }
    fn xor(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        _rhs: Value,
    ) -> ExpressionResult<Value> {
        UnsupportedOperationError::raise(&self, stack_trace, "logical xor")
    }
    fn bit_and(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        _rhs: Value,
    ) -> ExpressionResult<Value> {
        UnsupportedOperationError::raise(&self, stack_trace, "binary and")
    }
    fn bit_or(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        _rhs: Value,
    ) -> ExpressionResult<Value> {
        UnsupportedOperationError::raise(&self, stack_trace, "binary or")
    }
    fn bit_xor(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        _rhs: Value,
    ) -> ExpressionResult<Value> {
        UnsupportedOperationError::raise(&self, stack_trace, "binary xor")
    }
    fn cmp(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        _rhs: Value,
    ) -> ExpressionResult<Ordering> {
        UnsupportedOperationError::raise(&self, stack_trace, "compare")
    }
    fn eq(
        self,
        log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: Value,
    ) -> ExpressionResult<bool> {
        Ok(matches!(self.cmp(log, stack_trace, rhs)?, Ordering::Equal))
    }
    fn addition(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        _rhs: Value,
    ) -> ExpressionResult<Value> {
        UnsupportedOperationError::raise(&self, stack_trace, "addition")
    }
    fn subtraction(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        _rhs: Value,
    ) -> ExpressionResult<Value> {
        UnsupportedOperationError::raise(&self, stack_trace, "subtraction")
    }
    fn multiply(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        _rhs: Value,
    ) -> ExpressionResult<Value> {
        UnsupportedOperationError::raise(&self, stack_trace, "multiply")
    }
    fn divide(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        _rhs: Value,
    ) -> ExpressionResult<Value> {
        UnsupportedOperationError::raise(&self, stack_trace, "divide")
    }
    fn floor_divide(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        _rhs: Value,
    ) -> ExpressionResult<Value> {
        UnsupportedOperationError::raise(&self, stack_trace, "floor_divide")
    }
    fn exponent(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        _rhs: Value,
    ) -> ExpressionResult<Value> {
        UnsupportedOperationError::raise(&self, stack_trace, "exponent")
    }
    fn left_shift(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        _rhs: Value,
    ) -> ExpressionResult<Value> {
        UnsupportedOperationError::raise(&self, stack_trace, "left shift")
    }
    fn right_shift(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        _rhs: Value,
    ) -> ExpressionResult<Value> {
        UnsupportedOperationError::raise(&self, stack_trace, "right shift")
    }
    fn get_attribute_ref(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        attribute: &str,
    ) -> ExpressionResult<&StoredValue> {
        Err(MissingAttributeError {
            name: attribute.into(),
        }
        .to_error(stack_trace))
    }
    fn get_attribute_mut(
        &mut self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        _attribute: &str,
    ) -> ExpressionResult<&mut StoredValue> {
        UnsupportedOperationError::raise(self, stack_trace, "set attribute")
    }
    fn insert_attribute(
        &mut self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        _attribute: impl Into<String>,
        _new_value: Value,
    ) -> ExpressionResult<()> {
        UnsupportedOperationError::raise(self, stack_trace, "insert attribute")
    }
    fn call(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        _argument: Value,
    ) -> ExpressionResult<Value> {
        UnsupportedOperationError::raise(self, stack_trace, "call")
    }
    fn index(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        _index: Value,
    ) -> ExpressionResult<Value> {
        UnsupportedOperationError::raise(self, stack_trace, "index")
    }
    fn iterate(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
    ) -> ExpressionResult<Box<dyn Iterator<Item = Value>>> {
        UnsupportedOperationError::raise(self, stack_trace, "iterate")
    }
    fn unary_plus(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
    ) -> ExpressionResult<Value> {
        UnsupportedOperationError::raise(&self, stack_trace, "unary plus")
    }
    fn unary_minus(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
    ) -> ExpressionResult<Value> {
        UnsupportedOperationError::raise(&self, stack_trace, "unary minus")
    }
    fn unary_not(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
    ) -> ExpressionResult<Value> {
        UnsupportedOperationError::raise(&self, stack_trace, "unary not")
    }

    // fn export(
    //     &self,
    //     _log: &mut dyn RuntimeLog,
    //     stack_trace: &[SourceReference],
    // ) -> OperatorResult<SerializableValue> {
    //     UnsupportedOperationError::raise(self, stack_trace, "export")
    // }
}

/// Implements and indicates the copy rules of an object.
#[enum_dispatch]
pub trait ObjectCopy {
    /// Creates a copy of the object. Returning None indicates that the object should not be
    /// passed by copy, and instead should be moved.
    fn object_copy(&self) -> Option<Value> {
        None
    }
}

impl<O> ObjectCopy for O
where
    O: Object + Copy + Into<Value>,
{
    fn object_copy(&self) -> Option<Value> {
        Some(self.clone().into())
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum StoredValue {
    Value(Value),
    Moved {
        /// Where in the source code it was moved.
        location: SourceReference,
    },
}

impl StoredValue {
    pub fn replace(&mut self, new: Value) {
        *self = Self::Value(new);
    }

    pub fn take(&mut self, stack_trace: &[SourceReference]) -> ExpressionResult<Value> {
        if let Self::Value(value) = &self {
            if let Some(value) = value.object_copy() {
                // No need to move if it's a copy type.
                return Ok(value);
            }
        }

        let mut old_self = Self::Moved {
            location: stack_trace.last().expect("Stack trace was empty").clone(),
        };
        std::mem::swap(self, &mut old_self);

        match old_self {
            StoredValue::Value(value) => Ok(value),
            StoredValue::Moved { location } => {
                Err(ValueMovedError { location }.to_error(stack_trace))
            }
        }
    }

    pub fn access(&self, stack_trace: &[SourceReference]) -> ExpressionResult<&Value> {
        match self {
            StoredValue::Value(value) => Ok(value),
            StoredValue::Moved { location } => Err(ValueMovedError {
                location: location.clone(),
            }
            .to_error(stack_trace)),
        }
    }

    pub fn access_mut(&mut self, stack_trace: &[SourceReference]) -> ExpressionResult<&mut Value> {
        match self {
            StoredValue::Value(value) => Ok(value),
            StoredValue::Moved { location } => Err(ValueMovedError {
                location: location.clone(),
            }
            .to_error(stack_trace)),
        }
    }
}

impl std::hash::Hash for StoredValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // We only contribute to the hash if we actually contain a value.
        if let Self::Value(value) = self {
            core::mem::discriminant(value).hash(state);
        }
    }
}

impl<V> From<V> for StoredValue
where
    V: Into<Value>,
{
    fn from(value: V) -> Self {
        Self::Value(value.into())
    }
}

#[derive(Debug, Eq, PartialEq)]
struct ValueMovedError {
    location: SourceReference,
}

impl ErrorType for ValueMovedError {}

impl Display for ValueMovedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "value has was moved at {}", self.location)
    }
}

#[enum_dispatch(Object, ObjectCopy)]
#[derive(Debug, Eq, PartialEq, EnumDowncast, EnumAs)]
pub enum Value {
    Void,
    Default(DefaultValue),
    SignedInteger,
    UnsignedInteger,
    Boolean,
    Scalar,
    UserClosure,
    ValueType,
    Dictionary(Dictionary),
    // List(List<S>),
    // String(SString),
    // Range(Range),
    // Closure(Closure<S>),
    // Vector2(Vector2),
    // Vector3(Vector3),
    // Vector4(Vector4),
    // Transform2D,
    // Transform3D,
    // Quaternion,
    // Cycle,
    // Region,
    // Sketch,
    // Surface,
    // Solid,
    // Shell,
    // Face,
    // Curve,
    // HalfEdge,
    // Vertex,
}

impl StaticTypeName for Value {
    fn static_type_name() -> &'static str {
        "Value"
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct DowncastError {
    pub expected: Cow<'static, str>,
    pub got: Cow<'static, str>,
}

impl ErrorType for DowncastError {}

impl Display for DowncastError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Expected type `{}`, got {}", self.expected, self.got)
    }
}

impl Value {
    pub fn downcast_ref<T>(&self, stack_trace: &[SourceReference]) -> ExpressionResult<&T>
    where
        T: StaticTypeName,
        Self: AsVariant<T>,
    {
        if let Some(value) = self.enum_downcast_ref() {
            Ok(value)
        } else {
            Err(DowncastError {
                expected: T::static_type_name().into(),
                got: self.type_name(),
            }
            .to_error(stack_trace))
        }
    }

    pub fn downcast<T>(self, stack_trace: &[SourceReference]) -> ExpressionResult<T>
    where
        T: StaticTypeName,
        Self: IntoVariant<T>,
    {
        match self.into_variant() {
            Ok(value) => Ok(value),
            Err(original) => Err(DowncastError {
                expected: T::static_type_name().into(),
                got: original.type_name(),
            }
            .to_error(stack_trace)),
        }
    }

    pub fn downcast_optional<T>(
        self,
        stack_trace: &[SourceReference],
    ) -> ExpressionResult<Option<T>>
    where
        T: StaticTypeName,
        Self: IntoVariant<T>,
    {
        match self {
            Self::Void(_) => Ok(None),
            this => Ok(Some(this.downcast::<T>(stack_trace)?)),
        }
    }
}
