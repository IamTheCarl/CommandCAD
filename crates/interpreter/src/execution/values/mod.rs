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

use crate::{compile::SourceReference, execution::stack::Stack};

use super::{
    errors::{ErrorType, ExpressionResult, Raise as _},
    logging::RuntimeLog,
};

mod void;
pub use void::ValueNone;

mod boolean;
pub use boolean::Boolean;

mod integer;
pub use integer::{SignedInteger, UnsignedInteger};

mod scalar;
pub use scalar::Scalar;

mod vector;
pub use vector::{Vector2, Vector3, Vector4};

mod closure;
pub use closure::{BuiltinCallableDatabase, BuiltinFunction, UserClosure};

mod dictionary;
pub use dictionary::Dictionary;

mod list;
pub use list::List;

mod string;
pub use string::IString;

mod value_type;
pub use value_type::{StructDefinition, StructMember, ValueType};

pub trait StaticTypeName {
    /// Provides the type name without having an instance of the object.
    /// This is used for formatting error messages when failing to cast to an expected type.
    fn static_type_name() -> &'static str;
}

pub trait StaticType {
    // Provites the TypeValue without having an instance of the object.
    // This is used for type checking built-in functions and methods.
    // Not all types provide this, and thus, not all types can be used with
    // built in functions.
    fn static_type() -> ValueType;
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
pub trait Object: StaticTypeName + Sized + Eq + PartialEq + Clone {
    fn get_type(&self, callable_database: &BuiltinCallableDatabase) -> ValueType;

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
    fn get_attribute(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        _callable_database: &BuiltinCallableDatabase,
        attribute: &str,
    ) -> ExpressionResult<Value> {
        Err(MissingAttributeError {
            name: attribute.into(),
        }
        .to_error(stack_trace))
    }
    fn call(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &mut Vec<SourceReference>,
        _stack: &mut Stack,
        _callable_database: &BuiltinCallableDatabase,
        _argument: Dictionary,
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

#[enum_dispatch(Object, ObjectCopy)]
#[derive(Debug, Eq, PartialEq, EnumDowncast, EnumAs, Clone)]
pub enum Value {
    ValueNone,
    SignedInteger,
    UnsignedInteger,
    Boolean,
    Scalar,
    UserClosure,
    BuiltinFunction,
    ValueType,
    Dictionary(Dictionary),
    List(List),
    String(IString),
    // Range(Range),
    // Closure(Closure<S>),
    Vector2(Vector2),
    Vector3(Vector3),
    Vector4(Vector4),
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

impl StaticType for Value {
    fn static_type() -> ValueType {
        ValueType::Any
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

impl IntoVariant<Self> for Value {
    fn into_variant(self) -> Result<Self, Self>
    where
        Self: Sized,
    {
        Ok(self)
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
            Self::ValueNone(_) => Ok(None),
            this => Ok(Some(this.downcast::<T>(stack_trace)?)),
        }
    }
}
