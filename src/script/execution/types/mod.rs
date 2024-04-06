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

use std::{borrow::Cow, cmp::Ordering, fmt::Write};

use enum_dispatch::enum_dispatch;
use enum_downcast::{AsVariant, EnumDowncast};

use crate::script::{
    logging::RuntimeLog,
    parsing::{Expression, Litteral, VariableType},
    Span,
};

use super::{ExecutionContext, Failure};

mod none;
pub use none::NoneType;

mod default;
pub use default::DefaultValue;

mod boolean;
pub use boolean::Boolean;

mod number;

pub mod function;
pub use function::{BuiltinFunction, UserFunction};

mod structures;
pub use structures::{StructDefinition, Structure};

mod list;
pub use list::List;

mod string;
pub use string::{
    formatting::{Style, UnwrapFormattingResult},
    SString,
};

mod math;
use self::function::BuiltinFunctionRef;
pub use self::math::{
    print_all_supported_units, Quaternion, Scalar, Transform2D, Transform3D, Vector2, Vector3,
    Vector4,
};

mod range;
pub use range::Range;

mod closure;
pub use closure::Closure;

mod serializable;
pub use serializable::SerializableValue;

mod modeling;
pub use modeling::{
    curve::Curve, cycle::Cycle, face::Face, half_edge::HalfEdge, region::Region, shell::Shell,
    sketch::Sketch, solid::Solid, surface::Surface, vertex::Vertex,
};

pub fn register_globals<S: Span>(context: &mut ExecutionContext<'_, S>) {
    math::register_globals(context);
    modeling::register_globals(context);
}

// TODO this should probably be moved to failure_message.rs
pub type OperatorResult<S, R> = std::result::Result<R, Failure<S>>;

fn unsupported_operation_message<S: Span, R, O: Object<S> + ?Sized>(
    object: &O,
    span: &S,
    operation_name: &'static str,
) -> OperatorResult<S, R> {
    Err(Failure::UnsupportedOperation(
        span.clone(),
        object.type_name(),
        operation_name,
    ))
}

pub trait NamedObject {
    fn static_type_name() -> &'static str;
}

#[enum_dispatch]
pub trait Object<S: Span>: NamedObject {
    fn matches_type(
        &self,
        ty: &VariableType<S>,
        log: &mut dyn RuntimeLog<S>,
        variable_name_span: &S,
    ) -> OperatorResult<S, bool>;

    fn format(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        _f: &mut dyn Write,
        _style: Style,
        _precision: Option<u8>,
    ) -> OperatorResult<S, ()> {
        unsupported_operation_message(self, span, "format")
    }

    fn type_name(&self) -> Cow<'static, str> {
        Self::static_type_name().into()
    }

    fn and(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        _rhs: &Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        unsupported_operation_message(self, span, "and")
    }
    fn or(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        _rhs: &Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        unsupported_operation_message(self, span, "or")
    }
    fn cmp(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        _rhs: &Value<S>,
    ) -> OperatorResult<S, Ordering> {
        unsupported_operation_message(self, span, "compare")
    }
    fn eq(&self, log: &mut dyn RuntimeLog<S>, span: &S, rhs: &Value<S>) -> OperatorResult<S, bool> {
        Ok(matches!(self.cmp(log, span, rhs)?, Ordering::Equal))
    }
    fn addition(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        _rhs: &Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        unsupported_operation_message(self, span, "addition")
    }
    fn subtraction(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        _rhs: &Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        unsupported_operation_message(self, span, "subtraction")
    }
    fn multiply(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        _rhs: &Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        unsupported_operation_message(self, span, "multiply")
    }
    fn divide(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        _rhs: &Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        unsupported_operation_message(self, span, "divide")
    }
    fn attribute(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        _span: &S,
        attribute: &S,
    ) -> OperatorResult<S, Value<S>> {
        Err(Failure::UnknownAttribute(attribute.clone()))
    }
    fn call(
        &self,
        _context: &mut ExecutionContext<S>,
        span: &S,
        _arguments: Vec<Value<S>>,
        _spans: &[Expression<S>],
    ) -> OperatorResult<S, Value<S>> {
        unsupported_operation_message(self, span, "call")
    }
    fn method_call(
        &self,
        _context: &mut ExecutionContext<S>,
        _span: &S,
        attribute: &S,
        _arguments: Vec<Value<S>>,
        _spans: &[Expression<S>],
    ) -> OperatorResult<S, Value<S>> {
        Err(Failure::UnknownAttribute(attribute.clone()))
    }
    fn index(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        _index: Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        unsupported_operation_message(self, span, "index")
    }
    fn iterate(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
    ) -> OperatorResult<S, Box<dyn Iterator<Item = Value<S>>>> {
        unsupported_operation_message(self, span, "iterate")
    }
    fn unary_plus(&self, _log: &mut dyn RuntimeLog<S>, span: &S) -> OperatorResult<S, Value<S>> {
        unsupported_operation_message(self, span, "unary plus")
    }
    fn unary_minus(&self, _log: &mut dyn RuntimeLog<S>, span: &S) -> OperatorResult<S, Value<S>> {
        unsupported_operation_message(self, span, "unary minus")
    }
    fn unary_logical_not(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
    ) -> OperatorResult<S, Value<S>> {
        unsupported_operation_message(self, span, "unary logical not")
    }

    fn export(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
    ) -> OperatorResult<S, SerializableValue> {
        unsupported_operation_message(self, span, "export")
    }
}

trait UnwrapBorrowFailure<T> {
    fn unwrap_borrow_failure<S: Span>(self, span: &S) -> Result<T, Failure<S>>;
}

impl<T> UnwrapBorrowFailure<T> for Result<T, std::cell::BorrowError> {
    fn unwrap_borrow_failure<S: Span>(self, span: &S) -> Result<T, Failure<S>> {
        self.map_err(|_error| Failure::CannotBorrowImmutably(span.clone(), span.clone()))
    }
}

impl<T> UnwrapBorrowFailure<T> for Result<T, std::cell::BorrowMutError> {
    fn unwrap_borrow_failure<S: Span>(self, span: &S) -> Result<T, Failure<S>> {
        self.map_err(|_error| Failure::CannotBorrowMutably(span.clone(), span.clone()))
    }
}

#[enum_dispatch(Object<S>, ObjectClone<S>, FormatArgument)]
#[derive(Debug, PartialEq, EnumDowncast, Clone)]
pub enum Value<S: Span> {
    NoneType,
    Default(DefaultValue),
    Boolean,
    BuiltinFunction(BuiltinFunctionRef<S>),
    UserFunction(UserFunction<S>),
    Structure(Structure<S>),
    StructDefinition(StructDefinition<S>),
    List(List<S>),
    String(SString),
    Range(Range),
    Closure(Closure<S>),
    Scalar,
    Vector2(Vector2),
    Vector3(Vector3),
    Vector4(Vector4),
    Transform2D,
    Transform3D,
    Quaternion,
    Cycle,
    Region,
    Sketch,
    Surface,
    Solid,
    Shell,
    Face,
    Curve,
    HalfEdge,
    Vertex,
}

impl<S: Span> NamedObject for Value<S> {
    fn static_type_name() -> &'static str {
        "Value"
    }
}

impl<S: Span> Value<S> {
    pub fn downcast_ref<T>(&self, span: &S) -> OperatorResult<S, &T>
    where
        T: NamedObject,
        Self: AsVariant<T>,
    {
        if let Some(value) = self.enum_downcast_ref() {
            Ok(value)
        } else {
            Err(Failure::ExpectedGot(
                span.clone(),
                T::static_type_name().into(),
                self.type_name(),
            ))
        }
    }

    pub fn downcast<T>(self, span: &S) -> OperatorResult<S, T>
    where
        T: NamedObject,
        Self: TryInto<T>,
    {
        let type_name = self.type_name();

        match self.try_into() {
            Ok(value) => Ok(value),
            Err(_original) => Err(Failure::ExpectedGot(
                span.clone(),
                T::static_type_name().into(),
                type_name,
            )),
        }
    }

    pub fn downcast_optional<T>(self, span: &S) -> OperatorResult<S, Option<T>>
    where
        T: NamedObject,
        Self: TryInto<T>,
    {
        match self {
            Self::NoneType(_) => Ok(None),
            this => Ok(Some(this.downcast::<T>(span)?)),
        }
    }

    pub fn from_litteral(
        context: &mut ExecutionContext<S>,
        value: &Litteral<S>,
    ) -> OperatorResult<S, Self> {
        match value {
            Litteral::Scalar(scalar) => Scalar::from_parsed(scalar),
            Litteral::String(string) => SString::from_parsed(string),
            Litteral::List(list) => List::from_parsed(context, list),
            Litteral::Boolean(_span, value) => Ok(Self::Boolean(*value)),
            Litteral::Default(_span) => Ok(DefaultValue.into()),
            Litteral::Closure(closure) => Ok(Closure::from(closure).into()),
        }
    }
}
