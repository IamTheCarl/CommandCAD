use std::{borrow::Cow, cmp::Ordering, fmt::Display};

use enum_dispatch::enum_dispatch;
use enum_downcast::{AsVariant, EnumDowncast};
use value_type::VariableType;

use super::{
    errors::{ErrorType, OperatorResult, Raise as _},
    logging::{RuntimeLog, StackPoint},
};

mod none;
use none::NoneType;

mod default;
use default::DefaultValue;

mod value_type;

pub trait StaticTypeName {
    /// Provides the type name without having an instance of the object.
    /// This is used for formatting error messages when failing to cast to an expected type.
    fn static_type_name() -> &'static str;
}

#[derive(Debug, Eq, PartialEq)]
struct UnsupportedOperationError {
    type_name: Cow<'static, str>,
    operation_name: &'static str,
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
        stack_trace: &[StackPoint],
        operation_name: &'static str,
    ) -> OperatorResult<R> {
        Self {
            type_name: object.type_name(),
            operation_name,
        }
        .raise(stack_trace)
    }
}

#[enum_dispatch]
pub trait Object: StaticTypeName + Sized + std::hash::Hash + Eq + PartialEq {
    fn matches_type(
        &self,
        ty: &VariableType,
        log: &mut dyn RuntimeLog,
        stack_trace: &[StackPoint],
    ) -> OperatorResult<bool>;

    // fn format(
    //     &self,
    //     _log: &mut dyn RuntimeLog,
    //     stack_trace: stack_trace: &S[StackPoint],
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
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[StackPoint],
        _rhs: &Value,
    ) -> OperatorResult<Value> {
        UnsupportedOperationError::raise(self, stack_trace, "and")
    }
    fn or(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[StackPoint],
        _rhs: Value,
    ) -> OperatorResult<Value> {
        UnsupportedOperationError::raise(self, stack_trace, "or")
    }
    fn cmp(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[StackPoint],
        _rhs: &Value,
    ) -> OperatorResult<Ordering> {
        UnsupportedOperationError::raise(self, stack_trace, "compare")
    }
    fn eq(
        &self,
        log: &mut dyn RuntimeLog,
        stack_trace: &[StackPoint],
        rhs: &Value,
    ) -> OperatorResult<bool> {
        Ok(matches!(self.cmp(log, stack_trace, rhs)?, Ordering::Equal))
    }
    fn addition(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[StackPoint],
        _rhs: &Value,
    ) -> OperatorResult<Value> {
        UnsupportedOperationError::raise(self, stack_trace, "addition")
    }
    fn subtraction(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[StackPoint],
        _rhs: &Value,
    ) -> OperatorResult<Value> {
        UnsupportedOperationError::raise(self, stack_trace, "subtraction")
    }
    fn multiply(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[StackPoint],
        _rhs: &Value,
    ) -> OperatorResult<Value> {
        UnsupportedOperationError::raise(self, stack_trace, "multiply")
    }
    fn divide(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[StackPoint],
        _rhs: &Value,
    ) -> OperatorResult<Value> {
        UnsupportedOperationError::raise(self, stack_trace, "divide")
    }
    // fn attribute(
    //     &self,
    //     _log: &mut dyn RuntimeLog,
    //     _stack_trace: &[StackPoint],
    //     attribute: &S,
    // ) -> OperatorResult<S, Value> {
    //     Err(Failure::UnknownAttribute(attribute.clone()))
    // }
    // fn call(
    //     &self,
    //     _context: &mut ExecutionContext,
    //     stack_trace: &[StackPoint],
    //     _arguments: Vec<Value>,
    //     _stack_traces: &[Expression],
    // ) -> OperatorResult<Value> {
    //     UnsupportedOperationError::raise(self, stack_trace, "call")
    // }
    // fn method_call(
    //     &self,
    //     _context: &mut ExecutionContext,
    //     _stack_trace: &[StackPoint],
    //     attribute: &S,
    //     _arguments: Vec<Value>,
    //     _stack_traces: &[Expression],
    // ) -> OperatorResult<Value> {
    //     Err(Failure::UnknownAttribute(attribute.clone()))
    // }
    fn index(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[StackPoint],
        _index: Value,
    ) -> OperatorResult<Value> {
        UnsupportedOperationError::raise(self, stack_trace, "index")
    }
    fn iterate(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[StackPoint],
    ) -> OperatorResult<Box<dyn Iterator<Item = Value>>> {
        UnsupportedOperationError::raise(self, stack_trace, "iterate")
    }
    fn unary_plus(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[StackPoint],
    ) -> OperatorResult<Value> {
        UnsupportedOperationError::raise(self, stack_trace, "unary plus")
    }
    fn unary_minus(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[StackPoint],
    ) -> OperatorResult<Value> {
        UnsupportedOperationError::raise(self, stack_trace, "unary minus")
    }
    fn unary_logical_not(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[StackPoint],
    ) -> OperatorResult<Value> {
        UnsupportedOperationError::raise(self, stack_trace, "unary logical not")
    }

    // fn export(
    //     &self,
    //     _log: &mut dyn RuntimeLog,
    //     stack_trace: &[StackPoint],
    // ) -> OperatorResult<SerializableValue> {
    //     UnsupportedOperationError::raise(self, stack_trace, "export")
    // }
}

#[enum_dispatch(Object)]
#[derive(Debug, Hash, Eq, PartialEq, EnumDowncast, Clone)]
pub enum Value {
    NoneType,
    Default(DefaultValue),
    // Boolean,
    // BuiltinFunction(BuiltinFunctionRef<S>),
    // UserFunction(UserFunction<S>),
    // Structure(Structure<S>),
    // StructDefinition(StructDefinition<S>),
    // List(List<S>),
    // String(SString),
    // Range(Range),
    // Closure(Closure<S>),
    // Scalar,
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
struct DowncastError {
    expected: Cow<'static, str>,
    got: Cow<'static, str>,
}

impl ErrorType for DowncastError {}

impl Display for DowncastError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Expected type `{}`, got {}", self.expected, self.got)
    }
}

impl Value {
    pub fn downcast_ref<T>(&self, stack_trace: &[StackPoint]) -> OperatorResult<&T>
    where
        T: StaticTypeName,
        Self: AsVariant<T>,
    {
        if let Some(value) = self.enum_downcast_ref() {
            Ok(value)
        } else {
            DowncastError {
                expected: T::static_type_name().into(),
                got: self.type_name(),
            }
            .raise(stack_trace)
        }
    }

    pub fn downcast<T>(self, stack_trace: &[StackPoint]) -> OperatorResult<T>
    where
        T: StaticTypeName,
        Self: TryInto<T, Error = Self>,
    {
        match self.try_into() {
            Ok(value) => Ok(value),
            Err(original) => DowncastError {
                expected: T::static_type_name().into(),
                got: original.type_name(),
            }
            .raise(stack_trace)?,
        }
    }

    pub fn downcast_optional<T>(self, stack_trace: &[StackPoint]) -> OperatorResult<Option<T>>
    where
        T: StaticTypeName,
        Self: TryInto<T, Error = Self>,
    {
        match self {
            Self::NoneType(_) => Ok(None),
            this => Ok(Some(this.downcast::<T>(stack_trace)?)),
        }
    }

    // pub fn from_litteral(
    //     context: &mut ExecutionContext<S>,
    //     value: &Litteral<S>,
    // ) -> OperatorResult<S, Self> {
    //     match value {
    //         Litteral::Scalar(scalar) => Scalar::from_parsed(scalar),
    //         Litteral::String(string) => SString::from_parsed(string),
    //         Litteral::List(list) => List::from_parsed(context, list),
    //         Litteral::Boolean(_span, value) => Ok(Self::Boolean(*value)),
    //         Litteral::Default(_span) => Ok(DefaultValue.into()),
    //         Litteral::Closure(closure) => Ok(Closure::from(closure).into()),
    //     }
    // }
}
