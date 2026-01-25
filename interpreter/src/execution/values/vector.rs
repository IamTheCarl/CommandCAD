use common_data_types::Dimension;
use enum_downcast::{AsVariant, IntoVariant};
use nalgebra::{Dim, RawStorage};

use crate::{
    build_closure_type, build_method,
    compile::{self, AstNode},
    execute_expression,
    execution::{
        errors::{ExpressionResult, GenericFailure, Raise as _},
        logging::StackTrace,
        values::{
            closure::BuiltinCallableDatabase, scalar::UnwrapNotNan, string::formatting::Style,
            BuiltinFunction, DowncastForBinaryOpError, MissingAttributeError, Object, Scalar,
            StaticType, StaticTypeName, Value, ValueType,
        },
        ExecutionContext,
    },
};

use std::{
    borrow::Cow,
    hash::Hash,
    ops::{Add, Div, Mul, Neg, Sub},
};

type Float = f64;

pub type Vector2 = Vector<nalgebra::Vector2<Float>>;
pub type Vector3 = Vector<nalgebra::Vector3<Float>>;
pub type Vector4 = Vector<nalgebra::Vector4<Float>>;

#[derive(Debug, Hash, Clone, Copy, PartialEq)]
pub struct Vector<I> {
    dimension: Dimension,
    value: I,
}

impl<I> Eq for Vector<I> where I: PartialEq {}

impl<I> Object for Vector<I>
where
    I: VectorInternalType,
    Self: StaticTypeName + Into<Value>,
    Value: IntoVariant<Self> + AsVariant<Self>,
{
    fn get_type(&self, _context: &ExecutionContext) -> ValueType {
        I::get_type(self.dimension)
    }

    fn format(
        &self,
        context: &ExecutionContext,
        f: &mut dyn std::fmt::Write,
        style: Style,
        precision: Option<u8>,
    ) -> std::fmt::Result {
        let mut components = self.value.iter().peekable();

        write!(f, "<(")?;

        while let Some(value) = components.next() {
            let c = Scalar {
                dimension: self.dimension,
                value: common_data_types::Float::new(value).expect("Vector has NaN component"),
            };

            c.format(context, f, style, precision)?;

            if components.peek().is_some() {
                write!(f, ", ")?;
            }
        }

        write!(f, ")>")?;

        Ok(())
    }

    fn addition(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let rhs = self.unpack_same_dimension(context.stack_trace, rhs)?;
        let value = self.value + rhs.value;

        Ok(Self::new_raw(context, self.dimension, value)?.into())
    }
    fn subtraction(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let rhs = self.unpack_same_dimension(context.stack_trace, rhs)?;
        let value = self.value - rhs.value;

        Ok(Self::new_raw(context, self.dimension, value)?.into())
    }
    fn multiply(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let rhs = rhs.downcast_for_binary_op_ref::<Scalar>(context.stack_trace)?;
        let value = self.value * *rhs.value;
        let dimension = self.dimension + rhs.dimension;

        Ok(Self::new_raw(context, dimension, value)?.into())
    }
    fn divide(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let rhs = rhs.downcast_for_binary_op_ref::<Scalar>(context.stack_trace)?;
        let value = self.value / *rhs.value;
        let dimension = self.dimension - rhs.dimension;

        Ok(Self::new_raw(context, dimension, value)?.into())
    }
    fn unary_plus(self, _context: &ExecutionContext) -> ExpressionResult<Value> {
        Ok(self.clone().into())
    }
    fn unary_minus(self, _context: &ExecutionContext) -> ExpressionResult<Value> {
        Ok(Self {
            value: -self.value,
            ..self.clone()
        }
        .into())
    }

    fn eq(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<bool> {
        let rhs: Self = rhs.downcast_for_binary_op(context.stack_trace)?;
        Ok(self.dimension == rhs.dimension && self.value == rhs.value)
    }

    fn get_attribute(
        &self,
        context: &ExecutionContext,
        attribute: &str,
    ) -> ExpressionResult<Value> {
        if let Some(value) = self
            .value
            .get_attribute(context, attribute, self.dimension)?
        {
            Ok(value)
        } else {
            match attribute {
                "abs" => Ok(BuiltinFunction::new::<
                    <<I as VectorInternalType>::MethodSet as methods::MethodSet>::Abs,
                >()
                .into()),
                "add_scalar" => Ok(BuiltinFunction::new::<
                    <<I as VectorInternalType>::MethodSet as methods::MethodSet>::AddScalar,
                >()
                .into()),
                "amax" => Ok(BuiltinFunction::new::<
                    <<I as VectorInternalType>::MethodSet as methods::MethodSet>::AMax,
                >()
                .into()),
                "amin" => Ok(BuiltinFunction::new::<
                    <<I as VectorInternalType>::MethodSet as methods::MethodSet>::AMin,
                >()
                .into()),
                "dot" => Ok(BuiltinFunction::new::<
                    <<I as VectorInternalType>::MethodSet as methods::MethodSet>::Dot,
                >()
                .into()),
                "norm" | "length" => Ok(BuiltinFunction::new::<
                    <<I as VectorInternalType>::MethodSet as methods::MethodSet>::Norm,
                >()
                .into()),
                "normalize" => Ok(BuiltinFunction::new::<
                    <<I as VectorInternalType>::MethodSet as methods::MethodSet>::Normalize,
                >()
                .into()),
                "angle" => Ok(BuiltinFunction::new::<
                    <<I as VectorInternalType>::MethodSet as methods::MethodSet>::Angle,
                >()
                .into()),
                "apply" | "map" => Ok(BuiltinFunction::new::<
                    <<I as VectorInternalType>::MethodSet as methods::MethodSet>::Map,
                >()
                .into()),
                "fold" => Ok(BuiltinFunction::new::<
                    <<I as VectorInternalType>::MethodSet as methods::MethodSet>::Fold,
                >()
                .into()),
                _ => Err(MissingAttributeError {
                    name: attribute.into(),
                }
                .to_error(context.stack_trace)),
            }
        }
    }
}

impl<I> Vector<I>
where
    I: VectorInternalType + std::fmt::Debug,
    Self: StaticTypeName + Into<Value>,
    Value: IntoVariant<Self> + AsVariant<Self>,
{
    pub fn new(
        context: &ExecutionContext,
        dimension: Dimension,
        value: I::BuildFrom,
    ) -> ExpressionResult<Self> {
        let value = I::build(value);

        Self::new_raw(context, dimension, value)
    }

    pub fn from_ast(
        context: &ExecutionContext,
        ast_node: &AstNode<Box<I::NodeType>>,
    ) -> ExpressionResult<Self> {
        I::from_ast(context, ast_node)
    }

    fn new_raw(
        context: &ExecutionContext,
        dimension: Dimension,
        value: I,
    ) -> ExpressionResult<Self> {
        if !value.is_nan() {
            Ok(Self { dimension, value })
        } else {
            Err(
                GenericFailure("Result of arithmetic operation is NaN".into())
                    .to_error(context.stack_trace),
            )
        }
    }

    fn unpack_same_dimension(self, stack_trace: &StackTrace, rhs: Value) -> ExpressionResult<Self> {
        let rhs: Vector<I> = rhs.downcast_for_binary_op(stack_trace)?;

        if self.dimension == rhs.dimension {
            Ok(rhs)
        } else {
            Err(DowncastForBinaryOpError {
                expected: self.type_name(),
                got: rhs.type_name(),
            }
            .to_error(stack_trace))
        }
    }
}

impl<I> StaticTypeName for Vector<I>
where
    I: StaticTypeName,
{
    fn static_type_name() -> Cow<'static, str> {
        I::static_type_name()
    }
}

impl<I> StaticType for Vector<I>
where
    I: StaticType,
{
    fn static_type() -> ValueType {
        I::static_type()
    }
}

mod methods {
    use stack::ArrayVec;
    use std::collections::HashMap;

    use super::*;
    use crate::{
        build_method,
        execution::values::{BuiltinCallableDatabase, Dictionary},
    };

    pub trait MethodSet {
        type Abs;
        type AddScalar;
        type AMax;
        type AMin;
        type Dot;
        type Norm;
        type Normalize;
        type Angle;
        type Map;
        type Fold;
    }

    macro_rules! build_method_set {
        ($name:ident) => {
            paste::paste! {
                pub struct [<$name Abs>];
                pub struct [<$name AddScalar>];
                pub struct [<$name AMax>];
                pub struct [<$name AMin>];
                pub struct [<$name Dot>];
                pub struct [<$name Norm>];
                pub struct [<$name Normalize>];
                pub struct [<$name Angle>];
                pub struct [<$name Map>];
                pub struct [<$name Fold>];

                pub struct [<$name MethodSet>];
                impl MethodSet for [<$name MethodSet>] {
                    type Abs = [<$name Abs>];
                    type AddScalar = [<$name AddScalar>];
                    type AMax = [<$name AMax>];
                    type AMin = [<$name AMin>];
                    type Dot = [<$name Dot>];
                    type Norm = [<$name Norm>];
                    type Normalize = [<$name Normalize>];
                    type Angle = [<$name Angle>];
                    type Map = [<$name Map>];
                    type Fold = [<$name Fold>];
                }
            }
        };
    }

    build_method_set!(Vector2);
    build_method_set!(Vector3);
    build_method_set!(Vector4);

    pub struct Vector3Cross;

    pub fn register_methods<I, M>(database: &mut BuiltinCallableDatabase, dimension: usize)
    where
        I: VectorInternalType,
        Vector<I>: StaticTypeName + Into<Value>,
        Value: IntoVariant<Vector<I>> + AsVariant<Vector<I>>,
        M: MethodSet + 'static,
    {
        build_closure_type!(MapClosure(c: Scalar) -> Scalar);
        build_closure_type!(FoldClosure(previous: Scalar, c: Scalar) -> Scalar);

        build_method!(
            database,
            M::Abs, format!("Vector{dimension}::abs"), (
                context: &ExecutionContext,
                this: Vector<I>) -> Vector<I>
            {
                let value = this.value.abs();
                Vector::new_raw(context, this.dimension, value)
            }
        );
        build_method!(
            database,
            M::AddScalar, format!("Vector{dimension}::add_scalar"), (
                context: &ExecutionContext,
                this: Vector<I>,
                value: Scalar) -> Vector<I>
            {
                if this.dimension == value.dimension {
                    let value = this.value.add_scalar(*value.value);
                    Vector::new_raw(context, this.dimension, value)
                } else {
                    Err(DowncastForBinaryOpError {
                        expected: this.type_name(),
                        got: value.type_name(),
                    }
                    .to_error(context.stack_trace))
                }
            }
        );
        build_method!(
            database,
            M::AMax, format!("Vector{dimension}::amax"), (
                context: &ExecutionContext,
                this: Vector<I>) -> Scalar
            {
                let value = common_data_types::Float::new(this.value.amax()).unwrap_not_nan(context.stack_trace)?;

                Ok(Scalar {
                    dimension: this.dimension,
                    value
                })
            }
        );
        build_method!(
            database,
            M::AMin, format!("Vector{dimension}::amin"), (
                context: &ExecutionContext,
                this: Vector<I>) -> Scalar
            {
                let value = common_data_types::Float::new(this.value.amin()).unwrap_not_nan(context.stack_trace)?;

                Ok(Scalar {
                    dimension: this.dimension,
                    value
                })
            }
        );
        build_method!(
            database,
            M::Dot, format!("Vector{dimension}::dot"), (
                context: &ExecutionContext,
                this: Vector<I>,
                rhs: Vector<I>) -> Scalar
            {
                if this.dimension == rhs.dimension {
                    let value = common_data_types::Float::new(this.value.dot(&rhs.value)).unwrap_not_nan(context.stack_trace)?;

                    Ok(Scalar {
                        dimension: this.dimension,
                        value
                    })
                } else {
                    Err(DowncastForBinaryOpError {
                        expected: this.type_name(),
                        got: rhs.type_name(),
                    }
                    .to_error(context.stack_trace))
                }
            }
        );
        build_method!(
            database,
            M::Norm, format!("Vector{dimension}::norm"),(
                context: &ExecutionContext,
                this: Vector<I>) -> Scalar
            {
                let value = common_data_types::Float::new(this.value.norm()).unwrap_not_nan(context.stack_trace)?;

                Ok(Scalar {
                    dimension: this.dimension,
                    value
                })
            }
        );
        build_method!(
            database,
            M::Normalize, format!("Vector{dimension}::normalize"),(
                context: &ExecutionContext,
                this: Vector<I>) -> Vector<I>
            {
                let value = this.value.normalize();
                Vector::<I>::new_raw(context, Dimension::zero(), value)
            }
        );
        build_method!(
            database,
            M::Angle, format!("Vector{dimension}::angle"),(
                context: &ExecutionContext,
                this: Vector<I>,
                other: Vector<I>) -> Scalar
            {
                let value = common_data_types::Float::new(this.value.angle(&other.value)).unwrap_not_nan(context.stack_trace)?;

                Ok(Scalar {
                    dimension: Dimension::angle(),
                    value
                })
            }
        );
        build_method!(
            database,
            M::Map, format!("Vector{dimension}::map"),(
                context: &ExecutionContext,
                this: Vector<I>,
                f: MapClosure) -> Vector<I>
            {
                let operations: ArrayVec<[Value; 4]> = this.value.iter().map(|c| f.call(context, Dictionary::new(context, HashMap::from_iter([
                    (
                        "c".into(),
                        Scalar {
                            dimension: this.dimension,
                            value: common_data_types::Float::new(c).unwrap_not_nan(context.stack_trace)?
                        }.into()
                    )
                ])))).collect::<ExpressionResult<_>>()?;

                let result: ArrayVec<[Scalar; 4]> = operations.into_iter().map(|v| v.downcast::<Scalar>(context.stack_trace)).collect::<ExpressionResult<_>>()?;

                // The smallest vector we support is 2, so this should never panic.
                let dimension = result[0].dimension;

                for component in result.iter() {
                    if component.dimension != dimension {
                        return Err(GenericFailure("All components of a vector must match".into())
                            .to_error(context.stack_trace));
                    }
                }

                Ok(Vector::<I>::new_raw(context, dimension, I::from_iterator(result.iter().map(|c| *c.value)))?)
            }
        );
        build_method!(
            database,
            M::Fold, format!("Vector{dimension}::fold"),(
                context: &ExecutionContext,
                this: Vector<I>,
                init: Value,
                f: FoldClosure) -> Value
            {

                let mut accumulator = init;
                for component in this.value.iter() {
                    accumulator = f.call(context, Dictionary::new(context, HashMap::from_iter([
                        (
                            "c".into(),
                            Scalar {
                                dimension: this.dimension,
                                value: common_data_types::Float::new(component).unwrap_not_nan(context.stack_trace)?
                            }.into()
                        ),
                        (
                            "previous".into(),
                            accumulator
                        )
                    ])))?;
                }

                Ok(accumulator)
            }
        );
    }
}

pub fn register_methods(database: &mut BuiltinCallableDatabase) {
    methods::register_methods::<nalgebra::Vector2<Float>, methods::Vector2MethodSet>(database, 2);
    methods::register_methods::<nalgebra::Vector3<Float>, methods::Vector3MethodSet>(database, 3);
    methods::register_methods::<nalgebra::Vector4<Float>, methods::Vector4MethodSet>(database, 4);

    build_method!(
        database,
        methods::Vector3Cross, "Vector3::cross", (
            context: &ExecutionContext,
            this: Vector3,
            rhs: Vector3) -> Vector3
        {
            if this.dimension == rhs.dimension {
                let value = this.value.cross(&rhs.value);
                Vector3::new_raw(context, this.dimension, value)
            } else {
                Err(DowncastForBinaryOpError {
                    expected: this.type_name(),
                    got: rhs.type_name(),
                }
                .to_error(context.stack_trace))
            }
        }
    );
}

pub trait VectorInternalType:
    Add<Self, Output = Self>
    + Div<Float, Output = Self>
    + Mul<Float, Output = Self>
    + Sub<Self, Output = Self>
    + Copy
    + PartialEq
    + Neg<Output = Self>
    + StaticTypeName
    + StaticType
    + IsNan
    + std::fmt::Debug
    + Send
    + Sync
    + 'static
{
    type BuildFrom;
    type NodeType;
    type MethodSet: methods::MethodSet;

    fn get_type(dimension: Dimension) -> ValueType;
    fn build(value: Self::BuildFrom) -> Self;
    fn from_ast(
        context: &ExecutionContext,
        ast_node: &AstNode<Box<Self::NodeType>>,
    ) -> ExpressionResult<Vector<Self>>;
    fn from_iterator<I>(iterator: I) -> Self
    where
        I: IntoIterator<Item = Float>;

    fn get_attribute(
        &self,
        context: &ExecutionContext,
        attribute: &str,
        dimension: Dimension,
    ) -> ExpressionResult<Option<Value>>;

    fn abs(&self) -> Self;
    fn add_scalar(&self, value: Float) -> Self;
    fn amax(&self) -> Float;
    fn amin(&self) -> Float;
    fn dot(&self, rhs: &Self) -> Float;
    fn norm(&self) -> Float;
    fn normalize(&self) -> Self;
    fn angle(&self, other: &Self) -> Float;
    fn iter(&self) -> impl Iterator<Item = Float>;
}

pub trait IsNan {
    fn is_nan(&self) -> bool;
}

impl<R, C, S> IsNan for nalgebra::Matrix<Float, R, C, S>
where
    R: Dim,
    C: Dim,
    S: RawStorage<Float, R, C>,
{
    fn is_nan(&self) -> bool {
        self.iter().any(|&component| component.is_nan())
    }
}

macro_rules! get_component {
    ($context:ident, $ast_node:ident, $c:ident) => {
        execute_expression($context, &$ast_node.node.$c)?
            .downcast::<Scalar>($context.stack_trace)?
    };
}

impl VectorInternalType for nalgebra::Vector2<Float> {
    type BuildFrom = [Float; 2];
    type NodeType = compile::Vector2;
    type MethodSet = methods::Vector2MethodSet;

    fn get_type(dimension: Dimension) -> ValueType {
        ValueType::Vector2(Some(dimension))
    }

    fn build(value: Self::BuildFrom) -> Self {
        Self::from_iterator(value.into_iter())
    }

    fn from_ast(
        context: &ExecutionContext,
        ast_node: &AstNode<Box<Self::NodeType>>,
    ) -> ExpressionResult<Vector<Self>> {
        let x = get_component!(context, ast_node, x);
        let y = get_component!(context, ast_node, y);

        if x.dimension == y.dimension {
            Ok(Vector {
                dimension: x.dimension,
                value: Self::new(*x.value, *y.value),
            })
        } else {
            Err(
                GenericFailure("All components of a vector must match".into())
                    .to_error(context.stack_trace),
            )
        }
    }

    fn from_iterator<I>(iterator: I) -> Self
    where
        I: IntoIterator<Item = Float>,
    {
        Self::from_iterator(iterator)
    }

    fn get_attribute(
        &self,
        context: &ExecutionContext,
        attribute: &str,
        dimension: Dimension,
    ) -> ExpressionResult<Option<Value>> {
        match attribute {
            "x" => Ok(Some(
                Scalar {
                    dimension,
                    value: common_data_types::Float::new(self.x)
                        .unwrap_not_nan(context.stack_trace)?,
                }
                .into(),
            )),
            "y" => Ok(Some(
                Scalar {
                    dimension,
                    value: common_data_types::Float::new(self.y)
                        .unwrap_not_nan(context.stack_trace)?,
                }
                .into(),
            )),
            _ => Ok(None),
        }
    }

    fn abs(&self) -> Self {
        nalgebra::Vector2::abs(self)
    }

    fn add_scalar(&self, value: Float) -> Self {
        nalgebra::Vector2::add_scalar(self, value)
    }

    fn amax(&self) -> Float {
        nalgebra::Vector2::amax(self)
    }

    fn amin(&self) -> Float {
        nalgebra::Vector2::amin(self)
    }

    fn dot(&self, rhs: &Self) -> Float {
        nalgebra::Vector2::dot(self, rhs)
    }

    fn norm(&self) -> Float {
        nalgebra::Vector2::norm(self)
    }

    fn normalize(&self) -> Self {
        nalgebra::Vector2::normalize(self)
    }

    fn angle(&self, other: &Self) -> Float {
        nalgebra::Vector2::angle(self, other)
    }

    fn iter(&self) -> impl Iterator<Item = Float> {
        self.iter().copied()
    }
}
impl StaticTypeName for nalgebra::Vector2<Float> {
    fn static_type_name() -> Cow<'static, str> {
        "Vector2".into()
    }
}

impl StaticType for nalgebra::Vector2<Float> {
    fn static_type() -> ValueType {
        ValueType::Vector2(None)
    }
}

impl VectorInternalType for nalgebra::Vector3<Float> {
    type BuildFrom = [Float; 3];
    type NodeType = compile::Vector3;
    type MethodSet = methods::Vector3MethodSet;

    fn get_type(dimension: Dimension) -> ValueType {
        ValueType::Vector3(Some(dimension))
    }

    fn build(value: Self::BuildFrom) -> Self {
        Self::from_iterator(value.into_iter())
    }

    fn from_ast(
        context: &ExecutionContext,
        ast_node: &AstNode<Box<Self::NodeType>>,
    ) -> ExpressionResult<Vector<Self>> {
        let x = get_component!(context, ast_node, x);
        let y = get_component!(context, ast_node, y);
        let z = get_component!(context, ast_node, z);

        if x.dimension == y.dimension && x.dimension == z.dimension {
            Ok(Vector {
                dimension: x.dimension,
                value: Self::new(*x.value, *y.value, *z.value),
            })
        } else {
            Err(
                GenericFailure("All components of a vector must match".into())
                    .to_error(context.stack_trace),
            )
        }
    }

    fn from_iterator<I>(iterator: I) -> Self
    where
        I: IntoIterator<Item = Float>,
    {
        Self::from_iterator(iterator)
    }

    fn get_attribute(
        &self,
        context: &ExecutionContext,
        attribute: &str,
        dimension: Dimension,
    ) -> ExpressionResult<Option<Value>> {
        match attribute {
            "x" => Ok(Some(
                Scalar {
                    dimension,
                    value: common_data_types::Float::new(self.x)
                        .unwrap_not_nan(context.stack_trace)?,
                }
                .into(),
            )),
            "y" => Ok(Some(
                Scalar {
                    dimension,
                    value: common_data_types::Float::new(self.y)
                        .unwrap_not_nan(context.stack_trace)?,
                }
                .into(),
            )),
            "z" => Ok(Some(
                Scalar {
                    dimension,
                    value: common_data_types::Float::new(self.z)
                        .unwrap_not_nan(context.stack_trace)?,
                }
                .into(),
            )),
            "cross" => Ok(Some(BuiltinFunction::new::<methods::Vector3Cross>().into())),
            _ => Ok(None),
        }
    }

    fn abs(&self) -> Self {
        nalgebra::Vector3::abs(self)
    }

    fn add_scalar(&self, value: Float) -> Self {
        nalgebra::Vector3::add_scalar(self, value)
    }

    fn amax(&self) -> Float {
        nalgebra::Vector3::amax(self)
    }

    fn amin(&self) -> Float {
        nalgebra::Vector3::amin(self)
    }

    fn dot(&self, rhs: &Self) -> Float {
        nalgebra::Vector3::dot(self, rhs)
    }

    fn norm(&self) -> Float {
        nalgebra::Vector3::norm(self)
    }

    fn normalize(&self) -> Self {
        nalgebra::Vector3::normalize(self)
    }

    fn angle(&self, other: &Self) -> Float {
        nalgebra::Vector3::angle(self, other)
    }

    fn iter(&self) -> impl Iterator<Item = Float> {
        self.iter().copied()
    }
}
impl StaticTypeName for nalgebra::Vector3<Float> {
    fn static_type_name() -> Cow<'static, str> {
        "Vector3".into()
    }
}

impl StaticType for nalgebra::Vector3<Float> {
    fn static_type() -> ValueType {
        ValueType::Vector3(None)
    }
}

impl VectorInternalType for nalgebra::Vector4<Float> {
    type BuildFrom = [Float; 4];
    type NodeType = compile::Vector4;
    type MethodSet = methods::Vector4MethodSet;

    fn get_type(dimension: Dimension) -> ValueType {
        ValueType::Vector4(Some(dimension))
    }

    fn build(value: Self::BuildFrom) -> Self {
        Self::from_iterator(value.into_iter())
    }

    fn from_ast(
        context: &ExecutionContext,
        ast_node: &AstNode<Box<Self::NodeType>>,
    ) -> ExpressionResult<Vector<Self>> {
        let x = get_component!(context, ast_node, x);
        let y = get_component!(context, ast_node, y);
        let z = get_component!(context, ast_node, z);
        let w = get_component!(context, ast_node, w);

        if x.dimension == y.dimension && x.dimension == z.dimension && x.dimension == w.dimension {
            Ok(Vector {
                dimension: w.dimension,
                value: Self::new(*x.value, *y.value, *z.value, *w.value),
            })
        } else {
            Err(
                GenericFailure("All components of a vector must match".into())
                    .to_error(context.stack_trace),
            )
        }
    }

    fn from_iterator<I>(iterator: I) -> Self
    where
        I: IntoIterator<Item = Float>,
    {
        Self::from_iterator(iterator)
    }

    fn get_attribute(
        &self,
        context: &ExecutionContext,
        attribute: &str,
        dimension: Dimension,
    ) -> ExpressionResult<Option<Value>> {
        match attribute {
            "x" => Ok(Some(
                Scalar {
                    dimension,
                    value: common_data_types::Float::new(self.x)
                        .unwrap_not_nan(context.stack_trace)?,
                }
                .into(),
            )),
            "y" => Ok(Some(
                Scalar {
                    dimension,
                    value: common_data_types::Float::new(self.y)
                        .unwrap_not_nan(context.stack_trace)?,
                }
                .into(),
            )),
            "z" => Ok(Some(
                Scalar {
                    dimension,
                    value: common_data_types::Float::new(self.z)
                        .unwrap_not_nan(context.stack_trace)?,
                }
                .into(),
            )),
            "w" => Ok(Some(
                Scalar {
                    dimension,
                    value: common_data_types::Float::new(self.w)
                        .unwrap_not_nan(context.stack_trace)?,
                }
                .into(),
            )),
            _ => Ok(None),
        }
    }

    fn abs(&self) -> Self {
        nalgebra::Vector4::abs(self)
    }

    fn add_scalar(&self, value: Float) -> Self {
        nalgebra::Vector4::add_scalar(self, value)
    }

    fn amax(&self) -> Float {
        nalgebra::Vector4::amax(self)
    }
    fn amin(&self) -> Float {
        nalgebra::Vector4::amin(self)
    }

    fn dot(&self, rhs: &Self) -> Float {
        nalgebra::Vector4::dot(self, rhs)
    }

    fn norm(&self) -> Float {
        nalgebra::Vector4::norm(self)
    }

    fn normalize(&self) -> Self {
        nalgebra::Vector4::normalize(self)
    }

    fn angle(&self, other: &Self) -> Float {
        nalgebra::Vector4::angle(self, other)
    }

    fn iter(&self) -> impl Iterator<Item = Float> {
        self.iter().copied()
    }
}
impl StaticTypeName for nalgebra::Vector4<Float> {
    fn static_type_name() -> Cow<'static, str> {
        "Vector4".into()
    }
}

impl StaticType for nalgebra::Vector4<Float> {
    fn static_type() -> ValueType {
        ValueType::Vector4(None)
    }
}

#[cfg(test)]
mod test {
    use crate::execution::{test_context, test_run, values::Boolean};
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn construct_vector2() {
        test_context([], |context| {
            let product = test_run("<(1m, 2m)>").unwrap();
            assert_eq!(
                product,
                Vector2::new(&context, Dimension::length(), [1.0, 2.0])
                    .unwrap()
                    .into()
            );

            let product = test_run("<(-1m, -2m)>").unwrap();
            assert_eq!(
                product,
                Vector2::new(&context, Dimension::length(), [-1.0, -2.0])
                    .unwrap()
                    .into()
            );
        })
    }

    #[test]
    fn construct_vector3() {
        test_context([], |context| {
            let product = test_run("<(1m, 2m, 3m)>").unwrap();
            assert_eq!(
                product,
                Vector3::new(&context, Dimension::length(), [1.0, 2.0, 3.0])
                    .unwrap()
                    .into()
            );

            let product = test_run("<(-1m, -2m, -3m)>").unwrap();
            assert_eq!(
                product,
                Vector3::new(&context, Dimension::length(), [-1.0, -2.0, -3.0])
                    .unwrap()
                    .into()
            );
        })
    }

    #[test]
    fn construct_vector4() {
        test_context([], |context| {
            let product = test_run("<(1m, 2m, 3m, 4m)>").unwrap();
            assert_eq!(
                product,
                Vector4::new(&context, Dimension::length(), [1.0, 2.0, 3.0, 4.0])
                    .unwrap()
                    .into()
            );

            let product = test_run("<(-1m, -2m, -3m, -4m)>").unwrap();
            assert_eq!(
                product,
                Vector4::new(&context, Dimension::length(), [-1.0, -2.0, -3.0, -4.0])
                    .unwrap()
                    .into()
            );
        })
    }

    #[test]
    fn missmatched_dimensions_vector2() {
        test_run("<(1deg, 2m)>").unwrap_err();
        test_run("<(1m, 2deg)>").unwrap_err();
    }

    #[test]
    fn missmatched_dimensions_vector3() {
        test_run("<(1deg, 2m, 3m)>").unwrap_err();
        test_run("<(1m, 2deg, 3m)>").unwrap_err();
        test_run("<(1m, 2m, 3deg)>").unwrap_err();
    }

    #[test]
    fn missmatched_dimensions_vector4() {
        test_run("<(1deg, 2m, 3m, 4m)>").unwrap_err();
        test_run("<(1m, 2deg, 3m, 4m)>").unwrap_err();
        test_run("<(1m, 2m, 3deg, 4m)>").unwrap_err();
        test_run("<(1m, 2m, 3m, 4deg)>").unwrap_err();
    }

    #[test]
    fn construccomponent_access_vector2() {
        let product = test_run("let vec = <(1m, 2m)>; in vec.x").unwrap();
        assert_eq!(
            product,
            Scalar {
                dimension: Dimension::length(),
                value: common_data_types::Float::new(1.0).unwrap()
            }
            .into()
        );

        let product = test_run("let vec = <(1m, 2m)>; in vec.y").unwrap();
        assert_eq!(
            product,
            Scalar {
                dimension: Dimension::length(),
                value: common_data_types::Float::new(2.0).unwrap()
            }
            .into()
        );
    }

    #[test]
    fn construccomponent_access_vector3() {
        let product = test_run("let vec = <(1m, 2m, 3m)>; in vec.x").unwrap();
        assert_eq!(
            product,
            Scalar {
                dimension: Dimension::length(),
                value: common_data_types::Float::new(1.0).unwrap()
            }
            .into()
        );

        let product = test_run("let vec = <(1m, 2m, 3m)>; in vec.y").unwrap();
        assert_eq!(
            product,
            Scalar {
                dimension: Dimension::length(),
                value: common_data_types::Float::new(2.0).unwrap()
            }
            .into()
        );

        let product = test_run("let vec = <(1m, 2m, 3m)>; in vec.z").unwrap();
        assert_eq!(
            product,
            Scalar {
                dimension: Dimension::length(),
                value: common_data_types::Float::new(3.0).unwrap()
            }
            .into()
        );
    }

    #[test]
    fn construccomponent_access_vector4() {
        let product = test_run("let vec = <(1m, 2m, 3m, 4m)>; in vec.x").unwrap();
        assert_eq!(
            product,
            Scalar {
                dimension: Dimension::length(),
                value: common_data_types::Float::new(1.0).unwrap()
            }
            .into()
        );

        let product = test_run("let vec = <(1m, 2m, 3m, 4m)>; in vec.y").unwrap();
        assert_eq!(
            product,
            Scalar {
                dimension: Dimension::length(),
                value: common_data_types::Float::new(2.0).unwrap()
            }
            .into()
        );

        let product = test_run("let vec = <(1m, 2m, 3m, 4m)>; in vec.z").unwrap();
        assert_eq!(
            product,
            Scalar {
                dimension: Dimension::length(),
                value: common_data_types::Float::new(3.0).unwrap()
            }
            .into()
        );

        let product = test_run("let vec = <(1m, 2m, 3m, 4m)>; in vec.w").unwrap();
        assert_eq!(
            product,
            Scalar {
                dimension: Dimension::length(),
                value: common_data_types::Float::new(4.0).unwrap()
            }
            .into()
        );
    }

    #[test]
    fn compare_vector2() {
        let product = test_run("<(1m, 2m)> == <(1m, 2m)>").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("<(1m, 2m)> != <(1m, 2m)>").unwrap();
        assert_eq!(product, Boolean(false).into());

        let product = test_run("<(2m, 2m)> == <(1m, 2m)>").unwrap();
        assert_eq!(product, Boolean(false).into());

        let product = test_run("<(2m, 2m)> != <(1m, 2m)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn compare_vector3() {
        let product = test_run("<(1m, 2m, 3m)> == <(1m, 2m, 3m)>").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("<(1m, 2m, 3m)> != <(1m, 2m, 3m)>").unwrap();
        assert_eq!(product, Boolean(false).into());

        let product = test_run("<(2m, 2m, 3m)> == <(1m, 2m, 3m)>").unwrap();
        assert_eq!(product, Boolean(false).into());

        let product = test_run("<(2m, 2m, 3m)> != <(1m, 2m, 3m)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn compare_vector4() {
        let product = test_run("<(1m, 2m, 3m, 4m)> == <(1m, 2m, 3m, 4m)>").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("<(1m, 2m, 3m, 4m)> != <(1m, 2m, 3m, 4m)>").unwrap();
        assert_eq!(product, Boolean(false).into());

        let product = test_run("<(2m, 2m, 3m, 4m)> == <(1m, 2m, 3m, 4m)>").unwrap();
        assert_eq!(product, Boolean(false).into());

        let product = test_run("<(2m, 2m, 3m, 4m)> != <(1m, 2m, 3m, 4m)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn add_vector2() {
        let product = test_run("<(1m, 2m)> + <(2m, 3m)> == <(3m, 5m)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn add_vector3() {
        let product = test_run("<(1m, 2m, 3m)> + <(2m, 3m, 4m)> == <(3m, 5m, 7m)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn add_vector4() {
        let product =
            test_run("<(1m, 2m, 3m, 4m)> + <(2m, 3m, 4m, 5m)> == <(3m, 5m, 7m, 9m)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn sub_vector2() {
        let product = test_run("<(1m, 2m)> - <(2m, 3m)> == <(-1m, -1m)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn sub_vector3() {
        let product = test_run("<(1m, 2m, 3m)> - <(2m, 3m, 4m)> == <(-1m, -1m, -1m)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn sub_vector4() {
        let product =
            test_run("<(1m, 2m, 3m, 4m)> - <(2m, 3m, 4m, 5m)> == <(-1m, -1m, -1m, -1m)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn multiply_vector2() {
        let product = test_run("<(1m, 2m)> * 2.0 == <(2m, 4m)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn multiply_vector3() {
        let product = test_run("<(1m, 2m, 3m)> * 2.0 == <(2m, 4m, 6m)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn multiply_vector4() {
        let product = test_run("<(1m, 2m, 3m, 4m)> * 2.0 == <(2m, 4m, 6m, 8m)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn divide_vector2() {
        let product = test_run("<(2m, 4m)> / 2.0 == <(1m, 2m)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn divide_vector3() {
        let product = test_run("<(2m, 4m, 6m)> / 2.0 == <(1m, 2m, 3m)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn divide_vector4() {
        let product = test_run("<(2m, 4m, 6m, 8m)> / 2.0 == <(1m, 2m, 3m, 4m)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn abs_vector2() {
        let product = test_run("<(-1m, -2m)>::abs() == <(1m, 2m)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn abs_vector3() {
        let product = test_run("<(-1m, -2m, -3m)>::abs() == <(1m, 2m, 3m)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn abs_vector4() {
        let product = test_run("<(-1m, -2m, -3m, -4m)>::abs() == <(1m, 2m, 3m, 4m)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn add_scalar_vector2() {
        let product = test_run("<(1m, 2m)>::add_scalar(value = 1m) == <(2m, 3m)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn add_scalar_vector3() {
        let product = test_run("<(1m, 2m, 3m)>::add_scalar(value = 1m) == <(2m, 3m, 4m)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn add_scalar_vector4() {
        let product =
            test_run("<(1m, 2m, 3m, 4m)>::add_scalar(value = 1m) == <(2m, 3m, 4m, 5m)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn amax_vector2() {
        let product = test_run("<(1m, 2m)>::amax() == 2m").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn amax_vector3() {
        let product = test_run("<(1m, 2m, 3m)>::amax() == 3m").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn amax_vector4() {
        let product = test_run("<(1m, 2m, 3m, 4m)>::amax() == 4m").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn dot_vector2() {
        let product = test_run("<(1m, 0m)>::dot(rhs = <(0.5m, 10m)>) == 0.5m").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn dot_vector3() {
        let product = test_run("<(1m, 0m, 0m)>::dot(rhs = <(0.5m, 10m, 10m)>) == 0.5m").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn dot_vector4() {
        let product =
            test_run("<(1m, 0m, 0m, 0m)>::dot(rhs = <(0.5m, 10m, 10m, 10m)>) == 0.5m").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn norm_vector2() {
        let product = test_run("<(1m, 0m)>::norm() == 1m").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn norm_vector3() {
        let product = test_run("<(1m, 0m, 0m)>::norm() == 1m").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn norm_vector4() {
        let product = test_run("<(1m, 0m, 0m, 0m)>::norm() == 1m").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn length_vector2() {
        let product = test_run("<(1m, 0m)>::length() == 1m").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn length_vector3() {
        let product = test_run("<(1m, 0m, 0m)>::length() == 1m").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn length_vector4() {
        let product = test_run("<(1m, 0m, 0m, 0m)>::length() == 1m").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn normalize_vector2() {
        let product = test_run("<(5m, 0m)>::normalize() == <(1, 0)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn normalize_vector3() {
        let product = test_run("<(5m, 0m, 0m)>::normalize() == <(1, 0, 0)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn normalize_vector4() {
        let product = test_run("<(5m, 0m, 0m, 0m)>::normalize() == <(1, 0, 0, 0)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn normalize_zero_vector2() {
        test_run("<(0m, 0m)>::normalize()").unwrap_err();
    }

    #[test]
    fn normalize_zero_vector3() {
        test_run("<(0m, 0m, 0m)>::normalize()").unwrap_err();
    }

    #[test]
    fn normalize_zero_vector4() {
        test_run("<(0m, 0m, 0m, 0m)>::normalize()").unwrap_err();
    }

    #[test]
    fn cross_vector3() {
        let product =
            test_run("<(1m, 0m, 0m)>::cross(rhs = <(0m, 1m, 0m)>) == <(0m, 0m, 1m)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn angle_vector2() {
        let product = test_run("<(1m, 0m)>::angle(other = <(0m, 1m)>) - 90deg < 0.001deg").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn angle_vector3() {
        let product =
            test_run("<(1m, 0m, 0m)>::angle(other = <(0m, 1m, 0m)>) - 90deg < 0.001deg").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn angle_vector4() {
        let product =
            test_run("<(1m, 0m, 0m, 0m)>::angle(other = <(0m, 1m, 0m, 0m)>) - 90deg < 0.001deg")
                .unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn apply_vector2() {
        let product = test_run("<(0m, 1m)>::apply(f = (c: std.scalar.Length) -> std.scalar.Length: c + 1m) == <(1m, 2m)>").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("<(0m, 1m)>::apply(f = (c: std.scalar.Length) -> std.scalar.Area: c * 1m) == <(0 'm^2', 1 'm^2')>").unwrap();
        assert_eq!(product, Boolean(true).into());

        test_run("<(0m, 1m)>::apply(f = (c: std.scalar.Length) -> std.scalar.Any: if c == 0m then 1m else 1 'm^2')").unwrap_err();
    }

    #[test]
    fn apply_vector3() {
        let product = test_run("<(0m, 1m, 2m)>::apply(f = (c: std.scalar.Length) -> std.scalar.Length: c + 1m) == <(1m, 2m, 3m)>").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("<(0m, 1m, 2m)>::apply(f = (c: std.scalar.Length) -> std.scalar.Area: c * 1m) == <(0 'm^2', 1 'm^2', 2 'm^2')>").unwrap();
        assert_eq!(product, Boolean(true).into());

        test_run("<(0m, 1m, 1m)>::apply(f = (c: std.scalar.Length) -> std.scalar.Any: if c == 0m then 1m else 1 'm^2')").unwrap_err();
    }

    #[test]
    fn apply_vector4() {
        let product = test_run("<(0m, 1m, 2m, 3m)>::apply(f = (c: std.scalar.Length) -> std.scalar.Length: c + 1m) == <(1m, 2m, 3m, 4m)>").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("<(0m, 1m, 2m, 3m)>::apply(f = (c: std.scalar.Length) -> std.scalar.Area: c * 1m) == <(0 'm^2', 1 'm^2', 2 'm^2', 3 'm^2')>").unwrap();
        assert_eq!(product, Boolean(true).into());

        test_run("<(0m, 1m, 1m, 1m)>::apply(f = (c: std.scalar.Length) -> std.scalar.Any: if c == 0m then 1m else 1 'm^2')").unwrap_err();
    }

    #[test]
    fn fold_vector2() {
        let product = test_run("<(1m, 2m)>::fold(init = 0m, f = (previous: std.scalar.Length, c: std.scalar.Length) -> std.scalar.Length: previous + c)").unwrap();
        assert_eq!(
            product,
            Scalar {
                dimension: Dimension::length(),
                value: common_data_types::Float::new(3.0).unwrap()
            }
            .into()
        );
    }

    #[test]
    fn fold_vector3() {
        let product = test_run("<(1m, 2m, 3m)>::fold(init = 0m, f = (previous: std.scalar.Length, c: std.scalar.Length) -> std.scalar.Length: previous + c)").unwrap();
        assert_eq!(
            product,
            Scalar {
                dimension: Dimension::length(),
                value: common_data_types::Float::new(6.0).unwrap()
            }
            .into()
        );
    }

    #[test]
    fn fold_vector4() {
        let product = test_run("<(1m, 2m, 3m, 4m)>::fold(init = 0m, f = (previous: std.scalar.Length, c: std.scalar.Length) -> std.scalar.Length: previous + c)").unwrap();
        assert_eq!(
            product,
            Scalar {
                dimension: Dimension::length(),
                value: common_data_types::Float::new(10.0).unwrap()
            }
            .into()
        );
    }

    #[test]
    fn format() {
        let product = test_run(
            "\"{a} {b} {c:.2}\"::format(a = <(1, 2)>, b = <(1m, 2m)>, c = <(1.234, 2.345)>) == \"<(1, 2)> <(1m, 2m)> <(1.23, 2.34)>\"",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run(
            "\"{a} {b} {c:.2}\"::format(a = <(1, 2, 3)>, b = <(1m, 2m, 3m)>, c = <(1.234, 2.345, 3.456)>) == \"<(1, 2, 3)> <(1m, 2m, 3m)> <(1.23, 2.34, 3.46)>\"",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run(
            "\"{a} {b} {c:.2}\"::format(a = <(1, 2, 3, 4)>, b = <(1m, 2m, 3m, 4m)>, c = <(1.234, 2.345, 3.456, 4.567)>) == \"<(1, 2, 3, 4)> <(1m, 2m, 3m, 4m)> <(1.23, 2.34, 3.46, 4.57)>\"",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());
    }
}
