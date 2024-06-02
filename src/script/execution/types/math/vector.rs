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

use std::borrow::Cow;

use arrayvec::ArrayVec;
use common_data_types::{consts, Dimension, Float, RawFloat};
use enum_downcast::{AsVariant, EnumDowncast, IntoVariant};
use fj_math::Scalar as FornjotScalar;
use nalgebra::{
    allocator::Allocator, base::dimension::Const, DefaultAllocator, DimName, ToTypenum,
};

use crate::script::{
    execution::{
        types::{
            function::{AutoCall, BuiltinFunctionRef},
            number::UnwrapNotNan,
            BuiltinFunction, NamedObject, Object, OperatorResult, TypedObject, Value,
        },
        ExecutionContext, Failure,
    },
    logging::RuntimeLog,
    parsing::{Expression, VariableType},
    Span,
};

use super::{get_dimension_name, CheckNan, ConvertUnit, Number, Scalar};

pub(super) type NVector<D> = nalgebra::OVector<RawFloat, D>;

pub type Vector2 = Vector<Const<2>>;
pub type Vector3 = Vector<Const<3>>;
pub type Vector4 = Vector<Const<4>>;

pub type LengthVector2 = LengthVector<Const<2>>;
pub type LengthVector3 = LengthVector<Const<3>>;

macro_rules! define_fixed_dimension_vector {
    ($name:ident, $dimension_name:literal, $dimension:expr) => {
        #[derive(Debug, Clone, PartialEq)]
        pub struct $name<D: DimName>
        where
            DefaultAllocator: Allocator<RawFloat, D>,
        {
            pub value: NVector<D>,
        }

        impl<D: DimName> From<$name<D>> for Vector<D>
        where
            DefaultAllocator: Allocator<RawFloat, D>,
        {
            fn from(value: $name<D>) -> Vector<D> {
                Vector {
                    dimension: $dimension,
                    value: value.value,
                }
            }
        }

        impl<S: Span, D: DimName> From<$name<D>> for Value<S>
        where
            DefaultAllocator: Allocator<RawFloat, D>,
            Value<S>: From<Vector<D>>,
        {
            fn from(value: $name<D>) -> Value<S> {
                Value::from(Vector::from(value))
            }
        }

        impl<S: Span, D: DimName> TryFrom<Value<S>> for $name<D>
        where
            DefaultAllocator: Allocator<RawFloat, D>,
            Value<S>: IntoVariant<Vector<D>>,
        {
            type Error = ();

            fn try_from(value: Value<S>) -> Result<Self, ()> {
                let vector: Vector<D> = value.enum_downcast().map_err(|_| ())?;
                Self::try_from(vector).map_err(|_| ())
            }
        }

        impl<D: DimName> TryFrom<Vector<D>> for $name<D>
        where
            DefaultAllocator: Allocator<RawFloat, D>,
        {
            type Error = Vector<D>;

            fn try_from(value: Vector<D>) -> Result<Self, Self::Error> {
                if value.dimension == $dimension {
                    Ok(Self { value: value.value })
                } else {
                    Err(value)
                }
            }
        }

        impl<'a, D: DimName> TryFrom<&'a Vector<D>> for $name<D>
        where
            DefaultAllocator: Allocator<RawFloat, D>,
        {
            type Error = &'a Vector<D>;

            fn try_from(value: &'a Vector<D>) -> Result<Self, Self::Error> {
                if value.dimension == $dimension {
                    Ok(Self {
                        value: value.value.clone(),
                    })
                } else {
                    Err(value)
                }
            }
        }

        impl<D: DimName> TypedObject for $name<D>
        where
            DefaultAllocator: Allocator<RawFloat, D>,
        {
            fn get_type<S: Span>() -> VariableType<S> {
                VariableType::Vector(D::USIZE as u8, S::from_str($dimension_name))
            }
        }

        impl<D: DimName> NamedObject for $name<D>
        where
            Vector<D>: NamedObject,
            DefaultAllocator: Allocator<RawFloat, D>,
        {
            fn static_type_name() -> &'static str {
                Vector::<D>::static_type_name()
            }
        }
    };
}

define_fixed_dimension_vector!(LengthVector, "Length", Dimension::length());

impl<D: DimName> LengthVector<D>
where
    DefaultAllocator: Allocator<RawFloat, D>,
{
    fn as_fornjot_scale_array<S: Span, const FD: usize>(
        &self,
        context: &ExecutionContext<S>,
    ) -> [Float; FD] {
        let mut number_array = context
            .global_resources
            .fornjot_unit_conversion_factor
            .convert_from_vector_to_iter_without_dimension_check(&self.value);

        std::array::from_fn(|_| number_array.next().unwrap())
    }

    pub fn as_fornjot_scale_nalgebra_vector<S: Span, const FD: usize>(
        &self,
        context: &ExecutionContext<S>,
    ) -> NVector<D> {
        let components: [_; FD] = self.as_fornjot_scale_array(context).map(|c| c.into_inner());
        NVector::from_row_slice(&components)
    }

    pub fn as_fornjot_vector<S: Span, const FD: usize>(
        &self,
        context: &ExecutionContext<S>,
    ) -> fj_math::Vector<FD>
    where
        Const<FD>: ToTypenum,
    {
        let components = self
            .as_fornjot_scale_array(context)
            .map(|c| FornjotScalar::from_f64(c.into_inner()));

        fj_math::Vector { components }
    }

    pub fn as_fornjot_point<S: Span, const FD: usize>(
        &self,
        context: &ExecutionContext<S>,
    ) -> fj_math::Point<FD>
    where
        Const<FD>: ToTypenum,
    {
        fj_math::Point {
            coords: self.as_fornjot_vector(context),
        }
    }
}

pub fn register_globals<S: Span>(context: &mut ExecutionContext<S>) {
    fn build_constructor<S: Span, D: DimName>() -> BuiltinFunctionRef<S>
    where
        DefaultAllocator: Allocator<RawFloat, D>,
        Value<S>: From<Vector<D>>,
    {
        let closure: Box<BuiltinFunction<S>> = Box::new(
            |context: &mut ExecutionContext<S>,
             span: &S,
             arguments: Vec<Value<S>>,
             expressions: &[Expression<S>]| {
                let splat_arguments = arguments.to_vec();

                match Vector::<D>::splat.auto_call(context, span, splat_arguments, expressions) {
                    Ok(vector) => Ok(vector),
                    Err(Failure::FunctionCall(_call_failure)) => {
                        Vector::<D>::from_components(context, span, arguments, expressions)
                    }
                    Err(failure) => Err(failure),
                }
            },
        );
        BuiltinFunctionRef::from(closure)
    }

    context
        .stack
        .new_variable_str("vec2", build_constructor::<S, Const<2>>().into());
    context
        .stack
        .new_variable_str("vec3", build_constructor::<S, Const<3>>().into());
    context
        .stack
        .new_variable_str("vec4", build_constructor::<S, Const<4>>().into());
}

#[derive(Debug, Clone, PartialEq)]
pub struct Vector<D: DimName>
where
    DefaultAllocator: Allocator<RawFloat, D>,
{
    pub(super) dimension: Dimension,
    pub(super) value: NVector<D>,
}

impl<S, D: DimName> Object<S> for Vector<D>
where
    S: Span,
    DefaultAllocator: Allocator<RawFloat, D>,
    Vector<D>: NamedObject + Into<Value<S>>,
    Value<S>: AsVariant<Vector<D>> + TryInto<Vector<D>>,
    NVector<D>: Copy,
{
    fn matches_type(
        &self,
        ty: &VariableType<S>,
        _log: &mut dyn RuntimeLog<S>,
        _variable_name_span: &S,
    ) -> OperatorResult<S, bool> {
        Ok(if let VariableType::Vector(dimension, name) = ty {
            *dimension as usize == D::USIZE
                && name.as_str() == Object::<S>::type_name(self).as_ref()
        } else {
            false
        })
    }

    fn type_name(&self) -> Cow<'static, str> {
        get_dimension_name(&self.dimension)
    }

    // TODO add formatting support.

    fn index(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        index: Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        let index = index.downcast::<Scalar>(span)?;
        let index = Number::try_from(index.clone()).map_err(|_| {
            Failure::ExpectedGot(
                span.clone(),
                Number::static_type_name().into(),
                <Scalar as Object<S>>::type_name(&index),
            )
        })?;
        let index = index.to_index();

        if let Some(component) = self.value.get(index as usize).copied() {
            Ok(Float::new(component).unwrap().into())
        } else {
            Err(Failure::IndexOutOfRange(span.clone(), index))
        }
    }

    fn attribute(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        attribute: &S,
    ) -> OperatorResult<S, Value<S>> {
        let swizzle = attribute.as_str();
        let len = swizzle.len();
        let mut chars = swizzle.chars();

        match len {
            1 => {
                let value = self.index_by_character(span, chars.next().unwrap())?;
                Ok(Scalar {
                    dimension: self.dimension,
                    value,
                }
                .into())
            }
            2 => {
                let x = self
                    .index_by_character(span, chars.next().unwrap())?
                    .into_inner();
                let y = self
                    .index_by_character(span, chars.next().unwrap())?
                    .into_inner();

                Ok(Vector::<Const<2>> {
                    dimension: self.dimension,
                    value: NVector::<Const<2>>::new(x, y),
                }
                .into())
            }
            3 => {
                let x = self
                    .index_by_character(span, chars.next().unwrap())?
                    .into_inner();
                let y = self
                    .index_by_character(span, chars.next().unwrap())?
                    .into_inner();
                let z = self
                    .index_by_character(span, chars.next().unwrap())?
                    .into_inner();

                Ok(Vector::<Const<3>> {
                    dimension: self.dimension,
                    value: NVector::<Const<3>>::new(x, y, z),
                }
                .into())
            }
            4 => {
                let x = self
                    .index_by_character(span, chars.next().unwrap())?
                    .into_inner();
                let y = self
                    .index_by_character(span, chars.next().unwrap())?
                    .into_inner();
                let z = self
                    .index_by_character(span, chars.next().unwrap())?
                    .into_inner();
                let w = self
                    .index_by_character(span, chars.next().unwrap())?
                    .into_inner();

                Ok(Vector::<Const<4>> {
                    dimension: self.dimension,
                    value: NVector::<Const<4>>::new(x, y, z, w),
                }
                .into())
            }
            _ => Err(Failure::SwizzleTooLong(span.clone(), len)),
        }
    }

    fn addition(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        rhs: &Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        let rhs = self.unpack_for_addition_or_subtraction(span, rhs)?;

        let value = self.value + rhs.value;
        value.check_nan(span)?;

        Ok(Self {
            dimension: self.dimension,
            value,
        }
        .into())
    }
    fn subtraction(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        rhs: &Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        let rhs = self.unpack_for_addition_or_subtraction(span, rhs)?;

        let value = self.value - rhs.value;
        value.check_nan(span)?;

        Ok(Self {
            dimension: self.dimension,
            value,
        }
        .into())
    }
    fn multiply(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        rhs: &Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        let scalar = rhs.downcast_ref::<Scalar>(span)?;

        let dimension = self.dimension + scalar.dimension;
        let value = self.value * scalar.value.into_inner();
        value.check_nan(span)?;

        Ok(Self { dimension, value }.into())
    }
    fn divide(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        rhs: &Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        let scalar = rhs.downcast_ref::<Scalar>(span)?;

        let dimension = self.dimension - scalar.dimension;
        let value = self.value / scalar.value.into_inner();
        value.check_nan(span)?;

        Ok(Self { dimension, value }.into())
    }
    fn method_call(
        &self,
        context: &mut ExecutionContext<S>,
        span: &S,
        attribute: &S,
        arguments: Vec<Value<S>>,
        expressions: &[Expression<S>],
    ) -> OperatorResult<S, Value<S>> {
        match attribute.as_str() {
            "dot" => |_context: &mut ExecutionContext<S>,
                      span: &S,
                      rhs: Vector<D>|
             -> OperatorResult<S, Value<S>> {
                self.validate_dimensions_match(span, &rhs)?;

                Ok(Scalar {
                    dimension: Dimension::zero(),
                    value: Float::new(self.value.dot(&rhs.value)).unwrap_not_nan(span)?,
                }
                .into())
            }
            .auto_call(context, span, arguments, expressions),
            "cross" => |_context: &mut ExecutionContext<S>,
                        span: &S,
                        rhs: Vector<D>|
             -> OperatorResult<S, Value<S>> {
                self.validate_dimensions_match(span, &rhs)?;
                let value = self.value.cross(&rhs.value);
                value.check_nan(span)?;

                Ok(Self {
                    dimension: self.dimension,
                    value,
                }
                .into())
            }
            .auto_call(context, span, arguments, expressions),
            "abs" => {
                |_context: &mut ExecutionContext<S>, _span: &S| -> OperatorResult<S, Value<S>> {
                    Ok(Self {
                        dimension: self.dimension,
                        value: self.value.abs(),
                    }
                    .into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "add_scalar" => |_context: &mut ExecutionContext<S>,
                             span: &S,
                             scalar: Scalar|
             -> OperatorResult<S, Value<S>> {
                if self.dimension == scalar.dimension {
                    Ok(Self {
                        dimension: self.dimension,
                        value: self.value.add_scalar(*scalar.value),
                    }
                    .into())
                } else {
                    Err(Failure::ExpectedGot(
                        span.clone(),
                        <Self as Object<S>>::type_name(self),
                        Object::<S>::type_name(&scalar),
                    ))
                }
            }
            .auto_call(context, span, arguments, expressions),
            "sub_scalar" => |_context: &mut ExecutionContext<S>,
                             span: &S,
                             scalar: Scalar|
             -> OperatorResult<S, Value<S>> {
                if self.dimension == scalar.dimension {
                    Ok(Self {
                        dimension: self.dimension,
                        value: self.value.add_scalar(-*scalar.value),
                    }
                    .into())
                } else {
                    Err(Failure::ExpectedGot(
                        span.clone(),
                        <Self as Object<S>>::type_name(self),
                        Object::<S>::type_name(&scalar),
                    ))
                }
            }
            .auto_call(context, span, arguments, expressions),
            "angle" => |_context: &mut ExecutionContext<S>,
                        span: &S,
                        rhs: Vector<D>|
             -> OperatorResult<S, Value<S>> {
                // In Radians
                let value = self.value.angle(&rhs.value) / consts::PI;
                let value = Float::new(value).unwrap_not_nan(span)?;
                let dimension = Dimension::angle();

                Ok(Scalar { dimension, value }.into())
            }
            .auto_call(context, span, arguments, expressions),
            "norm" => {
                |_context: &mut ExecutionContext<S>, span: &S| -> OperatorResult<S, Value<S>> {
                    Ok(Scalar {
                        dimension: self.dimension,
                        value: Float::new(self.value.norm()).unwrap_not_nan(span)?,
                    }
                    .into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "norm_squared" => {
                |_context: &mut ExecutionContext<S>, span: &S| -> OperatorResult<S, Value<S>> {
                    Ok(Scalar {
                        dimension: self.dimension,
                        value: Float::new(self.value.norm_squared()).unwrap_not_nan(span)?,
                    }
                    .into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "normalize" => {
                |_context: &mut ExecutionContext<S>, span: &S| -> OperatorResult<S, Value<S>> {
                    let value = self.value.normalize();
                    value.check_nan(span)?;

                    Ok(Self {
                        dimension: Dimension::zero(),
                        value,
                    }
                    .into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            _ => Err(Failure::UnknownAttribute(attribute.clone())),
        }
    }
}

impl NamedObject for Vector<Const<2>> {
    fn static_type_name() -> &'static str {
        "Vector2"
    }
}

impl NamedObject for Vector<Const<3>> {
    fn static_type_name() -> &'static str {
        "Vector3"
    }
}

impl NamedObject for Vector<Const<4>> {
    fn static_type_name() -> &'static str {
        "Vector4"
    }
}

impl<D: DimName> Vector<D>
where
    DefaultAllocator: Allocator<RawFloat, D>,
{
    pub(super) fn check_dimension<S: Span>(
        &self,
        span: &S,
        dimension: Dimension,
    ) -> OperatorResult<S, ()> {
        if self.dimension == dimension {
            Ok(())
        } else {
            Err(Failure::ExpectedGot(
                span.clone(),
                get_dimension_name(&dimension),
                get_dimension_name(&self.dimension),
            ))
        }
    }

    pub(super) fn from_value_ref<'b, S: Span>(
        span: &S,
        value: &'b Value<S>,
        dimension: Dimension,
    ) -> OperatorResult<S, &'b Self>
    where
        Self: NamedObject,
        Value<S>: AsVariant<Self>,
    {
        let value = value.downcast_ref::<Self>(span)?;
        value.check_dimension(span, dimension)?;

        Ok(value)
    }
    fn validate_dimensions_match<S: Span>(&self, span: &S, rhs: &Self) -> OperatorResult<S, ()>
    where
        Vector<D>: NamedObject + Object<S>,
    {
        if self.dimension == rhs.dimension {
            Ok(())
        } else {
            Err(Failure::ExpectedGot(
                span.clone(),
                <Self as Object<S>>::type_name(self),
                <Self as Object<S>>::type_name(rhs),
            ))
        }
    }

    fn unpack_for_addition_or_subtraction<'b, S: Span>(
        &'b self,
        span: &S,
        rhs: &'b Value<S>,
    ) -> OperatorResult<S, &Self>
    where
        Vector<D>: NamedObject + Object<S>,
        Value<S>: AsVariant<Vector<D>>,
    {
        let rhs = rhs.downcast_ref::<Vector<D>>(span)?;
        self.validate_dimensions_match(span, rhs)?;

        Ok(rhs)
    }

    fn index_by_character<S: Span>(&self, span: &S, c: char) -> OperatorResult<S, Float> {
        let index = match c {
            'x' | 'r' => 0,
            'y' | 'g' => 1,
            'z' | 'b' => 2,
            'w' | 'a' => 3,
            _ => return Err(Failure::CharacterNotInVector(span.clone(), c)),
        };

        if let Some(component) = self.value.get(index).copied() {
            Ok(Float::new(component).unwrap())
        } else {
            Err(Failure::CharacterNotInVector(span.clone(), c))
        }
    }

    fn splat<S: Span>(
        _context: &mut ExecutionContext<S>,
        _span: &S,
        splat_value: Scalar,
    ) -> OperatorResult<S, Value<S>>
    where
        Value<S>: From<Self>,
    {
        let dimension = splat_value.dimension;
        let value = NVector::repeat(*splat_value.value);

        Ok(Vector { dimension, value }.into())
    }

    fn from_components<S: Span>(
        _context: &mut ExecutionContext<S>,
        span: &S,
        arguments: Vec<Value<S>>,
        expressions: &[Expression<S>],
    ) -> OperatorResult<S, Value<S>>
    where
        Value<S>: From<Self>,
    {
        let mut expression_iter = expressions.iter();
        let mut argument_iter = arguments.into_iter();

        let mut components = ArrayVec::<RawFloat, 4>::new();

        let first_argument = argument_iter
            .next()
            .ok_or(Failure::MissingArguments(span.clone()))?;
        let first_span = expression_iter.next().unwrap();

        let first_argument: Scalar = first_argument.downcast(first_span.get_span())?;

        let dimension = first_argument.dimension;
        components.push(*first_argument.value);

        for _ in 1..D::USIZE {
            if let Some(value) = argument_iter.next() {
                let value: Scalar = value.downcast(expression_iter.next().unwrap().get_span())?;

                if value.dimension == dimension {
                    components.push(*value.value);
                } else {
                    return Err(Failure::ExpectedGot(
                        span.clone(),
                        get_dimension_name(&dimension),
                        get_dimension_name(&value.dimension),
                    ));
                }
            } else {
                return Err(Failure::MissingArguments(span.clone()));
            }
        }

        if let Some(extra_expression) = expression_iter.next() {
            Err(Failure::TooManyArguments(
                extra_expression.get_span().clone(),
            ))
        } else {
            let components = components.into_iter();
            Ok(Self {
                dimension,
                value: NVector::<D>::from_iterator(components),
            }
            .into())
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use common_data_types::{Dimension, RatioTypeHint, RawFloat};

    use crate::script::{
        execution::{expressions::run_expression, ExecutionContext},
        parsing::Expression,
        Runtime,
    };

    #[test]
    fn construct_vec2() {
        ExecutionContext::create(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(context, &Expression::parse("vec2(1)").unwrap().1),
                Ok(Vector {
                    dimension: Dimension::zero(),
                    value: NVector::<Const<2>>::new(1.0, 1.0),
                }
                .into())
            );
            assert_eq!(
                run_expression(context, &Expression::parse("vec2(1, 2)").unwrap().1),
                Ok(Vector {
                    dimension: Dimension::zero(),
                    value: NVector::<Const<2>>::new(1.0, 2.0),
                }
                .into())
            );
        });
    }

    #[test]
    fn construct_vec3() {
        ExecutionContext::create(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(context, &Expression::parse("vec3(1)").unwrap().1),
                Ok(Vector {
                    dimension: Dimension {
                        length: 0,
                        mass: 0,
                        time: 0,
                        electric_current: 0,
                        thermodynamic_temprature: 0,
                        amount_of_substance: 0,
                        luminous_intensity: 0,
                        ratio_type_hint: RatioTypeHint(0),
                    },
                    value: NVector::<Const<3>>::new(1.0, 1.0, 1.0),
                }
                .into())
            );
            assert_eq!(
                run_expression(context, &Expression::parse("vec3(1, 2, 3)").unwrap().1),
                Ok(Vector {
                    dimension: Dimension {
                        length: 0,
                        mass: 0,
                        time: 0,
                        electric_current: 0,
                        thermodynamic_temprature: 0,
                        amount_of_substance: 0,
                        luminous_intensity: 0,
                        ratio_type_hint: RatioTypeHint(0),
                    },
                    value: NVector::<Const<3>>::new(1.0, 2.0, 3.0),
                }
                .into())
            );
        });
    }

    #[test]
    fn construct_vec4() {
        ExecutionContext::create(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(context, &Expression::parse("vec4(1)").unwrap().1),
                Ok(Vector {
                    dimension: Dimension {
                        length: 0,
                        mass: 0,
                        time: 0,
                        electric_current: 0,
                        thermodynamic_temprature: 0,
                        amount_of_substance: 0,
                        luminous_intensity: 0,
                        ratio_type_hint: RatioTypeHint(0),
                    },
                    value: NVector::<Const<4>>::new(1.0, 1.0, 1.0, 1.0),
                }
                .into())
            );
            assert_eq!(
                run_expression(context, &Expression::parse("vec4(1, 2, 3, 4)").unwrap().1),
                Ok(Vector {
                    dimension: Dimension {
                        length: 0,
                        mass: 0,
                        time: 0,
                        electric_current: 0,
                        thermodynamic_temprature: 0,
                        amount_of_substance: 0,
                        luminous_intensity: 0,
                        ratio_type_hint: RatioTypeHint(0),
                    },
                    value: NVector::<Const<4>>::new(1.0, 2.0, 3.0, 4.0),
                }
                .into())
            );
        });
    }

    #[test]
    fn to_fornjot_vector() {
        ExecutionContext::create(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(context, &Expression::parse("vec2(1mm, 2mm)").unwrap().1)
                    .unwrap()
                    .downcast::<Vector::<Const<2>>>(&"")
                    .map(|v| -> LengthVector2 { v.try_into().unwrap() })
                    .unwrap()
                    .as_fornjot_vector(context),
                fj_math::Vector::<2> {
                    components: [1.0.into(), 2.0.into()]
                }
            );
        });
    }
    #[test]
    fn to_fornjot_point() {
        ExecutionContext::create(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(context, &Expression::parse("vec2(1mm, 2mm)").unwrap().1)
                    .unwrap()
                    .downcast::<Vector::<Const<2>>>(&"")
                    .map(|v| -> LengthVector2 { v.try_into().unwrap() })
                    .unwrap()
                    .as_fornjot_point(context),
                fj_math::Point::<2> {
                    coords: fj_math::Vector::<2> {
                        components: [1.0.into(), 2.0.into()]
                    }
                }
            );
        });
    }

    #[test]
    fn index() {
        ExecutionContext::create(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(context, &Expression::parse("vec2(1, 2)[0]").unwrap().1),
                Ok(Scalar::from_number(Float::new(1.0).unwrap()).into())
            );
            assert_eq!(
                run_expression(context, &Expression::parse("vec2(1, 2)[1]").unwrap().1),
                Ok(Scalar::from_number(Float::new(2.0).unwrap()).into())
            );
            assert_eq!(
                run_expression(context, &Expression::parse("vec2(1, 2)[2]").unwrap().1),
                Err(Failure::IndexOutOfRange("vec2", 2))
            );
            assert_eq!(
                run_expression(context, &Expression::parse("vec2(1, 2)[-1]").unwrap().1),
                Err(Failure::IndexOutOfRange("vec2", -1))
            );
        });
    }

    #[test]
    fn swizzle() {
        ExecutionContext::create(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(context, &Expression::parse("vec4(1, 2, 3, 4).x").unwrap().1),
                Ok(Float::new(1.0).unwrap().into())
            );
            assert_eq!(
                run_expression(context, &Expression::parse("vec4(1, 2, 3, 4).y").unwrap().1),
                Ok(Float::new(2.0).unwrap().into())
            );
            assert_eq!(
                run_expression(context, &Expression::parse("vec4(1, 2, 3, 4).z").unwrap().1),
                Ok(Float::new(3.0).unwrap().into())
            );
            assert_eq!(
                run_expression(context, &Expression::parse("vec4(1, 2, 3, 4).w").unwrap().1),
                Ok(Float::new(4.0).unwrap().into())
            );

            assert_eq!(
                run_expression(context, &Expression::parse("vec4(1, 2, 3, 4).r").unwrap().1),
                Ok(Float::new(1.0).unwrap().into())
            );
            assert_eq!(
                run_expression(context, &Expression::parse("vec4(1, 2, 3, 4).g").unwrap().1),
                Ok(Float::new(2.0).unwrap().into())
            );
            assert_eq!(
                run_expression(context, &Expression::parse("vec4(1, 2, 3, 4).b").unwrap().1),
                Ok(Float::new(3.0).unwrap().into())
            );
            assert_eq!(
                run_expression(context, &Expression::parse("vec4(1, 2, 3, 4).a").unwrap().1),
                Ok(Float::new(4.0).unwrap().into())
            );

            assert_eq!(
                run_expression(
                    context,
                    &Expression::parse("vec4(1, 2, 3, 4).xy").unwrap().1
                ),
                Ok(Vector {
                    dimension: Dimension {
                        length: 0,
                        mass: 0,
                        time: 0,
                        electric_current: 0,
                        thermodynamic_temprature: 0,
                        amount_of_substance: 0,
                        luminous_intensity: 0,
                        ratio_type_hint: RatioTypeHint(0),
                    },
                    value: NVector::<Const<2>>::new(1.0, 2.0),
                }
                .into())
            );
            assert_eq!(
                run_expression(
                    context,
                    &Expression::parse("vec4(1, 2, 3, 4).xyz").unwrap().1
                ),
                Ok(Vector {
                    dimension: Dimension {
                        length: 0,
                        mass: 0,
                        time: 0,
                        electric_current: 0,
                        thermodynamic_temprature: 0,
                        amount_of_substance: 0,
                        luminous_intensity: 0,
                        ratio_type_hint: RatioTypeHint(0),
                    },
                    value: NVector::<Const<3>>::new(1.0, 2.0, 3.0,),
                }
                .into())
            );
            assert_eq!(
                run_expression(
                    context,
                    &Expression::parse("vec4(1, 2, 3, 4).xyzw").unwrap().1
                ),
                Ok(Vector {
                    dimension: Dimension {
                        length: 0,
                        mass: 0,
                        time: 0,
                        electric_current: 0,
                        thermodynamic_temprature: 0,
                        amount_of_substance: 0,
                        luminous_intensity: 0,
                        ratio_type_hint: RatioTypeHint(0),
                    },
                    value: NVector::<Const<4>>::new(1.0, 2.0, 3.0, 4.0,),
                }
                .into())
            );
        });
    }

    #[test]
    fn addition() {
        ExecutionContext::create(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(
                    context,
                    &Expression::parse("vec2(1, 2) + vec2(3, 4)").unwrap().1
                ),
                Ok(Vector {
                    dimension: Dimension::zero(),
                    value: NVector::<Const<2>>::new(4.0, 6.0),
                }
                .into())
            );
        });
    }

    #[test]
    fn subtraction() {
        ExecutionContext::create(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(
                    context,
                    &Expression::parse("vec2(1, 2) - vec2(3, 4)").unwrap().1
                ),
                Ok(Vector {
                    dimension: Dimension::zero(),
                    value: NVector::<Const<2>>::new(-2.0, -2.0),
                }
                .into())
            );
        });
    }

    #[test]
    fn scalar_multiplication() {
        ExecutionContext::create(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(context, &Expression::parse("vec2(1, 2) * 4").unwrap().1),
                Ok(Vector {
                    dimension: Dimension::zero(),
                    value: NVector::<Const<2>>::new(4.0, 8.0),
                }
                .into())
            );
        });
    }

    #[test]
    fn scalar_division() {
        ExecutionContext::create(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(context, &Expression::parse("vec2(1, 2) / 2").unwrap().1),
                Ok(Vector {
                    dimension: Dimension::zero(),
                    value: NVector::<Const<2>>::new(0.5, 1.0),
                }
                .into())
            );
            assert_eq!(
                run_expression(context, &Expression::parse("vec2(1, 2) / 0").unwrap().1),
                Ok(Vector {
                    dimension: Dimension::zero(),
                    value: NVector::<Const<2>>::new(RawFloat::INFINITY, RawFloat::INFINITY,),
                }
                .into())
            );
        });
    }

    #[test]
    fn nan_check() {
        assert_eq!(NVector::<Const<2>>::new(1.0, 1.0).check_nan(&"nan"), Ok(()));
        assert_eq!(
            NVector::<Const<2>>::new(RawFloat::NAN, 1.0).check_nan(&"nan"),
            Err(Failure::ResultIsNan("nan")),
        );
        assert_eq!(
            NVector::<Const<2>>::new(1.0, RawFloat::NAN).check_nan(&"nan"),
            Err(Failure::ResultIsNan("nan")),
        );

        assert_eq!(
            NVector::<Const<3>>::new(1.0, 1.0, 1.0).check_nan(&"nan"),
            Ok(())
        );
        assert_eq!(
            NVector::<Const<3>>::new(RawFloat::NAN, 1.0, 1.0).check_nan(&"nan"),
            Err(Failure::ResultIsNan("nan")),
        );
        assert_eq!(
            NVector::<Const<3>>::new(1.0, RawFloat::NAN, 1.0).check_nan(&"nan"),
            Err(Failure::ResultIsNan("nan")),
        );
        assert_eq!(
            NVector::<Const<3>>::new(1.0, 1.0, RawFloat::NAN).check_nan(&"nan"),
            Err(Failure::ResultIsNan("nan")),
        );
    }

    #[test]
    fn angle() {
        ExecutionContext::create(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(
                    context,
                    &Expression::parse("vec2(0, 1).angle(vec2(1, 0))").unwrap().1
                ),
                Ok(Scalar {
                    dimension: Dimension::angle(),
                    value: Float::new(0.5).unwrap(),
                }
                .into())
            );
        });
    }
}
