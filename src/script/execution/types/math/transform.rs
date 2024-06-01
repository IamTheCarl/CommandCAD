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

use common_data_types::{Dimension, RawFloat};
use enum_downcast::AsVariant;
use nalgebra::{
    allocator::Allocator, Const, DefaultAllocator, Dim, DimName, DimNameDiff, DimNameSub, OPoint,
    U1,
};

use crate::script::{
    execution::{
        types::{
            function::{AutoCall, IntoBuiltinFunction},
            NamedObject, Object, OperatorResult, Value,
        },
        ExecutionContext, Failure,
    },
    logging::RuntimeLog,
    parsing::{Expression, VariableType},
    Span,
};

use super::{
    get_dimension_name,
    vector::{LengthVector, NVector, Vector},
    Angle, Vector2,
};

pub type Transform<D> = nalgebra::OMatrix<RawFloat, D, D>;

pub type Transform2D = Transform<Const<3>>;
pub type Transform3D = Transform<Const<4>>;

pub fn register_globals<S: Span>(context: &mut ExecutionContext<S>) {
    context.stack.new_variable_str(
        "new_transform2D",
        |_context: &mut ExecutionContext<S>, _span: &S| -> OperatorResult<S, Value<S>> {
            Ok(Transform2D::identity().into())
        }
        .into_builtin_function()
        .into(),
    );
    context.stack.new_variable_str(
        "new_transform3D",
        |_context: &mut ExecutionContext<S>, _span: &S| -> OperatorResult<S, Value<S>> {
            Ok(Transform3D::identity().into())
        }
        .into_builtin_function()
        .into(),
    );
}

impl<S, D> Object<S> for Transform<D>
where
    D: DimName + DimNameSub<U1>,
    S: Span,
    DefaultAllocator: Allocator<RawFloat, D>
        + Allocator<RawFloat, <D as DimNameSub<U1>>::Output, <D as DimNameSub<U1>>::Output>
        + Allocator<RawFloat, D, D>
        + Allocator<RawFloat, <D as DimNameSub<U1>>::Output>,
    Self: NamedObject
        + TransformPoint<<D as DimNameSub<U1>>::Output>
        + Rotate
        + CustomMultiply<<D as DimNameSub<U1>>::Output>,
    Vector<D>: Into<Value<S>>,
    Vector<<D as DimNameSub<U1>>::Output>: NamedObject + Into<Value<S>>,
    Value<S>: From<Self>
        + AsVariant<Vector<<D as DimNameSub<U1>>::Output>>
        + TryInto<Vector<<D as DimNameSub<U1>>::Output>>
        + TryInto<LengthVector<<D as DimNameSub<U1>>::Output>>,
{
    fn matches_type(
        &self,
        ty: &VariableType<S>,
        _log: &mut dyn RuntimeLog<S>,
        _variable_name_span: &S,
    ) -> OperatorResult<S, bool> {
        Ok(if let VariableType::Transform(dimension) = ty {
            *dimension as usize == D::USIZE
        } else {
            false
        })
    }

    fn type_name(&self) -> Cow<'static, str> {
        Self::static_type_name().into()
    }

    fn unary_minus(&self, _log: &mut dyn RuntimeLog<S>, _span: &S) -> OperatorResult<S, Value<S>> {
        Ok((-self.clone()).into())
    }
    fn addition(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        rhs: &Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        let translation =
            Vector::<DimNameDiff<D, U1>>::from_value_ref(span, rhs, Dimension::length())?;
        let translation = &translation.value;
        let translation = Self::new_translation(translation);

        Ok((self * translation).into())
    }
    fn subtraction(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        rhs: &Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        let translation =
            Vector::<DimNameDiff<D, U1>>::from_value_ref(span, rhs, Dimension::length())?;

        let translation = &translation.value;
        let translation = Self::new_translation(&-translation);

        Ok((self * translation).into())
    }
    fn multiply(
        &self,
        log: &mut dyn RuntimeLog<S>,
        span: &S,
        rhs: &Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        self.custom_multiply(log, span, rhs)
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
            "apply_to_vector" => |_context: &mut ExecutionContext<S>,
                                  _span: &S,
                                  vector: LengthVector<DimNameDiff<D, U1>>|
             -> OperatorResult<S, Value<S>> {
                let value = self.transform_point(&vector.value);
                Ok(Vector::<DimNameDiff<D, U1>> {
                    dimension: Dimension::length(),
                    value,
                }
                .into())
            }
            .auto_call(context, span, arguments, expressions),
            "rotate" => self.rotate(context, span, arguments, expressions),
            _ => Err(Failure::UnknownAttribute(attribute.clone())),
        }
    }
}

impl NamedObject for Transform2D {
    fn static_type_name() -> &'static str {
        "Transform2D"
    }
}

impl NamedObject for Transform3D {
    fn static_type_name() -> &'static str {
        "Transform3D"
    }
}

trait Rotate {
    fn rotate<S: Span>(
        &self,
        context: &mut ExecutionContext<S>,
        span: &S,
        arguments: Vec<Value<S>>,
        expressions: &[Expression<S>],
    ) -> OperatorResult<S, Value<S>>;
}

impl Rotate for Transform2D {
    fn rotate<S: Span>(
        &self,
        context: &mut ExecutionContext<S>,
        span: &S,
        arguments: Vec<Value<S>>,
        expressions: &[Expression<S>],
    ) -> OperatorResult<S, Value<S>> {
        |_context: &mut ExecutionContext<S>,
         _span: &S,
         angle: Angle|
         -> OperatorResult<S, Value<S>> {
            let angle = angle.value;
            let angle = Self::new_rotation(*angle);

            Ok((self * angle).into())
        }
        .auto_call(context, span, arguments, expressions)
    }
}

impl Rotate for Transform3D {
    fn rotate<S: Span>(
        &self,
        context: &mut ExecutionContext<S>,
        span: &S,
        arguments: Vec<Value<S>>,
        expressions: &[Expression<S>],
    ) -> OperatorResult<S, Value<S>> {
        |_context: &mut ExecutionContext<S>,
         span: &S,
         axis: Vector<Const<3>>,
         angle: Angle|
         -> OperatorResult<S, Value<S>> {
            axis.check_dimension(span, Dimension::zero())?;

            let angle = angle.value;
            let rotation = Self::new_rotation(*angle * axis.value);

            Ok((self * rotation).into())
        }
        .auto_call(context, span, arguments, expressions)
    }
}

trait CustomMultiply<D>
where
    D: Dim,
{
    fn custom_multiply<S: Span>(
        &self,
        log: &mut dyn RuntimeLog<S>,
        span: &S,
        rhs: &Value<S>,
    ) -> OperatorResult<S, Value<S>>;
}

impl CustomMultiply<Const<2>> for Transform2D {
    fn custom_multiply<S: Span>(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        rhs: &Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        let scale = rhs.downcast_ref::<Vector2>(span)?;
        if scale.dimension.is_zero_dimension() {
            let scale = Self::new_nonuniform_scaling(&scale.value);
            Ok((self * scale).into())
        } else {
            Err(Failure::ExpectedGot(
                span.clone(),
                "Number".into(),
                get_dimension_name(&scale.dimension),
            ))
        }
    }
}

impl CustomMultiply<Const<3>> for Transform3D {
    fn custom_multiply<S: Span>(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        rhs: &Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        match rhs {
            Value::Vector3(scale) => {
                if scale.dimension.is_zero_dimension() {
                    let scale = Self::new_nonuniform_scaling(&scale.value);
                    Ok((self * scale).into())
                } else {
                    Err(Failure::ExpectedGot(
                        span.clone(),
                        "Number".into(),
                        get_dimension_name(&scale.dimension),
                    ))
                }
            }
            Value::Quaternion(quat) => Ok((self * quat.to_homogeneous()).into()),
            _ => Err(Failure::ExpectedGot(
                span.clone(),
                "Scalar Vector3 or Quaternion".into(),
                Object::type_name(rhs),
            )),
        }
    }
}

trait TransformPoint<D: DimName>
where
    DefaultAllocator: Allocator<f64, D>,
{
    fn transform_point(&self, pt: &NVector<D>) -> NVector<D>;
}

impl TransformPoint<Const<2>> for Transform2D
where
    DefaultAllocator: Allocator<RawFloat, Const<2>> + Allocator<RawFloat, Const<2>, Const<2>>,
{
    fn transform_point(&self, pt: &NVector<Const<2>>) -> NVector<Const<2>> {
        let point = OPoint { coords: *pt };
        let point = self.transform_point(&point);
        point.coords
    }
}

impl TransformPoint<Const<3>> for Transform3D
where
    DefaultAllocator: Allocator<RawFloat, Const<3>> + Allocator<RawFloat, Const<3>, Const<3>>,
{
    fn transform_point(&self, pt: &NVector<Const<3>>) -> NVector<Const<3>> {
        let point = OPoint { coords: *pt };
        let point = self.transform_point(&point);
        point.coords
    }
}

#[cfg(test)]
mod test {
    use crate::script::{
        execution::{expressions::run_expression, types::math::vector::NVector},
        Runtime,
    };

    use super::*;

    #[test]
    fn translate() {
        ExecutionContext::create(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(
                    context,
                    &Expression::parse(
                        "(new_transform2D() + vec2(1m, 2m)).apply_to_vector(vec2(0m, 0m))"
                    )
                    .unwrap()
                    .1
                ),
                Ok(Vector {
                    dimension: Dimension::length(),
                    value: NVector::<Const<2>>::new(1.0, 2.0),
                }
                .into())
            );

            assert_eq!(
                run_expression(
                    context,
                    &Expression::parse(
                        "(new_transform2D() - vec2(1m, 2m)).apply_to_vector(vec2(0m, 0m))"
                    )
                    .unwrap()
                    .1
                ),
                Ok(Vector {
                    dimension: Dimension::length(),
                    value: NVector::<Const<2>>::new(-1.0, -2.0),
                }
                .into())
            );

            assert_eq!(
                run_expression(
                    context,
                    &Expression::parse(
                        "(new_transform2D() + vec2(1m, 2m)).apply_to_vector(vec2(1m, 2m))"
                    )
                    .unwrap()
                    .1
                ),
                Ok(Vector {
                    dimension: Dimension::length(),
                    value: NVector::<Const<2>>::new(2.0, 4.0),
                }
                .into())
            );

            assert_eq!(
                run_expression(
                    context,
                    &Expression::parse(
                        "(new_transform2D() - vec2(1m, 2m)).apply_to_vector(vec2(2m, 3m))"
                    )
                    .unwrap()
                    .1
                ),
                Ok(Vector {
                    dimension: Dimension::length(),
                    value: NVector::<Const<2>>::new(1.0, 1.0),
                }
                .into())
            );
        });
    }

    #[test]
    fn scale() {
        ExecutionContext::create(&mut Runtime::default(), |context| {
            assert_eq!(
                run_expression(
                    context,
                    &Expression::parse(
                        "(new_transform2D() * vec2(1, 2)).apply_to_vector(vec2(2m, 2m))"
                    )
                    .unwrap()
                    .1
                ),
                Ok(Vector {
                    dimension: Dimension::length(),
                    value: NVector::<Const<2>>::new(2.0, 4.0),
                }
                .into())
            );
        });
    }

    #[test]
    fn rotate_2d() {
        ExecutionContext::create(&mut Runtime::default(), |context| {
            let result = run_expression(
                context,
                &Expression::parse(
                    "(new_transform2D().rotate(90deg)).apply_to_vector(vec2(1m, 0m))",
                )
                .unwrap()
                .1,
            )
            .unwrap();
            let vector = result.downcast::<Vector<Const<2>>>(&"").unwrap();

            assert!((vector.value - NVector::<Const<2>>::new(0.0, 1.0)).norm() < 0.01);
        });
    }

    #[test]
    fn rotate_3d() {
        ExecutionContext::create(&mut Runtime::default(), |context| {
            let result = run_expression(
            context,
        &        Expression::parse(
                    "(new_transform3D().rotate(vec3(0, 0, 1), 90deg)).apply_to_vector(vec3(1m, 0m, 0m))",
                )
                .unwrap()
                .1,
        )
        .unwrap();
            let vector = result.downcast::<Vector<Const<3>>>(&"").unwrap();

            assert!((vector.value - NVector::<Const<3>>::new(0.0, 1.0, 0.0)).norm() < 0.01);
        });
    }
}
