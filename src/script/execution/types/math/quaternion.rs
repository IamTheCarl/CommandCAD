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
use nalgebra::Unit;

use crate::script::{
    execution::{
        types::{function::IntoBuiltinFunction, NamedObject, Object, OperatorResult, Value},
        ExecutionContext,
    },
    logging::RuntimeLog,
    parsing::VariableType,
    Span,
};

use super::{Angle, Scalar, Vector3};

pub type Quaternion = nalgebra::UnitQuaternion<RawFloat>;

pub fn register_globals<S: Span>(context: &mut ExecutionContext<S>) {
    context.stack.new_variable_str(
        "new_quaternion",
        |_context: &mut ExecutionContext<S>, _span: &S| -> OperatorResult<S, Value<S>> {
            Ok(Quaternion::identity().into())
        }
        .into_builtin_function()
        .into(),
    );

    context.stack.new_variable_str(
        "vector_to_quaternion",
        |_context: &mut ExecutionContext<S>,
         span: &S,
         axis: Vector3|
         -> OperatorResult<S, Value<S>> {
            axis.check_dimension(span, Dimension::zero())?;
            Ok(Quaternion::new(axis.value).into())
        }
        .into_builtin_function()
        .into(),
    );

    context.stack.new_variable_str(
        "axis_quaternion",
        |_context: &mut ExecutionContext<S>,
         span: &S,
         axis: Vector3,
         angle: Angle|
         -> OperatorResult<S, Value<S>> {
            axis.check_dimension(span, Dimension::zero())?;

            let angle = angle.value;

            Ok(Quaternion::from_axis_angle(&Unit::new_normalize(axis.value), *angle).into())
        }
        .into_builtin_function()
        .into(),
    );
}

impl<S> Object<S> for Quaternion
where
    S: Span,
{
    fn matches_type(
        &self,
        ty: &VariableType<S>,
        _log: &mut dyn RuntimeLog<S>,
        _variable_name_span: &S,
    ) -> OperatorResult<S, bool> {
        Ok(matches!(ty, VariableType::Quaternion))
    }

    fn type_name(&self) -> Cow<'static, str> {
        Self::static_type_name().into()
    }

    fn addition(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        rhs: &Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        let rhs = rhs.downcast_ref::<Self>(span)?;

        Ok(Quaternion::new_normalize(self.into_inner() * rhs.into_inner()).into())
    }
    fn subtraction(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        rhs: &Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        let rhs = rhs.downcast_ref::<Self>(span)?;

        Ok(Quaternion::new_normalize(self.into_inner() * rhs.inverse().into_inner()).into())
    }
}

impl NamedObject for Quaternion {
    fn static_type_name() -> &'static str {
        "Quaternion"
    }
}

#[cfg(test)]
mod test {
    use nalgebra::Const;

    use crate::script::{
        execution::{
            expressions::run_expression,
            types::math::vector::{NVector, Vector},
        },
        parsing::Expression,
        Runtime,
    };

    use super::*;

    #[test]
    fn rotate() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            let result = run_expression(
            context,
            &
                Expression::parse(
                    "(new_transform3D() * axis_quaternion(vec3(0, 0, 1), 90deg)).apply_to_vector(vec3(1m, 0m, 0m))",
                )
                .unwrap()
                .1,
        )
        .unwrap();
            let vector = result.downcast::<Vector<Const<3>>>(&"").unwrap();

            assert!((vector.value - NVector::<Const<3>>::new(0.0, 1.0, 0.0)).norm() < 0.01);
        });
    }

    #[test]
    fn addition() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            let result = run_expression(
            context,
                &Expression::parse(
                    "(new_transform3D() * (axis_quaternion(vec3(0, 0, 1), 45deg) + axis_quaternion(vec3(0, 0, 1), 45deg))).apply_to_vector(vec3(1m, 0m, 0m))",
                )
                .unwrap()
                .1,
        )
        .unwrap();
            let vector = result.downcast::<Vector<Const<3>>>(&"").unwrap();

            assert!((vector.value - NVector::<Const<3>>::new(0.0, 1.0, 0.0)).norm() < 0.01);
        });
    }

    #[test]
    fn subtraction() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            let result = run_expression(
            context,
                &Expression::parse(
                    "(new_transform3D() * (axis_quaternion(vec3(0, 0, 1), 45deg) - axis_quaternion(vec3(0, 0, 1), 45deg))).apply_to_vector(vec3(1m, 0m, 0m))",
                )
                .unwrap()
                .1,
        )
        .unwrap();
            let vector = result.downcast::<Vector<Const<3>>>(&"").unwrap();

            assert!((dbg!(vector.value) - NVector::<Const<3>>::new(1.0, 0.0, 0.0)).norm() < 0.01);
        });
    }
}
