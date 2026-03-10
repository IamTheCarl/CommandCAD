/*
 * Copyright 2026 James Carl
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

use common_data_types::RawFloat;
use enum_downcast::{AsVariant, IntoVariant};
use nalgebra::{Matrix3, Matrix4, Rotation2, Rotation3, Translation2, Translation3, Unit};

use crate::{
    build_method,
    execution::errors::Raise as _,
    values::{
        scalar::Angle,
        vector::{Length2, Length3, Zero2, Zero3},
        BuiltinCallableDatabase, BuiltinFunction, MissingAttributeError, Object, StaticType,
        StaticTypeName, Style, Value, ValueType, Vector2, Vector3,
    },
    ExecutionContext, ExecutionResult,
};

type Float = RawFloat;

pub type Transform2d = Transform<Matrix3<Float>>;
pub type Transform3d = Transform<Matrix4<Float>>;

#[derive(Debug, Hash, Clone, Copy, PartialEq)]
pub struct Transform<T>(pub T);

impl<T> Transform<T> {
    pub fn new(inner: T) -> Self {
        Self(inner)
    }
}

impl<I> Eq for Transform<I> where I: PartialEq {}

impl<I> Object for Transform<I>
where
    I: TransformInternalType,
    Self: StaticTypeName + Into<Value>,
    Value: IntoVariant<Self> + AsVariant<Self>,
{
    fn get_type(&self, _context: &ExecutionContext) -> ValueType {
        I::get_type()
    }

    fn format(
        &self,
        _context: &ExecutionContext,
        f: &mut dyn std::fmt::Write,
        _style: Style,
        _precision: Option<u8>,
    ) -> std::fmt::Result {
        write!(f, "{}", self.type_name())
    }

    fn unary_minus(self, _context: &ExecutionContext) -> ExecutionResult<Value> {
        Ok(Self(self.0.invert()).into())
    }

    fn eq(self, context: &ExecutionContext, rhs: Value) -> ExecutionResult<bool> {
        let rhs: Self = rhs.downcast_for_binary_op(context)?;
        Ok(self == rhs)
    }

    fn get_attribute(&self, context: &ExecutionContext, attribute: &str) -> ExecutionResult<Value> {
        match attribute {
            "rotate" => Ok(BuiltinFunction::new::<
                <<I as TransformInternalType>::MethodSet as methods::MethodSet>::Rotate,
            >()
            .into()),
            "translate" => Ok(BuiltinFunction::new::<
                <<I as TransformInternalType>::MethodSet as methods::MethodSet>::Translate,
            >()
            .into()),
            "scale" => Ok(BuiltinFunction::new::<
                <<I as TransformInternalType>::MethodSet as methods::MethodSet>::Scale,
            >()
            .into()),
            _ => Err(MissingAttributeError {
                name: attribute.into(),
            }
            .to_error(context)),
        }
    }
}

impl<I> StaticTypeName for Transform<I>
where
    I: StaticTypeName,
{
    fn static_type_name() -> Cow<'static, str> {
        I::static_type_name()
    }
}

impl<I> StaticType for Transform<I>
where
    I: StaticType,
{
    fn static_type() -> ValueType {
        I::static_type()
    }
}

mod methods {
    pub trait MethodSet {
        type Rotate;
        type Translate;
        type Scale;
    }

    macro_rules! build_method_set {
        ($name:ident) => {
            paste::paste! {
                pub struct [<$name Rotate>];
                pub struct [<$name Translate>];
                pub struct [<$name Scale>];

                pub struct [<$name MethodSet>];
                impl MethodSet for [<$name MethodSet>] {
                    type Rotate = [<$name Rotate>];
                    type Translate = [<$name Translate>];
                    type Scale = [<$name Scale>];
                }
            }
        };
    }

    build_method_set!(Transform2d);
    build_method_set!(Transform3d);
}

pub fn register_methods(database: &mut BuiltinCallableDatabase) {
    // Transform2d
    build_method!(
        database,
        <methods::Transform2dMethodSet as methods::MethodSet>::Rotate, "Transform2d::rotate", (
            context: &ExecutionContext,
            this: Transform2d,
            angle: Angle) -> Transform2d
        {
            let rotation = Rotation2::new(*angle.value);
            Ok(Transform(this.0 * rotation.to_homogeneous()))
        }
    );
    build_method!(
        database,
        <methods::Transform2dMethodSet as methods::MethodSet>::Translate, "Transform2d::translate", (
            context: &ExecutionContext,
            this: Transform2d,
            offset: Length2) -> Transform2d
        {
            let offset: Vector2 = offset.into();
            let offset = offset.raw_value();

            let translation = Translation2::from(offset);
            Ok(Transform(this.0 * translation.to_homogeneous()))
        }
    );
    build_method!(
        database,
        <methods::Transform2dMethodSet as methods::MethodSet>::Scale, "Transform2d::scale", (
            context: &ExecutionContext,
            this: Transform2d,
            scale: Zero2) -> Transform2d
        {
            let scale: Vector2 = scale.into();
            let scale = scale.raw_value();

            Ok(Transform(this.0 * Matrix3::new_nonuniform_scaling(&scale)))
        }
    );

    // Transform3d
    build_method!(
        database,
        <methods::Transform3dMethodSet as methods::MethodSet>::Rotate, "Transform3d::rotate", (
            context: &ExecutionContext,
            this: Transform3d,
            axis: Zero3,
            angle: Angle) -> Transform3d
        {
            let axis: Vector3 = axis.into();
            let axis = axis.raw_value();
            let axis = Unit::new_normalize(axis);

            let rotation = Rotation3::from_axis_angle(&axis, *angle.value);
            Ok(Transform(this.0 * rotation.to_homogeneous()))
        }
    );
    build_method!(
        database,
        <methods::Transform3dMethodSet as methods::MethodSet>::Translate, "Transform3d::translate", (
            context: &ExecutionContext,
            this: Transform3d,
            offset: Length3) -> Transform3d
        {
            let offset: Vector3 = offset.into();
            let offset = offset.raw_value();

            let translation = Translation3::from(offset);
            Ok(Transform(this.0 * translation.to_homogeneous()))
        }
    );
    build_method!(
        database,
        <methods::Transform3dMethodSet as methods::MethodSet>::Scale, "Transform3d::scale", (
            context: &ExecutionContext,
            this: Transform3d,
            scale: Zero3) -> Transform3d
        {
            let scale: Vector3 = scale.into();
            let scale = scale.raw_value();

            Ok(Transform(this.0 * Matrix4::new_nonuniform_scaling(&scale)))
        }
    );
}

pub(crate) trait TransformInternalType:
    Copy + PartialEq + StaticTypeName + StaticType + 'static
{
    type MethodSet: methods::MethodSet;

    fn get_type() -> ValueType;
    fn invert(&self) -> Self;
}

impl TransformInternalType for Matrix3<Float> {
    type MethodSet = methods::Transform2dMethodSet;

    fn get_type() -> ValueType {
        ValueType::Transform2d
    }

    fn invert(&self) -> Self {
        self.try_inverse().expect("Matrix wasn't square")
    }
}

impl StaticTypeName for Matrix3<Float> {
    fn static_type_name() -> Cow<'static, str> {
        "Transform2d".into()
    }
}

impl StaticType for Matrix3<Float> {
    fn static_type() -> ValueType {
        ValueType::Transform2d
    }
}

impl TransformInternalType for Matrix4<Float> {
    type MethodSet = methods::Transform3dMethodSet;

    fn get_type() -> ValueType {
        ValueType::Transform3d
    }

    fn invert(&self) -> Self {
        self.try_inverse().expect("Matrix wasn't square")
    }
}

impl StaticTypeName for Matrix4<Float> {
    fn static_type_name() -> Cow<'static, str> {
        "Transform3d".into()
    }
}

impl StaticType for Matrix4<Float> {
    fn static_type() -> ValueType {
        ValueType::Transform3d
    }
}

#[cfg(test)]
mod test {
    use crate::execution::{test_run, values::Boolean};
    use pretty_assertions::assert_eq;

    #[test]
    fn identity_transform2d() {
        let product =
            test_run("<(1m, 2m)>::transform(t = std.consts.Transform2d) == <(1m, 2m)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn identity_transform3d() {
        let product =
            test_run("<(1m, 2m, 3m)>::transform(t = std.consts.Transform3d) == <(1m, 2m, 3m)>")
                .unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn translate2d() {
        let product =
            test_run("<(1m, 2m)>::transform(t = std.consts.Transform2d::translate(offset = <(2m, 4m)>)) == <(3m, 6m)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn translate3d() {
        let product =
            test_run("<(1m, 2m, 3m)>::transform(t = std.consts.Transform3d::translate(offset = <(2m, 4m, 6m)>)) == <(3m, 6m, 9m)>")
                .unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn rotate2d() {
        let product =
            test_run("let rotated = <(0m, 1m)>::transform(t = std.consts.Transform2d::rotate(angle = 90deg)); x = (rotated.x + 1m)::abs() < 0.0001m; y = rotated.y::abs() < 0.0001m; in x && y").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn rotate3d() {
        let product =
            test_run("let rotated = <(0m, 1m, 0m)>::transform(t = std.consts.Transform3d::rotate(axis = <(0, 0, 1)>, angle = 90deg)); x = (rotated.x + 1m)::abs() < 0.0001m; y = rotated.y::abs() < 0.0001m; z = rotated.z::abs() < 0.0001m; in x && y && z")
                .unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn scale2d() {
        let product =
            test_run("<(1m, 2m)>::transform(t = std.consts.Transform2d::scale(scale = <(3, 4)>)) == <(3m, 8m)>").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn scale3d() {
        let product =
            test_run("<(1m, 2m, 3m)>::transform(t = std.consts.Transform3d::scale(scale = <(3, 4, 5)>)) == <(3m, 8m, 15m)>")
                .unwrap();
        assert_eq!(product, Boolean(true).into());
    }
}
