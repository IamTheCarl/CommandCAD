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

use std::sync::Arc;

use common_data_types::{Dimension, RawFloat};
use nalgebra::Matrix3;

use crate::{
    execution::errors::Raise,
    values::{
        iterators::IterableObject, BuiltinCallableDatabase, BuiltinFunction, MissingAttributeError,
        Object, StaticType, StaticTypeName, Style, Value, ValueType, Vector2,
    },
    ExecutionContext, ExecutionResult,
};

// Add
// Subtract
// BitOr
// BitXor
// BitAnd
// We need a way to collect a list of polygons into a multi-polygon
// Ray-cast with linestrings and polygons
// to_svg
// from_svg?
// text?
// polygons from fonts
// projection - should probably live in manifold
// slice - should probably live in manifold
// bounding rec - manifold should also have one

/// Applies a transformation to an object.
trait ApplyTransform {
    fn apply_transform(&mut self, transform: &Matrix3<RawFloat>);
}

#[derive(Debug, Clone)]
pub struct LineString(Arc<geo::LineString>);

impl Object for LineString {
    fn get_type(&self, _context: &ExecutionContext) -> ValueType {
        ValueType::LineString
    }

    fn format(
        &self,
        context: &ExecutionContext,
        f: &mut dyn std::fmt::Write,
        style: Style,
        precision: Option<u8>,
    ) -> std::fmt::Result {
        todo!()
    }

    fn get_attribute(
        &self,
        context: &ExecutionContext,
        attribute: &str,
    ) -> crate::ExecutionResult<super::Value> {
        match attribute {
            "append" => {
                Ok(BuiltinFunction::new::<methods_and_functions::line_string::Append>().into())
            }
            "add_point" => {
                Ok(BuiltinFunction::new::<methods_and_functions::line_string::AddPoint>().into())
            }
            "is_closed" => {
                Ok(BuiltinFunction::new::<methods_and_functions::line_string::IsClosed>().into())
            }
            "close" => {
                Ok(BuiltinFunction::new::<methods_and_functions::line_string::Close>().into())
            }
            "open" => Ok(BuiltinFunction::new::<methods_and_functions::line_string::Open>().into()),
            "iter_points" => {
                Ok(BuiltinFunction::new::<methods_and_functions::line_string::IterPoints>().into())
            }
            "rev_iter_points" => Ok(BuiltinFunction::new::<
                methods_and_functions::line_string::RevIterPoints,
            >()
            .into()),
            "num_points" => {
                Ok(BuiltinFunction::new::<methods_and_functions::line_string::NumPoints>().into())
            }
            "length" => Ok(
                BuiltinFunction::new::<methods_and_functions::line_string::StringLength>().into(),
            ),
            "bounding_box" => Ok(BuiltinFunction::new::<
                methods_and_functions::line_string::BoundingBox,
            >()
            .into()),
            "centroid" => {
                Ok(BuiltinFunction::new::<methods_and_functions::line_string::Centroid>().into())
            }
            "transform" => {
                Ok(BuiltinFunction::new::<methods_and_functions::line_string::Transform>().into())
            }
            _ => Err(MissingAttributeError {
                name: attribute.into(),
            }
            .to_error(context)),
        }
    }
}

impl std::cmp::Eq for LineString {}

impl std::cmp::PartialEq for LineString {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl StaticTypeName for LineString {
    fn static_type_name() -> std::borrow::Cow<'static, str> {
        "LineString".into()
    }
}

impl StaticType for LineString {
    fn static_type() -> ValueType {
        ValueType::LineString
    }
}

impl ApplyTransform for geo::LineString {
    fn apply_transform(&mut self, transform: &Matrix3<RawFloat>) {
        for point in self.coords_mut() {
            let new_point = transform.transform_point(&nalgebra::Point {
                coords: nalgebra::Vector2::new(point.x, point.y),
            });

            *point = geo::Coord {
                x: new_point.x,
                y: new_point.y,
            };
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LineStringIterator {
    line_string: LineString,
}

impl IterableObject for LineStringIterator {
    fn iterate<R>(
        &self,
        context: &ExecutionContext,
        callback: impl FnOnce(&mut dyn Iterator<Item = ExecutionResult<Value>>) -> ExecutionResult<R>,
    ) -> ExecutionResult<R> {
        let mut iter = self.line_string.0.coords().map(|point| {
            Vector2::new(context, Dimension::length(), [point.x, point.y])
                .map(|vector| vector.into())
        });
        callback(&mut iter)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct RevLineStringIterator {
    line_string: LineString,
}

impl IterableObject for RevLineStringIterator {
    fn iterate<R>(
        &self,
        context: &ExecutionContext,
        callback: impl FnOnce(&mut dyn Iterator<Item = ExecutionResult<Value>>) -> ExecutionResult<R>,
    ) -> ExecutionResult<R> {
        let mut iter = self.line_string.0.coords().rev().map(|point| {
            Vector2::new(context, Dimension::length(), [point.x, point.y])
                .map(|vector| vector.into())
        });
        callback(&mut iter)
    }
}

#[derive(Debug, Clone)]
pub struct Polygon(Arc<geo::Polygon>);

impl Object for Polygon {
    fn get_type(&self, context: &ExecutionContext) -> ValueType {
        ValueType::Polygon
    }

    fn format(
        &self,
        context: &ExecutionContext,
        f: &mut dyn std::fmt::Write,
        style: Style,
        precision: Option<u8>,
    ) -> std::fmt::Result {
        todo!()
    }

    fn get_attribute(
        &self,
        context: &ExecutionContext,
        attribute: &str,
    ) -> crate::ExecutionResult<super::Value> {
        match attribute {
            "exterior" => {
                Ok(BuiltinFunction::new::<methods_and_functions::polygon::Exterior>().into())
            }
            "interiors" => {
                Ok(BuiltinFunction::new::<methods_and_functions::polygon::Interiors>().into())
            }
            "num_interiors" => {
                Ok(BuiltinFunction::new::<methods_and_functions::polygon::NumInteriors>().into())
            }
            "is_convex" => {
                Ok(BuiltinFunction::new::<methods_and_functions::polygon::IsConvex>().into())
            }
            "area" => Ok(BuiltinFunction::new::<methods_and_functions::polygon::Area>().into()),
            "bounding_box" => {
                Ok(BuiltinFunction::new::<methods_and_functions::polygon::BoundingBox>().into())
            }
            "centroid" => {
                Ok(BuiltinFunction::new::<methods_and_functions::polygon::Centroid>().into())
            }
            "transform" => {
                Ok(BuiltinFunction::new::<methods_and_functions::polygon::Transform>().into())
            }
            "" => Ok(BuiltinFunction::new::<methods_and_functions::polygon::IntoSet>().into()),
            _ => Err(MissingAttributeError {
                name: attribute.into(),
            }
            .to_error(context)),
        }
    }
}

impl std::cmp::Eq for Polygon {}

impl std::cmp::PartialEq for Polygon {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl StaticTypeName for Polygon {
    fn static_type_name() -> std::borrow::Cow<'static, str> {
        "Polygon".into()
    }
}

impl StaticType for Polygon {
    fn static_type() -> ValueType {
        ValueType::Polygon
    }
}

impl ApplyTransform for geo::Polygon {
    fn apply_transform(&mut self, transform: &Matrix3<RawFloat>) {
        self.exterior_mut(|exterior| exterior.apply_transform(transform));

        self.interiors_mut(|interiors| {
            for interior in interiors {
                interior.apply_transform(transform);
            }
        });
    }
}

#[derive(Debug, Clone)]
pub struct PolygonSet(Arc<geo::MultiPolygon>);

impl Object for PolygonSet {
    fn get_type(&self, context: &ExecutionContext) -> ValueType {
        ValueType::PolygonSet
    }

    fn format(
        &self,
        context: &ExecutionContext,
        f: &mut dyn std::fmt::Write,
        style: Style,
        precision: Option<u8>,
    ) -> std::fmt::Result {
        todo!()
    }

    fn get_attribute(
        &self,
        context: &ExecutionContext,
        attribute: &str,
    ) -> crate::ExecutionResult<super::Value> {
        match attribute {
            "iter_poly" => {
                Ok(BuiltinFunction::new::<methods_and_functions::polygon_set::IterPoly>().into())
            }
            "num_poly" => {
                Ok(BuiltinFunction::new::<methods_and_functions::polygon_set::NumPoly>().into())
            }
            "area" => Ok(BuiltinFunction::new::<methods_and_functions::polygon_set::Area>().into()),
            "bounding_box" => Ok(BuiltinFunction::new::<
                methods_and_functions::polygon_set::BoundingBox,
            >()
            .into()),
            "centroid" => {
                Ok(BuiltinFunction::new::<methods_and_functions::polygon_set::Centroid>().into())
            }
            "transform" => {
                Ok(BuiltinFunction::new::<methods_and_functions::polygon_set::Transform>().into())
            }
            _ => Err(MissingAttributeError {
                name: attribute.into(),
            }
            .to_error(context)),
        }
    }
}

impl ApplyTransform for geo::MultiPolygon {
    fn apply_transform(&mut self, transform: &Matrix3<RawFloat>) {
        for polygon in self.0.iter_mut() {
            polygon.apply_transform(transform);
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct InteriorIterator {
    polygon: Polygon,
}

impl IterableObject for InteriorIterator {
    fn iterate<R>(
        &self,
        _context: &ExecutionContext,
        callback: impl FnOnce(&mut dyn Iterator<Item = ExecutionResult<Value>>) -> ExecutionResult<R>,
    ) -> ExecutionResult<R> {
        let mut iter = self
            .polygon
            .0
            .interiors()
            .iter()
            .map(|string| LineString(Arc::new(string.clone())).into())
            .map(Ok);
        callback(&mut iter)
    }
}

impl std::cmp::Eq for PolygonSet {}

impl std::cmp::PartialEq for PolygonSet {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl StaticTypeName for PolygonSet {
    fn static_type_name() -> std::borrow::Cow<'static, str> {
        "PolygonSet".into()
    }
}

impl StaticType for PolygonSet {
    fn static_type() -> ValueType {
        ValueType::PolygonSet
    }
}

pub mod methods_and_functions {
    use common_data_types::{Dimension, Float, RawFloat};
    use geo::{Area, BoundingRect, Centroid, Point, Rect};
    use imstr::ImString;

    use super::{InteriorIterator, LineString, Polygon, PolygonSet};
    use crate::values::scalar::UnwrapNotNan;
    use crate::values::Value;
    use crate::ExecutionResult;
    use crate::{build_function, build_method, values::BuiltinCallableDatabase};
    use crate::{
        values::{
            iterators::ValueIterator,
            scalar::Scalar,
            vector::{Length2, Vector2},
            Boolean, Dictionary, List, Transform2d, UnsignedInteger,
        },
        ExecutionContext,
    };
    use std::{collections::HashMap, sync::Arc};

    fn bounding_box<B>(context: &ExecutionContext, bound: &B) -> ExecutionResult<Option<Dictionary>>
    where
        B: BoundingRect<RawFloat, Output = Option<Rect<RawFloat>>>,
    {
        if let Some(rect) = bound.bounding_rect() {
            let min = rect.min();
            let max = rect.max();

            let rectangle: HashMap<ImString, Value> = HashMap::from_iter([
                (
                    "min".into(),
                    Vector2::new(context, Dimension::length(), [min.x, min.y])?.into(),
                ),
                (
                    "max".into(),
                    Vector2::new(context, Dimension::length(), [max.x, max.y])?.into(),
                ),
            ]);

            Ok(Some(Dictionary::new(context, rectangle)))
        } else {
            Ok(None)
        }
    }

    fn centroid<C>(context: &ExecutionContext, centroid: &C) -> ExecutionResult<Option<Vector2>>
    where
        C: Centroid<Output = Option<Point<RawFloat>>>,
    {
        if let Some(centroid) = centroid.centroid() {
            Ok(Some(Vector2::new(
                context,
                Dimension::length(),
                [centroid.x(), centroid.y()],
            )?))
        } else {
            Ok(None)
        }
    }

    fn area<A>(context: &ExecutionContext, area: &A) -> ExecutionResult<Scalar>
    where
        A: Area<RawFloat>,
    {
        let area = area.unsigned_area();
        Ok(Scalar {
            dimension: Dimension::area(),
            value: Float::new(area).unwrap_not_nan(context)?,
        })
    }

    pub mod line_string {

        use common_data_types::{Dimension, Float};
        use geo::{Distance as _, Euclidean};

        use crate::values::polygon::ApplyTransform;
        use crate::values::scalar::UnwrapNotNan as _;

        use super::super::{LineStringIterator, RevLineStringIterator};
        use super::*;

        pub struct FromPoints;
        pub struct Append;
        pub struct AddPoint;
        pub struct IsClosed;
        pub struct Close;
        pub struct Open;
        pub struct IterPoints;
        pub struct RevIterPoints;
        pub struct NumPoints;
        pub struct StringLength;
        pub struct BoundingBox;
        pub struct Centroid;
        pub struct Transform;

        pub fn register_methods_and_functions(database: &mut BuiltinCallableDatabase) {
            build_function!(
                database,
                FromPoints, "LineString::from_points", (
                    context: &ExecutionContext,
                    points: List) -> LineString
                {
                    let mut collected = Vec::with_capacity(points.len());
                    for value in points {
                        let point: Length2 = value.downcast(context)?;
                        let point: Vector2 = point.into();
                        collected.push(geo::Coord { x: point.raw_value().x, y: point.raw_value().y });
                    }

                    Ok(LineString(Arc::new(geo::LineString(collected))))
                }
            );
            build_method!(
                database,
                Append, "LineString::append", (
                    context: &ExecutionContext,
                    this: LineString,
                    other: LineString) -> LineString
                {
                    let mut this = this;
                    let line_string = Arc::make_mut(&mut this.0);
                    for point in other.0.coords() {
                        line_string.0.push(*point);
                    }

                    Ok(this)
                }
            );
            build_method!(
                database,
                AddPoint, "LineString::add_point", (
                    context: &ExecutionContext,
                    this: LineString,
                    p: Length2) -> LineString
                {
                    let mut this = this;
                    let line_string = Arc::make_mut(&mut this.0);

                    let point: Vector2 = p.into();
                    line_string.0.push(geo::Coord { x: point.raw_value().x, y: point.raw_value().y });

                    Ok(this)
                }
            );
            build_method!(
                database,
                IsClosed, "LineString::is_closed", (
                    context: &ExecutionContext,
                    this: LineString) -> Boolean
                {
                    Ok(Boolean(this.0.is_closed()))
                }
            );
            build_method!(
                database,
                Close, "LineString::close", (
                    context: &ExecutionContext,
                    this: LineString) -> LineString
                {
                    if this.0.is_closed() {
                        Ok(this)
                    } else {
                        let mut this = this;
                        let line_string = Arc::make_mut(&mut this.0);
                        line_string.close();

                        Ok(this)
                    }
                }
            );
            build_method!(
                database,
                Open, "LineString::open", (
                    context: &ExecutionContext,
                    this: LineString) -> LineString
                {
                    if this.0.is_closed() {
                        let mut this = this;
                        let line_string = Arc::make_mut(&mut this.0);
                        line_string.0.pop();

                        Ok(this)
                    } else {
                        Ok(this)
                    }
                }
            );
            build_method!(
                database,
                IterPoints, "LineString::iter_points", (
                    context: &ExecutionContext,
                    this: LineString) -> ValueIterator
                {
                    Ok(ValueIterator::new(LineStringIterator { line_string: this }))
                }
            );
            build_method!(
                database,
                RevIterPoints, "LineString::rev_iter_points", (
                    context: &ExecutionContext,
                    this: LineString) -> ValueIterator
                {
                    Ok(ValueIterator::new(RevLineStringIterator { line_string: this }))
                }
            );
            build_method!(
                database,
                NumPoints, "LineString::num_points", (
                    context: &ExecutionContext,
                    this: LineString) -> UnsignedInteger
                {
                    Ok(UnsignedInteger::from(this.0.0.len() as u64))
                }
            );
            build_method!(
                database,
                StringLength, "LineString::length", (
                    context: &ExecutionContext,
                    this: LineString) -> Scalar
                {
                    let mut points = this.0.0.iter().peekable();

                    let mut length = 0.0;
                    while let Some(point) = points.next() {
                        if let Some(next_point) = points.peek() {
                            let distance_between_points = Euclidean.distance(*point, **next_point);
                            length += distance_between_points;
                        }
                    }

                    Ok(Scalar {
                        dimension: Dimension::length(),
                        value: Float::new(length).unwrap_not_nan(context)?
                    })
                }
            );
            build_method!(
                database,
                BoundingBox, "LineString::bounding_box", (
                    context: &ExecutionContext,
                    this: LineString) -> Option<Dictionary>
                {
                    bounding_box(context, &*this.0)
                }
            );
            build_method!(
                database,
                Centroid, "LineString::centroid", (
                    context: &ExecutionContext,
                    this: LineString) -> Option<Vector2>
                {
                    centroid(context, &*this.0)
                }
            );
            build_method!(
                database,
                Transform, "LineString::transform", (
                    context: &ExecutionContext,
                    this: LineString,
                    t: Transform2d) -> LineString
                {
                    let mut this = this;
                    Arc::make_mut(&mut this.0).apply_transform(&t.0);

                    Ok(this)
                }
            );
        }
    }

    pub mod polygon {

        use std::f64::consts::PI;

        use geo::IsConvex as _;
        use thiserror::Error;

        use crate::{
            execution::errors::Raise,
            values::{
                polygon::ApplyTransform as _,
                scalar::{Angle, Length},
                ValueNone,
            },
        };

        use super::*;

        pub struct FromPoints;
        pub struct FromLineStrings;
        pub struct Circle;
        pub struct BuildArc;
        pub struct BuildBox;
        pub struct BuildBoxFromPoints;
        pub struct Exterior;
        pub struct Interiors;
        pub struct NumInteriors;
        pub struct IsConvex;
        pub struct Area;
        pub struct BoundingBox;
        pub struct Centroid;
        pub struct Transform;
        pub struct IntoSet;

        #[derive(Debug, Error)]
        enum CircleError {
            #[error("Could not derive radius, please provide radius or diameter")]
            NoRadius,

            #[error("Both a radius and a diameter were provided, please provide only one")]
            AmbigiousRadius,

            #[error("Cannot determine angle between segment points, provide exactly one of the following: angle_between_points, distance_between_points, or number_of_points.")]
            AmbigiousSegmentAngle,
        }

        #[derive(Debug)]
        struct CircleInfo {
            radius: RawFloat,
            segment_angle: RawFloat,
        }

        impl CircleInfo {
            fn new(
                context: &ExecutionContext,
                radius: Option<Length>,
                diameter: Option<Length>,
                angle_between_points: Option<Angle>,
                distance_between_points: Option<Angle>,
                number_of_points: Option<UnsignedInteger>,
                segment_angle_radians: f64,
            ) -> ExecutionResult<Self> {
                let radius = match (radius, diameter) {
                    (Some(radius), None) => *radius.value,
                    (None, Some(diameter)) => *diameter.value / 2.0,
                    (Some(_), Some(_)) => {
                        return Err(CircleError::AmbigiousRadius.to_error(context))
                    }
                    (None, None) => return Err(CircleError::NoRadius.to_error(context)),
                };

                let segment_angle = match (
                    angle_between_points,
                    distance_between_points,
                    number_of_points,
                ) {
                    (Some(angle_between_points), None, None) => *angle_between_points.value,
                    (None, Some(distance_between_points), None) => {
                        segment_angle_radians / *distance_between_points.value
                    }
                    (None, None, Some(number_of_points)) => {
                        segment_angle_radians / number_of_points.0 as RawFloat
                    }
                    (_, _, _) => return Err(CircleError::AmbigiousSegmentAngle.to_error(context)),
                };

                Ok(Self {
                    radius,
                    segment_angle,
                })
            }
        }

        pub fn register_methods_and_functions(database: &mut BuiltinCallableDatabase) {
            build_function!(
                database,
                FromPoints, "Polygon::from_points", (
                    context: &ExecutionContext,
                    points: List) -> Polygon
                {
                    let mut collected = Vec::with_capacity(points.len());
                    for value in points {
                        let point: Length2 = value.downcast(context)?;
                        let point: Vector2 = point.into();
                        collected.push(geo::Coord { x: point.raw_value().x, y: point.raw_value().y });
                    }

                    let external = geo::LineString(collected);
                    Ok(Polygon(Arc::new(geo::Polygon::new(external, vec![]))))
                }
            );
            build_function!(
                database,
                FromLineStrings, "Polygon::from_line_strings", (
                    context: &ExecutionContext,
                    exterior: LineString,
                    interiors: List = List::default().into()) -> Polygon
                {
                    let mut collected = Vec::with_capacity(interiors.len());
                    for value in interiors {
                        let string: LineString = value.downcast(context)?;
                        collected.push((*string.0).clone());
                    }

                    let exterior = Arc::unwrap_or_clone(exterior.0);
                    Ok(Polygon(Arc::new(geo::Polygon::new(exterior, collected))))
                }
            );
            build_function!(
                database,
                Circle, "Polygon::circle", (
                    context: &ExecutionContext,
                    radius: Option<Length> = ValueNone.into(),
                    diameter: Option<Length> = ValueNone.into(),
                    angle_between_points: Option<Angle> = ValueNone.into(),
                    distance_between_points: Option<Angle> = ValueNone.into(),
                    number_of_points: Option<UnsignedInteger> = ValueNone.into()) -> Polygon
                {
                    let circle_info = CircleInfo::new(
                        context,
                        radius,
                        diameter,
                        angle_between_points,
                        distance_between_points,
                        number_of_points,
                        PI
                    )?;



                    Ok(todo!())
                }
            );
            build_function!(
                database,
                BuildArc, "Polygon::arc", (
                    context: &ExecutionContext,
                    radius: Option<Length>,
                    diameter: Option<Length>,
                    angle_between_points: Option<Angle>,
                    distance_between_points: Option<Angle>,
                    number_of_points: Option<UnsignedInteger>,
                    // This is the distance between the chord and the edge of the circle.
                    sagitta: Length,
                    chord: Length) -> Dictionary
                {
                    Ok(todo!())
                }
            );
            build_function!(
                database,
                BuildBox, "Polygon::box", (
                    context: &ExecutionContext,
                    size: Length2,
                    center: Boolean = Boolean(false).into()) -> Polygon
                {
                    let size = size.raw_value();

                    let (a, b) = if center.0 {
                        let half = size / 2.0;
                        (-half, half)
                    } else {
                        (nalgebra::Vector2::zeros(), size)
                    };


                    let rect = geo::Rect::new(
                        geo::Coord { x: a.x, y: a.y },
                        geo::Coord { x: b.x, y: b.y }
                    );

                    Ok(Polygon(Arc::new(rect.to_polygon())))
                }
            );
            build_function!(
                database,
                BuildBoxFromPoints, "Polygon::box_from_points", (
                    context: &ExecutionContext,
                    a: Length2,
                    b: Length2) -> Polygon
                {
                    let a = a.raw_value();
                    let b = b.raw_value();

                    let rect = geo::Rect::new(
                        geo::Coord { x: a.x, y: a.y },
                        geo::Coord { x: b.x, y: b.y }
                    );

                    Ok(Polygon(Arc::new(rect.to_polygon())))
                }
            );

            build_method!(
                database,
                Exterior, "Polygon::exterior", (
                    context: &ExecutionContext,
                    this: Polygon) -> LineString
                {
                    Ok(LineString(Arc::new(this.0.exterior().clone())))
                }
            );
            build_method!(
                database,
                Interiors, "Polygon::interiors", (
                    context: &ExecutionContext,
                    this: Polygon) -> ValueIterator
                {
                    dbg!(&this);
                    Ok(ValueIterator::new(InteriorIterator { polygon: this }))
                }
            );
            build_method!(
                database,
                NumInteriors, "Polygon::num_interiors", (
                    context: &ExecutionContext,
                    this: Polygon) -> UnsignedInteger
                {
                    Ok(UnsignedInteger::from(this.0.interiors().len() as u64))
                }
            );
            build_method!(
                database,
                IsConvex, "Polygon::is_convex", (
                    context: &ExecutionContext,
                    this: Polygon) -> Boolean
                {
                    Ok(Boolean(this.0.exterior().is_convex()))
                }
            );
            build_method!(
                database,
                Area, "Polygon::area", (
                    context: &ExecutionContext,
                    this: Polygon) -> Scalar
                {
                    area(context, &*this.0)
                }
            );
            build_method!(
                database,
                BoundingBox, "Polygon::bounding_box", (
                    context: &ExecutionContext,
                    this: Polygon) -> Option<Dictionary>
                {
                    bounding_box(context, &*this.0)
                }
            );
            build_method!(
                database,
                Centroid, "Polygon::centroid", (
                    context: &ExecutionContext,
                    this: Polygon) -> Option<Vector2>
                {
                    centroid(context, &*this.0)
                }
            );
            build_method!(
                database,
                Transform, "Polygon::transform", (
                    context: &ExecutionContext,
                    this: Polygon,
                    t: Transform2d) -> Polygon
                {
                    let mut this = this;
                    Arc::make_mut(&mut this.0).apply_transform(&t.0);

                    Ok(this)
                }
            );
            build_method!(
                database,
                IntoSet, "Polygon::into_set", (
                    context: &ExecutionContext,
                    this: Polygon) -> PolygonSet
                {
                    Ok(todo!())
                }
            );
        }
    }

    pub mod polygon_set {
        use super::*;

        pub struct FromPolys;
        pub struct IterPoly;
        pub struct NumPoly;
        pub struct Area;
        pub struct BoundingBox;
        pub struct Centroid;
        pub struct Transform;

        pub fn register_methods_and_functions(database: &mut BuiltinCallableDatabase) {
            build_function!(
                database,
                FromPolys, "PolygonSet::from_polys", (
                    context: &ExecutionContext,
                    polys: List) -> PolygonSet
                {
                    Ok(todo!())
                }
            );

            build_method!(
                database,
                IterPoly, "PolygonSet::iter_polys", (
                    context: &ExecutionContext,
                    this: PolygonSet) -> ValueIterator
                {
                    Ok(todo!())
                }
            );
            build_method!(
                database,
                NumPoly, "PolygonSet::num_polys", (
                    context: &ExecutionContext,
                    this: PolygonSet) -> UnsignedInteger
                {
                    Ok(todo!())
                }
            );
            build_method!(
                database,
                Area, "PolygonSet::area", (
                    context: &ExecutionContext,
                    this: PolygonSet) -> Scalar
                {
                    Ok(todo!())
                }
            );
            build_method!(
                database,
                BoundingBox, "PolygonSet::bounding_box", (
                    context: &ExecutionContext,
                    this: PolygonSet) -> Dictionary
                {
                    Ok(todo!())
                }
            );
            build_method!(
                database,
                Centroid, "PolygonSet::centroid", (
                    context: &ExecutionContext,
                    this: PolygonSet) -> Vector2
                {
                    Ok(todo!())
                }
            );
            build_method!(
                database,
                Transform, "PolygonSet::transform", (
                    context: &ExecutionContext,
                    this: PolygonSet,
                    t: Transform2d) -> PolygonSet
                {
                    Ok(todo!())
                }
            );
        }
    }
}

pub fn register_methods_and_functions(database: &mut BuiltinCallableDatabase) {
    methods_and_functions::line_string::register_methods_and_functions(database);
    methods_and_functions::polygon::register_methods_and_functions(database);
    methods_and_functions::polygon_set::register_methods_and_functions(database);
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::execution::{run_assert_eq, test_run};
    use geo::Coord;

    #[test]
    fn line_string_from_points() {
        let result =
            test_run("std.line_string.from_points(points = [{0m, 1m}, {2m, 3m}])").unwrap();
        assert_eq!(
            result,
            LineString(Arc::new(geo::LineString(vec![
                Coord { x: 0.0, y: 1.0 },
                Coord { x: 2.0, y: 3.0 }
            ])))
            .into()
        );
    }

    #[test]
    fn line_string_append() {
        run_assert_eq(
            "std.line_string.from_points(points = [{0m, 1m}, {2m, 3m}])::append(other = std.line_string.from_points(points = [{4m, 5m}, {6m, 7m}]))::iter_points()::collect_list()",
            "[{0m, 1m}, {2m, 3m}, {4m, 5m}, {6m, 7m}]",
        );
    }

    #[test]
    fn line_string_add_point() {
        run_assert_eq(
            "std.line_string.from_points(points = [{0m, 1m}, {2m, 3m}])::add_point(p = {4m, 5m})::iter_points()::collect_list()",
            "[{0m, 1m}, {2m, 3m}, {4m, 5m}]",
        );
    }

    #[test]
    fn line_string_is_closed() {
        run_assert_eq(
            "std.line_string.from_points(points = [{0m, 1m}, {2m, 3m}])::is_closed()",
            "false",
        );
        run_assert_eq(
            "std.line_string.from_points(points = [{0m, 1m}, {2m, 3m}, {0m, 1m}])::is_closed()",
            "true",
        );
    }

    #[test]
    fn line_string_close() {
        run_assert_eq(
            "std.line_string.from_points(points = [{0m, 1m}, {2m, 3m}])::close()",
            "std.line_string.from_points(points = [{0m, 1m}, {2m, 3m}, {0m, 1m}])",
        );
        run_assert_eq(
            "std.line_string.from_points(points = [{0m, 1m}, {2m, 3m}, {0m, 1m}])::close()",
            "std.line_string.from_points(points = [{0m, 1m}, {2m, 3m}, {0m, 1m}])",
        );
    }

    #[test]
    fn line_string_open() {
        run_assert_eq(
            "std.line_string.from_points(points = [{0m, 1m}, {2m, 3m}])::open()",
            "std.line_string.from_points(points = [{0m, 1m}, {2m, 3m}])",
        );
        run_assert_eq(
            "std.line_string.from_points(points = [{0m, 1m}, {2m, 3m}, {0m, 1m}])::open()",
            "std.line_string.from_points(points = [{0m, 1m}, {2m, 3m}])",
        );
    }

    #[test]
    fn line_string_iter_points() {
        run_assert_eq(
            "std.line_string.from_points(points = [{0m, 1m}, {2m, 3m}])::iter_points()::collect_list()",
            "[{0m, 1m}, {2m, 3m}]",
        );
    }

    #[test]
    fn line_string_rev_iter_points() {
        run_assert_eq(
            "std.line_string.from_points(points = [{0m, 1m}, {2m, 3m}])::rev_iter_points()::collect_list()",
            "[{2m, 3m}, {0m, 1m}]",
        );
    }

    #[test]
    fn line_string_num_points() {
        run_assert_eq(
            "std.line_string.from_points(points = [{0m, 1m}, {2m, 3m}])::num_points()",
            "2u",
        );
    }

    #[test]
    fn line_string_length() {
        run_assert_eq(
            "std.line_string.from_points(points = [{0m, 1m}, {0m, 2m}, {0m, 4m}])::length()",
            "3m",
        );
    }

    #[test]
    fn line_string_bounding_box() {
        run_assert_eq(
            "std.line_string.from_points(points = [{-1m, 2m}, {1m, -2m}])::bounding_box()",
            "(min = {-1m, -2m}, max = {1m, 2m})",
        );
    }

    #[test]
    fn line_string_centroid() {
        run_assert_eq(
            "std.line_string.from_points(points = [{-1m, 1m}, {1m, -1m}])::centroid()",
            "{0m, 0m}",
        );
    }

    #[test]
    fn line_string_transform() {
        run_assert_eq(
            "std.line_string.from_points(points = [{0m, 1m}, {2m, 3m}])::transform(t = std.consts.Transform2d::translate(offset = {1m, 2m}))",
            "std.line_string.from_points(points = [{1m, 3m}, {3m, 5m}])",
        );
    }

    #[test]
    fn polygon_from_points() {
        let result =
            test_run("std.polygon.from_points(points = [{0m, 0m}, {1m, 0m}, {1m, 1m}, {0m, 1m}])")
                .unwrap();
        assert_eq!(
            result,
            Polygon(Arc::new(geo::Polygon::new(
                geo::LineString(vec![
                    Coord { x: 0.0, y: 0.0 },
                    Coord { x: 1.0, y: 0.0 },
                    Coord { x: 1.0, y: 1.0 },
                    Coord { x: 0.0, y: 1.0 }
                ]),
                vec![]
            )))
            .into()
        );
    }

    #[test]
    fn polygon_from_line_strings() {
        let result =
            test_run("std.polygon.from_line_strings(exterior = std.line_string.from_points(points = [{0m, 0m}, {1m, 0m}, {1m, 1m}, {0m, 1m}]))")
                .unwrap();
        assert_eq!(
            result,
            Polygon(Arc::new(geo::Polygon::new(
                geo::LineString(vec![
                    Coord { x: 0.0, y: 0.0 },
                    Coord { x: 1.0, y: 0.0 },
                    Coord { x: 1.0, y: 1.0 },
                    Coord { x: 0.0, y: 1.0 }
                ]),
                vec![]
            )))
            .into()
        );
    }

    #[test]
    fn polygon_from_circle() {
        // Test distance_between_points
        let result =
            test_run("std.polygon.circle(radius = 1m, distance_between_points = 1rad)").unwrap();
        let polygon = result.as_polygon().unwrap();
        assert_eq!(polygon.0.exterior().0.len(), 4);
        assert!(polygon.0.interiors().is_empty());

        let result =
            test_run("std.polygon.circle(radius = 1m, distance_between_points = 0.5rad)").unwrap();
        let polygon = result.as_polygon().unwrap();
        assert_eq!(polygon.0.exterior().0.len(), 5);
        assert!(polygon.0.interiors().is_empty());

        // Test angle_between_points
        let result =
            test_run("std.polygon.circle(radius = 1m, angle_between_points = 1rad)").unwrap();
        let polygon = result.as_polygon().unwrap();
        assert_eq!(polygon.0.exterior().0.len(), 4);
        assert!(polygon.0.interiors().is_empty());

        let result =
            test_run("std.polygon.circle(radius = 1m, angle_between_points = 0.5rad)").unwrap();
        let polygon = result.as_polygon().unwrap();
        assert_eq!(polygon.0.exterior().0.len(), 5);
        assert!(polygon.0.interiors().is_empty());

        // Test number_of_points
        let result = test_run("std.polygon.circle(radius = 1m, number_of_points = 2u)").unwrap();
        let polygon = result.as_polygon().unwrap();
        assert_eq!(polygon.0.exterior().0.len(), 2);
        assert!(polygon.0.interiors().is_empty());

        let result = test_run("std.polygon.circle(radius = 1m, number_of_points = 3u)").unwrap();
        let polygon = result.as_polygon().unwrap();
        assert_eq!(polygon.0.exterior().0.len(), 3);
        assert!(polygon.0.interiors().is_empty());

        let result =
            test_run("std.polygon.circle(radius = 1m, distance_between_points = 1rad)").unwrap();
        let polygon = result.as_polygon().unwrap();
        assert_eq!(
            *polygon,
            Polygon(Arc::new(geo::Polygon::new(
                geo::LineString(vec![
                    Coord { x: -1.0, y: 0.0 },
                    Coord { x: 0.0, y: 1.0 },
                    Coord { x: 1.0, y: 0.0 },
                    Coord { x: 0.0, y: -1.0 },
                    Coord { x: -1.0, y: 0.0 },
                ]),
                vec![]
            )))
        );

        let result =
            test_run("std.polygon.circle(diameter = 1m, distance_between_points = 1rad)").unwrap();
        let polygon = result.as_polygon().unwrap();
        assert_eq!(
            *polygon,
            Polygon(Arc::new(geo::Polygon::new(
                geo::LineString(vec![
                    Coord { x: -0.5, y: 0.0 },
                    Coord { x: 0.0, y: 0.5 },
                    Coord { x: 0.5, y: 0.0 },
                    Coord { x: 0.0, y: -0.5 },
                    Coord { x: -0.5, y: 0.0 },
                ]),
                vec![]
            )))
        );
    }

    #[test]
    fn polygon_from_arc() {
        // let result =
        //     test_run("std.polygon.arc(radius = 1m, distance_between_points = 1rad, sagitta = 1m)")
        //         .unwrap();
        // let polygon = result.as_polygon().unwrap();
        // assert_eq!(polygon.0.exterior().0.len(), 2);
        // assert!(polygon.0.interiors().is_empty());

        // let result =
        //     test_run("std.polygon.arc(radius = 1m, distance_between_points = 1rad, chord = 1m)")
        //         .unwrap();
        // let polygon = result.as_polygon().unwrap();
        // assert_eq!(polygon.0.exterior().0.len(), 2);
        // assert!(polygon.0.interiors().is_empty());
        todo!()
    }

    #[test]
    fn polygon_from_box() {
        run_assert_eq(
            "std.polygon.box(size = {1m, 2m})::exterior()::iter_points()::collect_list()",
            "[{1m, 0m}, {1m, 2m}, {0m, 2m}, {0m, 0m}, {1m, 0m}]",
        );

        run_assert_eq(
            "std.polygon.box(size = {1m, 2m}, center = true)::exterior()::iter_points()::collect_list()",
            "[{0.5m, -1m}, {0.5m, 1m}, {-0.5m, 1m}, {-0.5m, -1m}, {0.5m, -1m}]",
        );
    }

    #[test]
    fn polygon_exterior() {
        run_assert_eq(
            "std.polygon.from_line_strings(exterior = std.line_string.from_points(points = [{0m, 0m}, {1m, 0m}, {1m, 1m}, {0m, 1m}]))::exterior()",
            "std.line_string.from_points(points = [{0m, 0m}, {1m, 0m}, {1m, 1m}, {0m, 1m}, {0m, 0m}])",
        );
    }

    #[test]
    fn polygon_interiors() {
        run_assert_eq(
            "std.polygon.from_line_strings(exterior = std.line_string.from_points(points = [{-2m, -2m}, {2m, -2m}, {2m, 2m}, {-2m, 2m}]), interiors = [std.line_string.from_points(points = [{0m, 1m}, {1m, 1m}, {1m, 0m}, {0m, 0m}])])::interiors()::collect_list()",
            "[std.line_string.from_points(points = [{0m, 1m}, {1m, 1m}, {1m, 0m}, {0m, 0m}, {0m, 1m}])]",
        );
    }

    #[test]
    fn polygon_num_interiors() {
        run_assert_eq(
            "std.polygon.from_line_strings(exterior = std.line_string.from_points(points = []), interiors = [std.line_string.from_points(points = []), std.line_string.from_points(points = []), std.line_string.from_points(points = [])])::num_interiors()",
            "3u",
        );
    }

    #[test]
    fn polygon_is_convex() {
        run_assert_eq(
            "std.polygon.from_line_strings(exterior = std.line_string.from_points(points = [{0m, 0m}, {1m, 0m}, {1m, 1m}, {0m, 1m}]))::is_convex()",
            "true",
        );

        run_assert_eq(
            "std.polygon.from_line_strings(exterior = std.line_string.from_points(points = [{0m, 0m}, {1m, 0m}, {1m, 1m}, {0.5m, 0.5m}, {0m, 1m}]))::is_convex()",
            "false",
        );
    }

    #[test]
    fn polygon_area() {
        run_assert_eq(
            "std.polygon.from_line_strings(exterior = std.line_string.from_points(points = [{0m, 0m}, {1m, 0m}, {1m, 1m}, {0m, 1m}]))::area()",
            "1 'm^2'",
        );
    }

    #[test]
    fn polygon_bounding_box() {
        run_assert_eq(
            "std.polygon.from_line_strings(exterior = std.line_string.from_points(points = [{0m, 0m}, {1m, 0m}, {1m, 1m}, {0m, 1m}]))::bounding_box()",
            "(min = {0m, 0m}, max = {1m, 1m})",
        );
    }

    #[test]
    fn polygon_centroid() {
        run_assert_eq(
            "std.polygon.from_line_strings(exterior = std.line_string.from_points(points = [{0m, 0m}, {1m, 0m}, {1m, 1m}, {0m, 1m}]))::centroid()",
            "{0.5m, 0.5m}",
        );
    }

    #[test]
    fn polygon_transform() {
        run_assert_eq(
            "std.polygon.from_line_strings(exterior = std.line_string.from_points(points = [{0m, 0m}, {1m, 0m}, {1m, 1m}, {0m, 1m}]))::transform(t = std.consts.Transform2d::translate(offset = {1m, 2m}))",
            "std.polygon.from_line_strings(exterior = std.line_string.from_points(points = [{1m, 2m}, {2m, 2m}, {2m, 3m}, {1m, 3m}]))",
        );
    }

    // IntoSet;
}
