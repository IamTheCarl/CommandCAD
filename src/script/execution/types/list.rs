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

use common_data_types::Number;
use enum_downcast::{AsVariant, EnumDowncast, IntoVariant};
use std::{fmt::Write, isize, rc::Rc};

use crate::script::{
    execution::{expressions::run_expression, ExecutionContext, Failure},
    logging::RuntimeLog,
    parsing::{self, Expression, VariableType},
    Span,
};

use super::{
    function::AutoCall, number::UnwrapNotNan, serializable::SerializableValue,
    string::formatting::Style, NamedObject, Object, OperatorResult, Scalar, Value,
};

#[derive(Debug, Clone, PartialEq)]
pub struct List<'a, S: Span> {
    vector: Rc<Vec<Value<'a, S>>>,
}

impl<'a, S: Span, I> From<I> for List<'a, S>
where
    I: IntoIterator<Item = Value<'a, S>>,
{
    fn from(value: I) -> Self {
        let value = value.into_iter();

        Self {
            vector: Rc::new(value.collect()),
        }
    }
}

impl<'a, S: Span> List<'a, S> {
    fn unwrap_or_clone(&self) -> Vec<Value<'a, S>> {
        // TODO In order to actually implement an "unwrap or clone" we need to be mutable.
        self.vector.to_vec()
    }

    pub(crate) fn from_parsed(
        context: &mut ExecutionContext<'a, S>,
        list: &'a parsing::List<S>,
    ) -> OperatorResult<S, Value<'a, S>> {
        let mut values = Vec::with_capacity(list.expressions.len());

        for expression in list.expressions.iter() {
            let value = run_expression(context, expression)?;

            values.push(value);
        }

        Ok(Self {
            vector: Rc::new(values),
        }
        .into())
    }

    fn internalize_index(&self, span: &S, index: isize) -> OperatorResult<S, usize> {
        let new_index = if index >= 0 {
            Ok(index as usize)
        } else if let Some(new_index) = self.vector.len().checked_sub(index.unsigned_abs()) {
            Ok(new_index)
        } else {
            Err(Failure::IndexOutOfRange(span.clone(), index))
        }?;

        if new_index >= self.vector.len() {
            Err(Failure::IndexOutOfRange(span.clone(), index))
        } else {
            Ok(new_index)
        }
    }

    pub fn len(&self) -> usize {
        self.vector.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Value<'a, S>> + Clone {
        self.vector.iter()
    }

    pub fn consume(self) -> impl Iterator<Item = Value<'a, S>> + Clone {
        Rc::unwrap_or_clone(self.vector).into_iter()
    }
}

impl<'a, S: Span> Object<'a, S> for List<'a, S> {
    fn matches_type(&self, ty: &VariableType<S>) -> bool {
        matches!(ty, VariableType::List)
    }

    fn format(
        &self,
        log: &mut dyn RuntimeLog<S>,
        span: &S,
        f: &mut dyn Write,
        style: Style,
        precision: Option<u8>,
    ) -> OperatorResult<S, ()> {
        for item in self.vector.iter() {
            item.format(log, span, f, style, precision)?;
        }

        Ok(())
    }

    fn index(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        index: Value<'a, S>,
    ) -> OperatorResult<S, Value<'a, S>> {
        match index {
            Value::Scalar(index) => {
                let index = index.to_index(span)?;

                let localized_index = self.internalize_index(span, index)?;

                self.vector
                    .get(localized_index)
                    .cloned()
                    .ok_or(Failure::IndexOutOfRange(span.clone(), index))
            }
            Value::Range(range) => {
                // TODO could we keep an immutable reference to the original list to avoid a copy?
                let slice = match (
                    range.lower_bound,
                    range.upper_bound,
                    range.upper_bound_is_inclusive,
                ) {
                    (None, None, false) => self.vector.get(..).ok_or((None, None)),
                    (Some(lower_bound), None, false) => {
                        let signed_lower_bound = lower_bound.to_index(span)?;
                        let lower_bound = self.internalize_index(span, signed_lower_bound)?;
                        self.vector
                            .get(lower_bound..)
                            .ok_or((Some(signed_lower_bound), None))
                    }
                    (None, Some(upper_bound), false) => {
                        let signed_upper_bound = upper_bound.to_index(span)?;
                        let upper_bound = self.internalize_index(span, signed_upper_bound)?;
                        self.vector
                            .get(..upper_bound)
                            .ok_or((None, Some(signed_upper_bound)))
                    }
                    (None, Some(upper_bound), true) => {
                        let signed_upper_bound = upper_bound.to_index(span)?;
                        let upper_bound = self.internalize_index(span, signed_upper_bound)?;
                        self.vector
                            .get(..=upper_bound)
                            .ok_or((None, Some(signed_upper_bound)))
                    }
                    (Some(lower_bound), Some(upper_bound), false) => {
                        let signed_lower_bound = lower_bound.to_index(span)?;
                        let lower_bound = self.internalize_index(span, signed_lower_bound)?;
                        let signed_upper_bound = upper_bound.to_index(span)?;
                        let upper_bound = self.internalize_index(span, signed_upper_bound)?;
                        self.vector
                            .get(lower_bound..upper_bound)
                            .ok_or((Some(signed_lower_bound), Some(signed_upper_bound)))
                    }
                    (Some(lower_bound), Some(upper_bound), true) => {
                        let signed_lower_bound = lower_bound.to_index(span)?;
                        let lower_bound = self.internalize_index(span, signed_lower_bound)?;
                        let signed_upper_bound = upper_bound.to_index(span)?;
                        let upper_bound = self.internalize_index(span, signed_upper_bound)?;
                        self.vector
                            .get(lower_bound..=upper_bound)
                            .ok_or((Some(signed_lower_bound), Some(signed_upper_bound)))
                    }
                    (_, None, true) => unreachable!(), // Inclusive ranges without an upper bound are illegal to construct.
                };

                // TODO String has an identical error handling. We should probably move this to a common library.
                let range_type = if range.upper_bound_is_inclusive {
                    "..="
                } else {
                    ".."
                };

                slice
                    .map(|slice| Self::from(slice.iter().cloned()).into())
                    .map_err(|(lower_bound, upper_bound)| {
                        Failure::SliceOutOfRange(span.clone(), lower_bound, range_type, upper_bound)
                    })
            }
            _ => Err(Failure::ExpectedGot(
                span.clone(),
                "Number or Range".into(),
                index.type_name(),
            )),
        }
    }

    fn iterate(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        _span: &S,
    ) -> OperatorResult<S, Box<dyn Iterator<Item = Value<'a, S>> + '_>> {
        Ok(Box::new(self.vector.iter().cloned()))
    }

    fn method_call(
        &self,
        context: &mut ExecutionContext<'a, S>,
        span: &S,
        attribute: &S,
        arguments: Vec<Value<'a, S>>,
        expressions: &[Expression<S>],
    ) -> OperatorResult<S, Value<'a, S>> {
        match attribute.as_str() {
            "append" => {
                |_context: &mut ExecutionContext<'a, S>,
                 _span: &S,
                 other: List<'a, S>|
                 -> OperatorResult<S, Value<S>> {
                    // TODO make this accept any iteratable value.

                    let mut vector = self.unwrap_or_clone();
                    vector.extend_from_slice(&other.vector);

                    Ok(Self {
                        vector: Rc::new(vector),
                    }
                    .into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "dedup" => {
                |_context: &mut ExecutionContext<'a, S>, _span: &S| -> OperatorResult<S, Value<S>> {
                    let mut vector = self.unwrap_or_clone();
                    vector.dedup();

                    Ok(Self {
                        vector: Rc::new(vector),
                    }
                    .into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "insert" => |_context: &mut ExecutionContext<'a, S>,
                         span: &S,
                         index: Scalar,
                         value: Value<'a, S>|
             -> OperatorResult<S, Value<S>> {
                let index = index.to_index(span)?;
                let mut vector = self.unwrap_or_clone();
                let index = self.internalize_index(span, index)?;
                vector.insert(index, value);

                Ok(Self {
                    vector: Rc::new(vector),
                }
                .into())
            }
            .auto_call(context, span, arguments, expressions),
            "is_empty" => {
                |_context: &mut ExecutionContext<'a, S>, _span: &S| -> OperatorResult<S, Value<S>> {
                    Ok(self.vector.is_empty().into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "len" => {
                |_context: &mut ExecutionContext<'a, S>, span: &S| -> OperatorResult<S, Value<S>> {
                    Number::new(self.vector.len() as f64)
                        .unwrap_not_nan(span)
                        .map(|n| n.into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "push" => |_context: &mut ExecutionContext<'a, S>,
                       _span: &S,
                       other: Value<'a, S>|
             -> OperatorResult<S, Value<S>> {
                let mut vector = self.unwrap_or_clone();
                vector.push(other);

                Ok(Self {
                    vector: Rc::new(vector),
                }
                .into())
            }
            .auto_call(context, span, arguments, expressions),
            "remove" => |_context: &mut ExecutionContext<'a, S>,
                         span: &S,
                         index: Scalar|
             -> OperatorResult<S, Value<S>> {
                let index = index.to_index(span)?;
                let index = self.internalize_index(span, index)?;

                let mut vector = self.unwrap_or_clone();
                vector.remove(index);

                Ok(Self {
                    vector: Rc::new(vector),
                }
                .into())
            }
            .auto_call(context, span, arguments, expressions),
            "contains" => |_context: &mut ExecutionContext<'a, S>,
                           _span: &S,
                           search: Value<'a, S>|
             -> OperatorResult<S, Value<S>> {
                Ok(self.vector.contains(&search).into())
            }
            .auto_call(context, span, arguments, expressions),
            "last" => {
                |_context: &mut ExecutionContext<'a, S>, span: &S| -> OperatorResult<S, Value<S>> {
                    let last = self.vector.last();

                    if let Some(last) = last {
                        Ok(last.clone())
                    } else {
                        Err(Failure::ListIsEmpty(span.clone()))
                    }
                }
                .auto_call(context, span, arguments, expressions)
            }
            "first" => {
                |_context: &mut ExecutionContext<'a, S>, span: &S| -> OperatorResult<S, Value<S>> {
                    let first = self.vector.first();

                    if let Some(first) = first {
                        Ok(first.clone())
                    } else {
                        Err(Failure::ListIsEmpty(span.clone()))
                    }
                }
                .auto_call(context, span, arguments, expressions)
            }
            "reverse" => {
                |_context: &mut ExecutionContext<'a, S>, _span: &S| -> OperatorResult<S, Value<S>> {
                    let mut vector = self.unwrap_or_clone();
                    vector.reverse();

                    Ok(Self {
                        vector: Rc::new(vector),
                    }
                    .into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "rotate_left" => |_context: &mut ExecutionContext<'a, S>,
                              span: &S,
                              mid: Scalar|
             -> OperatorResult<S, Value<S>> {
                let mid = mid.to_index(span)?;
                if mid.is_positive() {
                    let mid = mid as usize % self.vector.len();
                    let mut vector = self.unwrap_or_clone();
                    vector.rotate_left(mid);

                    Ok(Self {
                        vector: Rc::new(vector),
                    }
                    .into())
                } else {
                    Err(Failure::NumberMustBePositive(span.clone()))
                }
            }
            .auto_call(context, span, arguments, expressions),
            "rotate_right" => |_context: &mut ExecutionContext<'a, S>,
                               _span: &S,
                               mid: Scalar|
             -> OperatorResult<S, Value<S>> {
                let mid = mid.to_index(span)?;
                if mid.is_positive() {
                    let mid = mid as usize % self.vector.len();
                    let mut vector = self.unwrap_or_clone();
                    vector.rotate_right(mid);

                    Ok(Self {
                        vector: Rc::new(vector),
                    }
                    .into())
                } else {
                    Err(Failure::NumberMustBePositive(span.clone()))
                }
            }
            .auto_call(context, span, arguments, expressions),
            _ => Err(Failure::UnknownAttribute(attribute.clone())),
        }
    }

    fn export(
        &self,
        log: &mut dyn RuntimeLog<S>,
        span: &S,
    ) -> OperatorResult<S, SerializableValue> {
        let mut list = Vec::with_capacity(self.vector.len());

        for item in self.vector.iter() {
            let serializable = item.export(log, span)?;
            list.push(serializable);
        }

        Ok(SerializableValue::List(list))
    }
}

impl<'a, S: Span> NamedObject for List<'a, S> {
    fn static_type_name() -> &'static str {
        "List"
    }
}

impl<'a, S: Span> List<'a, S> {
    pub fn unpack_dynamic_length<T>(
        self,
        span: &S,
    ) -> OperatorResult<S, impl Iterator<Item = T> + Clone + 'a>
    where
        T: NamedObject + Clone,
        Value<'a, S>: IntoVariant<T> + AsVariant<T> + TryInto<T>,
    {
        // Verify that they're all of the right type.
        for (index, item) in self.iter().enumerate() {
            if item.enum_downcast_ref::<T>().is_none() {
                return Err(Failure::ListElement(
                    span.clone(),
                    index,
                    Box::new(Failure::ExpectedGot(
                        span.clone(),
                        T::static_type_name().into(),
                        item.type_name(),
                    )),
                ));
            }
        }

        // Okay, we've validated them. Now we can really take them.
        // The unwraps will not fail because we've already validated the types.
        let iter = self.consume().map(|v| v.enum_downcast::<T>().unwrap());
        Ok(iter)
    }

    pub fn unpack_fixed_length<T, const D: usize>(
        self,
        span: &S,
    ) -> OperatorResult<S, impl Iterator<Item = T> + Clone + 'a>
    where
        T: NamedObject + Clone,
        Value<'a, S>: IntoVariant<T> + AsVariant<T> + TryInto<T>,
    {
        if self.len() == D {
            // This cannot exceed length D because we already validated that the list matched that length.
            self.unpack_dynamic_length(span)
        } else {
            Err(Failure::ListWrongLength(span.clone(), D, self.len()))
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::script::{
        execution::{expressions::run_expression, ExecutionContext},
        parsing::Expression,
    };

    #[test]
    fn index() {
        let mut context = ExecutionContext::default();
        assert_eq!(
            run_expression(
                &mut context,
                Box::leak(Box::new(Expression::parse("[1, 2, 3][0]").unwrap().1))
            ),
            Ok(Number::new(1.0).unwrap().into())
        );

        assert_eq!(
            run_expression(
                &mut context,
                Box::leak(Box::new(Expression::parse("[1, 2, 3][-1]").unwrap().1))
            ),
            Ok(Number::new(3.0).unwrap().into())
        );

        assert_eq!(
            run_expression(
                &mut context,
                Box::leak(Box::new(Expression::parse("[1, 2, 3][3]").unwrap().1))
            ),
            Err(Failure::IndexOutOfRange("[", 3))
        );
        assert_eq!(
            run_expression(
                &mut context,
                Box::leak(Box::new(Expression::parse("[1, 2, 3][-4]").unwrap().1))
            ),
            Err(Failure::IndexOutOfRange("[", -4))
        );
    }

    #[test]
    fn test_unpack() {
        assert_eq!(
            List::<'_, &'static str>::from([
                Number::new(1.0).unwrap().into(),
                Number::new(2.0).unwrap().into(),
                Number::new(3.0).unwrap().into(),
            ])
            .unpack_fixed_length::<Scalar, 3usize>(&"span",)
            .map(|v| v.collect::<Vec<_>>()),
            Ok(vec![
                Number::new(1.0).unwrap().into(),
                Number::new(2.0).unwrap().into(),
                Number::new(3.0).unwrap().into(),
            ])
        );

        let values = [
            Number::new(1.0).unwrap().into(),
            Number::new(2.0).unwrap().into(),
        ];

        assert_eq!(
            List::<'_, &'static str>::from(values)
                .unpack_fixed_length::<Scalar, 3usize>(&"span",)
                .map(|v| v.collect::<Vec<_>>()),
            Err(Failure::ListWrongLength("span", 3, 2))
        );

        let values = [Number::new(1.0).unwrap().into(), true.into()];

        assert_eq!(
            List::<'_, &'static str>::from(values)
                .unpack_fixed_length::<Scalar, 2usize>(&"span")
                .map(|v| v.collect::<Vec<_>>()),
            Err(Failure::ListElement(
                "span",
                1,
                Box::new(Failure::ExpectedGot(
                    "span",
                    "Scalar".into(),
                    "Boolean".into()
                ))
            ))
        );
    }
}
