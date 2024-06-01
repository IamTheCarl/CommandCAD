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

use common_data_types::Float;
use enum_downcast::{AsVariant, EnumDowncast, IntoVariant};
use ouroboros::self_referencing;
use std::{cell::RefCell, fmt::Write, isize, rc::Rc};

use crate::script::{
    execution::{expressions::run_expression, ExecutionContext, Failure},
    logging::RuntimeLog,
    parsing::{self, Expression, VariableType},
    Span,
};

use super::{
    function::AutoCall, math::Number, number::UnwrapNotNan, serializable::SerializableValue,
    string::formatting::Style, NamedObject, NoneType, Object, OperatorResult, Scalar, TypedObject,
    UnwrapBorrowFailure, Value,
};

#[derive(Debug, Clone, PartialEq)]
pub struct List<S: Span> {
    vector: Rc<RefCell<Vec<Value<S>>>>,
}

impl<S: Span, I> From<I> for List<S>
where
    I: IntoIterator<Item = Value<S>>,
{
    fn from(value: I) -> Self {
        let value = value.into_iter();

        Self {
            vector: Rc::new(RefCell::new(value.collect())),
        }
    }
}

impl<S: Span> List<S> {
    pub(crate) fn from_parsed(
        context: &mut ExecutionContext<S>,
        list: &parsing::List<S>,
    ) -> OperatorResult<S, Value<S>> {
        let mut vector = Vec::with_capacity(list.expressions.len());

        for expression in list.expressions.iter() {
            let value = run_expression(context, expression)?;

            vector.push(value);
        }

        Ok(Self {
            vector: Rc::new(RefCell::new(vector)),
        }
        .into())
    }

    fn internalize_index(&self, span: &S, index: isize) -> OperatorResult<S, usize> {
        let vector = self.vector.try_borrow().unwrap_borrow_failure(span)?;

        let new_index = if index >= 0 {
            Ok(index as usize)
        } else if let Some(new_index) = vector.len().checked_sub(index.unsigned_abs()) {
            Ok(new_index)
        } else {
            Err(Failure::IndexOutOfRange(span.clone(), index))
        }?;

        if new_index >= vector.len() {
            Err(Failure::IndexOutOfRange(span.clone(), index))
        } else {
            Ok(new_index)
        }
    }

    pub fn len(&self, span: &S) -> OperatorResult<S, usize> {
        let vector = self.vector.try_borrow().unwrap_borrow_failure(span)?;
        Ok(vector.len())
    }

    pub fn iter(&self, span: &S) -> OperatorResult<S, impl Iterator<Item = Value<S>>> {
        let vector = Rc::clone(&self.vector);

        #[self_referencing]
        struct ListIter<S: Span> {
            vector: Rc<RefCell<Vec<Value<S>>>>,

            #[borrows(vector)]
            #[not_covariant]
            reference: std::cell::Ref<'this, Vec<Value<S>>>,

            #[borrows(reference)]
            #[not_covariant]
            iterator: std::iter::Cloned<std::slice::Iter<'this, Value<S>>>,
        }

        impl<S: Span> Iterator for ListIter<S> {
            type Item = Value<S>;

            fn next(&mut self) -> Option<Self::Item> {
                self.with_iterator_mut(|iterator| iterator.next())
            }
        }

        ListIter::try_new(
            vector,
            |vector| vector.try_borrow().unwrap_borrow_failure(span),
            |reference| Ok(reference.iter().cloned()),
        )
    }
}

impl<S: Span> Object<S> for List<S> {
    fn matches_type(
        &self,
        ty: &VariableType<S>,
        _log: &mut dyn RuntimeLog<S>,
        _variable_name_span: &S,
    ) -> OperatorResult<S, bool> {
        Ok(matches!(ty, VariableType::List))
    }

    fn format(
        &self,
        log: &mut dyn RuntimeLog<S>,
        span: &S,
        f: &mut dyn Write,
        style: Style,
        precision: Option<u8>,
    ) -> OperatorResult<S, ()> {
        let vector = self.vector.try_borrow().unwrap_borrow_failure(span)?;

        for item in vector.iter() {
            item.format(log, span, f, style, precision)?;
        }

        Ok(())
    }

    fn index(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        span: &S,
        index: Value<S>,
    ) -> OperatorResult<S, Value<S>> {
        match index {
            Value::Scalar(scalar) => {
                let index = Number::try_from(scalar.clone()).map_err(|_| {
                    Failure::ExpectedGot(
                        span.clone(),
                        Number::static_type_name().into(),
                        <Scalar as Object<S>>::type_name(&scalar),
                    )
                })?;
                let index = index.to_index();

                let localized_index = self.internalize_index(span, index)?;

                self.vector
                    .try_borrow()
                    .unwrap_borrow_failure(span)?
                    .get(localized_index)
                    .cloned()
                    .ok_or(Failure::IndexOutOfRange(span.clone(), index))
            }
            Value::Range(range) => {
                let vector = self.vector.try_borrow().unwrap_borrow_failure(span)?;

                // TODO could we keep an immutable reference to the original list to avoid a copy?
                let slice = match (
                    range.lower_bound,
                    range.upper_bound,
                    range.upper_bound_is_inclusive,
                ) {
                    (None, None, false) => vector.get(..).ok_or((None, None)),
                    (Some(lower_bound), None, false) => {
                        let signed_lower_bound = lower_bound.to_index();
                        let lower_bound = self.internalize_index(span, signed_lower_bound)?;
                        vector
                            .get(lower_bound..)
                            .ok_or((Some(signed_lower_bound), None))
                    }
                    (None, Some(upper_bound), false) => {
                        let signed_upper_bound = upper_bound.to_index();
                        let upper_bound = self.internalize_index(span, signed_upper_bound)?;
                        vector
                            .get(..upper_bound)
                            .ok_or((None, Some(signed_upper_bound)))
                    }
                    (None, Some(upper_bound), true) => {
                        let signed_upper_bound = upper_bound.to_index();
                        let upper_bound = self.internalize_index(span, signed_upper_bound)?;
                        vector
                            .get(..=upper_bound)
                            .ok_or((None, Some(signed_upper_bound)))
                    }
                    (Some(lower_bound), Some(upper_bound), false) => {
                        let signed_lower_bound = lower_bound.to_index();
                        let lower_bound = self.internalize_index(span, signed_lower_bound)?;
                        let signed_upper_bound = upper_bound.to_index();
                        let upper_bound = self.internalize_index(span, signed_upper_bound)?;
                        vector
                            .get(lower_bound..upper_bound)
                            .ok_or((Some(signed_lower_bound), Some(signed_upper_bound)))
                    }
                    (Some(lower_bound), Some(upper_bound), true) => {
                        let signed_lower_bound = lower_bound.to_index();
                        let lower_bound = self.internalize_index(span, signed_lower_bound)?;
                        let signed_upper_bound = upper_bound.to_index();
                        let upper_bound = self.internalize_index(span, signed_upper_bound)?;
                        vector
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
        span: &S,
    ) -> OperatorResult<S, Box<dyn Iterator<Item = Value<S>>>> {
        Ok(Box::new(self.iter(span)?))
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
            "append" => |_context: &mut ExecutionContext<S>,
                         span: &S,
                         other: List<S>|
             -> OperatorResult<S, Value<S>> {
                let mut other = other.vector.try_borrow_mut().unwrap_borrow_failure(span)?;

                self.vector
                    .try_borrow_mut()
                    .unwrap_borrow_failure(span)?
                    .append(&mut other);

                Ok(NoneType.into())
            }
            .auto_call(context, span, arguments, expressions),
            "dedup" => {
                |_context: &mut ExecutionContext<S>, _span: &S| -> OperatorResult<S, Value<S>> {
                    self.vector
                        .try_borrow_mut()
                        .unwrap_borrow_failure(span)?
                        .dedup();
                    Ok(NoneType.into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "insert" => move |_context: &mut ExecutionContext<S>,
                              span: &S,
                              index: Number,
                              value: Value<S>|
                  -> OperatorResult<S, Value<S>> {
                let index = index.to_index();
                let index = self.internalize_index(span, index)?;
                self.vector
                    .try_borrow_mut()
                    .unwrap_borrow_failure(span)?
                    .insert(index, value);

                Ok(NoneType.into())
            }
            .auto_call(context, span, arguments, expressions),
            "is_empty" => {
                |_context: &mut ExecutionContext<S>, span: &S| -> OperatorResult<S, Value<S>> {
                    Ok(self
                        .vector
                        .try_borrow()
                        .unwrap_borrow_failure(span)?
                        .is_empty()
                        .into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "len" => {
                |_context: &mut ExecutionContext<S>, span: &S| -> OperatorResult<S, Value<S>> {
                    Float::new(self.vector.try_borrow().unwrap_borrow_failure(span)?.len() as f64)
                        .unwrap_not_nan(span)
                        .map(|n| n.into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "push" => |_context: &mut ExecutionContext<S>,
                       span: &S,
                       other: Value<S>|
             -> OperatorResult<S, Value<S>> {
                self.vector
                    .try_borrow_mut()
                    .unwrap_borrow_failure(span)?
                    .push(other);

                Ok(NoneType.into())
            }
            .auto_call(context, span, arguments, expressions),
            "remove" => |_context: &mut ExecutionContext<S>,
                         span: &S,
                         index: Number|
             -> OperatorResult<S, Value<S>> {
                let index = index.to_index();
                let index = self.internalize_index(span, index)?;

                self.vector
                    .try_borrow_mut()
                    .unwrap_borrow_failure(span)?
                    .remove(index);

                Ok(NoneType.into())
            }
            .auto_call(context, span, arguments, expressions),
            "contains" => |_context: &mut ExecutionContext<S>,
                           span: &S,
                           search: Value<S>|
             -> OperatorResult<S, Value<S>> {
                Ok(self
                    .vector
                    .try_borrow()
                    .unwrap_borrow_failure(span)?
                    .contains(&search)
                    .into())
            }
            .auto_call(context, span, arguments, expressions),
            "last" => {
                |_context: &mut ExecutionContext<S>, span: &S| -> OperatorResult<S, Value<S>> {
                    let vector = self.vector.try_borrow().unwrap_borrow_failure(span)?;
                    let last = vector.last();

                    if let Some(last) = last {
                        Ok(last.clone())
                    } else {
                        Err(Failure::ListIsEmpty(span.clone()))
                    }
                }
                .auto_call(context, span, arguments, expressions)
            }
            "first" => {
                |_context: &mut ExecutionContext<S>, span: &S| -> OperatorResult<S, Value<S>> {
                    let vector = self.vector.try_borrow().unwrap_borrow_failure(span)?;
                    let first = vector.first();

                    if let Some(first) = first {
                        Ok(first.clone())
                    } else {
                        Err(Failure::ListIsEmpty(span.clone()))
                    }
                }
                .auto_call(context, span, arguments, expressions)
            }
            "reverse" => {
                |_context: &mut ExecutionContext<S>, span: &S| -> OperatorResult<S, Value<S>> {
                    self.vector
                        .try_borrow_mut()
                        .unwrap_borrow_failure(span)?
                        .reverse();

                    Ok(NoneType.into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            "rotate_left" => |_context: &mut ExecutionContext<S>,
                              span: &S,
                              mid: Number|
             -> OperatorResult<S, Value<S>> {
                let mid = mid.to_index();
                if mid.is_positive() {
                    let mut vector = self.vector.try_borrow_mut().unwrap_borrow_failure(span)?;
                    let mid = mid as usize % vector.len();
                    vector.rotate_left(mid);

                    Ok(NoneType.into())
                } else {
                    Err(Failure::NumberMustBePositive(span.clone()))
                }
            }
            .auto_call(context, span, arguments, expressions),
            "rotate_right" => |_context: &mut ExecutionContext<S>,
                               span: &S,
                               mid: Number|
             -> OperatorResult<S, Value<S>> {
                let mid = mid.to_index();
                if mid.is_positive() {
                    let mut vector = self.vector.try_borrow_mut().unwrap_borrow_failure(span)?;
                    let mid = mid as usize % vector.len();
                    vector.rotate_right(mid);

                    Ok(NoneType.into())
                } else {
                    Err(Failure::NumberMustBePositive(span.clone()))
                }
            }
            .auto_call(context, span, arguments, expressions),
            "clone" => {
                |_context: &mut ExecutionContext<S>, span: &S| -> OperatorResult<S, Value<S>> {
                    Ok(List::from(self.iter(span)?).into())
                }
                .auto_call(context, span, arguments, expressions)
            }
            _ => Err(Failure::UnknownAttribute(attribute.clone())),
        }
    }

    fn export(
        &self,
        log: &mut dyn RuntimeLog<S>,
        span: &S,
    ) -> OperatorResult<S, SerializableValue> {
        let vector = self.vector.try_borrow().unwrap_borrow_failure(span)?;

        let mut list = Vec::with_capacity(vector.len());

        for item in vector.iter() {
            let serializable = item.export(log, span)?;
            list.push(serializable);
        }

        Ok(SerializableValue::List(list))
    }
}

impl<S: Span> TypedObject for List<S> {
    fn get_type<LS: Span>() -> VariableType<LS> {
        VariableType::List
    }
}

impl<S: Span> NamedObject for List<S> {
    fn static_type_name() -> &'static str {
        "List"
    }
}

impl<S: Span> List<S> {
    pub fn unpack_dynamic_length<T>(self, span: &S) -> OperatorResult<S, impl Iterator<Item = T>>
    where
        T: NamedObject,
        Value<S>: IntoVariant<T> + AsVariant<T> + TryInto<T>,
    {
        // We're going to return copies of all the values in the end anyway, so may as well just own/copy it now.
        let vector = Rc::unwrap_or_clone(self.vector).into_inner();

        // Verify that they're all of the right type.
        for (index, item) in vector.iter().enumerate() {
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
        let iter = vector.into_iter().map(|v| v.enum_downcast::<T>().unwrap());
        Ok(iter)
    }

    pub fn unpack_fixed_length<T, const D: usize>(
        self,
        span: &S,
    ) -> OperatorResult<S, impl Iterator<Item = T>>
    where
        T: NamedObject,
        Value<S>: IntoVariant<T> + AsVariant<T> + TryInto<T>,
    {
        let length = self.vector.try_borrow().unwrap_borrow_failure(span)?.len();
        if length == D {
            // This cannot exceed length D because we already validated that the list matched that length.
            self.unpack_dynamic_length(span)
        } else {
            Err(Failure::ListWrongLength(span.clone(), D, length))
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::script::{
        execution::{expressions::run_expression, ExecutionContext},
        parsing::Expression,
        Runtime, Scalar,
    };

    #[test]
    fn index() {
        let mut runtime = Runtime::default();
        ExecutionContext::create(&mut runtime, |context| {
            assert_eq!(
                run_expression(context, &Expression::parse("[1, 2, 3][0]").unwrap().1),
                Ok(Float::new(1.0).unwrap().into())
            );

            assert_eq!(
                run_expression(context, &Expression::parse("[1, 2, 3][-1]").unwrap().1),
                Ok(Float::new(3.0).unwrap().into())
            );

            assert_eq!(
                run_expression(context, &Expression::parse("[1, 2, 3][3]").unwrap().1),
                Err(Failure::IndexOutOfRange("[", 3))
            );
            assert_eq!(
                run_expression(context, &Expression::parse("[1, 2, 3][-4]").unwrap().1),
                Err(Failure::IndexOutOfRange("[", -4))
            );
        });
    }

    #[test]
    fn test_unpack() {
        assert_eq!(
            List::<&'static str>::from([
                Float::new(1.0).unwrap().into(),
                Float::new(2.0).unwrap().into(),
                Float::new(3.0).unwrap().into(),
            ])
            .unpack_fixed_length::<Scalar, 3usize>(&"span",)
            .map(|v| v.collect::<Vec<_>>()),
            Ok(vec![
                Float::new(1.0).unwrap().into(),
                Float::new(2.0).unwrap().into(),
                Float::new(3.0).unwrap().into(),
            ])
        );

        let values = [
            Float::new(1.0).unwrap().into(),
            Float::new(2.0).unwrap().into(),
        ];

        assert_eq!(
            List::<&'static str>::from(values)
                .unpack_fixed_length::<Scalar, 3usize>(&"span",)
                .map(|v| v.collect::<Vec<_>>()),
            Err(Failure::ListWrongLength("span", 3, 2))
        );

        let values = [Float::new(1.0).unwrap().into(), true.into()];

        assert_eq!(
            List::<&'static str>::from(values)
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
