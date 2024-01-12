use std::{fmt::Write, rc::Rc};

use crate::script::{
    execution::{
        expressions::run_expression, types::Number, ControlFlow, ExecutionContext, ExecutionResult,
    },
    parsing::{self, Expression, VariableType},
    LogMessage, RuntimeLog, Span,
};

use super::{
    function::AutoCall, number::UnwrapNotNan, serializable::SerializableValue,
    string::formatting::Style, NamedObject, Object, Value,
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
        list: &parsing::List<S>,
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
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

    fn internalize_index(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        index: Number,
    ) -> ExecutionResult<'a, S, usize> {
        let raw_index = index.trunc() as isize;

        let index = if raw_index >= 0 {
            Ok(raw_index as usize)
        } else if let Some(index) = self.vector.len().checked_sub(raw_index.unsigned_abs()) {
            Ok(index)
        } else {
            log.push(LogMessage::IndexOutOfRange(span.clone(), raw_index));
            Err(ControlFlow::Failure)
        }?;

        if index >= self.vector.len() {
            log.push(LogMessage::IndexOutOfRange(span.clone(), raw_index));
            Err(ControlFlow::Failure)
        } else {
            Ok(index)
        }
    }

    pub fn len(&self) -> usize {
        self.vector.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Value<'a, S>> {
        self.vector.iter()
    }
}

impl<'a, S: Span> Object<'a, S> for List<'a, S> {
    fn matches_type(&self, ty: &VariableType<S>) -> bool {
        matches!(ty, VariableType::List)
    }

    fn format(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        f: &mut dyn Write,
        style: Style,
        precision: Option<u8>,
    ) -> ExecutionResult<'a, S, ()> {
        for item in self.vector.iter() {
            item.format(log, span, f, style, precision)?;
        }

        Ok(())
    }

    fn index(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        index: Value<'a, S>,
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        match index {
            Value::Number(index) => {
                let index = self.internalize_index(log, span, index)?;

                self.vector.get(index).cloned().ok_or(ControlFlow::Failure)
            }
            Value::Range(range) => {
                // TODO could we keep an immutable reference to the original list to avoid a copy?
                let slice = match (
                    range.lower_bound,
                    range.upper_bound,
                    range.upper_bound_is_inclusive,
                ) {
                    (None, None, false) => self.vector.get(..),
                    (Some(lower_bound), None, false) => {
                        let lower_bound = self.internalize_index(log, span, lower_bound)?;
                        self.vector.get(lower_bound..)
                    }
                    (None, Some(upper_bound), false) => {
                        let upper_bound = self.internalize_index(log, span, upper_bound)?;
                        self.vector.get(..upper_bound)
                    }
                    (None, Some(upper_bound), true) => {
                        let upper_bound = self.internalize_index(log, span, upper_bound)?;
                        self.vector.get(..=upper_bound)
                    }
                    (Some(lower_bound), Some(upper_bound), false) => {
                        let lower_bound = self.internalize_index(log, span, lower_bound)?;
                        let upper_bound = self.internalize_index(log, span, upper_bound)?;
                        self.vector.get(lower_bound..upper_bound)
                    }
                    (Some(lower_bound), Some(upper_bound), true) => {
                        let lower_bound = self.internalize_index(log, span, lower_bound)?;
                        let upper_bound = self.internalize_index(log, span, upper_bound)?;
                        self.vector.get(lower_bound..=upper_bound)
                    }
                    (_, None, true) => unreachable!(), // Inclusive ranges without an upper bound are illegal to construct.
                };

                slice
                    .map(|slice| Self::from(slice.iter().cloned()).into())
                    .ok_or(ControlFlow::Failure)
            }
            _ => {
                log.push(LogMessage::ExpectedGot(
                    span.clone(),
                    "Number or Range".into(),
                    index.type_name(),
                ));
                Err(ControlFlow::Failure)
            }
        }
    }

    fn iterate(
        &self,
        _log: &mut RuntimeLog<S>,
        _span: &S,
    ) -> ExecutionResult<'a, S, Box<dyn Iterator<Item = Value<'a, S>> + '_>> {
        Ok(Box::new(self.vector.iter().cloned()))
    }

    fn method_call(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        attribute: &S,
        arguments: Vec<Value<'a, S>>,
        expressions: &[Expression<S>],
    ) -> ExecutionResult<'a, S, Value<'a, S>> {
        match attribute.as_str() {
            "append" => |_log: &mut RuntimeLog<S>,
                         _span: &S,
                         other: List<'a, S>|
             -> ExecutionResult<S, Value<S>> {
                // TODO make this accept any iteratable value.

                let mut vector = self.unwrap_or_clone();
                vector.extend_from_slice(&other.vector);

                Ok(Self {
                    vector: Rc::new(vector),
                }
                .into())
            }
            .auto_call(log, span, arguments, expressions),
            "dedup" => |_log: &mut RuntimeLog<S>, _span: &S| -> ExecutionResult<S, Value<S>> {
                let mut vector = self.unwrap_or_clone();
                vector.dedup();

                Ok(Self {
                    vector: Rc::new(vector),
                }
                .into())
            }
            .auto_call(log, span, arguments, expressions),
            "insert" => |log: &mut RuntimeLog<S>,
                         span: &S,
                         index: Number,
                         value: Value<'a, S>|
             -> ExecutionResult<S, Value<S>> {
                let mut vector = self.unwrap_or_clone();
                let index = self.internalize_index(log, span, index)?;
                vector.insert(index, value);

                Ok(Self {
                    vector: Rc::new(vector),
                }
                .into())
            }
            .auto_call(log, span, arguments, expressions),
            "is_empty" => |_log: &mut RuntimeLog<S>, _span: &S| -> ExecutionResult<S, Value<S>> {
                Ok(self.vector.is_empty().into())
            }
            .auto_call(log, span, arguments, expressions),
            "len" => |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<S, Value<S>> {
                Number::new(self.vector.len() as f64).unwrap_not_nan(log, span)
            }
            .auto_call(log, span, arguments, expressions),
            "push" => |_log: &mut RuntimeLog<S>,
                       _span: &S,
                       other: Value<'a, S>|
             -> ExecutionResult<S, Value<S>> {
                let mut vector = self.unwrap_or_clone();
                vector.push(other);

                Ok(Self {
                    vector: Rc::new(vector),
                }
                .into())
            }
            .auto_call(log, span, arguments, expressions),
            "remove" => {
                |log: &mut RuntimeLog<S>, span: &S, index: Number| -> ExecutionResult<S, Value<S>> {
                    let index = self.internalize_index(log, span, index)?;

                    let mut vector = self.unwrap_or_clone();
                    vector.remove(index);

                    Ok(Self {
                        vector: Rc::new(vector),
                    }
                    .into())
                }
                .auto_call(log, span, arguments, expressions)
            }
            "contains" => |_log: &mut RuntimeLog<S>,
                           _span: &S,
                           search: Value<'a, S>|
             -> ExecutionResult<S, Value<S>> {
                Ok(self.vector.contains(&search).into())
            }
            .auto_call(log, span, arguments, expressions),
            "last" => |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<S, Value<S>> {
                let last = self.vector.last();

                if let Some(last) = last {
                    Ok(last.clone())
                } else {
                    log.push(LogMessage::ListIsEmpty(span.clone()));
                    Err(ControlFlow::Failure)
                }
            }
            .auto_call(log, span, arguments, expressions),
            "first" => |log: &mut RuntimeLog<S>, span: &S| -> ExecutionResult<S, Value<S>> {
                let first = self.vector.first();

                if let Some(first) = first {
                    Ok(first.clone())
                } else {
                    log.push(LogMessage::ListIsEmpty(span.clone()));
                    Err(ControlFlow::Failure)
                }
            }
            .auto_call(log, span, arguments, expressions),
            "reverse" => |_log: &mut RuntimeLog<S>, _span: &S| -> ExecutionResult<S, Value<S>> {
                let mut vector = self.unwrap_or_clone();
                vector.reverse();

                Ok(Self {
                    vector: Rc::new(vector),
                }
                .into())
            }
            .auto_call(log, span, arguments, expressions),
            "rotate_left" => {
                |_log: &mut RuntimeLog<S>, _span: &S, mid: Number| -> ExecutionResult<S, Value<S>> {
                    let mid = mid.trunc() as usize % self.vector.len();
                    let mut vector = self.unwrap_or_clone();
                    vector.rotate_left(mid);

                    Ok(Self {
                        vector: Rc::new(vector),
                    }
                    .into())
                }
                .auto_call(log, span, arguments, expressions)
            }
            "rotate_right" => {
                |_log: &mut RuntimeLog<S>, _span: &S, mid: Number| -> ExecutionResult<S, Value<S>> {
                    let mid = mid.trunc() as usize % self.vector.len();
                    let mut vector = self.unwrap_or_clone();
                    vector.rotate_right(mid);

                    Ok(Self {
                        vector: Rc::new(vector),
                    }
                    .into())
                }
                .auto_call(log, span, arguments, expressions)
            }
            _ => {
                log.push(LogMessage::UnknownAttribute(attribute.clone()));
                Err(ControlFlow::Failure)
            }
        }
    }

    fn export(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
    ) -> ExecutionResult<'a, S, SerializableValue> {
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

#[cfg(test)]
mod test {
    use super::*;

    use crate::script::{
        execution::{expressions::run_expression, ExecutionContext},
        parsing::Expression,
    };

    #[test]
    fn index() {
        let mut context = ExecutionContext {
            log: Default::default(),
            stack: Default::default(),
        };

        assert_eq!(
            run_expression(&mut context, &Expression::parse("[1, 2, 3][0]").unwrap().1),
            Ok(Number::new(1.0).unwrap().into())
        );

        assert_eq!(
            run_expression(&mut context, &Expression::parse("[1, 2, 3][-1]").unwrap().1),
            Ok(Number::new(3.0).unwrap().into())
        );

        assert_eq!(
            run_expression(&mut context, &Expression::parse("[1, 2, 3][3]").unwrap().1),
            Err(ControlFlow::Failure)
        );
        assert_eq!(
            run_expression(&mut context, &Expression::parse("[1, 2, 3][-4]").unwrap().1),
            Err(ControlFlow::Failure)
        );
    }
}
