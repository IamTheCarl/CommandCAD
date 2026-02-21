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

use crate::{
    build_closure_type, build_method,
    compile::{AstNode, Expression},
    execute_expression,
    execution::{
        errors::{Error, ErrorType, GenericFailure, Raise as _},
        values::{
            closure::BuiltinCallableDatabase, string::formatting::Style, Boolean, BuiltinFunction,
            Dictionary, MissingAttributeError, StaticType, UnsignedInteger, ValueNone,
        },
        ExecutionContext,
    },
    values::iterators::{IterableObject, ValueIterator},
};

use super::{value_type::ValueType, ExpressionResult, Object, StaticTypeName, Value};

use std::{borrow::Cow, cmp::Ordering, collections::HashMap, sync::Arc};

use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct List {
    // In theory, we could use a lot less memory by dynamically sizing everything to fit
    // our smallest type, but we aren't going to implement that today.

    // None means that the list is empty and can be used anywhere.
    internal_type: Option<ValueType>,
    values: Arc<Vec<Value>>,
}

impl List {
    pub fn from_ast(
        context: &ExecutionContext,
        ast_node: &AstNode<Vec<AstNode<Expression>>>,
    ) -> ExpressionResult<Self> {
        let values: ExpressionResult<Vec<Value>> = ast_node
            .node
            .par_iter()
            .map(|expression| execute_expression(context, expression))
            .collect();

        Ok(List::from_iter(context, values?))
    }

    pub fn from_iter<I>(context: &ExecutionContext, iterator: I) -> Self
    where
        I: IntoIterator<Item = Value>,
    {
        let values: Vec<_> = iterator.into_iter().collect();

        let internal_type =
            values
                .first()
                .map(|first| first.get_type(context))
                .map(|initial_type| {
                    values.iter().fold(initial_type, |accumulated, next| {
                        accumulated.merge(next.get_type(context))
                    })
                });

        Self {
            internal_type,
            values: Arc::new(values),
        }
    }

    fn map_raw(
        &self,
        context: &ExecutionContext,
        operation_name: &'static str,
        mut operation: impl FnMut(&Value) -> ExpressionResult<Value>,
    ) -> ExpressionResult<Self> {
        let values: Vec<Value> = self
            .values
            .iter()
            .enumerate()
            .map(|(index, value)| {
                operation(value).map_err(|error| OperationMappingError {
                    operation_name,
                    index,
                    error,
                })
            })
            .collect::<Result<_, OperationMappingError>>()
            .map_err(|error| error.to_error(context.stack_trace))?;

        Ok(Self::from_iter(context, values))
    }

    fn map_operation(
        &self,
        context: &ExecutionContext,
        operation_name: &'static str,
        operation: impl FnMut(&Value) -> ExpressionResult<Value>,
    ) -> ExpressionResult<Value> {
        self.map_raw(context, operation_name, operation)
            .map(|value| value.into())
    }
}

impl Object for List {
    fn get_type(&self, _context: &ExecutionContext) -> ValueType {
        ValueType::List(self.internal_type.clone().map(Box::new))
    }

    fn format(
        &self,
        context: &ExecutionContext,
        f: &mut dyn std::fmt::Write,
        style: Style,
        precision: Option<u8>,
    ) -> std::fmt::Result {
        write!(f, "[")?;

        let mut items = self.values.iter().peekable();

        while let Some(value) = items.next() {
            value.format(context, f, style, precision)?;
            if items.peek().is_some() {
                write!(f, ", ")?;
            }
        }

        write!(f, "]")?;

        Ok(())
    }

    fn eq(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<bool> {
        let rhs: &Self = rhs.downcast_for_binary_op_ref(context.stack_trace)?;
        Ok(self.values == rhs.values)
    }

    fn get_attribute(
        &self,
        context: &ExecutionContext,
        attribute: &str,
    ) -> ExpressionResult<Value> {
        match attribute {
            "append" => Ok(BuiltinFunction::new::<methods::Append>().into()),
            "slice" => Ok(BuiltinFunction::new::<methods::Slice>().into()),
            "get" => Ok(BuiltinFunction::new::<methods::Get>().into()),
            "chunks" => Ok(BuiltinFunction::new::<methods::Chunks>().into()),

            "retain" => Ok(BuiltinFunction::new::<methods::Retain>().into()),

            "sort" => Ok(BuiltinFunction::new::<methods::Sort>().into()),
            "reverse" => Ok(BuiltinFunction::new::<methods::Reverse>().into()),
            "truncate" => Ok(BuiltinFunction::new::<methods::Truncate>().into()),

            "deduplicate" => Ok(BuiltinFunction::new::<methods::Deduplicate>().into()),
            "union" => Ok(BuiltinFunction::new::<methods::Union>().into()),
            "intersection" => Ok(BuiltinFunction::new::<methods::Intersection>().into()),
            "difference" => Ok(BuiltinFunction::new::<methods::Difference>().into()),
            "symmetric_difference" => {
                Ok(BuiltinFunction::new::<methods::SymmetricDifference>().into())
            }
            "cartesian_product" => Ok(BuiltinFunction::new::<methods::CartesianProduct>().into()),
            "iter" => Ok(BuiltinFunction::new::<methods::Iterate>().into()),
            "iter_reverse" => Ok(BuiltinFunction::new::<methods::IterateReverse>().into()),
            _ => Err(MissingAttributeError {
                name: attribute.into(),
            }
            .to_error(context.stack_trace)),
        }
    }

    fn and(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        self.map_operation(context, "and", move |value| {
            value.clone().and(context, rhs.clone())
        })
    }
    fn or(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        self.map_operation(context, "or", move |value| {
            value.clone().or(context, rhs.clone())
        })
    }
    fn xor(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        self.map_operation(context, "xor", move |value| {
            value.clone().xor(context, rhs.clone())
        })
    }
    fn bit_and(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        self.map_operation(context, "bit and", move |value| {
            value.clone().bit_and(context, rhs.clone())
        })
    }
    fn bit_or(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        self.map_operation(context, "bit or", move |value| {
            value.clone().bit_or(context, rhs.clone())
        })
    }
    fn bit_xor(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        self.map_operation(context, "bit xor", move |value| {
            value.clone().bit_xor(context, rhs.clone())
        })
    }
    fn addition(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        self.map_operation(context, "addition", move |value| {
            value.clone().addition(context, rhs.clone())
        })
    }
    fn subtraction(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        self.map_operation(context, "subtraction", move |value| {
            value.clone().subtraction(context, rhs.clone())
        })
    }
    fn multiply(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        self.map_operation(context, "multiply", move |value| {
            value.clone().multiply(context, rhs.clone())
        })
    }
    fn divide(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        self.map_operation(context, "divide", move |value| {
            value.clone().divide(context, rhs.clone())
        })
    }
    fn exponent(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        self.map_operation(context, "exponent", move |value| {
            value.clone().exponent(context, rhs.clone())
        })
    }
    fn left_shift(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        self.map_operation(context, "left shift", move |value| {
            value.clone().left_shift(context, rhs.clone())
        })
    }
    fn right_shift(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        self.map_operation(context, "right shift", move |value| {
            value.clone().right_shift(context, rhs.clone())
        })
    }
    fn unary_plus(self, context: &ExecutionContext) -> ExpressionResult<Value> {
        self.map_operation(context, "unary plus", move |value| {
            value.clone().unary_plus(context)
        })
    }
    fn unary_minus(self, context: &ExecutionContext) -> ExpressionResult<Value> {
        self.map_operation(context, "unary minus", move |value| {
            value.clone().unary_minus(context)
        })
    }
    fn unary_not(self, context: &ExecutionContext) -> ExpressionResult<Value> {
        self.map_operation(context, "unary not", move |value| {
            value.clone().unary_not(context)
        })
    }
}

impl StaticTypeName for List {
    fn static_type_name() -> Cow<'static, str> {
        "List".into()
    }
}

impl StaticType for List {
    fn static_type() -> ValueType {
        ValueType::List(None)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ListIterator {
    list: List,
}

impl IterableObject for ListIterator {
    fn iterate<R>(
        &self,
        callback: impl FnOnce(&mut dyn Iterator<Item = Value>) -> ExpressionResult<R>,
    ) -> ExpressionResult<R> {
        let mut iter = self.list.values.iter().cloned();
        callback(&mut iter)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ListReverseIterator {
    list: List,
}

impl IterableObject for ListReverseIterator {
    fn iterate<R>(
        &self,
        callback: impl FnOnce(&mut dyn Iterator<Item = Value>) -> ExpressionResult<R>,
    ) -> ExpressionResult<R> {
        let mut iter = self.list.values.iter().rev().cloned();
        callback(&mut iter)
    }
}

#[derive(Debug)]
struct OperationMappingError {
    pub operation_name: &'static str,
    pub index: usize,
    pub error: Error,
}

impl ErrorType for OperationMappingError {}

impl std::fmt::Display for OperationMappingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Failed to map operation `{}` to index {}: {}",
            self.operation_name, self.index, self.error
        )
    }
}

#[derive(Debug)]
struct SortingError {
    pub errors: Vec<Error>,
}

impl ErrorType for SortingError {}

impl std::fmt::Display for SortingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.errors.len() == 1 {
            write!(
                f,
                "Failed to sort. An element of the list could not be compared: {}",
                self.errors[0]
            )
        } else {
            writeln!(
                f,
                "Failed to sort. Multiple elements could not be compared:",
            )?;

            for error in self.errors.iter() {
                writeln!(f, "\t{}", error)?;
            }

            Ok(())
        }
    }
}

mod methods {
    pub struct Append;
    pub struct Slice;
    pub struct Get;
    pub struct Chunks;

    pub struct Retain;

    pub struct Sort;
    pub struct Reverse;
    pub struct Truncate;

    pub struct Deduplicate;
    pub struct Union;
    pub struct Intersection;
    pub struct Difference;
    pub struct SymmetricDifference;
    pub struct CartesianProduct;

    pub struct Iterate;
    pub struct IterateReverse;
}

pub fn register_methods(database: &mut BuiltinCallableDatabase) {
    build_closure_type!(MapClosure(c: Value) -> Value);
    build_closure_type!(FoldClosure(previous: Value, c: Value) -> Value);
    build_closure_type!(RetainClosure(c: Value) -> Boolean);

    build_method!(
        database,
        methods::Append, "List::append", (
            context: &ExecutionContext,
            this: List,
            rhs: List
        ) -> List {
            let mut content = Arc::unwrap_or_clone(this.values);
            content.extend_from_slice(&rhs.values);

            Ok(List::from_iter(context, content))
        }
    );
    build_method!(
        database,
        methods::Slice, "List::slice", (
            context: &ExecutionContext,
            this: List,
            start: Option<UnsignedInteger> = ValueNone.into(),
            end: Option<UnsignedInteger> = ValueNone.into()
        ) -> List {

            let start = if let Some(start) = start {
                start.0 as usize
            } else {
                0usize
            };

            let end = if let Some(end) = end {
                end.0 as usize
            } else {
                this.values.len()
            };

            let slice = this.values.get(start..end);

            if let Some(slice) = slice {
                Ok(List::from_iter(context, slice.iter().cloned()))
            } else {
                Err(GenericFailure("Slice out of range".into()).to_error(context.stack_trace))
            }
        }
    );
    build_method!(
        database,
        methods::Get, "List::get", (
            context: &ExecutionContext,
            this: List,
            i: UnsignedInteger
        ) -> Value {
            let slice = this.values.get(i.0 as usize);

            if let Some(slice) = slice {
                Ok(slice.clone())
            } else {
                Err(GenericFailure("Index out of range".into()).to_error(context.stack_trace))
            }
        }
    );
    build_method!(
        database,
        methods::Chunks, "List::chunks", (
            context: &ExecutionContext,
            this: List,
            size: UnsignedInteger,
            strict: Boolean = Boolean(true).into()
        ) -> List {
            fn wrap_chunks<'i, I: Iterator<Item = &'i [Value]>>(context: &ExecutionContext, chunks: I) -> List {
                let mut list = Vec::new();
                for chunk in chunks {
                    list.push(List::from_iter(context, chunk.iter().cloned()).into());
                }
                List::from_iter(context, list.into_iter())
            }

            let chunks = if strict.0 {
                wrap_chunks(context, this.values.chunks_exact(size.0 as usize))
            } else {
                wrap_chunks(context, this.values.chunks(size.0 as usize))
            };

            Ok(chunks)

        }
    );
    build_method!(
        database,
        methods::Retain, "List::retain",(
            context: &ExecutionContext,
            this: List,
            f: RetainClosure
        ) -> List {
            let mut values: Vec<Value> = Vec::with_capacity(this.values.len());

            for value in this.values.iter() {
                let retain = f.call(context, Dictionary::new(context, HashMap::from_iter([
                    (
                        "c".into(),
                        value.clone()
                    )
                ])))?.downcast::<Boolean>(context.stack_trace)?;

                if retain.0 {
                    values.push(value.clone());
                }
            }

            Ok(List::from_iter(context, values.into_iter()))
        }
    );
    build_method!(
        database,
        methods::Sort, "List::sort",(
            context: &ExecutionContext,
            this: List
        ) -> List {
            let mut errors: Vec<Error> = vec![];
            let mut values: Vec<Value> = Arc::unwrap_or_clone(this.values);
            values.sort_unstable_by(|left: &Value, right: &Value| {
                let result = left.clone().cmp(context, right.clone());
                match result {
                    Ok(cmp) => cmp,
                    Err(error) => {
                        errors.push(error);
                        Ordering::Less
                    }
                }
            });

            if errors.is_empty() {
                Ok(List::from_iter(context, values.into_iter()))
            } else {
                Err(SortingError { errors }.to_error(context.stack_trace))
            }
        }
    );
    build_method!(
        database,
        methods::Reverse, "List::reverse",(
            context: &ExecutionContext,
            this: List
        ) -> List {
            let mut values: Vec<Value> = Arc::unwrap_or_clone(this.values);
            values.reverse();

            Ok(List::from_iter(context, values.into_iter()))
        }
    );
    build_method!(
        database,
        methods::Truncate, "List::truncate",(
            context: &ExecutionContext,
            this: List,
            length: UnsignedInteger
        ) -> List {
            let mut values: Vec<Value> = Arc::unwrap_or_clone(this.values);
            values.truncate(length.0 as usize);

            Ok(List::from_iter(context, values.into_iter()))
        }
    );
    build_method!(
        database,
        methods::Deduplicate, "List::deduplicate",(
            context: &ExecutionContext,
            this: List
        ) -> List {
            let mut values: Vec<Value> = Arc::unwrap_or_clone(this.values);
            values.dedup_by(|left, right| left.clone().eq(context, right.clone()).unwrap_or(false));

            Ok(List::from_iter(context, values.into_iter()))
        }
    );
    build_method!(
        database,
        methods::Union, "List::union",(
            context: &ExecutionContext,
            this: List,
            other: List
        ) -> List {
            let mut values: Vec<Value> = Arc::unwrap_or_clone(this.values);

            for new in other.values.iter() {
                if !values.iter().any(|old| old.clone().eq(context, new.clone()).unwrap_or(false)) {
                    values.push(new.clone());
                }
            }

            Ok(List::from_iter(context, values.into_iter()))
        }
    );
    build_method!(
        database,
        methods::Intersection, "List::intersection",(
            context: &ExecutionContext,
            this: List,
            other: List
        ) -> List {
            let mut values: Vec<Value> = Vec::new();

            for new in other.values.iter() {
                if this.values.iter().any(|old| old.clone().eq(context, new.clone()).unwrap_or(false)) {
                    values.push(new.clone());
                }
            }

            Ok(List::from_iter(context, values.into_iter()))
        }
    );
    build_method!(
        database,
        methods::Difference, "List::difference",(
            context: &ExecutionContext,
            this: List,
            other: List
        ) -> List {
            let mut values: Vec<Value> = Vec::new();

            // Add values from ourselves that the other list does not have.
            for new in this.values.iter() {
                if !other.values.iter().any(|old| old.clone().eq(context, new.clone()).unwrap_or(false)) {
                    values.push(new.clone());
                }
            }

            Ok(List::from_iter(context, values.into_iter()))
        }
    );
    build_method!(
        database,
        methods::SymmetricDifference, "List::symmetric_difference",(
            context: &ExecutionContext,
            this: List,
            other: List
        ) -> List {
            let mut values: Vec<Value> = Vec::new();

            // Add values from ourselves that the other list does not have.
            for new in this.values.iter() {
                if !other.values.iter().any(|old| old.clone().eq(context, new.clone()).unwrap_or(false)) {
                    values.push(new.clone());
                }
            }

            // Add values from the other list that are not already in the new list.
            for new in other.values.iter() {
                if !this.values.iter().any(|old| old.clone().eq(context, new.clone()).unwrap_or(false)) {
                    values.push(new.clone());
                }
            }

            Ok(List::from_iter(context, values.into_iter()))
        }
    );
    build_method!(
        database,
        methods::CartesianProduct, "List::cartesian_product",(
            context: &ExecutionContext,
            this: List,
            other: List
        ) -> List {
            let mut values: Vec<Value> = Vec::with_capacity(this.values.len() * other.values.len());

            for a in this.values.iter() {
                for b in other.values.iter() {
                    let list = List::from_iter(context, [a.clone(), b.clone()].into_iter());
                    values.push(list.into());
                }
            }

            Ok(List::from_iter(context, values.into_iter()))
        }
    );

    build_method!(
        database,
        methods::Iterate, "List::iter",(
            context: &ExecutionContext,
            this: List
        ) -> ValueIterator {
            Ok(ValueIterator::new(ListIterator { list: this }))
        }
    );
    build_method!(
        database,
        methods::IterateReverse, "List::iter_reverse",(
            context: &ExecutionContext,
            this: List
        ) -> ValueIterator {
            Ok(ValueIterator::new(ListReverseIterator { list: this }))
        }
    );
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::execution::{
        test_context, test_run,
        values::{Boolean, SignedInteger, UnsignedInteger},
    };

    #[test]
    fn create_empty() {
        test_context([], |context| {
            let product = test_run("[]").unwrap();
            assert_eq!(product, List::from_iter(context, []).into());
        })
    }

    #[test]
    fn create() {
        test_context([], |context| {
            let product = test_run("[1u, 2u, 3u]").unwrap();
            assert_eq!(
                product,
                List::from_iter(
                    context,
                    [
                        UnsignedInteger::from(1).into(),
                        UnsignedInteger::from(2).into(),
                        UnsignedInteger::from(3).into()
                    ]
                )
                .into()
            );
        })
    }

    #[test]
    fn create_multi_type() {
        test_context([], |context| {
            let product = test_run("[1u, 2i, 3u]").unwrap();
            assert_eq!(
                product,
                List::from_iter(
                    context,
                    [
                        UnsignedInteger::from(1).into(),
                        SignedInteger::from(2).into(),
                        UnsignedInteger::from(3).into()
                    ]
                )
                .into()
            );
        })
    }

    #[test]
    fn type_detection() {
        test_context([], |context| {
            assert_eq!(
                List::from_iter(
                    context,
                    [
                        UnsignedInteger::from(1).into(),
                        SignedInteger::from(2).into(),
                        UnsignedInteger::from(3).into()
                    ]
                )
                .internal_type,
                Some(ValueType::MultiType(
                    Box::new(ValueType::UnsignedInteger),
                    Box::new(ValueType::SignedInteger)
                ))
            );
        })
    }

    #[test]
    fn and() {
        let product = test_run("([true, false] && true) == [true, false]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn or() {
        let product = test_run("([true, false] || true) == [true, true]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn xor() {
        let product = test_run("([true, false] ^^ false) == [false, true]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn bit_and() {
        let product = test_run("([0xFFFFFFFFFFFFFFFFu, 0x0000000000000000u] & 0x0F0F0F0F0F0F0F0Fu) == [0x0F0F0F0F0F0F0F0Fu, 0x0000000000000000u]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn bit_or() {
        let product = test_run("([0xFFFFFFFFFFFFFFFFu, 0x0000000000000000u] | 0x0F0F0F0F0F0F0F0Fu) == [0xFFFFFFFFFFFFFFFFu, 0x0F0F0F0F0F0F0F0Fu]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn bit_xor() {
        let product = test_run("([0xFFFFFFFFFFFFFFFFu, 0x0000000000000000u] ^ 0x0F0F0F0F0F0F0F0Fu) == [0xF0F0F0F0F0F0F0F0u, 0x0F0F0F0F0F0F0F0Fu]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn addition() {
        let product = test_run("([1, 2] + 10) == [11, 12]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn subtraction() {
        let product = test_run("([1, 2] - 10) == [-9, -8]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn multiply() {
        let product = test_run("([1, 2] * 10) == [10, 20]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn divide() {
        let product = test_run("([5, 10] / 2) == [2.5, 5]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn exponent() {
        let product = test_run("([2, 10] ** 3) == [8, 1000]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn left_shift() {
        let product = test_run("([0x0Au, 0x0Fu] << 4u) == [0xA0u, 0xF0u]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn right_shift() {
        let product = test_run("([0xA0u, 0xF0u] >> 4u) == [0x0Au, 0x0Fu]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn unary_plus() {
        let product = test_run("(+[1, 2]) == [1, 2]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn unary_minus() {
        let product = test_run("(-[1, 2]) == [-1, -2]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn unary_not() {
        let product = test_run("(![true, false]) == [false, true]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_append() {
        let product = test_run("[1, 2]::append(rhs = [3, 4]) == [1, 2, 3, 4]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_slice() {
        let product = test_run("[1, 2, 3, 4]::slice(start = 1u, end = 3u) == [2, 3]").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("[1, 2, 3, 4]::slice(end = 3u) == [1, 2, 3]").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("[1, 2, 3, 4]::slice(start = 1u) == [2, 3, 4]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_get() {
        let product = test_run("let list = [1, 2, 3, 4]; in list::get(i = 0u) == 1 && list::get(i = 1u) == 2 && list::get(i = 2u) == 3 && list::get(i = 3u) == 4").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_chunks() {
        let product =
            test_run("[1, 2, 3, 4, 5, 6, 7]::chunks(size = 2u) == [[1, 2], [3, 4], [5, 6]]")
                .unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run(
            "[1, 2, 3, 4, 5, 6, 7]::chunks(size = 2u, strict = true) == [[1, 2], [3, 4], [5, 6]]",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("[1, 2, 3, 4, 5, 6, 7]::chunks(size = 2u, strict = false) == [[1, 2], [3, 4], [5, 6], [7]]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_retain() {
        let product = test_run(
            "[1u, 2u, 3u, 4u]::retain(f = (c: std.types.UInt) -> std.types.Bool: c == 1u || c == 3u) == [1u, 3u]",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_sort() {
        let product = test_run("[3, 2, 4, 1]::sort() == [1, 2, 3, 4]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_reverse() {
        let product = test_run("[1, 2, 3, 4]::reverse() == [4, 3, 2, 1]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_truncate() {
        let product = test_run("[1, 2, 3, 4]::truncate(length = 2u) == [1, 2]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_deduplicate() {
        let product =
            test_run("[1, 2, 1, 4, 3, 4, 3]::sort()::deduplicate() == [1, 2, 3, 4]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_union() {
        let product = test_run("[1, 2, 3]::union(other = [3, 4, 5]) == [1, 2, 3, 4, 5]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_intersection() {
        let product = test_run("[1, 2, 3]::intersection(other = [3, 4, 5]) == [3]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_difference() {
        let product = test_run("[1, 2, 3]::difference(other = [3, 4, 5]) == [1, 2]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_symmetric_difference() {
        let product =
            test_run("[1, 2, 3]::symmetric_difference(other = [3, 4, 5]) == [1, 2, 4, 5]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_cartesian_product() {
        let product = test_run("[1, 2, 3]::cartesian_product(other = [3, 4, 5]) == [[1, 3], [1, 4], [1, 5], [2, 3], [2, 4], [2, 5], [3, 3], [3, 4], [3, 5]]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn format() {
        let product = test_run("\"{value}\"::format(value = [1u]) == \"[1]\"").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("\"{value}\"::format(value = [1u, 2u]) == \"[1, 2]\"").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product =
            test_run("\"{value:X}\"::format(value = [0xDEADBEEFu]) == \"[DEADBEEF]\"").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn iterate() {
        let product = test_run("[1, 2, 3]::iter()::collect_list() == [1, 2, 3]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn iterate_reverse() {
        let product = test_run("[1, 2, 3]::iter_reverse()::collect_list() == [3, 2, 1]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }
}
