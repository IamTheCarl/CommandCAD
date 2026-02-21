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

use std::{borrow::Cow, cmp::Ordering, collections::HashMap};

use enum_dispatch::enum_dispatch;

use crate::{
    build_closure_type, build_method,
    execution::errors::Raise as _,
    values::{
        integer::{RangeSInt, RangeUInt},
        list::{ListIterator, ListReverseIterator},
        string::{CharIterator, LineIterator},
        Boolean, BuiltinCallableDatabase, BuiltinFunction, Dictionary, IString, List,
        MissingAttributeError, Object, StaticType, StaticTypeName, Style, UnsignedInteger, Value,
        ValueNone, ValueType,
    },
    ExecutionContext, ExecutionResult,
};

use itertools::Itertools;

#[enum_dispatch]
pub trait IterableObject {
    fn iterate<R>(
        &self,
        callback: impl FnOnce(&mut dyn Iterator<Item = Value>) -> ExecutionResult<R>,
    ) -> ExecutionResult<R>;
}

#[allow(clippy::enum_variant_names)] // They're struct names, not just enum varients.
#[enum_dispatch(IterableObject)]
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum IterableSource {
    ListIterator,
    ListReverseIterator,
    CharIterator,
    LineIterator,
    RangeUInt,
    RangeSInt,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ValueIterator {
    source: IterableSource,
    stages: Vec<IteratorStage>,
}

impl Object for ValueIterator {
    fn get_type(&self, _context: &ExecutionContext) -> ValueType {
        ValueType::Iterator
    }

    fn format(
        &self,
        context: &ExecutionContext,
        f: &mut dyn std::fmt::Write,
        style: Style,
        precision: Option<u8>,
    ) -> std::fmt::Result {
        // TODO evaluate the iterator and print all the values.
        write!(f, "Iterator(")?;

        let result = self.iterate(
            context,
            Box::new(|context, iterator| {
                let f = &mut *f;
                let mut trampoline = move || -> std::fmt::Result {
                    let mut items = iterator.peekable();

                    while let Some(value) = items.next() {
                        match value {
                            Ok(value) => value.format(context, f, style, precision)?,
                            Err(error) => write!(f, "{error}")?,
                        }

                        if items.peek().is_some() {
                            write!(f, ", ")?;
                        }
                    }

                    Ok(())
                };

                Ok(trampoline())
            }),
        );

        match result {
            Ok(Ok(_)) => {}
            Ok(Err(error)) => write!(f, " - {error}")?,
            Err(error) => write!(f, " - {error}")?,
        }

        write!(f, ")")
    }

    fn get_attribute(&self, context: &ExecutionContext, attribute: &str) -> ExecutionResult<Value> {
        match attribute {
            "chunks" => Ok(BuiltinFunction::new::<methods::Chunks>().into()),
            "chunks_exact" => Ok(BuiltinFunction::new::<methods::ChunksExact>().into()),
            "chain" => Ok(BuiltinFunction::new::<methods::Chain>().into()),
            "cycle" => Ok(BuiltinFunction::new::<methods::Cycle>().into()),
            "debug" => Ok(BuiltinFunction::new::<methods::Debug>().into()),
            "enumerate" => Ok(BuiltinFunction::new::<methods::Enumerate>().into()),
            "filter" => Ok(BuiltinFunction::new::<methods::Filter>().into()),
            "filter_map" => Ok(BuiltinFunction::new::<methods::FilterMap>().into()),
            "flatten" => Ok(BuiltinFunction::new::<methods::Flatten>().into()),
            "map" => Ok(BuiltinFunction::new::<methods::Map>().into()),
            "map_while" => Ok(BuiltinFunction::new::<methods::MapWhile>().into()),
            "skip" => Ok(BuiltinFunction::new::<methods::Skip>().into()),
            "skip_while" => Ok(BuiltinFunction::new::<methods::SkipWhile>().into()),
            "step_by" => Ok(BuiltinFunction::new::<methods::StepBy>().into()),
            "take" => Ok(BuiltinFunction::new::<methods::Take>().into()),
            "take_while" => Ok(BuiltinFunction::new::<methods::TakeWhile>().into()),
            "zip" => Ok(BuiltinFunction::new::<methods::Zip>().into()),

            "all" => Ok(BuiltinFunction::new::<methods::All>().into()),
            "any" => Ok(BuiltinFunction::new::<methods::Any>().into()),
            "collect_list" => Ok(BuiltinFunction::new::<methods::CollectList>().into()),
            "collect_string" => Ok(BuiltinFunction::new::<methods::CollectString>().into()),
            "count" => Ok(BuiltinFunction::new::<methods::Count>().into()),
            "first" => Ok(BuiltinFunction::new::<methods::First>().into()),
            "fold" => Ok(BuiltinFunction::new::<methods::Fold>().into()),
            "last" => Ok(BuiltinFunction::new::<methods::Last>().into()),
            "max" => Ok(BuiltinFunction::new::<methods::Max>().into()),
            "min" => Ok(BuiltinFunction::new::<methods::Min>().into()),
            "nth" => Ok(BuiltinFunction::new::<methods::Nth>().into()),
            "product" => Ok(BuiltinFunction::new::<methods::Product>().into()),
            "sum" => Ok(BuiltinFunction::new::<methods::Sum>().into()),
            _ => Err(MissingAttributeError {
                name: attribute.into(),
            }
            .to_error(context)),
        }
    }
}

impl StaticTypeName for ValueIterator {
    fn static_type_name() -> Cow<'static, str> {
        "Iterator".into()
    }
}

impl StaticType for ValueIterator {
    fn static_type() -> ValueType {
        ValueType::Iterator
    }
}

/// We have to pass our callback as a box to our top-level iterate function, otherwise the compiler ends up with an
/// infinite recursion while trying to build the function graph.
type IterateCallback<'s, R> = Box<
    dyn FnOnce(
            &ExecutionContext,
            &mut dyn Iterator<Item = ExecutionResult<Value>>,
        ) -> ExecutionResult<R>
        + 's,
>;

impl ValueIterator {
    pub fn new(object: impl Into<IterableSource>) -> Self {
        Self {
            source: object.into(),
            stages: vec![],
        }
    }

    fn iterate<'s, R>(
        &'s self,
        context: &ExecutionContext,
        callback: IterateCallback<'s, R>,
    ) -> ExecutionResult<R> {
        self.source.iterate(move |iterator| {
            let mut stages = self.stages.iter();
            let iterator = &mut iterator.map(Ok);

            if let Some(first_stage) = stages.next() {
                first_stage.process(context, &mut stages, iterator, callback)
            } else {
                callback(context, iterator)
            }
        })
    }
}

build_closure_type!(FilterClosure(c: Value) -> Boolean);
build_closure_type!(FilterMapClosure(c: Value) -> Value);
build_closure_type!(MapClosure(c: Value) -> Value);
build_closure_type!(MapWhileClosure(c: Value) -> Value);
build_closure_type!(SkipWhileClosure(c: Value) -> Boolean);
build_closure_type!(TakeWhileClosure(c: Value) -> Boolean);

#[derive(Debug, Eq, PartialEq, Clone)]
enum IteratorStage {
    Chunks { size: usize },
    ChunksExact { size: usize },
    Chain { next: ValueIterator },
    Cycle { count: usize },
    Debug,
    Enumerate,
    Filter { filter: FilterClosure },
    FilterMap { filter_map: FilterMapClosure },
    Flatten,
    Map { map: MapClosure },
    MapWhile { map: MapWhileClosure },
    Skip { count: usize },
    SkipWhile { predicate: SkipWhileClosure },
    StepBy { count: usize },
    Take { count: usize },
    TakeWhile { predicate: TakeWhileClosure },
    Zip { other: ValueIterator },
}

impl IteratorStage {
    fn process<R>(
        &self,
        context: &ExecutionContext,
        stage_iter: &mut dyn Iterator<Item = &IteratorStage>,
        iterator: &mut dyn Iterator<Item = ExecutionResult<Value>>,
        callback: impl FnOnce(
            &ExecutionContext,
            &mut dyn Iterator<Item = ExecutionResult<Value>>,
        ) -> ExecutionResult<R>,
    ) -> ExecutionResult<R> {
        let start_next_stage = move |iterator: &mut dyn Iterator<
            Item = ExecutionResult<Value>,
        >|
              -> ExecutionResult<R> {
            if let Some(next_stage) = stage_iter.next() {
                next_stage.process::<R>(context, stage_iter, iterator, callback)
            } else {
                // This was the final stage.
                callback(context, iterator)
            }
        };

        match self {
            IteratorStage::Chunks { size } => {
                let chunks = iterator.chunks(*size);
                let mut iterator = chunks.into_iter().map(|chunk| {
                    let buffer: Vec<_> = chunk.collect::<ExecutionResult<_>>()?;
                    let list = List::from_iter(context, buffer);
                    Ok(list.into())
                });

                start_next_stage(&mut iterator)
            }
            IteratorStage::ChunksExact { size } => {
                let chunks = iterator.chunks(*size);
                let mut iterator = chunks.into_iter().filter_map(|chunk| {
                    let collection: ExecutionResult<Vec<_>> = chunk.collect();

                    match collection {
                        Ok(buffer) => {
                            if buffer.len() == *size {
                                let list = List::from_iter(context, buffer);
                                Some(Ok(list.into()))
                            } else {
                                None
                            }
                        }
                        Err(error) => Some(Err(error)),
                    }
                });

                start_next_stage(&mut iterator)
            }
            IteratorStage::Chain { next } => next.iterate(
                context,
                Box::new(|_context, next| {
                    let mut iterator = &mut iterator.chain(next);
                    start_next_stage(&mut iterator)
                }),
            ),
            IteratorStage::Cycle { count } => {
                let data: Vec<_> = iterator.collect::<ExecutionResult<_>>()?;
                let mut iterator = &mut std::iter::repeat_n(data.iter(), *count)
                    .flatten()
                    .cloned()
                    .map(Ok);
                start_next_stage(&mut iterator)
            }
            IteratorStage::Debug => {
                let mut iterator = iterator.inspect(|value| {
                    if let Ok(value) = value {
                        let mut message = String::new();

                        // This should never fail since we're writing to a string.
                        value
                            .format(context, &mut message, Style::Default, None)
                            .ok();

                        context.log.push_message(crate::LogMessage {
                            origin: context.stack_trace.bottom().clone(),
                            level: crate::LogLevel::Info,
                            message: message.into(),
                        });
                    }
                });

                start_next_stage(&mut iterator)
            }
            IteratorStage::Enumerate => {
                let enumerated = iterator.enumerate();
                let mut iterator = enumerated.map(|(index, value)| {
                    let value = value?;
                    let list = List::from_iter(
                        context,
                        [UnsignedInteger::from(index as u64).into(), value],
                    );
                    Ok(list.into())
                });

                start_next_stage(&mut iterator)
            }
            IteratorStage::Filter { filter } => {
                let iterator = &mut iterator.filter_map(|result| match result {
                    Ok(value) => {
                        let result = filter
                            .call(
                                context,
                                Dictionary::new(
                                    context,
                                    HashMap::from_iter([("c".into(), value.clone())]),
                                ),
                            )
                            .and_then(|value| value.downcast::<Boolean>(context.stack_trace));

                        match result {
                            Ok(keep) => {
                                if keep.0 {
                                    Some(Ok(value))
                                } else {
                                    None
                                }
                            }

                            Err(error) => Some(Err(error)),
                        }
                    }
                    Err(error) => Some(Err(error)),
                });

                start_next_stage(iterator)
            }
            IteratorStage::FilterMap { filter_map } => {
                let iterator = &mut iterator.filter_map(|result| match result {
                    Ok(value) => {
                        let result = filter_map.call(
                            context,
                            Dictionary::new(
                                context,
                                HashMap::from_iter([("c".into(), value.clone())]),
                            ),
                        );

                        match result {
                            Ok(mapped) => {
                                if mapped != ValueNone.into() {
                                    Some(Ok(mapped))
                                } else {
                                    None
                                }
                            }

                            Err(error) => Some(Err(error)),
                        }
                    }
                    Err(error) => Some(Err(error)),
                });

                start_next_stage(iterator)
            }
            IteratorStage::Flatten => {
                fn chain_iterator<R>(
                    context: &ExecutionContext,
                    start_next_stage: impl FnOnce(
                        &mut dyn Iterator<Item = ExecutionResult<Value>>,
                    ) -> ExecutionResult<R>,
                    previous_iterator: &mut dyn Iterator<Item = ExecutionResult<Value>>,
                    iterators: &mut dyn Iterator<Item = ExecutionResult<Value>>,
                ) -> ExecutionResult<R> {
                    if let Some(result) = iterators.next() {
                        let value = result?;
                        let sub_iterator: ValueIterator = value.downcast(context.stack_trace)?;

                        sub_iterator.iterate(
                            context,
                            Box::new(|context, sub_iterator| {
                                let mut new_iterator = previous_iterator.chain(sub_iterator);
                                chain_iterator(
                                    context,
                                    start_next_stage,
                                    &mut new_iterator,
                                    iterators,
                                )
                            }),
                        )
                    } else {
                        start_next_stage(previous_iterator)
                    }
                }

                chain_iterator(context, start_next_stage, &mut [].into_iter(), iterator)
            }
            IteratorStage::Map { map } => {
                let iterator = &mut iterator.map(|result| -> ExecutionResult<Value> {
                    let value = result?;
                    map.call(
                        context,
                        Dictionary::new(context, HashMap::from_iter([("c".into(), value.clone())])),
                    )
                });

                start_next_stage(iterator)
            }
            IteratorStage::MapWhile { map } => {
                let iterator = &mut iterator.map_while(|result| match result {
                    Ok(value) => {
                        let result = map.call(
                            context,
                            Dictionary::new(
                                context,
                                HashMap::from_iter([("c".into(), value.clone())]),
                            ),
                        );

                        match result {
                            Ok(mapped) => {
                                if mapped != ValueNone.into() {
                                    Some(Ok(mapped))
                                } else {
                                    None
                                }
                            }

                            Err(error) => Some(Err(error)),
                        }
                    }
                    Err(error) => Some(Err(error)),
                });

                start_next_stage(iterator)
            }
            IteratorStage::Skip { count } => {
                let mut iterator = &mut iterator.skip(*count);
                start_next_stage(&mut iterator)
            }
            IteratorStage::SkipWhile { predicate } => {
                while let Some(value) = iterator.next() {
                    let value = value?;
                    let should_skip = predicate
                        .call(
                            context,
                            Dictionary::new(
                                context,
                                HashMap::from_iter([("c".into(), value.clone())]),
                            ),
                        )
                        .and_then(|value| value.downcast::<Boolean>(context.stack_trace))?
                        .0;

                    if !should_skip {
                        return start_next_stage(&mut [Ok(value)].into_iter().chain(iterator));
                    }
                }

                // If we get here, that means we skipped the entire iterator and there is nothing
                // left to iterate.
                start_next_stage(&mut [].into_iter())
            }
            IteratorStage::StepBy { count } => {
                let mut iterator = &mut iterator.step_by(*count);
                start_next_stage(&mut iterator)
            }
            IteratorStage::Take { count } => {
                let mut iterator = &mut iterator.take(*count);
                start_next_stage(&mut iterator)
            }
            IteratorStage::TakeWhile { predicate } => {
                let mut iterator = iterator.map_while(|result| match result {
                    Ok(value) => {
                        let result = predicate
                            .call(
                                context,
                                Dictionary::new(
                                    context,
                                    HashMap::from_iter([("c".into(), value.clone())]),
                                ),
                            )
                            .and_then(|value| value.downcast::<Boolean>(context.stack_trace));

                        match result {
                            Ok(should_continue) => {
                                if should_continue.0 {
                                    Some(Ok(value))
                                } else {
                                    None
                                }
                            }
                            Err(error) => Some(Err(error)),
                        }
                    }
                    Err(error) => Some(Err(error)),
                });

                start_next_stage(&mut iterator)
            }
            IteratorStage::Zip { other } => other.iterate(
                context,
                Box::new(|context, other| {
                    let mut iterator = std::iter::from_fn(|| {
                        let a = iterator.next()?;
                        let b = other.next()?;

                        match (a, b) {
                            (Err(error), _) | (_, Err(error)) => Some(Err(error)),
                            (Ok(a), Ok(b)) => Some(Ok(List::from_iter(context, [a, b]).into())),
                        }
                    });

                    start_next_stage(&mut iterator)
                }),
            ),
        }
    }
}

pub mod methods {
    // Methods to add stages to the iterator:
    pub struct Chunks;
    pub struct ChunksExact;
    pub struct Chain;
    pub struct Cycle;
    pub struct Debug;
    pub struct Enumerate;
    pub struct Filter;
    pub struct FilterMap;
    pub struct Flatten;
    pub struct Map;
    pub struct MapWhile;
    pub struct Skip;
    pub struct SkipWhile;
    pub struct StepBy;
    pub struct Take;
    pub struct TakeWhile;
    pub struct Zip;

    // Methods that collect iterators into a proper output.
    pub struct All;
    pub struct Any;
    pub struct CollectList;
    pub struct CollectString;
    pub struct Count;
    pub struct First;
    pub struct Fold;
    pub struct Last;
    pub struct Max;
    pub struct Min;
    pub struct Nth;
    pub struct Product;
    pub struct Sum;
}

pub fn register_methods(database: &mut BuiltinCallableDatabase) {
    build_method!(
        database,
        methods::Chunks, "Iterator::chunks", (
            context: &ExecutionContext,
            this: ValueIterator,
            size: UnsignedInteger
        ) -> ValueIterator {
            let mut this = this;
            this.stages.push(IteratorStage::Chunks { size: size.0 as usize });
            Ok(this)
        }
    );
    build_method!(
        database,
        methods::ChunksExact, "Iterator::chunks_exact", (
            context: &ExecutionContext,
            this: ValueIterator,
            size: UnsignedInteger
        ) -> ValueIterator {
            let mut this = this;
            this.stages.push(IteratorStage::ChunksExact { size: size.0 as usize });
            Ok(this)
        }
    );
    build_method!(
        database,
        methods::Chain, "Iterator::chain", (
            context: &ExecutionContext,
            this: ValueIterator,
            next: ValueIterator
        ) -> ValueIterator {
            let mut this = this;
            this.stages.push(IteratorStage::Chain { next });
            Ok(this)
        }
    );
    build_method!(
        database,
        methods::Cycle, "Iterator::cycle", (
            context: &ExecutionContext,
            this: ValueIterator,
            count: UnsignedInteger
        ) -> ValueIterator {
            let mut this = this;
            this.stages.push(IteratorStage::Cycle { count: count.0 as usize });
            Ok(this)
        }
    );
    build_method!(
        database,
        methods::Debug, "Iterator::debug", (
            context: &ExecutionContext,
            this: ValueIterator
        ) -> ValueIterator {
            let mut this = this;
            this.stages.push(IteratorStage::Debug);
            Ok(this)
        }
    );
    build_method!(
        database,
        methods::Enumerate, "Iterator::enumerate", (
            context: &ExecutionContext,
            this: ValueIterator
        ) -> ValueIterator {
            let mut this = this;
            this.stages.push(IteratorStage::Enumerate);
            Ok(this)
        }
    );
    build_method!(
        database,
        methods::Filter, "Iterator::filter", (
            context: &ExecutionContext,
            this: ValueIterator,
            f: FilterClosure
        ) -> ValueIterator {
            let mut this = this;
            this.stages.push(IteratorStage::Filter { filter: f });
            Ok(this)
        }
    );
    build_method!(
        database,
        methods::FilterMap, "Iterator::filter_map", (
            context: &ExecutionContext,
            this: ValueIterator,
            f: FilterMapClosure
        ) -> ValueIterator {
            let mut this = this;
            this.stages.push(IteratorStage::FilterMap { filter_map: f });
            Ok(this)
        }
    );
    build_method!(
        database,
        methods::Flatten, "Iterator::flatten", (
            context: &ExecutionContext,
            this: ValueIterator
        ) -> ValueIterator {
            let mut this = this;
            this.stages.push(IteratorStage::Flatten);
            Ok(this)
        }
    );
    build_method!(
        database,
        methods::Map, "Iterator::map", (
            context: &ExecutionContext,
            this: ValueIterator,
            f: MapClosure
        ) -> ValueIterator {
            let mut this = this;
            this.stages.push(IteratorStage::Map { map: f });
            Ok(this)
        }
    );
    build_method!(
        database,
        methods::MapWhile, "Iterator::map_while", (
            context: &ExecutionContext,
            this: ValueIterator,
            f: MapWhileClosure
        ) -> ValueIterator {
            let mut this = this;
            this.stages.push(IteratorStage::MapWhile { map: f });
            Ok(this)
        }
    );
    build_method!(
        database,
        methods::Skip, "Iterator::skip", (
            context: &ExecutionContext,
            this: ValueIterator,
            count: UnsignedInteger
        ) -> ValueIterator {
            let mut this = this;
            this.stages.push(IteratorStage::Skip { count: count.0 as usize });
            Ok(this)
        }
    );
    build_method!(
        database,
        methods::SkipWhile, "Iterator::skip_while", (
            context: &ExecutionContext,
            this: ValueIterator,
            f: SkipWhileClosure
        ) -> ValueIterator {
            let mut this = this;
            this.stages.push(IteratorStage::SkipWhile { predicate: f });
            Ok(this)
        }
    );
    build_method!(
        database,
        methods::StepBy, "Iterator::step_by", (
            context: &ExecutionContext,
            this: ValueIterator,
            count: UnsignedInteger
        ) -> ValueIterator {
            let mut this = this;
            this.stages.push(IteratorStage::StepBy { count: count.0 as usize });
            Ok(this)
        }
    );
    build_method!(
        database,
        methods::Take, "Iterator::take", (
            context: &ExecutionContext,
            this: ValueIterator,
            count: UnsignedInteger
        ) -> ValueIterator {
            let mut this = this;
            this.stages.push(IteratorStage::Take { count: count.0 as usize });
            Ok(this)
        }
    );
    build_method!(
        database,
        methods::TakeWhile, "Iterator::take_while", (
            context: &ExecutionContext,
            this: ValueIterator,
            f: TakeWhileClosure
        ) -> ValueIterator {
            let mut this = this;
            this.stages.push(IteratorStage::TakeWhile { predicate: f });
            Ok(this)
        }
    );
    build_method!(
        database,
        methods::Zip, "Iterator::zip", (
            context: &ExecutionContext,
            this: ValueIterator,
            other: ValueIterator
        ) -> ValueIterator {
            let mut this = this;
            this.stages.push(IteratorStage::Zip { other });
            Ok(this)
        }
    );

    build_closure_type!(AllClosure(c: Value) -> Boolean);
    build_method!(
        database,
        methods::All, "Iterator::all", (
            context: &ExecutionContext,
            this: ValueIterator,
            f: AllClosure
        ) -> Boolean {
            this.iterate(context, Box::new(|context, iterator| {
                for result in iterator {
                    let value = result?;

                    let passes = f
                        .call(
                            context,
                            Dictionary::new(
                                context,
                                HashMap::from_iter([("c".into(), value.clone())]),
                            ),
                        )
                        .and_then(|value| value.downcast::<Boolean>(context.stack_trace))?;

                    if !passes.0 {
                        return Ok(Boolean(false));
                    }
                }

                Ok(Boolean(true))
            }))
        }
    );

    build_closure_type!(AnyClosure(c: Value) -> Boolean);
    build_method!(
        database,
        methods::Any, "Iterator::any", (
            context: &ExecutionContext,
            this: ValueIterator,
            f: AnyClosure
        ) -> Boolean {
            this.iterate(context, Box::new(|context, iterator| {
                for result in iterator {
                    let value = result?;

                    let passes = f
                        .call(
                            context,
                            Dictionary::new(
                                context,
                                HashMap::from_iter([("c".into(), value.clone())]),
                            ),
                        )
                        .and_then(|value| value.downcast::<Boolean>(context.stack_trace))?;

                    if passes.0 {
                        return Ok(Boolean(true));
                    }
                }

                Ok(Boolean(false))
            }))
        }
    );
    build_method!(
        database,
        methods::CollectList, "Iterator::collect_list", (
            context: &ExecutionContext,
            this: ValueIterator
        ) -> List {
            this.iterate(context, Box::new(|_context, iterator| {
                let values = iterator.collect::<ExecutionResult<Vec<Value>>>()?;
                Ok(List::from_iter(context, values.into_iter()))
            }))
        }
    );
    build_method!(
        database,
        methods::CollectString, "Iterator::collect_string", (
            context: &ExecutionContext,
            this: ValueIterator
        ) -> IString {
            this.iterate(context, Box::new(|_context, iterator| {
                let mut collected = String::new();

                for value in iterator {
                    let value = value?;
                    let string: IString = value.downcast(context.stack_trace)?;

                    collected += string.0.as_str();
                }

                Ok(IString(collected.into()))
            }))
        }
    );
    build_method!(
        database,
        methods::Count, "Iterator::count", (
            context: &ExecutionContext,
            this: ValueIterator
        ) -> UnsignedInteger {
            this.iterate(context, Box::new(|_context, iterator| {
                Ok(UnsignedInteger::from(iterator.count() as u64))
            }))
        }
    );
    build_method!(
        database,
        methods::First, "Iterator::first", (
            context: &ExecutionContext,
            this: ValueIterator
        ) -> Value {
            this.iterate(context, Box::new(|_context, iterator| {
                if let Some(value) = iterator.next() {
                    value
                } else {
                    Ok(ValueNone.into())
                }
            }))
        }
    );

    build_closure_type!(FoldClosure(previous: Value, c: Value) -> Value);
    build_method!(
        database,
        methods::Fold, "Iterator::fold",(
            context: &ExecutionContext,
            this: ValueIterator,
            init: Value,
            f: FoldClosure
        ) -> Value {

            this.iterate(context, Box::new(|context, iterator| {
                let mut accumulator = init;

                for component in iterator {
                    let component = component?;
                    accumulator = f.call(context, Dictionary::new(context, HashMap::from_iter([
                        (
                            "c".into(),
                            component.clone()
                        ),
                        (
                            "previous".into(),
                            accumulator
                        )
                    ])))?;
                }

                Ok(accumulator)
            }))

        }
    );
    build_method!(
        database,
        methods::Last, "Iterator::last", (
            context: &ExecutionContext,
            this: ValueIterator
        ) -> Value {
            this.iterate(context, Box::new(|_context, iterator| {
                if let Some(value) = iterator.last() {
                    value
                } else {
                    Ok(ValueNone.into())
                }
            }))
        }
    );
    build_method!(
        database,
        methods::Max, "Iterator::max", (
            context: &ExecutionContext,
            this: ValueIterator
        ) -> Value {
            this.iterate(context, Box::new(|_context, iterator| {
                if let Some(init) = iterator.next() {
                    let mut max = init?;

                    for value in iterator {
                        let value = value?;
                        if matches!(value.clone().cmp(context, max.clone())?, Ordering::Greater) {
                            max = value;
                        }
                    }

                    Ok(max)
                } else {
                    Ok(ValueNone.into())
                }
            }))
        }
    );
    build_method!(
        database,
        methods::Min, "Iterator::min", (
            context: &ExecutionContext,
            this: ValueIterator
        ) -> Value {
            this.iterate(context, Box::new(|_context, iterator| {
                if let Some(init) = iterator.next() {
                    let mut min = init?;

                    for value in iterator {
                        let value = value?;
                        if matches!(value.clone().cmp(context, min.clone())?, Ordering::Less) {
                            min = value;
                        }
                    }

                    Ok(min)
                } else {
                    Ok(ValueNone.into())
                }
            }))
        }
    );
    build_method!(
        database,
        methods::Nth, "Iterator::nth", (
            context: &ExecutionContext,
            this: ValueIterator,
            n: UnsignedInteger
        ) -> Value {
            this.iterate(context, Box::new(|_context, iterator| {
                if let Some(value) = iterator.nth(n.0 as usize) {
                    value
                } else {
                    Ok(ValueNone.into())
                }
            }))
        }
    );
    build_method!(
        database,
        methods::Product, "Iterator::product",(
            context: &ExecutionContext,
            this: ValueIterator
        ) -> Value {
            this.iterate(context, Box::new(|context, iterator| {
                if let Some(init) = iterator.next() {
                    let mut accumulator = init?;

                    for component in iterator {
                        let component = component?;
                        accumulator = accumulator.multiply(context, component)?;
                    }

                    Ok(accumulator)
                } else {
                    Ok(ValueNone.into())
                }
            }))

        }
    );
    build_method!(
        database,
        methods::Sum, "Iterator::sum",(
            context: &ExecutionContext,
            this: ValueIterator
        ) -> Value {
            this.iterate(context, Box::new(|context, iterator| {
                if let Some(init) = iterator.next() {
                    let mut accumulator = init?;

                    for component in iterator {
                        let component = component?;
                        accumulator = accumulator.addition(context, component)?;
                    }

                    Ok(accumulator)
                } else {
                    Ok(ValueNone.into())
                }
            }))

        }
    );
}

#[cfg(test)]
mod test {
    use crate::execution::{test_run, values::Boolean};

    #[test]
    fn chunks() {
        let product = test_run(
            "[1, 2, 3, 4, 5]::iter()::chunks(size = 2u)::collect_list() == [[1, 2], [3, 4], [5]]",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn chunks_exact() {
        let product = test_run(
            "[1, 2, 3, 4, 5]::iter()::chunks_exact(size = 2u)::collect_list() == [[1, 2], [3, 4]]",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn chain() {
        let product = test_run(
            "[1, 2]::iter()::chain(next = [3, 4]::iter())::collect_list() == [1, 2, 3, 4]",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn cycle() {
        let product = test_run(
            "[1, 2]::iter()::cycle(count = 4u)::collect_list() == [1, 2, 1, 2, 1, 2, 1, 2]",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    // IteratorStage::Debug => &mut iterator.map(|value| {

    #[test]
    fn enumerate() {
        let product = test_run(
            "[0, 1, 2, 3]::iter()::enumerate()::collect_list() == [[0u, 0], [1u, 1], [2u, 2], [3u, 3]]",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn filter() {
        let product = test_run(
            "[1, 2, 3, 4, 5]::iter()::filter(f = (c: std.scalar.Number) -> std.types.Bool: c != 4)::collect_list() == [1, 2, 3, 5]",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn filter_map() {
        let product = test_run(
            "[1, 2, 3, 4, 5]::iter()::filter_map(f = (c: std.scalar.Number) -> std.scalar.Number | std.types.None: if c != 4 then c * 2 else std.consts.None )::collect_list() == [2, 4, 6, 10]",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn fatten() {
        let product = test_run(
            "[[1, 2]::iter(), [3, 4]::iter(), [5]::iter()]::iter()::flatten()::collect_list() == [1, 2, 3, 4, 5]",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn map() {
        let product = test_run(
            "[1, 2, 3, 4, 5]::iter()::map(f = (c: std.scalar.Number) -> std.scalar.Number: c + 1 )::collect_list() == [2, 3, 4, 5, 6]",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn map_while() {
        let product = test_run(
            "[1, 2, 3, 4, 5]::iter()::map_while(f = (c: std.scalar.Number) -> std.scalar.Number | std.types.None: if c != 4 then c + 1 else std.consts.None )::collect_list() == [2, 3, 4]",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn skip() {
        let product =
            test_run("[1, 2, 3, 4]::iter()::skip(count = 2u)::collect_list() == [3, 4]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn skip_while() {
        let product =
            test_run("[1, 2, 3, 4, 5]::iter()::skip_while(f = (c: std.scalar.Number) -> std.types.Bool: c != 3)::collect_list() == [3, 4, 5]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn skip_while_all() {
        let product =
            test_run("[1, 2, 3, 4, 5]::iter()::skip_while(f = (c: std.scalar.Number) -> std.types.Bool: c != 10)::collect_list() == []").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn step_by() {
        let product = test_run(
            "[1, 2, 3, 4, 5, 6]::iter()::step_by(count = 2u)::collect_list() == [1, 3, 5]",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn take() {
        let product =
            test_run("[1, 2, 3, 4]::iter()::take(count = 2u)::collect_list() == [1, 2]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn take_while() {
        let product =
            test_run("[1, 2, 3, 4]::iter()::take_while(f = (c: std.scalar.Number) -> std.types.Bool: c != 3)::collect_list() == [1, 2]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn zip() {
        let product =
            test_run("[1, 2, 3, 4]::iter()::zip(other = [5, 6, 7, 8]::iter())::collect_list() == [[1, 5], [2, 6], [3, 7], [4, 8]]").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product =
            test_run("[1, 2, 3, 4]::iter()::zip(other = [5, 6, 7]::iter())::collect_list() == [[1, 5], [2, 6], [3, 7]]").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product =
            test_run("[1, 2, 3]::iter()::zip(other = [5, 6, 7, 8]::iter())::collect_list() == [[1, 5], [2, 6], [3, 7]]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn all() {
        let product =
            test_run("[1, 2, 3]::iter()::all(f = (c: std.scalar.Number) -> std.types.Bool: c < 5)")
                .unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run(
            "[1, 2, 3, 5]::iter()::all(f = (c: std.scalar.Number) -> std.types.Bool: c < 5)",
        )
        .unwrap();
        assert_eq!(product, Boolean(false).into());
    }

    #[test]
    fn any() {
        let product =
            test_run("[1, 2, 6]::iter()::any(f = (c: std.scalar.Number) -> std.types.Bool: c < 5)")
                .unwrap();
        assert_eq!(product, Boolean(true).into());

        let product =
            test_run("[5, 6, 7]::iter()::any(f = (c: std.scalar.Number) -> std.types.Bool: c < 5)")
                .unwrap();
        assert_eq!(product, Boolean(false).into());
    }

    #[test]
    fn collect_list() {
        let product = test_run("[1, 2, 3]::iter()::collect_list() == [1, 2, 3]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn collect_string() {
        let product = test_run("[1, 2, 3]::iter()::map(f = (c: std.scalar.Number) -> std.types.String: \"{c}\"::format())::collect_string() == \"123\"]").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn count() {
        let product = test_run("[1, 2, 3]::iter()::count() == 3u").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn first() {
        let product = test_run("[1, 2, 3]::iter()::first() == 1").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("[]::iter()::first() == std.consts.None").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn fold() {
        let product = test_run("[1, 2, 3]::iter()::fold(init = 0, f = (previous: std.scalar.Number, c: std.scalar.Number) -> std.scalar.Number: previous + c) == 6").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn last() {
        let product = test_run("[1, 2, 3]::iter()::last() == 3").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("[]::iter()::last() == std.consts.None").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn max() {
        let product = test_run("[1, 3, 2]::iter()::max() == 3").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("[]::iter()::max() == std.consts.None").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn min() {
        let product = test_run("[2, 3, 1]::iter()::min() == 1").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("[]::iter()::min() == std.consts.None").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn nth() {
        let product = test_run("[2, 3, 1]::iter()::nth(n = 1u) == 3").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("[]::iter()::nth(n = 0u) == std.consts.None").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn product() {
        let product = test_run("[2, 3, 4]::iter()::product() == 24").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn sum() {
        let product = test_run("[2, 3, 4]::iter()::sum() == 9").unwrap();
        assert_eq!(product, Boolean(true).into());
    }
}
