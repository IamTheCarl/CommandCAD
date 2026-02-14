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

use common_data_types::{Dimension, Float};
use hashable_map::HashableMap;
use imstr::ImString;

use crate::{
    build_closure_type, build_method,
    execution::{
        errors::{GenericFailure, Raise},
        logging::{LocatedStr, LogLevel, LogMessage},
        stack::ScopeType,
        values::{
            closure::{BuiltinCallable, Signature},
            string::formatting::{Format, Style},
            Boolean, BuiltinCallableDatabase, BuiltinFunction, Dictionary, List,
            MissingAttributeError, Scalar, SignedInteger, StaticType, StructDefinition,
            UnsignedInteger, ValueNone,
        },
        ExecutionContext,
    },
};

use super::{value_type::ValueType, ExpressionResult, Object, StaticTypeName, Value};

use std::{borrow::Cow, collections::HashMap, sync::Arc};

pub mod formatting;

#[derive(Debug, Hash, Clone, Eq, PartialEq)]
pub struct IString(pub ImString);

impl Object for IString {
    fn get_type(&self, _context: &ExecutionContext) -> ValueType {
        ValueType::String
    }

    fn format(
        &self,
        context: &ExecutionContext,
        f: &mut dyn std::fmt::Write,
        style: Style,
        precision: Option<u8>,
    ) -> std::fmt::Result {
        if !matches!(style, Style::Default) {
            context.log.push_message(LogMessage {
                origin: context.stack_trace.bottom().clone(),
                level: LogLevel::Warning,
                message: "Strings only support default formatting".into(),
            });
        }

        if precision.is_some() {
            context.log.push_message(LogMessage {
                origin: context.stack_trace.bottom().clone(),
                level: LogLevel::Warning,
                message: "Strings cannot be formatted with precision".into(),
            });
        }

        write!(f, "{}", self.0)
    }

    fn eq(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<bool> {
        let rhs: &Self = rhs.downcast_for_binary_op_ref(context.stack_trace)?;
        Ok(self.0 == rhs.0)
    }

    fn get_attribute(
        &self,
        context: &ExecutionContext,
        attribute: &str,
    ) -> ExpressionResult<Value> {
        match attribute {
            "format" => Ok(BuiltinFunction::new::<methods::Format>().into()),

            "append" => Ok(BuiltinFunction::new::<methods::Append>().into()),
            "slice" => Ok(BuiltinFunction::new::<methods::Slice>().into()),
            "chunks" => Ok(BuiltinFunction::new::<methods::Chunks>().into()),
            "lines" => Ok(BuiltinFunction::new::<methods::Lines>().into()),

            "map" => Ok(BuiltinFunction::new::<methods::Map>().into()),
            "fold" => Ok(BuiltinFunction::new::<methods::Fold>().into()),
            "retain" => Ok(BuiltinFunction::new::<methods::Retain>().into()),

            "reverse" => Ok(BuiltinFunction::new::<methods::Reverse>().into()),
            "truncate" => Ok(BuiltinFunction::new::<methods::Truncate>().into()),

            "to_lowercase" => Ok(BuiltinFunction::new::<methods::ToLowercase>().into()),
            "to_uppercase" => Ok(BuiltinFunction::new::<methods::ToUppercase>().into()),

            "parse_scalar" => Ok(BuiltinFunction::new::<methods::ParseScalar>().into()),
            "parse_unsigned_integer" => {
                Ok(BuiltinFunction::new::<methods::ParseUnsignedInteger>().into())
            }
            "parse_signed_integer" => {
                Ok(BuiltinFunction::new::<methods::ParseSignedInteger>().into())
            }

            "contains" => Ok(BuiltinFunction::new::<methods::Contains>().into()),
            _ => Err(MissingAttributeError {
                name: attribute.into(),
            }
            .to_error(context.stack_trace)),
        }
    }
}

impl StaticTypeName for IString {
    fn static_type_name() -> Cow<'static, str> {
        "String".into()
    }
}

impl StaticType for IString {
    fn static_type() -> ValueType {
        ValueType::String
    }
}

impl<S> From<S> for IString
where
    S: Into<ImString>,
{
    fn from(value: S) -> Self {
        Self(value.into())
    }
}

mod methods {
    pub struct Format;

    pub struct Append;
    pub struct Slice;
    pub struct Chunks;
    pub struct Lines;

    pub struct Map;
    pub struct Fold;
    pub struct Retain;

    pub struct Reverse;
    pub struct Truncate;

    pub struct ToLowercase;
    pub struct ToUppercase;

    pub struct ParseScalar;
    pub struct ParseUnsignedInteger;
    pub struct ParseSignedInteger;

    pub struct Contains;
}

fn register_format_method(database: &mut BuiltinCallableDatabase) {
    struct BuiltFunction {
        signature: Arc<Signature>,
    }

    impl BuiltinCallable for BuiltFunction {
        fn call(
            &self,
            context: &ExecutionContext,
            argument: Dictionary,
        ) -> ExpressionResult<Value> {
            let this = context
                .get_variable(LocatedStr {
                    location: context.stack_trace.bottom().clone(),
                    string: "self",
                })?
                .downcast_ref::<IString>(context.stack_trace)?
                .clone();

            let (excess, format) = Format::parse(&this.0).map_err(|error| {
                GenericFailure(format!("Failed to parse formatting string: {error:?}").into())
                    .to_error(context.stack_trace)
            })?;
            assert!(excess.is_empty());

            let mut output = String::new();
            format.format(context, &mut output, argument)?;

            Ok(IString(ImString::from(output)).into())
        }

        fn name(&self) -> &str {
            "String::format"
        }

        fn signature(&self) -> &Arc<Signature> {
            &self.signature
        }

        fn scope_type(&self) -> ScopeType {
            ScopeType::Inherited
        }
    }

    let callable = BuiltFunction {
        signature: Arc::new(Signature {
            argument_type: StructDefinition {
                members: Arc::new(HashableMap::from(HashMap::new())),
                variadic: true,
            },
            return_type: ValueType::String,
        }),
    };

    database.register::<methods::Format>(Box::new(callable))
}

pub fn register_methods(database: &mut BuiltinCallableDatabase) {
    build_closure_type!(MapClosure(c: Value) -> Value);
    build_closure_type!(FoldClosure(previous: Value, c: Value) -> Value);
    build_closure_type!(RetainClosure(c: Value) -> Boolean);

    register_format_method(database);

    build_method!(
        database,
        methods::Append, "String::append", (
            _context: &ExecutionContext,
            this: IString,
            rhs: IString
        ) -> IString {
            let mut new = this.0.into_std_string();
            new.push_str(&rhs.0);
            Ok(IString(new.into()))
        }
    );
    build_method!(
        database,
        methods::Slice, "String::slice", (
            _context: &ExecutionContext,
            this: IString,
            start: Option<UnsignedInteger> = ValueNone.into(),
            end: Option<UnsignedInteger> = ValueNone.into()
        ) -> IString {
            let start = if let Some(start) = start {
                start.0 as usize
            } else {
                0usize
            };

            let end = if let Some(end) = end {
                end.0 as usize
            } else {
                this.0.len()
            };

            Ok(IString(this.0.slice(start..end)))
        }
    );
    build_method!(
        database,
        methods::Chunks, "String::chunks", (
            context: &ExecutionContext,
            this: IString,
            size: UnsignedInteger,
            strict: Boolean = Boolean(true).into()
        ) -> List {
            let size = size.0 as usize;
            let strict = strict.0;
            let num_chunks = this.0.len() / size;
            let remainder = this.0.len() % size;
            let mut chunks: Vec<Value> = Vec::with_capacity(num_chunks + if !strict && remainder != 0 { 1 } else { 0 });

            for index in 0..num_chunks {
                let start = index * size;
                let end = start + size;
                chunks.push(IString(this.0.slice(start..end)).into());
            }

            if !strict {
                chunks.push(IString(this.0.slice((num_chunks * size)..this.0.len())).into());
            }

            Ok(List::from_iter(context, chunks))
        }
    );
    build_method!(
        database,
        methods::Lines, "String::lines", (
            context: &ExecutionContext,
            this: IString,
            include_empty: Boolean = Boolean(true).into()
        ) -> List {
            let include_empty = include_empty.0;
            let lines: Vec<Value> = if include_empty {
                this.0.lines().map(|line| IString(line).into()).collect()
            } else {
                this.0.lines().filter(|line| !line.is_empty()).map(|line| IString(line).into()).collect()
            };
            Ok(List::from_iter(context, lines))
        }
    );
    build_method!(
        database,
        methods::Map, "String::map", (
            context: &ExecutionContext,
            this: IString,
            f: MapClosure
        ) -> IString {
            let mut string = String::new();
            let result: ExpressionResult<()> = this.0.chars().try_for_each(|c| {
                let result = f.call(context, Dictionary::new(context, HashMap::from_iter([
                    (
                        "c".into(),
                        IString(ImString::from(format!("{c}"))).into()
                    )
                ])))?.downcast::<IString>(context.stack_trace)?;
                string.push_str(&result.0);

                Ok(())
            });
            result?;

            Ok(IString(ImString::from(string)))
        }
    );
    build_method!(
        database,
        methods::Fold, "String::fold", (
            context: &ExecutionContext,
            this: IString,
            init: Value,
            f: FoldClosure
        ) -> Value {
            let mut accumulator = init;
            for c in this.0.chars() {
                accumulator = f.call(context, Dictionary::new(context, HashMap::from_iter([
                    (
                        "c".into(),
                        IString(ImString::from(format!("{c}"))).into()
                    ),
                    (
                        "previous".into(),
                        accumulator
                    )
                ])))?;
            }

            Ok(accumulator)
        }
    );
    build_method!(
        database,
        methods::Retain, "String::retain",(
            context: &ExecutionContext,
            this: IString,
            f: RetainClosure
        ) -> IString {
            let mut product: String = String::with_capacity(this.0.len());

            for c in this.0.chars() {
                let retain = f.call(context, Dictionary::new(context, HashMap::from_iter([
                    (
                        "c".into(),
                        IString(ImString::from(format!("{c}"))).into()
                    )
                ])))?.downcast::<Boolean>(context.stack_trace)?;

                if retain.0 {
                    product.push(c);
                }
            }

            Ok(IString(ImString::from(product)))
        }
    );
    build_method!(
        database,
        methods::Reverse, "String::reverse",(
            _context: &ExecutionContext,
            this: IString
        ) -> IString {
            let reversed: String = this.0.as_str().chars().rev().collect();
            Ok(IString(ImString::from(reversed)))
        }
    );
    build_method!(
        database,
        methods::Truncate, "String::truncate",(
            _context: &ExecutionContext,
            this: IString,
            length: UnsignedInteger
        ) -> IString {
            let length = length.0 as usize;
            let length = this.0.len().min(length);

            Ok(IString(this.0.slice(0..length)))
        }
    );
    build_method!(
        database,
        methods::ToLowercase, "String::to_lowercase",(
            _context: &ExecutionContext,
            this: IString
        ) -> IString {
            let text = this.0.as_str().to_lowercase();
            Ok(IString(ImString::from(text)))
        }
    );
    build_method!(
        database,
        methods::ToUppercase, "String::to_uppercase",(
            _context: &ExecutionContext,
            this: IString
        ) -> IString {
            let text = this.0.as_str().to_uppercase();
            Ok(IString(ImString::from(text)))
        }
    );
    build_method!(
        database,
        methods::ParseScalar, "String::parse_scalar",(
            context: &ExecutionContext,
            this: IString
        ) -> Scalar {
            let value = this.0.parse::<Float>()
                .map_err(|error| GenericFailure(format!("Failed to parse scalar value: {error:?}").into()).to_error(context.stack_trace))?;
            Ok(Scalar {
                dimension: Dimension::zero(),
                value,
            })
        }
    );
    build_method!(
        database,
        methods::ParseUnsignedInteger, "String::parse_unsigned_integer",(
            context: &ExecutionContext,
            this: IString
        ) -> UnsignedInteger {
            let value = this.0.parse::<u64>()
                .map_err(|error| GenericFailure(format!("Failed to parse unsigned integer: {error:?}").into()).to_error(context.stack_trace))?;
            Ok(UnsignedInteger::from(value))
        }
    );
    build_method!(
        database,
        methods::ParseSignedInteger, "String::parse_signed_integer",(
            context: &ExecutionContext,
            this: IString
        ) -> SignedInteger {
            let value = this.0.parse::<i64>()
                .map_err(|error| GenericFailure(format!("Failed to parse signed integer: {error:?}").into()).to_error(context.stack_trace))?;
            Ok(SignedInteger::from(value))
        }
    );
    build_method!(
        database,
        methods::Contains, "String::contains",(
            _context: &ExecutionContext,
            this: IString,
            pattern: IString
        ) -> Boolean {
            let contained = this.0.contains(pattern.0.as_str());
            Ok(Boolean(contained))
        }
    );
}

#[cfg(test)]
mod test {
    use crate::execution::{test_run, values::Boolean};

    #[test]
    fn method_format() {
        let product =
            test_run("let one = 1; in \"Test {one} {two}\"::format(two = 2) == \"Test 1 2\"")
                .unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_append() {
        let product = test_run("\"1234\"::append(rhs = \"5678\") == \"12345678\"").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_slice() {
        let product = test_run("\"1234\"::slice(start = 1u, end = 3u) == \"23\"").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("\"1234\"::slice(end = 3u) == \"123\"").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("\"1234\"::slice(start = 1u) == \"234\"").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_chunks() {
        let product =
            test_run("\"1234567\"::chunks(size = 2u) == [\"12\", \"34\", \"56\"]").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product =
            test_run("\"1234567\"::chunks(size = 2u, strict = true) == [\"12\", \"34\", \"56\"]")
                .unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run(
            "\"1234567\"::chunks(size = 2u, strict = false) == [\"12\", \"34\", \"56\", \"7\"]",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_lines() {
        let product = test_run("\"12\\n34\\n56\"::lines() == [\"12\", \"34\", \"56\"]").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("\"12\\n34\\n56\\n\"::lines() == [\"12\", \"34\", \"56\"]").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product =
            test_run("\"12\\n34\\n\\n56\\n\"::lines() == [\"12\", \"34\", \"\", \"56\"]").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run(
            "\"12\\n34\\n56\\n\"::lines(include_empty = false) == [\"12\", \"34\", \"56\"]",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run(
            "\"12\\n34\\n\\n56\\n\"::lines(include_empty = false) == [\"12\", \"34\", \"56\"]",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run(
            "\"12\\n34\\n56\\n\\n\"::lines(include_empty = true) == [\"12\", \"34\", \"56\", \"\"]",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("\"12\\n34\\n\\n56\\n\\n\"::lines(include_empty = true) == [\"12\", \"34\", \"\", \"56\", \"\"]").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run(
            "\"12\\n34\\n\\n56\"::lines(include_empty = true) == [\"12\", \"34\", \"\", \"56\"]",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run(
            "\"12\\n34\\n\\n56\"::lines(include_empty = false) == [\"12\", \"34\", \"56\"]",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_map() {
        let product = test_run(
            "\"abcd\"::map(f= (c: std.types.String) -> std.types.String: if c == \"b\" then \"X\" else c) == \"aXcd\"",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_fold() {
        let product =
            test_run("\"aabbabaababb\"::fold(init = 0u, f = (previous: std.types.UInt, c: std.types.String) -> std.types.UInt: if c == \"a\" then previous + 1u else previous) == 6u").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_retain() {
        let product = test_run(
            "\"1234\"::retain(f = (c: std.types.String) -> std.types.Bool: c == \"1\" || c == \"3\") == \"13\"",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_reverse() {
        let product = test_run("\"1234\"::reverse() == \"4321\"").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_truncate() {
        let product = test_run("\"1234\"::truncate(length = 2u) == \"12\"").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_to_lowercase() {
        let product = test_run("\"ABcd\"::to_lowercase() == \"abcd\"").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_to_uppercase() {
        let product = test_run("\"ABcd\"::to_uppercase() == \"ABCD\"").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_parse_scalar() {
        let product = test_run("\"23\"::parse_scalar() == 23").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("\"23.45\"::parse_scalar() == 23.45").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("(\".45\"::parse_scalar() - 0.45)::abs() < 0.0001").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("\"-23\"::parse_scalar() == -23").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("\"-23.45\"::parse_scalar() == -23.45").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("(\"-.45\"::parse_scalar() - (-0.45))::abs() < 0.0001").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_parse_unsigned_integer() {
        let product = test_run("\"23\"::parse_unsigned_integer() == 23u").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_parse_signed_integer() {
        let product = test_run("\"23\"::parse_signed_integer() == 23i").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("\"-23\"::parse_signed_integer() == -23i").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn method_contains() {
        let product = test_run("\"abcd\"::contains(pattern = \"a\")").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("\"abcd\"::contains(pattern = \"f\")").unwrap();
        assert_eq!(product, Boolean(false).into());

        let product = test_run("\"abcd\"::contains(pattern = \"bcd\")").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("\"abcd\"::contains(pattern = \"bcde\")").unwrap();
        assert_eq!(product, Boolean(false).into());
    }

    #[test]
    fn format() {
        let product = test_run(
            "\"outer text: {value}\"::format(value = \"inner text\") == \"outer text: inner text\"",
        )
        .unwrap();
        assert_eq!(product, Boolean(true).into());
    }
}
