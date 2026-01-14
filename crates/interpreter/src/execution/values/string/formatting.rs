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

use std::fmt::Write;

use imstr::ImString;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::char as nom_char,
    combinator::{all_consuming, cut, map, recognize, success, value, verify},
    error::context,
    multi::{fold_many0, many0},
    sequence::{delimited, pair, preceded},
    IResult, Parser,
};

use crate::execution::{
    errors::{ExpressionResult, GenericFailure, Raise},
    logging::{LocatedStr, StackTrace},
    values::{Dictionary, Object, UnsignedInteger},
    ExecutionContext,
};

pub type VResult<I, O> = IResult<I, O, nom::error::Error<I>>;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Style {
    Default,
    Debug,
    Octal,
    Hex,
    CapitalizedHex,
    Exponent,
    CapitalizedExponent,
}

impl Style {
    fn parse(input: &str) -> VResult<&str, Self> {
        alt((
            value(Self::Debug, nom_char('?')),
            value(Self::Octal, nom_char('o')),
            value(Self::Hex, nom_char('x')),
            value(Self::CapitalizedHex, nom_char('X')),
            value(Self::Exponent, nom_char('e')),
            value(Self::CapitalizedExponent, nom_char('E')),
        ))
        .parse(input)
    }
}

fn ident(input: &str) -> VResult<&str, &str> {
    recognize(pair(
        take_while1(|c: char| c.is_alphabetic()),
        take_while(|c: char| c.is_numeric()),
    ))
    .parse(input)
}

#[derive(Debug, PartialEq, Clone)]
enum Precision {
    Default,
    Inline(u8),
    Referenced(ImString),
}

impl Precision {
    fn parse(input: &str) -> VResult<&str, Self> {
        preceded(
            nom_char('.'),
            alt((
                map(ident, |name| Self::Referenced(ImString::from(name))),
                map(number, Self::Inline),
            )),
        )
        .parse(input)
    }
}

#[derive(Debug, PartialEq)]
struct Parameter {
    name: ImString,
    style: Style,
    precision: Precision,
}

impl Parameter {
    fn parse(input: &str) -> VResult<&str, Self> {
        delimited(
            nom_char('{'),
            cut(map(
                pair(
                    map(ident, |name| ImString::from(name)),
                    alt((
                        preceded(
                            nom_char(':'),
                            pair(
                                alt((Style::parse, success(Style::Default))),
                                alt((Precision::parse, success(Precision::Default))),
                            ),
                        ),
                        success((Style::Default, Precision::Default)),
                    )),
                ),
                |(name, (style, precision))| Parameter {
                    name,
                    style,
                    precision,
                },
            )),
            cut(context("Expected closing `}` for parameter", nom_char('}'))),
        )
        .parse(input)
    }
}

#[derive(Debug, PartialEq)]
enum Component {
    Litteral(String),
    Parameter(Parameter),
}

impl Component {
    fn parse(input: &str) -> VResult<&str, Self> {
        alt((Self::parse_litteral, map(Parameter::parse, Self::Parameter))).parse(input)
    }

    fn parse_litteral(input: &str) -> VResult<&str, Self> {
        map(
            verify(
                fold_many0(
                    alt((take_while1(|c| c != '{'), tag("{{"), tag("}}"))),
                    String::default,
                    |mut accum, item: &str| {
                        match item {
                            "{{" => accum.push('{'),
                            "}}" => accum.push('}'),
                            _ => {
                                accum.push_str(item);
                            }
                        }

                        accum
                    },
                ),
                |output: &str| !output.is_empty(),
            ),
            Self::Litteral,
        )
        .parse(input)
    }
}

#[derive(Debug, PartialEq)]
pub struct Format {
    components: Vec<Component>,
}

impl Format {
    pub fn parse(input: &str) -> VResult<&str, Self> {
        map(all_consuming(many0(Component::parse)), |components| Self {
            components,
        })
        .parse(input)
    }

    pub fn format(
        &self,
        context: &ExecutionContext,
        f: &mut dyn Write,
        arguments: Dictionary,
    ) -> ExpressionResult<()> {
        fn get_precision(
            context: &ExecutionContext,
            precision: &Precision,
            arguments: &Dictionary,
        ) -> ExpressionResult<Option<u8>> {
            match precision {
                Precision::Default => Ok(Option::None),
                Precision::Inline(precision) => Ok(Some(*precision)),
                Precision::Referenced(name) => {
                    if let Some(argument) = arguments.get(name.as_str()).or_else(|| {
                        context
                            .get_variable(LocatedStr {
                                location: context.stack_trace.bottom().clone(),
                                string: name.as_str(),
                            })
                            .ok()
                    }) {
                        let precision = argument
                            .downcast_ref::<UnsignedInteger>(context.stack_trace)?
                            .0;

                        if precision <= u8::MAX as u64 {
                            Ok(Some(precision as u8))
                        } else {
                            Err(GenericFailure(
                                format!(
                                    "Precision of {precision} is not in the valid range of 0 to {}",
                                    u8::MAX
                                )
                                .into(),
                            )
                            .to_error(context.stack_trace))
                        }
                    } else {
                        Err(
                            GenericFailure(format!("Could not find argument `{name}`").into())
                                .to_error(context.stack_trace),
                        )
                    }
                }
            }
        }

        for component in self.components.iter() {
            match component {
                Component::Litteral(text) => {
                    write!(f, "{}", text).unwrap_formatting_result(context.stack_trace)?
                }
                Component::Parameter(Parameter {
                    name,
                    style,
                    precision,
                }) => {
                    let precision = get_precision(context, &precision, &arguments)?;

                    if let Some(argument) = arguments.get(name.as_str()).or_else(|| {
                        context
                            .get_variable(LocatedStr {
                                location: context.stack_trace.bottom().clone(),
                                string: name.as_str(),
                            })
                            .ok()
                    }) {
                        argument
                            .format(context, f, *style, precision)
                            .map_err(|error| {
                                GenericFailure(format!("Error while formatting: {error:?}").into())
                                    .to_error(context.stack_trace)
                            })?;
                    } else {
                        return Err(GenericFailure(
                            format!("Could not find argument `{name}`").into(),
                        )
                        .to_error(context.stack_trace));
                    }
                }
            }
        }

        Ok(())
    }
}

fn number(input: &str) -> VResult<&str, u8> {
    map(take_while1(|c| "0123456789".contains(c)), |digits: &str| {
        digits.parse::<u8>().unwrap()
    })
    .parse(input)
}

pub trait UnwrapFormattingResult<R> {
    fn unwrap_formatting_result(self, stack_trace: &StackTrace) -> ExpressionResult<R>;
}

impl<R> UnwrapFormattingResult<R> for std::result::Result<R, std::fmt::Error> {
    fn unwrap_formatting_result(self, stack_trace: &StackTrace) -> ExpressionResult<R> {
        match self {
            Ok(result) => Ok(result),
            Err(error) => {
                Err(GenericFailure(format!("Failed to format: {error}",).into())
                    .to_error(stack_trace))
            }
        }
    }
}

#[cfg(test)]
mod test {
    use std::{collections::HashMap, sync::Mutex};

    use common_data_types::{Dimension, Float};

    use crate::execution::{
        build_prelude,
        stack::StackScope,
        values::{BuiltinCallableDatabase, Scalar},
    };

    use super::*;

    #[test]
    fn litterals() {
        assert_eq!(
            Component::parse_litteral("1234"),
            Ok(("", Component::Litteral("1234".into())))
        );

        assert_eq!(
            Component::parse_litteral("1234{{}"),
            Ok(("", Component::Litteral("1234{}".into())))
        );
    }

    #[test]
    fn parameters() {
        assert_eq!(
            Parameter::parse("{thing}"),
            Ok((
                "",
                Parameter {
                    name: "thing".into(),
                    style: Style::Default,
                    precision: Precision::Default
                }
            ))
        );
        assert_eq!(
            Parameter::parse("{thing:?}"),
            Ok((
                "",
                Parameter {
                    name: "thing".into(),
                    style: Style::Debug,
                    precision: Precision::Default
                }
            ))
        );
        assert_eq!(
            Parameter::parse("{thing:.5}"),
            Ok((
                "",
                Parameter {
                    name: "thing".into(),
                    style: Style::Default,
                    precision: Precision::Inline(5)
                }
            ))
        );
        assert_eq!(
            Parameter::parse("{thing:.precision}"),
            Ok((
                "",
                Parameter {
                    name: "thing".into(),
                    style: Style::Default,
                    precision: Precision::Referenced("precision".into())
                }
            ))
        );
    }

    #[test]
    fn litterals_and_parameters() {
        assert_eq!(Format::parse(""), Ok(("", Format { components: vec![] })));

        assert_eq!(
            Format::parse("  {{}}"),
            Ok((
                "",
                Format {
                    components: vec![Component::Litteral("  {}".into())]
                }
            ))
        );

        assert_eq!(
            Format::parse("{thang}"),
            Ok((
                "",
                Format {
                    components: vec![Component::Parameter(Parameter {
                        name: "thang".into(),
                        style: Style::Default,
                        precision: Precision::Default
                    })]
                }
            ))
        );
        assert_eq!(
            Format::parse("  {thang}"),
            Ok((
                "",
                Format {
                    components: vec![
                        Component::Litteral("  ".into()),
                        Component::Parameter(Parameter {
                            name: "thang".into(),
                            style: Style::Default,
                            precision: Precision::Default
                        })
                    ]
                }
            ))
        );
        assert_eq!(
            Format::parse("  {{}}{thang}"),
            Ok((
                "",
                Format {
                    components: vec![
                        Component::Litteral("  {}".into()),
                        Component::Parameter(Parameter {
                            name: "thang".into(),
                            style: Style::Default,
                            precision: Precision::Default
                        })
                    ]
                }
            ))
        );
    }

    #[test]
    fn do_format() {
        let database = BuiltinCallableDatabase::new();
        let prelude = build_prelude(&database);

        let context = ExecutionContext {
            log: &Mutex::new(Vec::new()),
            stack_trace: &StackTrace::test(),
            stack: &StackScope::top(&prelude),
            database: &database,
        };

        let mut formatted = String::default();
        Format::parse("Test {value}")
            .unwrap()
            .1
            .format(
                &context,
                &mut formatted,
                Dictionary::new(
                    &context,
                    HashMap::from_iter([(
                        "value".into(),
                        Scalar {
                            dimension: Dimension::zero(),
                            value: Float::new(42.24).unwrap(),
                        }
                        .into(),
                    )]),
                ),
            )
            .unwrap();
        assert_eq!(formatted, "Test 42.24");

        let mut formatted = String::default();
        Format::parse("Test {one} {two}")
            .unwrap()
            .1
            .format(
                &context,
                &mut formatted,
                Dictionary::new(
                    &context,
                    HashMap::from_iter([
                        (
                            "one".into(),
                            Scalar {
                                dimension: Dimension::zero(),
                                value: Float::new(1.0).unwrap(),
                            }
                            .into(),
                        ),
                        (
                            "two".into(),
                            Scalar {
                                dimension: Dimension::zero(),
                                value: Float::new(2.0).unwrap(),
                            }
                            .into(),
                        ),
                    ]),
                ),
            )
            .unwrap();
        assert_eq!(formatted, "Test 1 2");
    }
}
