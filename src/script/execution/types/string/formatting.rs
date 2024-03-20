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

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::char as nom_char,
    combinator::{all_consuming, cut, map, opt, success, value, verify},
    error::context,
    multi::{fold_many0, many0},
    sequence::{delimited, pair, preceded, terminated},
    IResult,
};

use crate::script::{
    execution::{
        types::{unsupported_operation_message, Object, OperatorResult, Value},
        Failure,
    },
    logging::RuntimeLog,
    Scalar, Span,
};

pub type VResult<I, O> = IResult<I, O, nom::error::VerboseError<I>>;

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
    fn parse<S: Span>(input: S) -> VResult<S, Self> {
        alt((
            value(Self::Debug, nom_char('?')),
            value(Self::Octal, nom_char('o')),
            value(Self::Hex, nom_char('x')),
            value(Self::CapitalizedHex, nom_char('X')),
            value(Self::Exponent, nom_char('e')),
            value(Self::CapitalizedExponent, nom_char('E')),
        ))(input)
    }
}

impl UnsupportedMessage for Style {
    fn unsupported_message<'a, S: Span, O: Object<'a, S>>(
        &self,
        object: &O,
        span: &S,
    ) -> OperatorResult<S, ()> {
        match self {
            Self::Default => {
                unsupported_operation_message::<S, (), O>(object, span, "format with default style")
            }
            Self::Debug => {
                unsupported_operation_message::<S, (), O>(object, span, "format with debug style")
            }
            Self::Octal => {
                unsupported_operation_message::<S, (), O>(object, span, "format with octal style")
            }
            Self::Hex => {
                unsupported_operation_message::<S, (), O>(object, span, "format with hex style")
            }
            Self::CapitalizedHex => unsupported_operation_message::<S, (), O>(
                object,
                span,
                "format with capitalized hex style",
            ),
            Self::Exponent => unsupported_operation_message::<S, (), O>(
                object,
                span,
                "format with exponent style",
            ),
            Self::CapitalizedExponent => unsupported_operation_message::<S, (), O>(
                object,
                span,
                "format with capitalized exponent style",
            ),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Precision {
    Default,
    Inline(u8),
    Referenced(u8),
}

impl Precision {
    fn parse<S: Span>(input: S) -> VResult<S, Self> {
        preceded(
            nom_char('.'),
            alt((
                map(terminated(number, nom_char('$')), Self::Referenced),
                map(number, Self::Inline),
            )),
        )(input)
    }
}

// We don't directly pass the precision information to objects when formatting, rather
// we dereference it (when applicable) before hand and pass them the final result.
impl UnsupportedMessage for Option<u8> {
    fn unsupported_message<'a, S: Span, O: Object<'a, S>>(
        &self,
        object: &O,
        span: &S,
    ) -> OperatorResult<S, ()> {
        match self {
            None => unsupported_operation_message::<S, (), O>(
                object,
                span,
                "format with default precision",
            ),
            Some(_) => {
                unsupported_operation_message::<S, (), O>(object, span, "format with precision")
            }
        }
    }
}

#[derive(Debug, PartialEq)]
struct Parameter {
    index: Option<u8>,
    style: Style,
    precision: Precision,
}

impl Parameter {
    fn parse<S: Span>(input: S) -> VResult<S, Self> {
        delimited(
            nom_char('{'),
            cut(map(
                pair(
                    opt(number),
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
                |(index, (style, precision))| Parameter {
                    index,
                    style,
                    precision,
                },
            )),
            cut(context("Expected closing `}` for parameter", nom_char('}'))),
        )(input)
    }
}

#[derive(Debug, PartialEq)]
enum Component {
    Litteral(String),
    Parameter(Parameter),
}

impl Component {
    fn parse<S: Span>(input: S) -> VResult<S, Self> {
        alt((Self::parse_litteral, map(Parameter::parse, Self::Parameter)))(input)
    }

    fn parse_litteral<S: Span>(input: S) -> VResult<S, Self> {
        map(
            verify(
                fold_many0(
                    alt((take_while1(|c| c != '{'), tag("{{"), tag("}}"))),
                    String::default,
                    |mut accum, item: S| {
                        let item = item.as_str();

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
        )(input)
    }
}

#[derive(Debug, PartialEq)]
pub struct Format {
    components: Vec<Component>,
}

impl Format {
    pub fn parse<S: Span>(input: S) -> VResult<S, Self> {
        map(
            all_consuming(many0(|input: S| Component::parse(input))),
            |components| Self { components },
        )(input)
    }

    pub fn format<S: Span>(
        &self,
        log: &mut dyn RuntimeLog<S>,
        span: &S,
        f: &mut dyn Write,
        arguments: &[Value<'_, S>],
    ) -> OperatorResult<S, ()> {
        let mut next_argument_index = 0;

        fn get_precision<S: Span>(
            span: &S,

            precision: &Precision,
            arguments: &[Value<'_, S>],
        ) -> OperatorResult<S, Option<u8>> {
            match precision {
                Precision::Default => Ok(None),
                Precision::Inline(precision) => Ok(Some(*precision)),
                Precision::Referenced(index) => {
                    if let Some(argument) = arguments.get(*index as usize) {
                        let precision = argument.downcast_ref::<Scalar>(span)?;
                        let precision = precision.to_index(span)?;

                        if precision.is_positive() {
                            Ok(Some(precision as u8))
                        } else {
                            Err(Failure::InvalidPrecision(span.clone(), precision))
                        }
                    } else {
                        Err(Failure::FormatArgumentIndexOutOfRange(
                            span.clone(),
                            *index as isize,
                        ))
                    }
                }
            }
        }

        for component in self.components.iter() {
            match component {
                Component::Litteral(text) => {
                    write!(f, "{}", text).unwrap_formatting_result(span)?
                }
                Component::Parameter(Parameter {
                    index: None,
                    style,
                    precision,
                }) => {
                    let precision = get_precision(span, precision, arguments)?;
                    let argument_index = next_argument_index;
                    next_argument_index += 1;

                    if let Some(argument) = arguments.get(argument_index) {
                        argument.format(log, span, f, *style, precision)?;
                    } else {
                        return Err(Failure::FormatArgumentIndexOutOfRange(
                            span.clone(),
                            argument_index as isize,
                        ));
                    }
                }
                Component::Parameter(Parameter {
                    index: Some(index),
                    style,
                    precision,
                }) => {
                    let precision = get_precision(span, precision, arguments)?;
                    let argument_index = *index as usize;

                    if let Some(argument) = arguments.get(argument_index) {
                        argument.format(log, span, f, *style, precision)?;
                    } else {
                        return Err(Failure::FormatArgumentIndexOutOfRange(
                            span.clone(),
                            argument_index as isize,
                        ));
                    }
                }
            }
        }

        Ok(())
    }
}

fn number<S: Span>(input: S) -> VResult<S, u8> {
    map(take_while1(|c| "0123456789".contains(c)), |digits: S| {
        digits.as_str().parse::<u8>().unwrap()
    })(input)
}

pub trait UnwrapFormattingResult<R> {
    fn unwrap_formatting_result<S: Span>(self, span: &S) -> OperatorResult<S, R>;
}

impl<R> UnwrapFormattingResult<R> for std::result::Result<R, std::fmt::Error> {
    fn unwrap_formatting_result<S: Span>(self, span: &S) -> OperatorResult<S, R> {
        match self {
            Ok(result) => Ok(result),
            Err(error) => Err(Failure::Formatting(span.clone(), error)),
        }
    }
}

pub trait UnsupportedMessage {
    fn unsupported_message<'a, S: Span, O: Object<'a, S>>(
        &self,
        object: &O,
        span: &S,
    ) -> OperatorResult<S, ()>;
}

#[cfg(test)]
mod test {
    use crate::script::logging::StandardLog;
    use common_data_types::Number;

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
            Parameter::parse("{}"),
            Ok((
                "",
                Parameter {
                    index: None,
                    style: Style::Default,
                    precision: Precision::Default
                }
            ))
        );
        assert_eq!(
            Parameter::parse("{1}"),
            Ok((
                "",
                Parameter {
                    index: Some(1),
                    style: Style::Default,
                    precision: Precision::Default
                }
            ))
        );
        assert_eq!(
            Parameter::parse("{:?}"),
            Ok((
                "",
                Parameter {
                    index: None,
                    style: Style::Debug,
                    precision: Precision::Default
                }
            ))
        );
        assert_eq!(
            Parameter::parse("{1:?}"),
            Ok((
                "",
                Parameter {
                    index: Some(1),
                    style: Style::Debug,
                    precision: Precision::Default
                }
            ))
        );
        assert_eq!(
            Parameter::parse("{:.5}"),
            Ok((
                "",
                Parameter {
                    index: None,
                    style: Style::Default,
                    precision: Precision::Inline(5)
                }
            ))
        );
        assert_eq!(
            Parameter::parse("{:.5$}"),
            Ok((
                "",
                Parameter {
                    index: None,
                    style: Style::Default,
                    precision: Precision::Referenced(5)
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
            Format::parse("{}"),
            Ok((
                "",
                Format {
                    components: vec![Component::Parameter(Parameter {
                        index: None,
                        style: Style::Default,
                        precision: Precision::Default
                    })]
                }
            ))
        );
        assert_eq!(
            Format::parse("  {}"),
            Ok((
                "",
                Format {
                    components: vec![
                        Component::Litteral("  ".into()),
                        Component::Parameter(Parameter {
                            index: None,
                            style: Style::Default,
                            precision: Precision::Default
                        })
                    ]
                }
            ))
        );
        assert_eq!(
            Format::parse("  {{}}{}"),
            Ok((
                "",
                Format {
                    components: vec![
                        Component::Litteral("  {}".into()),
                        Component::Parameter(Parameter {
                            index: None,
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
        let mut log = StandardLog;

        let mut formatted = String::default();
        Format::parse("Test {}")
            .unwrap()
            .1
            .format::<&str>(
                &mut log,
                &"scope",
                &mut formatted,
                &[Number::new(42.24).unwrap().into()],
            )
            .unwrap();
        assert_eq!(formatted, "Test 42.24");

        let mut formatted = String::default();
        Format::parse("Test {1} {0}")
            .unwrap()
            .1
            .format::<&str>(
                &mut log,
                &"scope",
                &mut formatted,
                &[
                    Number::new(1.0).unwrap().into(),
                    Number::new(2.0).unwrap().into(),
                ],
            )
            .unwrap();
        assert_eq!(formatted, "Test 2 1");
    }
}
