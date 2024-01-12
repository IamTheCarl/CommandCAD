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
        types::{unsupported_operation_message, Number, Object, Value},
        ControlFlow, ExecutionResult,
    },
    LogMessage, RuntimeLog, Span,
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
        log: &mut RuntimeLog<S>,
        span: &S,
    ) -> ExecutionResult<'a, S, ()> {
        match self {
            Self::Default => unsupported_operation_message::<S, (), O>(
                object,
                log,
                span,
                "format with default style",
            ),
            Self::Debug => unsupported_operation_message::<S, (), O>(
                object,
                log,
                span,
                "format with debug style",
            ),
            Self::Octal => unsupported_operation_message::<S, (), O>(
                object,
                log,
                span,
                "format with octal style",
            ),
            Self::Hex => unsupported_operation_message::<S, (), O>(
                object,
                log,
                span,
                "format with hex style",
            ),
            Self::CapitalizedHex => unsupported_operation_message::<S, (), O>(
                object,
                log,
                span,
                "format with capitalized hex style",
            ),
            Self::Exponent => unsupported_operation_message::<S, (), O>(
                object,
                log,
                span,
                "format with exponent style",
            ),
            Self::CapitalizedExponent => unsupported_operation_message::<S, (), O>(
                object,
                log,
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
        log: &mut RuntimeLog<S>,
        span: &S,
    ) -> ExecutionResult<'a, S, ()> {
        match self {
            None => unsupported_operation_message::<S, (), O>(
                object,
                log,
                span,
                "format with default precision",
            ),
            Some(_) => unsupported_operation_message::<S, (), O>(
                object,
                log,
                span,
                "format with precision",
            ),
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

    pub fn format<'a, S: Span>(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
        f: &mut dyn Write,
        arguments: &[Value<'a, S>],
    ) -> ExecutionResult<'a, S, ()> {
        let mut next_argument_index = 0;

        fn get_precision<'a, S: Span>(
            log: &mut RuntimeLog<S>,
            span: &S,

            precision: &Precision,
            arguments: &[Value<'a, S>],
        ) -> ExecutionResult<'a, S, Option<u8>> {
            match precision {
                Precision::Default => Ok(None),
                Precision::Inline(precision) => Ok(Some(*precision)),
                Precision::Referenced(index) => {
                    if let Some(argument) = arguments.get(*index as usize) {
                        let precision = argument
                            .downcast_ref::<Number>(log, span)?
                            .into_inner()
                            .trunc();

                        if precision > 0.0 {
                            Ok(Some(precision as u8))
                        } else {
                            log.push(LogMessage::InvalidPrecision(
                                span.clone(),
                                precision as isize,
                            ));
                            Err(ControlFlow::Failure)
                        }
                    } else {
                        log.push(LogMessage::FormatArgumentIndexOutOfRange(
                            span.clone(),
                            *index as isize,
                        ));
                        Err(ControlFlow::Failure)
                    }
                }
            }
        }

        for component in self.components.iter() {
            match component {
                Component::Litteral(text) => {
                    write!(f, "{}", text).unwrap_formatting_result(log, span)?
                }
                Component::Parameter(Parameter {
                    index: None,
                    style,
                    precision,
                }) => {
                    let precision = get_precision(log, span, precision, arguments)?;
                    let argument_index = next_argument_index;
                    next_argument_index += 1;

                    if let Some(argument) = arguments.get(argument_index) {
                        argument.format(log, span, f, *style, precision)?;
                    } else {
                        log.push(LogMessage::FormatArgumentIndexOutOfRange(
                            span.clone(),
                            argument_index as isize,
                        ));
                        return Err(ControlFlow::Failure);
                    }
                }
                Component::Parameter(Parameter {
                    index: Some(index),
                    style,
                    precision,
                }) => {
                    let precision = get_precision(log, span, precision, arguments)?;
                    let argument_index = *index as usize;

                    if let Some(argument) = arguments.get(argument_index) {
                        argument.format(log, span, f, *style, precision)?;
                    } else {
                        log.push(LogMessage::FormatArgumentIndexOutOfRange(
                            span.clone(),
                            argument_index as isize,
                        ));
                        return Err(ControlFlow::Failure);
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
    fn unwrap_formatting_result<'a, S: Span>(
        self,
        log: &mut RuntimeLog<S>,
        span: &S,
    ) -> ExecutionResult<'a, S, R>;
}

impl<R> UnwrapFormattingResult<R> for Result<R, std::fmt::Error> {
    fn unwrap_formatting_result<'a, S: Span>(
        self,
        log: &mut RuntimeLog<S>,
        span: &S,
    ) -> ExecutionResult<'a, S, R> {
        match self {
            Ok(result) => Ok(result),
            Err(error) => {
                log.push(LogMessage::Formatting(span.clone(), error));
                Err(ControlFlow::Failure)
            }
        }
    }
}

pub trait UnsupportedMessage {
    fn unsupported_message<'a, S: Span, O: Object<'a, S>>(
        &self,
        object: &O,
        log: &mut RuntimeLog<S>,
        span: &S,
    ) -> ExecutionResult<'a, S, ()>;
}

#[cfg(test)]
mod test {
    use crate::script::execution::types::Number;

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
        let mut log = RuntimeLog::default();

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
