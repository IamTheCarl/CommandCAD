use std::{borrow::Cow, num::ParseFloatError};

use anyhow::{bail, Result};

mod parsing;
use imstr::ImString;
use nom_locate::LocatedSpan;
use ordered_float::ParseNotNanError;
use ouroboros::self_referencing;
pub use parsing::Span;

mod module;
use module::Module;

use crate::script::execution::types::UserFunction;

pub use crate::script::execution::types::SerializableValue;
use execution::{types::Object, ExecutionContext, ModuleScope, Stack};

mod execution;

type RuntimeSpan = LocatedSpan<ImString>;

#[self_referencing]
pub struct Runtime {
    code: RuntimeSpan,
    module: Module<RuntimeSpan>,
    #[borrows(module)]
    #[not_covariant]
    context: ExecutionContext<'this, RuntimeSpan>,
}

impl Runtime {
    pub fn load(module: (impl Into<String>, impl Into<ImString>)) -> Result<Self> {
        let mut log = RuntimeLog::new();

        let file_name = module.0.into();
        let code = LocatedSpan::new(module.1.into());
        let module = Module::load(&mut log, file_name, code.clone())?;

        if log.containes_any_error() {
            bail!("Module failed validation");
        }

        Ok(RuntimeBuilder {
            code,
            module,
            context_builder: |module| {
                let module_scope = ModuleScope::new(module);
                let stack = Stack::new(module_scope);

                ExecutionContext { log, stack }
            },
        }
        .build())
    }

    pub fn run_sketch(&mut self, _name: &str) -> Result<SerializableValue> {
        bail!("Sketches are not yet supported.");
    }

    pub fn run_widget(&mut self, _name: &str) -> Result<SerializableValue> {
        bail!("Widgets are not yet supported.");
    }

    pub fn run_function(
        &mut self,
        name: &str,
        arguments: Vec<SerializableValue>,
    ) -> std::result::Result<SerializableValue, ()> {
        self.with_mut(|runtime| {
            let mut argument_values = Vec::with_capacity(arguments.len());
            for argument in arguments {
                let value =
                    argument.into_value_without_type_check(runtime.context, runtime.code)?;
                argument_values.push(value);
            }

            let function = runtime
                .context
                .stack
                .get_variable_str(&mut runtime.context.log, runtime.code, name)
                .map_err(|_| ())?;

            let function = function
                .downcast_ref::<UserFunction<RuntimeSpan>>(&mut runtime.context.log, runtime.code)
                .map_err(|_| ())?
                .clone();

            // TODO attaching a span to a user function would be useful for debug purposes.
            let result = function
                .call(runtime.context, runtime.code, argument_values, &[])
                .map_err(|_| ())?;

            let result = result
                .export(&mut runtime.context.log, runtime.code)
                .map_err(|_| ())?;

            Ok(result)
        })
    }

    pub fn log(&self) -> &RuntimeLog<RuntimeSpan> {
        &self.with_context(|context| &context.log)
    }
}

#[derive(Debug, Eq, PartialEq, Default)]
pub struct RuntimeLog<S> {
    pub messages: Vec<LogMessage<S>>,
}

impl<S: Span> RuntimeLog<S> {
    pub fn new() -> Self {
        Self {
            messages: Vec::new(),
        }
    }

    fn push(&mut self, message: LogMessage<S>) {
        self.messages.push(message);
    }

    fn containes_any_error(&self) -> bool {
        for message in self.messages.iter() {
            if matches!(message.log_level(), LogLevel::Error) {
                return true;
            }
        }

        false
    }

    fn contains_error(&self, predicate: impl Fn(&LogMessage<S>) -> bool) -> bool {
        self.messages.iter().any(predicate)
    }
}

// TODO We need stack traces.
#[derive(Debug, Eq, PartialEq)]
pub enum LogMessage<S> {
    UnclosedStatement(S),
    ReservedKeyword(S, &'static str),
    DuplicateGlobal(S, S),
    ExpectedGot(S, Cow<'static, str>, Cow<'static, str>),
    NumberConversion(S, ParseNotNanError<ParseFloatError>),
    VariableNotInScope(S, Cow<'static, str>),
    StructNotInScope(S, S),
    StructMissingAssignment(S, S),
    StructExcessAssignment(S),
    StructWrongInheritanceType(S, S, S),
    NoDefault(S, S),
    UnsupportedOperation(S, Cow<'static, str>, &'static str),
    UnknownAttribute(S),
    ResultIsNan(S),
    MissingArguments(S),
    TooManyArguments(S),
    IndexOutOfRange(S, isize),
    ListIsEmpty(S),
    UnknownUnitType(S, Cow<'static, str>),
    CannotConvertFromTo(S, Cow<'static, str>, Cow<'static, str>),
    InvalidCharIndex(S, isize),
    Formatting(S, std::fmt::Error),
    FormatIntegerPrecision(S),
    FormatArgumentIndexOutOfRange(S, isize),
    ParseFormatter(S),
    InvalidPrecision(S, isize),
    MissingUpperBound(S),
    ListLengthsDontMatch(S),
    DidNotMatch(S),
    BreakOutsideOfLoop(S),
    BreakLabelNotFound(S, S),
    ContinueOutsideOfLoop(S),
    ContinueLabelNotFound(S, S),
}

impl<S> LogMessage<S> {
    pub fn log_level(&self) -> LogLevel {
        match self {
            Self::UnclosedStatement(_) => LogLevel::Error,
            Self::ReservedKeyword(_, _) => LogLevel::Error,
            Self::DuplicateGlobal(_, _) => LogLevel::Error,
            Self::ExpectedGot(_, _, _) => LogLevel::Error,
            Self::NumberConversion(_, _) => LogLevel::Error,
            Self::VariableNotInScope(_, _) => LogLevel::Error,
            Self::StructNotInScope(_, _) => LogLevel::Error,
            Self::StructMissingAssignment(_, _) => LogLevel::Error,
            Self::StructExcessAssignment(_) => LogLevel::Error,
            Self::StructWrongInheritanceType(_, _, _) => LogLevel::Error,
            Self::NoDefault(_, _) => LogLevel::Error,
            Self::UnsupportedOperation(_, _, _) => LogLevel::Error,
            Self::UnknownAttribute(_) => LogLevel::Error,
            Self::ResultIsNan(_) => LogLevel::Error,
            Self::MissingArguments(_) => LogLevel::Error,
            Self::TooManyArguments(_) => LogLevel::Warning,
            Self::IndexOutOfRange(_, _) => LogLevel::Error,
            Self::ListIsEmpty(_) => LogLevel::Error,
            Self::UnknownUnitType(_, _) => LogLevel::Error,
            Self::CannotConvertFromTo(_, _, _) => LogLevel::Error,
            Self::InvalidCharIndex(_, _) => LogLevel::Error,
            Self::Formatting(_, _) => LogLevel::Error,
            Self::FormatIntegerPrecision(_) => LogLevel::Warning,
            Self::FormatArgumentIndexOutOfRange(_, _) => LogLevel::Error,
            Self::ParseFormatter(_) => LogLevel::Error,
            Self::InvalidPrecision(_, _) => LogLevel::Error,
            Self::MissingUpperBound(_) => LogLevel::Error,
            Self::ListLengthsDontMatch(_) => LogLevel::Error,
            Self::DidNotMatch(_) => LogLevel::Error,
            Self::BreakOutsideOfLoop(_) => LogLevel::Error,
            Self::BreakLabelNotFound(_, _) => LogLevel::Error,
            Self::ContinueOutsideOfLoop(_) => LogLevel::Error,
            Self::ContinueLabelNotFound(_, _) => LogLevel::Error,
        }
    }
}

impl<S: Span + FormatSpan> std::fmt::Display for LogMessage<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnclosedStatement(span) => {
                write!(
                    f,
                    "{}: Non-tail expressions must be closed with a semicolon",
                    span.format_span()
                )
            }
            Self::ReservedKeyword(span, keyword) => {
                write!(
                    f,
                    "{}: Keyword `{}` is reserved",
                    span.format_span(),
                    keyword
                )
            }
            Self::DuplicateGlobal(first_instance, second_instance) => {
                write!(
                    f,
                    "Duplicate global {}:{}",
                    first_instance.format_span(),
                    second_instance.format_span()
                )
            }
            Self::ExpectedGot(span, expected, got) => {
                write!(
                    f,
                    "{}: Expected `{}`, got `{}`",
                    span.format_span(),
                    expected,
                    got
                )
            }
            Self::NumberConversion(span, error) => {
                write!(
                    f,
                    "{}: Failed to convert to number: {:?}",
                    span.format_span(),
                    error
                )
            }
            Self::VariableNotInScope(span, name) => {
                write!(
                    f,
                    "{}: Variable `{}` was not found in current scope",
                    span.format_span(),
                    name
                )
            }
            Self::StructNotInScope(span, name) => {
                write!(
                    f,
                    "{}: A struct by the name of `{}` could not be found",
                    span.format_span(),
                    name.as_str()
                )
            }
            Self::StructMissingAssignment(span, name) => {
                write!(
                    f,
                    "{}: Missing assignment for struct: {}",
                    span.format_span(),
                    name.as_str()
                )
            }
            Self::StructExcessAssignment(assignment) => {
                write!(
                    f,
                    "{}: Excess value assigned to struct",
                    assignment.format_span()
                )
            }
            Self::StructWrongInheritanceType(span, expected_type, given_type) => {
                write!(
                    f,
                    "{}: Wrong type given for in inheritance. Expected `{}`, got `{}`",
                    span.format_span(),
                    expected_type.as_str(),
                    given_type.as_str()
                )
            }
            Self::NoDefault(span, name) => {
                write!(
                    f,
                    "{}: Default value not available for memeber `{}`",
                    span.format_span(),
                    name.as_str()
                )
            }
            Self::UnsupportedOperation(span, name, operation) => {
                write!(
                    f,
                    "{}: Type `{}` does not support {} operation",
                    span.format_span(),
                    name,
                    operation
                )
            }
            Self::UnknownAttribute(attribute) => write!(
                f,
                "{}: Unknown attribute: {}",
                attribute.format_span(),
                attribute.as_str()
            ),
            Self::ResultIsNan(span) => write!(
                f,
                "{}: Result of arithmetic operation is NaN",
                span.format_span()
            ),
            Self::MissingArguments(span) => write!(
                f,
                "{}: Not enough arguments for function",
                span.format_span()
            ),
            Self::TooManyArguments(span) => write!(
                f,
                "{}: Excess arguemnt to function, will be ignored",
                span.format_span()
            ),
            Self::IndexOutOfRange(span, index) => write!(
                f,
                "{}: Index `{}` is out of range",
                span.format_span(),
                index
            ),
            Self::ListIsEmpty(span) => write!(f, "{}: List is empty", span.format_span()),
            Self::UnknownUnitType(span, unit_type) => write!(
                f,
                "{}: Unknown unit type `{}`",
                span.format_span(),
                unit_type
            ),
            Self::CannotConvertFromTo(span, from, to) => {
                write!(
                    f,
                    "{}: Cannot convert from `{}` to `{}`",
                    span.format_span(),
                    from,
                    to
                )
            }
            Self::InvalidCharIndex(span, index) => {
                write!(
                    f,
                    "{}: Index `{}` does not lie on a character boundary",
                    span.format_span(),
                    index
                )
            }
            Self::Formatting(span, error) => {
                write!(
                    f,
                    "{}: Error formatting text: {:?}",
                    span.format_span(),
                    error
                )
            }
            Self::FormatIntegerPrecision(span) => {
                write!(
                    f,
                    "{}: Integer formats such as Octal and Hex ignore precision",
                    span.format_span()
                )
            }
            Self::FormatArgumentIndexOutOfRange(span, index) => {
                write!(
                    f,
                    "{}: Format requests argument index `{}` which is not present",
                    span.format_span(),
                    *index,
                )
            }
            Self::ParseFormatter(span) => {
                write!(
                    f,
                    "{}: Failed to parse string as format",
                    span.format_span()
                )
            }
            Self::InvalidPrecision(span, precision) => {
                write!(
                    f,
                    "{}: Invalid precision `{}`. Precision must be greater than zero",
                    span.format_span(),
                    *precision
                )
            }
            Self::MissingUpperBound(span) => {
                write!(
                    f,
                    "{}: Upper bound of range be specified when inclusive",
                    span.format_span()
                )
            }
            Self::ListLengthsDontMatch(span) => {
                write!(
		    f,
		    "{}: Attempt to assign list from list of a different length. You can use range access `[..]` to trim an oversized list down to the expected size",
		    span.format_span()
		)
            }
            Self::DidNotMatch(span) => {
                write!(
                    f,
                    "{}: No branches in match statement matched input",
                    span.format_span()
                )
            }
            Self::BreakOutsideOfLoop(span) => {
                write!(f, "{}: Break outside of loop", span.format_span())
            }
            Self::BreakLabelNotFound(span, label) => {
                write!(
                    f,
                    "{}: Could not find loop with label `{}`",
                    span.format_span(),
                    label.as_str()
                )
            }
            Self::ContinueOutsideOfLoop(span) => {
                write!(f, "{}: Continue outside of loop", span.format_span())
            }
            Self::ContinueLabelNotFound(span, label) => {
                write!(
                    f,
                    "{}: Could not find loop with label `{}`",
                    span.format_span(),
                    label.as_str()
                )
            }
        }
    }
}

pub trait FormatSpan {
    fn format_span(&self) -> String;
}

impl<'a> FormatSpan for &'a str {
    fn format_span(&self) -> String {
        format!("`{}`", self)
    }
}
impl<'a> FormatSpan for LocatedSpan<&'a str> {
    fn format_span(&self) -> String {
        format!("[{}:{}]", self.location_line(), self.get_column())
    }
}
impl FormatSpan for imstr::ImString {
    fn format_span(&self) -> String {
        format!("`{}`", self)
    }
}
impl FormatSpan for LocatedSpan<imstr::ImString> {
    fn format_span(&self) -> String {
        format!("[{}:{}]", self.location_line(), self.get_column())
    }
}

pub enum LogLevel {
    Warning,
    Error,
}
