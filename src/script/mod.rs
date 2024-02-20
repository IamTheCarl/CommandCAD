use anyhow::{bail, Result};

mod parsing;
use imstr::ImString;
use nom_locate::LocatedSpan;
use ouroboros::self_referencing;
pub use parsing::Span;

mod module;
use module::Module;

use crate::script::execution::types::{Solid, UserFunction};

pub use crate::script::execution::types::SerializableValue;
use execution::{types::Object, ExecutionContext, ModuleScope};

use self::execution::types::Sketch;
mod execution;

type RuntimeSpan = LocatedSpan<ImString>;
pub type Failure = self::execution::Failure<RuntimeSpan>;

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
        let mut log = Vec::new();

        let file_name = module.0.into();
        let code = LocatedSpan::new(module.1.into());
        let module = Module::load(&mut log, file_name, code.clone())?;

        if !log.is_empty() {
            // TODO there's got to be a better way to present validation errors.
            bail!("Module failed validation: {:?}", log);
        }

        Ok(RuntimeBuilder {
            code,
            module,
            context_builder: |module| {
                let module_scope = ModuleScope::new(module);

                ExecutionContext::new(module_scope)
            },
        }
        .build())
    }

    // TODO this needs unit testing, specifically: Feed it proper values, feed it default values
    pub fn run_sketch(
        &mut self,
        name: &str,
        arguments: Vec<SerializableValue>,
    ) -> std::result::Result<(), Failure> {
        // FIXME this will run functions that return sketches without complaint.

        self.with_mut(|runtime| {
            let mut argument_values = Vec::with_capacity(arguments.len());
            for argument in arguments {
                let value =
                    argument.into_value_without_type_check(runtime.context, runtime.code)?;
                argument_values.push(value);
            }

            let sketch = runtime.context.stack.get_variable_str(runtime.code, name)?;

            let sketch = sketch
                .downcast_ref::<UserFunction<RuntimeSpan>>(runtime.code)?
                .clone();

            // TODO attaching a span to a user function would be useful for debug purposes.
            let result = sketch.call(runtime.context, runtime.code, argument_values, &[])?;

            result.downcast::<Sketch>(runtime.code)?;

            log::warn!("Sketches currently cannot be serialized, so no output will be provied.");

            Ok(())
        })
    }

    // TODO this needs unit testing, specifically: Feed it proper values, feed it default values
    pub fn run_solid(
        &mut self,
        name: &str,
        arguments: Vec<SerializableValue>,
    ) -> std::result::Result<Solid, Failure> {
        // FIXME this will run functions that return solids without complaint.

        self.with_mut(|runtime| {
            let mut argument_values = Vec::with_capacity(arguments.len());
            for argument in arguments {
                let value =
                    argument.into_value_without_type_check(runtime.context, runtime.code)?;
                argument_values.push(value);
            }

            let solid = runtime.context.stack.get_variable_str(runtime.code, name)?;

            let solid = solid
                .downcast_ref::<UserFunction<RuntimeSpan>>(runtime.code)?
                .clone();

            // TODO attaching a span to a user function would be useful for debug purposes.
            let result = solid.call(runtime.context, runtime.code, argument_values, &[])?;

            let solid = result.downcast::<Solid>(runtime.code)?;

            Ok(solid)
        })
    }

    // TODO this needs unit testing, specifically: Feed it proper values, feed it default values
    pub fn run_function(
        &mut self,
        name: &str,
        arguments: Vec<SerializableValue>,
    ) -> std::result::Result<SerializableValue, Failure> {
        self.with_mut(|runtime| {
            let mut argument_values = Vec::with_capacity(arguments.len());
            for argument in arguments {
                let value =
                    argument.into_value_without_type_check(runtime.context, runtime.code)?;
                argument_values.push(value);
            }

            let function = runtime.context.stack.get_variable_str(runtime.code, name)?;

            let function = function
                .downcast_ref::<UserFunction<RuntimeSpan>>(runtime.code)?
                .clone();

            // TODO attaching a span to a user function would be useful for debug purposes.
            let result = function.call(runtime.context, runtime.code, argument_values, &[])?;

            let result = result.export(&mut runtime.context.log, runtime.code)?;

            Ok(result)
        })
    }

    pub fn log(&self) -> &RuntimeLog<RuntimeSpan> {
        self.with_context(|context| &context.log)
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
}

#[derive(Debug, Eq, PartialEq)]
pub enum LogMessage<S> {
    FormatIntegerPrecision(S),
}

impl<S> LogMessage<S> {
    pub fn log_level(&self) -> LogLevel {
        match self {
            Self::FormatIntegerPrecision(_) => LogLevel::Warning,
        }
    }
}

impl<S: Span + FormatSpan> std::fmt::Display for LogMessage<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FormatIntegerPrecision(span) => {
                write!(
                    f,
                    "{}: Integer formats such as Octal and Hex ignore precision",
                    span.format_span()
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
}
