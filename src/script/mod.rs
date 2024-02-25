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

use anyhow::{bail, Result};

mod parsing;
use imstr::ImString;
use nom_locate::LocatedSpan;
use ouroboros::self_referencing;
pub use parsing::Span;

use crate::script::{
    execution::{
        types::{Solid, UserFunction},
        Module,
    },
    parsing::FunctionSignature,
};

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

    pub fn run_sketch(
        &mut self,
        name: &str,
        arguments: Vec<SerializableValue>,
    ) -> std::result::Result<(), Failure> {
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

            if matches!(
                *sketch.signature,
                FunctionSignature::Sketch { arguments: _ }
            ) {
                // TODO attaching a span to a user function would be useful for debug purposes.
                let result = sketch.call(runtime.context, runtime.code, argument_values, &[])?;

                result.downcast::<Sketch>(runtime.code)?;

                log::warn!(
                    "Sketches currently cannot be serialized, so no output will be provied."
                );

                Ok(())
            } else {
                Err(Failure::ExpectedGot(
                    runtime.code.clone(),
                    "sketch".into(),
                    sketch.signature.to_string().into(),
                ))
            }
        })
    }

    pub fn run_solid(
        &mut self,
        name: &str,
        arguments: Vec<SerializableValue>,
    ) -> std::result::Result<Solid, Failure> {
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

            if matches!(*solid.signature, FunctionSignature::Solid { arguments: _ }) {
                // TODO attaching a span to a user function would be useful for debug purposes.
                let result = solid.call(runtime.context, runtime.code, argument_values, &[])?;

                let solid = result.downcast::<Solid>(runtime.code)?;

                Ok(solid)
            } else {
                Err(Failure::ExpectedGot(
                    runtime.code.clone(),
                    "solid".into(),
                    solid.signature.to_string().into(),
                ))
            }
        })
    }

    pub fn run_task(
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

            let task = runtime.context.stack.get_variable_str(runtime.code, name)?;

            let task = task
                .downcast_ref::<UserFunction<RuntimeSpan>>(runtime.code)?
                .clone();

            if matches!(
                *task.signature,
                FunctionSignature::Task {
                    return_type: _,
                    arguments: _
                }
            ) {
                // TODO attaching a span to a user function would be useful for debug purposes.
                let result = task.call(runtime.context, runtime.code, argument_values, &[])?;

                let result = result.export(&mut runtime.context.log, runtime.code)?;

                Ok(result)
            } else {
                Err(Failure::ExpectedGot(
                    runtime.code.clone(),
                    "task".into(),
                    task.signature.to_string().into(),
                ))
            }
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

#[cfg(test)]
mod test {
    use std::borrow::Cow;

    use uom::si::{
        f64::Length,
        length::{meter, millimeter},
    };

    use crate::script::execution::types::Measurement;

    use super::*;

    #[test]
    fn run_sketch() {
        let mut runtime = Runtime::load((
            "root_module",
            "sketch my_sketch(input: Length = 50m) { new_sketch([]) }",
        ))
        .unwrap();

        assert!(matches!(
            runtime.run_sketch("my_sketch", vec![]),
            Err(Failure::MissingArguments(_))
        ));

        runtime
            .run_sketch("my_sketch", vec![SerializableValue::Default])
            .unwrap();

        runtime
            .run_sketch(
                "my_sketch",
                vec![SerializableValue::Measurement(
                    Measurement::try_from(Length::new::<meter>(10.0)).unwrap(),
                )],
            )
            .unwrap();

        let mut runtime =
            Runtime::load(("root_module", "function my_sketch() -> Number { 2 }")).unwrap();

        assert!(matches!(
            dbg!(runtime.run_sketch("my_sketch", vec![])),
            Err(Failure::ExpectedGot(
                _,
                Cow::Borrowed("sketch"),
                _, // Cow::Borrowed("function() -> Number")
            ))
        ));
    }

    #[test]
    fn run_solid() {
        let mut runtime = Runtime::load((
            "root_module",
            "solid my_solid(input: Length = 1cm) { new_sketch(Circle { center = [0m, 0m], radius = input }).sweep(global_xy_plane(), [0cm, 0cm, 1cm]) }",
        ))
        .unwrap();

        assert!(matches!(
            runtime.run_solid("my_solid", vec![]),
            Err(Failure::MissingArguments(_))
        ));

        runtime
            .run_solid("my_solid", vec![SerializableValue::Default])
            .unwrap();

        runtime
            .run_solid(
                "my_solid",
                vec![SerializableValue::Measurement(
                    Measurement::try_from(Length::new::<millimeter>(10.0)).unwrap(),
                )],
            )
            .unwrap();

        let mut runtime =
            Runtime::load(("root_module", "function my_solid() -> Number { 2 }")).unwrap();

        assert!(matches!(
            runtime.run_solid("my_solid", vec![]),
            Err(Failure::ExpectedGot(
                _,
                Cow::Borrowed("solid"),
                _, // Cow::Borrowed("function() -> Number")
            ))
        ));
    }

    #[test]
    fn run_task() {
        let mut runtime = Runtime::load((
            "root_module",
            "task my_task(input: Number = 50) -> Number { input }",
        ))
        .unwrap();

        assert!(matches!(
            dbg!(runtime.run_task("my_task", vec![])),
            Err(Failure::MissingArguments(_))
        ));

        assert_eq!(
            runtime.run_task("my_task", vec![SerializableValue::Default]),
            Ok(SerializableValue::Number(50.0))
        );

        assert_eq!(
            runtime.run_task("my_task", vec![SerializableValue::Number(22.0)]),
            Ok(SerializableValue::Number(22.0))
        );

        let mut runtime =
            Runtime::load(("root_module", "sketch my_sketch() { new_sketch([]) }")).unwrap();

        assert!(matches!(
            runtime.run_task("my_sketch", vec![]),
            Err(Failure::ExpectedGot(
                _,
                Cow::Borrowed("task"),
                _, // Cow::Borrowed("sketch()")
            ))
        ));
    }
}
