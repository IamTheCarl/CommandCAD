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

use std::error::Error;

use fj_core::{validation::ValidationError, Core};

use super::{
    execution::{
        types::{function::IntoBuiltinFunction, NoneType, OperatorResult, SString, Value},
        ExecutionContext, Failure, GlobalResources,
    },
    Span,
};

pub fn register_globals<S: Span>(context: &mut ExecutionContext<S>) {
    context.stack.new_variable_str(
        "print",
        (|context: &mut ExecutionContext<S>,
          span: &S,
          text: SString|
         -> OperatorResult<S, Value<S>> {
            let text = text.to_string(span)?;
            context
                .log
                .push(LogMessage::UserMessage(span.clone(), text));

            Ok(NoneType.into())
        })
        .into_builtin_function()
        .into(),
    );

    context.stack.new_variable_str(
        "warn",
        (|context: &mut ExecutionContext<S>,
          span: &S,
          text: SString|
         -> OperatorResult<S, Value<S>> {
            let text = text.to_string(span)?;
            context
                .log
                .push(LogMessage::UserWarning(span.clone(), text));

            Ok(NoneType.into())
        })
        .into_builtin_function()
        .into(),
    );

    context.stack.new_variable_str(
        "error",
        (|_context: &mut ExecutionContext<S>,
          span: &S,
          text: SString|
         -> OperatorResult<S, Value<S>> {
            let text = text.to_string(span)?;

            Err(Failure::User(span.clone(), text))
        })
        .into_builtin_function()
        .into(),
    );
}

pub trait RuntimeLog<S: Span> {
    fn push(&mut self, message: LogMessage<S>);
}

#[derive(Debug)]
pub enum LogMessage<S: Span> {
    UserMessage(S, String),
    UserWarning(S, String),
    FormatIntegerPrecision(S),
    ModelValidation(S, Vec<ValidationError>),
}

impl<S: Span> LogMessage<S> {
    pub fn log_level(&self) -> LogLevel {
        match self {
            Self::UserMessage(_, _) => LogLevel::Info,
            Self::UserWarning(_, _) => LogLevel::Warning,
            Self::FormatIntegerPrecision(_) => LogLevel::Warning,
            Self::ModelValidation(_, _) => LogLevel::Warning,
        }
    }
}

impl<S: Span> std::fmt::Display for LogMessage<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UserMessage(span, message) => write!(f, "{}: {}", span.format_span(), message),
            Self::UserWarning(span, message) => write!(f, "{}: {}", span.format_span(), message),
            Self::FormatIntegerPrecision(span) => {
                write!(
                    f,
                    "{}: Integer formats such as Octal and Hex ignore precision",
                    span.format_span()
                )
            }
            Self::ModelValidation(span, error_map) => {
                writeln!(f, "{}: Model Validation errors:", span.format_span())?;

                for error in error_map.iter() {
                    writeln!(f, "\t{}", error)?;

                    let mut source = error.source();
                    while let Some(error) = source {
                        writeln!(f, "\n\tCaused by:\n\t{}", error)?;
                        source = error.source();
                    }
                }

                Ok(())
            }
        }
    }
}

pub enum LogLevel {
    Info,
    Warning,
}

pub trait UnpackValidationWarnings {
    fn unpack_validation_warnings<S: Span>(&mut self, span: &S, log: &mut dyn RuntimeLog<S>);
}

impl UnpackValidationWarnings for Core {
    fn unpack_validation_warnings<S: Span>(&mut self, span: &S, log: &mut dyn RuntimeLog<S>) {
        let errors = self.layers.validation.take_errors();

        if let Err(errors) = errors {
            log.push(LogMessage::ModelValidation(span.clone(), errors.0));
        }
    }
}

impl UnpackValidationWarnings for GlobalResources {
    fn unpack_validation_warnings<S: Span>(&mut self, span: &S, log: &mut dyn RuntimeLog<S>) {
        self.fornjot_core.unpack_validation_warnings(span, log)
    }
}

pub struct StandardLog;

impl StandardLog {
    pub fn global() -> &'static mut Self {
        static mut STANDARD_LOG: StandardLog = StandardLog;

        // SAFETY: We contain no data and are never modified, so we can safely have multiple mutable references.
        unsafe { &mut STANDARD_LOG }
    }
}

impl<'a, S: Span> RuntimeLog<S> for StandardLog {
    fn push(&mut self, message: LogMessage<S>) {
        match message.log_level() {
            LogLevel::Info => log::info!("{}", message),
            LogLevel::Warning => log::warn!("{}", message),
        }
    }
}
