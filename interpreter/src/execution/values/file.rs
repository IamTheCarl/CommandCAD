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

use std::{borrow::Cow, io::Read, sync::Arc};

use imstr::ImString;
use tempfile::NamedTempFile;

use crate::{
    build_method,
    execution::{
        errors::{GenericFailure, Raise as _},
        logging::{LogLevel, LogMessage},
        values::{
            string::formatting::Style, BuiltinCallableDatabase, IString, Object, StaticTypeName,
            ValueType,
        },
        ExecutionContext, ExpressionResult,
    },
};

#[derive(Debug, Clone)]
pub struct File {
    content: Arc<NamedTempFile>,
}

impl Eq for File {}

impl PartialEq for File {
    fn eq(&self, other: &Self) -> bool {
        self.content.path() == other.content.path()
    }
}

impl Object for File {
    fn get_type(&self, _context: &ExecutionContext) -> ValueType {
        ValueType::File
    }

    fn format(
        &self,
        context: &ExecutionContext,
        f: &mut dyn std::fmt::Write,
        style: Style,
        precision: Option<u8>,
    ) -> std::fmt::Result {
        if !matches!(style, Style::Debug) {
            context.log.push_message(LogMessage {
                origin: context.stack_trace.bottom().clone(),
                level: LogLevel::Warning,
                message: "Files only support debug formatting".into(),
            });
        }

        if precision.is_some() {
            context.log.push_message(LogMessage {
                origin: context.stack_trace.bottom().clone(),
                level: LogLevel::Warning,
                message: "Files cannot be formatted with precision".into(),
            });
        }

        write!(f, "{:?}", self.content.path())
    }
}

impl StaticTypeName for File {
    fn static_type_name() -> Cow<'static, str> {
        "File".into()
    }
}

mod methods {
    pub struct ToString;
}

pub fn register_methods(database: &mut BuiltinCallableDatabase) {
    build_method!(
        database,
        methods::ToString, "File::to_string", (
            context: &ExecutionContext,
            this: File
        ) -> IString {
            let mut file = this.content.reopen().map_err(|error| GenericFailure(format!("Failed to re-open file: {error:?}").into()).to_error(context.stack_trace))?;

            let mut content = String::new();
            file.read_to_string(&mut content).map_err(|error| GenericFailure(format!("File was not UTF8 encoded: {error:?}").into()).to_error(context.stack_trace))?;

            Ok(IString(ImString::from(content)))
        }
    );
}
