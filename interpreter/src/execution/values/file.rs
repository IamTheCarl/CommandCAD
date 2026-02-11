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

use std::{borrow::Cow, path::PathBuf, sync::Arc};

use imstr::ImString;

use crate::{
    build_method,
    execution::{
        errors::GenericFailure,
        logging::{LogLevel, LogMessage},
        values::{
            string::formatting::Style, BuiltinCallableDatabase, IString, Object, StaticTypeName,
            ValueType,
        },
        ExecutionContext,
    },
    values::StaticType,
};

#[derive(Debug, Clone)]
pub struct File {
    pub path: Arc<PathBuf>,
}

impl Eq for File {}

impl PartialEq for File {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path
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

        write!(f, "{:?}", self.path)
    }
}

impl StaticTypeName for File {
    fn static_type_name() -> Cow<'static, str> {
        "File".into()
    }
}

impl StaticType for File {
    fn static_type() -> ValueType {
        ValueType::File
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
            let content = std::fs::read_to_string(this.path.as_path())
                .map_err(|error| GenericFailure(format!("Failed to read file to string: {error:?}").into()).to_error(context.stack_trace))?;

            Ok(IString(ImString::from(content)))
        }
    );
}
