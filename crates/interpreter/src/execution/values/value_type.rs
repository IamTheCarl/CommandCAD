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
use std::{borrow::Cow, fmt::Display, sync::Arc};

use common_data_types::Dimension;

use super::{
    closure::Signature as ClosureSignature, Boolean, DefaultValue, Object, SignedInteger,
    StaticTypeName, UnsignedInteger, Value, Void,
};

use crate::{
    compile::{self, AstNode, SourceReference},
    execute_expression,
    execution::{errors::ExpressionResult, logging::RuntimeLog, stack::Stack, Heap},
};

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ValueType {
    Void,
    Default,
    Boolean,
    SignedInteger,
    UnsignedInteger,
    Scalar(Dimension),
    Closure(Arc<ClosureSignature>),
    Dictionary(StructDefinition),
    ValueType,
}

impl From<StructDefinition> for ValueType {
    fn from(value: StructDefinition) -> Self {
        Self::Dictionary(value)
    }
}

impl ValueType {
    pub fn name(&self) -> Cow<'static, str> {
        match self {
            Self::Void => Void::static_type_name().into(),
            Self::Default => DefaultValue::static_type_name().into(),
            Self::Boolean => Boolean::static_type_name().into(),
            Self::SignedInteger => SignedInteger::static_type_name().into(),
            Self::UnsignedInteger => UnsignedInteger::static_type_name().into(),
            Self::Scalar(dimension) => units::get_dimension_name(dimension).into(),
            Self::Closure(signature) => format!("{}", signature).into(),
            Self::Dictionary(definition) => format!("{}", definition).into(),
            Self::ValueType => "ValueType".into(),
        }
    }

    // TODO we need a method to validate types, and then provide that to the user as a method.
}

impl Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // We can avoid a copy operation if we write directly into the formatter.
            Self::Closure(signature) => write!(f, "{}", signature),
            Self::Dictionary(definition) => write!(f, "{}", definition),
            _ => write!(f, "{}", self.name()),
        }
    }
}

impl Object for ValueType {
    fn get_type(&self) -> ValueType {
        ValueType::ValueType
    }
}

impl StaticTypeName for ValueType {
    fn static_type_name() -> &'static str {
        "ValueType"
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct StructMember {
    pub name: String,
    pub ty: ValueType,
    pub default: Option<Value>,
}

impl StructMember {
    fn new(
        log: &mut dyn RuntimeLog,
        stack_trace: &mut Vec<SourceReference>,
        stack: &mut Stack,
        heap: &mut Heap,
        source: &AstNode<compile::StructMember>,
    ) -> ExpressionResult<Self> {
        let name = source.node.name.node.clone();
        let ty = execute_expression(log, stack_trace, stack, heap, &source.node.ty)?
            .downcast::<ValueType>(stack_trace)?;
        let default = if let Some(default) = source.node.default.as_ref() {
            Some(execute_expression(log, stack_trace, stack, heap, default)?)
        } else {
            None
        };

        Ok(Self { name, ty, default })
    }
}

impl Display for StructMember {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(_default) = self.default.as_ref() {
            write!(f, "{}: {} (optional)", self.name, self.ty)
        } else {
            write!(f, "{}: {}", self.name, self.ty)
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StructDefinition {
    pub members: Arc<Vec<StructMember>>,
    pub variadic: bool,
}

impl StructDefinition {
    pub fn new(
        log: &mut dyn RuntimeLog,
        stack_trace: &mut Vec<SourceReference>,
        stack: &mut Stack,
        heap: &mut Heap,
        source: &AstNode<compile::StructDefinition>,
    ) -> ExpressionResult<Self> {
        let mut members = Vec::new();
        for member in source.node.members.iter() {
            members.push(StructMember::new(log, stack_trace, stack, heap, member)?);
        }

        let members = Arc::new(members);
        let variadic = source.node.variadic;
        Ok(Self { members, variadic })
    }
}

impl Display for StructDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;

        let mut member_iter = self.members.iter();

        // The first member should not have a comma before it.
        if let Some(first_member) = member_iter.next() {
            write!(f, "{}", first_member)?;
        }

        // All other members get a comma before them.
        for member in member_iter {
            write!(f, ", {}", member)?;
        }

        // This struct is variadic.
        if self.variadic {
            // Only put a comma before the dots if there were any members before it.
            if self.members.is_empty() {
                write!(f, "...")?;
            } else {
                write!(f, ", ...")?;
            }
        }

        write!(f, ")")
    }
}
