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
use std::{borrow::Cow, fmt::Display};

use crate::execution::formatting::IteratorFormatter;

use super::Value;

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct ClosureSignature {
    return_type: Box<VariableType>,
    arguments: Vec<MemberVariableType>,
}

impl Display for ClosureSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({})[...] -> {}",
            IteratorFormatter(self.arguments.iter()),
            self.return_type
        )
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum VariableType {
    String,
    List,
    Boolean,
    SignedInteger,
    UnsignedInteger,
    Range,
    Struct(String),
    Scalar(String),
    Vector(u8, String),
    Transform(u8),
    Quaternion,
    Cycle,
    Region,
    Sketch,
    Surface,
    Solid,
    Shell,
    Face,
    Curve,
    HalfEdge,
    Vertex,
    Closure(ClosureSignature),
}

impl VariableType {
    pub fn name(&self) -> Cow<'static, str> {
        match self {
            Self::String => "String".into(),
            Self::List => "List".into(),
            Self::Boolean => "Boolean".into(),
            Self::SignedInteger => "Signed Integer".into(),
            Self::UnsignedInteger => "Unsigned Integer".into(),
            Self::Range => "Range".into(),
            Self::Struct(name) => format!("struct {}", name.as_str()).into(),
            Self::Scalar(name) => name.to_string().into(),
            Self::Vector(dimension, name) => {
                format!("Vector{}<{}>", dimension, name.as_str()).into()
            }
            Self::Transform(2) => "Transform2D".into(),
            Self::Transform(3) => "Transform3D".into(),
            Self::Transform(_) => unreachable!(),
            Self::Quaternion => "Quaternion".into(),
            Self::Cycle => "Cycle".into(),
            Self::Region => "Region".into(),
            Self::Sketch => "Sketch".into(),
            Self::Surface => "Surface".into(),
            Self::Solid => "Solid".into(),
            Self::Shell => "Shell".into(),
            Self::Face => "Face".into(),
            Self::Curve => "Curve".into(),
            Self::HalfEdge => "HalfEdge".into(),
            Self::Vertex => "Vertex".into(),
            Self::Closure(closure) => format!("{}", closure).into(),
        }
    }
}

impl Display for VariableType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Struct(name) => write!(f, "struct {}", name.as_str()),
            Self::Scalar(name) => write!(f, "{}", name.as_str()),
            Self::Vector(dimension, name) => {
                write!(f, "Vector{}<{}>", dimension, name.as_str())
            }
            Self::Closure(closure) => write!(f, "{}", closure),
            _ => write!(f, "{}", self.name()),
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct MemberVariableType {
    pub ty: VariableType,
    pub default_value: Option<Value>,
}

impl Display for MemberVariableType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.default_value.is_some() {
            write!(f, "{} = default", self.ty)
        } else {
            write!(f, "{}", self.ty)
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct MemberVariable {
    pub name: String,
    pub ty: MemberVariableType,
}

impl Display for MemberVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.ty.default_value.is_some() {
            write!(f, "{}: {} = default", self.name.as_str(), self.ty.ty)
        } else {
            write!(f, "{}: {}", self.name.as_str(), self.ty.ty)
        }
    }
}
