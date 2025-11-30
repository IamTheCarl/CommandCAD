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
use std::{borrow::Cow, collections::HashMap, fmt::Display, sync::Arc};

use common_data_types::Dimension;
use hashable_map::HashableMap;

use super::{
    closure::Signature as ClosureSignature, Boolean, DefaultValue, Object, SignedInteger,
    StaticTypeName, UnsignedInteger, Value, ValueNone,
};

use crate::{
    compile::{self, AstNode, SourceReference},
    execute_expression,
    execution::{
        errors::{ErrorType, ExpressionResult},
        logging::RuntimeLog,
        stack::Stack,
    },
};

#[derive(Debug, Eq, Clone, PartialEq)]
pub enum ValueType {
    TypeNone,
    Default,
    Boolean,
    SignedInteger,
    UnsignedInteger,
    Scalar(Dimension),
    Closure(Arc<ClosureSignature>),
    Dictionary(StructDefinition),
    ValueType,
    MultiType(Box<ValueType>, Box<ValueType>),
}

impl From<StructDefinition> for ValueType {
    fn from(value: StructDefinition) -> Self {
        Self::Dictionary(value)
    }
}

impl ValueType {
    pub fn name(&self) -> Cow<'static, str> {
        match self {
            Self::TypeNone => ValueNone::static_type_name().into(),
            Self::Default => DefaultValue::static_type_name().into(),
            Self::Boolean => Boolean::static_type_name().into(),
            Self::SignedInteger => SignedInteger::static_type_name().into(),
            Self::UnsignedInteger => UnsignedInteger::static_type_name().into(),
            Self::Scalar(dimension) => units::get_dimension_name(dimension).into(),
            _ => format!("{}", self).into(),
        }
    }

    // TODO we need to expose this method to the user.
    pub fn check_other_qualifies(
        &self,
        value_type: &ValueType,
    ) -> Result<(), TypeQualificationError> {
        match (self, value_type) {
            (Self::TypeNone, Self::TypeNone) => Ok(()),
            (Self::Boolean, Self::Boolean) => Ok(()),
            (Self::SignedInteger, Self::SignedInteger) => Ok(()),
            (Self::UnsignedInteger, Self::UnsignedInteger) => Ok(()),
            (Self::Scalar(our_dimension), Self::Scalar(their_dimension)) => {
                if our_dimension == their_dimension {
                    Ok(())
                } else {
                    Err(TypeQualificationError::This {
                        expected: self.clone(),
                        got: value_type.clone(),
                    })
                }
            }
            (Self::Closure(our_signature), Self::Closure(their_signature)) => {
                our_signature
                    .argument_type
                    .check_other_qualifies(&their_signature.argument_type)?;

                our_signature
                    .return_type
                    .check_other_qualifies(&their_signature.return_type)
            }
            (Self::Dictionary(our_definition), Self::Dictionary(their_definition)) => {
                our_definition.check_other_qualifies(their_definition)
            }
            (Self::ValueType, Self::ValueType) => Ok(()),
            (Self::MultiType(left, right), value_type) => {
                match (
                    left.check_other_qualifies(value_type),
                    right.check_other_qualifies(value_type),
                ) {
                    (Ok(_), _) | (_, Ok(_)) => Ok(()),
                    _ => Err(TypeQualificationError::This {
                        expected: self.clone(),
                        got: value_type.clone(),
                    }),
                }
            }
            (expected, got) => Err(TypeQualificationError::This {
                expected: expected.clone(),
                got: got.clone(),
            }),
        }
    }
}

impl Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // We can avoid a copy operation if we write directly into the formatter.
            Self::Closure(signature) => write!(f, "{}", signature),
            Self::Dictionary(definition) => write!(f, "{}", definition),
            Self::MultiType(left, right) => write!(f, "{left} | {right}"),
            _ => write!(f, "{}", self.name()),
        }
    }
}

impl Object for ValueType {
    fn get_type(&self) -> ValueType {
        ValueType::ValueType
    }

    fn bit_or(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: Value,
    ) -> ExpressionResult<Value> {
        let rhs: Self = rhs.downcast(stack_trace)?;

        Ok(Self::MultiType(Box::new(self), Box::new(rhs)).into())
    }
}

impl StaticTypeName for ValueType {
    fn static_type_name() -> &'static str {
        "ValueType"
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct StructMember {
    pub ty: ValueType,
    pub default: Option<Value>,
}

impl StructMember {
    fn new(
        log: &mut dyn RuntimeLog,
        stack_trace: &mut Vec<SourceReference>,
        stack: &mut Stack,
        source: &AstNode<compile::StructMember>,
    ) -> ExpressionResult<Self> {
        let ty = execute_expression(log, stack_trace, stack, &source.node.ty)?
            .downcast::<ValueType>(stack_trace)?;
        let default = if let Some(default) = source.node.default.as_ref() {
            Some(execute_expression(log, stack_trace, stack, default)?)
        } else {
            None
        };

        Ok(Self { ty, default })
    }
}

impl Display for StructMember {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(_default) = self.default.as_ref() {
            write!(f, "{} (optional)", self.ty)
        } else {
            write!(f, "{}", self.ty)
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StructDefinition {
    pub members: Arc<HashableMap<String, StructMember>>,
    pub variadic: bool,
}

impl StructDefinition {
    pub fn new(
        log: &mut dyn RuntimeLog,
        stack_trace: &mut Vec<SourceReference>,
        stack: &mut Stack,
        source: &AstNode<compile::StructDefinition>,
    ) -> ExpressionResult<Self> {
        let mut members = HashMap::new();
        for member in source.node.members.iter() {
            let name = member.node.name.node.clone();
            members.insert(name, StructMember::new(log, stack_trace, stack, member)?);
        }

        let members = Arc::new(HashableMap::from(members));
        let variadic = source.node.variadic;
        Ok(Self { members, variadic })
    }

    pub fn check_other_qualifies(
        &self,
        other: &StructDefinition,
    ) -> Result<(), TypeQualificationError> {
        let mut errors = Vec::new();

        // Check that all fields are present and correct.
        for (name, member) in self.members.iter() {
            if let Some(other_member) = other.members.get(name) {
                if let Err(error) = member.ty.check_other_qualifies(&other_member.ty) {
                    errors.push(MissmatchedField {
                        name: name.clone(),
                        error,
                    });
                }
            } else {
                errors.push(MissmatchedField {
                    name: name.clone(),
                    error: TypeQualificationError::This {
                        expected: member.ty.clone(),
                        got: ValueType::TypeNone,
                    },
                });
            }
        }

        // Checkt that there are no extra fields (unless of course, we're supposed to have extra
        // fields)
        if !self.variadic {
            for (name, member) in other.members.iter() {
                if self.members.get(name).is_none() {
                    errors.push(MissmatchedField {
                        name: name.clone(),
                        error: TypeQualificationError::This {
                            expected: ValueType::TypeNone,
                            got: member.ty.clone(),
                        },
                    });
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(TypeQualificationError::Fields {
                failed_feilds: errors,
            })
        }
    }
}

impl Display for StructDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;

        if self.members.is_empty() {
            if self.variadic {
                write!(f, "...")?;
            }
        } else {
            let mut member_iter = self.members.iter();

            // The first member should not have a comma before it.
            if let Some((name, member)) = member_iter.next() {
                write!(f, "{name}: {member}")?;
            }

            // All other members get a comma before them.
            for (name, member) in member_iter {
                write!(f, ", {name}: {member}")?;
            }

            // This struct is variadic.
            if self.variadic {
                // We're not empty, so something is guaranteed to have been printed before the
                // dots. Make sure to put a comma before the dots.
                write!(f, ", ...")?;
            }
        }

        write!(f, ")")
    }
}

impl StaticTypeName for StructDefinition {
    fn static_type_name() -> &'static str {
        "Struct Definition"
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct MissmatchedField {
    pub name: String,
    pub error: TypeQualificationError,
}

#[derive(Debug, Eq, PartialEq)]
pub enum TypeQualificationError {
    This {
        expected: ValueType,
        got: ValueType,
    },
    Fields {
        failed_feilds: Vec<MissmatchedField>,
    },
}

impl TypeQualificationError {
    fn write_failure(&self, tab_depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let tabs = "  ".repeat(tab_depth);

        match self {
            TypeQualificationError::This { expected, got } => {
                write!(f, "{tabs}Expected {expected}, got {got}")
            }
            TypeQualificationError::Fields { failed_feilds } => {
                writeln!(f, "{tabs}Dictionary did not meet requirements:")?;

                for field in failed_feilds {
                    write!(f, "{tabs}  {}: ", field.name)?;
                    field.error.write_failure(tab_depth + 2, f)?;
                }

                Ok(())
            }
        }
    }
}

impl ErrorType for TypeQualificationError {}

impl Display for TypeQualificationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.write_failure(0, f)
    }
}

#[cfg(test)]
mod test {
    use crate::execution::test_run;

    use super::*;

    #[test]
    fn type_none() {
        ValueType::TypeNone
            .check_other_qualifies(&ValueType::TypeNone)
            .unwrap();

        ValueType::TypeNone
            .check_other_qualifies(&ValueType::UnsignedInteger)
            .unwrap_err();
    }

    #[test]
    fn type_boolean() {
        ValueType::Boolean
            .check_other_qualifies(&ValueType::Boolean)
            .unwrap();

        ValueType::Boolean
            .check_other_qualifies(&ValueType::TypeNone)
            .unwrap_err();
    }

    #[test]
    fn type_signed_integer() {
        ValueType::SignedInteger
            .check_other_qualifies(&ValueType::SignedInteger)
            .unwrap();

        ValueType::SignedInteger
            .check_other_qualifies(&ValueType::TypeNone)
            .unwrap_err();
    }

    #[test]
    fn type_unsigned_integer() {
        ValueType::UnsignedInteger
            .check_other_qualifies(&ValueType::UnsignedInteger)
            .unwrap();

        ValueType::UnsignedInteger
            .check_other_qualifies(&ValueType::TypeNone)
            .unwrap_err();
    }

    #[test]
    fn type_scalar() {
        ValueType::Scalar(Dimension::length())
            .check_other_qualifies(&ValueType::Scalar(Dimension::length()))
            .unwrap();

        ValueType::Scalar(Dimension::length())
            .check_other_qualifies(&ValueType::Scalar(Dimension::area()))
            .unwrap_err();

        ValueType::Scalar(Dimension::length())
            .check_other_qualifies(&ValueType::TypeNone)
            .unwrap_err();
    }

    #[test]
    fn type_closure() {
        let closure = test_run("() -> std.types.None std.consts.None").unwrap();
        let closure = closure.as_userclosure().unwrap();

        closure
            .get_type()
            .check_other_qualifies(&closure.get_type())
            .unwrap();
    }

    #[test]
    fn type_empty_dictionary() {
        let structure = test_run("()").unwrap();
        let structure = structure.as_dictionary().unwrap();

        let dictionary = test_run("()").unwrap();
        let dictionary = dictionary.as_dictionary().unwrap();

        structure
            .get_type()
            .check_other_qualifies(&dictionary.get_type())
            .unwrap();
    }

    #[test]
    fn type_dictionary_with_value() {
        let structure = test_run("(a: std.types.None)").unwrap();
        let structure = structure.as_valuetype().unwrap();

        let dictionary = test_run("(a = std.consts.None)").unwrap();
        let dictionary = dictionary.as_dictionary().unwrap();

        structure
            .check_other_qualifies(&dictionary.get_type())
            .unwrap();
    }

    #[test]
    fn type_dictionary_with_extra_value() {
        let structure = test_run("(a: std.types.None)").unwrap();
        let structure = structure.as_valuetype().unwrap();

        let dictionary = test_run("(a = std.consts.None, b = std.consts.None)").unwrap();
        let dictionary = dictionary.as_dictionary().unwrap();

        structure
            .check_other_qualifies(&dictionary.get_type())
            .unwrap_err();
    }

    #[test]
    fn type_dictionary_varadic() {
        let structure = test_run("(a: std.types.None, ...)").unwrap();
        let structure = structure.as_valuetype().unwrap();

        let dictionary = test_run("(a = std.consts.None, b = std.consts.None)").unwrap();
        let dictionary = dictionary.as_dictionary().unwrap();

        structure
            .check_other_qualifies(&dictionary.get_type())
            .unwrap();
    }

    #[test]
    fn type_dictionary_nested() {
        let structure = test_run("(a: (b: std.types.None))").unwrap();
        let structure = structure.as_valuetype().unwrap();

        let dictionary = test_run("(a = (b = std.consts.None))").unwrap();
        let dictionary = dictionary.as_dictionary().unwrap();

        structure
            .check_other_qualifies(&dictionary.get_type())
            .unwrap();
    }

    #[test]
    fn type_value_type() {
        ValueType::ValueType
            .check_other_qualifies(&ValueType::ValueType)
            .unwrap();

        ValueType::UnsignedInteger
            .check_other_qualifies(&ValueType::TypeNone)
            .unwrap_err();
    }

    #[test]
    fn combined_type() {
        let value_type = test_run("std.types.None | std.types.UInt").unwrap();
        let value_type = value_type.as_valuetype().unwrap();

        value_type
            .check_other_qualifies(&ValueType::TypeNone)
            .unwrap();

        value_type
            .check_other_qualifies(&ValueType::UnsignedInteger)
            .unwrap();

        value_type
            .check_other_qualifies(&ValueType::SignedInteger)
            .unwrap_err();
    }

    #[test]
    fn triple_combined_type() {
        let value_type = test_run("std.types.None | std.types.UInt | std.types.SInt").unwrap();
        let value_type = value_type.as_valuetype().unwrap();

        value_type
            .check_other_qualifies(&ValueType::TypeNone)
            .unwrap();

        value_type
            .check_other_qualifies(&ValueType::UnsignedInteger)
            .unwrap();

        value_type
            .check_other_qualifies(&ValueType::SignedInteger)
            .unwrap();

        value_type
            .check_other_qualifies(&ValueType::Boolean)
            .unwrap_err();
    }
}
