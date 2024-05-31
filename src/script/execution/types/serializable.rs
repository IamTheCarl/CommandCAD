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

use std::{borrow::Cow, collections::HashMap};

use serde::{Deserialize, Serialize};

use crate::script::{
    execution::{types::StructDefinition, validate_assignment_type, ExecutionContext, Failure},
    logging::RuntimeLog,
    parsing::VariableType,
    Span,
};

use super::{DefaultValue, List, Object, OperatorResult, SString, Scalar, Structure, Value};

#[derive(Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum SerializableValue {
    #[serde(skip_serializing)]
    Default,
    #[serde(untagged)]
    Boolean(bool),
    #[serde(untagged)]
    Struct {
        #[serde(rename = "type")]
        ty: String,
        members: HashMap<String, SerializableValue>,
    },
    #[serde(untagged)]
    List(Vec<SerializableValue>),
    #[serde(untagged)]
    Scalar(Scalar),
    #[serde(untagged)]
    String(String),
}

impl SerializableValue {
    pub fn into_value_without_type_check<S: Span>(
        self,
        context: &mut ExecutionContext<S>,
        span: &S,
    ) -> OperatorResult<S, Value<S>> {
        match self {
            Self::Default => Ok(DefaultValue.into()),
            Self::Boolean(value) => Ok((value).into()),
            Self::Struct {
                ty,
                members: mut values,
            } => {
                let initalizer = context
                    .stack
                    .get_variable_str(span, ty.as_ref())?
                    .downcast_ref::<StructDefinition<S>>(span)?
                    .definition
                    .clone();

                let mut failures = Vec::new();
                let mut table = HashMap::with_capacity(initalizer.members.len());

                for member in initalizer.members.iter() {
                    if let Some(value) = values.remove(member.name.as_str()) {
                        match value.into_value(context, span, &member.ty.ty) {
                            Ok(value) => {
                                match validate_assignment_type(context, member, span, value, span) {
                                    Ok(value) => {
                                        table.insert(member.name.to_string(), value);
                                    }
                                    Err(failure) => failures.push(failure),
                                }
                            }
                            Err(failure) => {
                                failures.push(failure);
                            }
                        }
                    } else {
                        failures.push(Failure::StructMissingAssignment(
                            span.clone(),
                            member.name.clone(),
                        ));
                    }
                }

                if failures.is_empty() {
                    Ok(Structure::new(initalizer, table).into())
                } else {
                    Err(Failure::StructConstruction(span.clone(), failures))
                }
            }
            Self::List(values) => {
                let mut collected_values: Vec<Value<S>> = Vec::with_capacity(values.len());

                for value in values {
                    let value = value.into_value_without_type_check(context, span)?;
                    collected_values.push(value);
                }

                Ok(List::from(collected_values).into())
            }
            Self::String(value) => Ok(SString::from(value).into()),
            Self::Scalar(measurement) => Ok(measurement.into()),
        }
    }

    pub fn into_value<S: Span>(
        self,
        context: &mut ExecutionContext<S>,
        span: &S,
        ty: &VariableType<S>,
    ) -> OperatorResult<S, Value<S>> {
        if self.matches_type(ty, context.log, span)? {
            self.into_value_without_type_check(context, span)
        } else {
            Err(Failure::CannotConvertFromTo(
                span.clone(),
                self.type_name(),
                ty.name(),
            ))
        }
    }

    fn matches_type<S: Span>(
        &self,
        ty: &VariableType<S>,
        log: &mut dyn RuntimeLog<S>,
        variable_name_span: &S,
    ) -> OperatorResult<S, bool> {
        Ok(match (self, ty) {
            (Self::Boolean(_), VariableType::Boolean) => true,
            (Self::Struct { ty: s_ty, .. }, VariableType::Struct(v_ty)) => v_ty.as_str() == s_ty,
            (Self::List(_), VariableType::List) => true,
            (Self::String(_), VariableType::String) => true,
            (Self::Scalar(measurement), ty) => {
                measurement.matches_type(ty, log, variable_name_span)?
            }
            (Self::Default, _) => true,
            _ => false,
        })
    }

    fn type_name(&self) -> Cow<'static, str> {
        match self {
            SerializableValue::Default => "Default".into(),
            SerializableValue::Boolean(_) => "Boolean".into(),
            SerializableValue::Struct { ty, members: _ } => format!("struct {}", ty).into(),
            SerializableValue::List(_) => "List".into(),
            SerializableValue::String(_) => "String".into(),
            SerializableValue::Scalar(measurement) => {
                <Scalar as Object<&'static str>>::type_name(measurement)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use common_data_types::Float;
    use uom::si::{f64::Length, length::millimeter};

    use crate::script::{
        execution::{
            expressions::run_expression,
            types::{Object, SString, Scalar},
            Module,
        },
        parsing::Expression,
        Runtime,
    };

    use super::*;

    #[test]
    fn deserialize_boolean() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                serde_yaml::from_str::<SerializableValue>("true")
                    .unwrap()
                    .into_value(context, &"", &VariableType::Boolean),
                Ok(true.into())
            );
            assert_eq!(
                serde_yaml::from_str::<SerializableValue>("false")
                    .unwrap()
                    .into_value(context, &"", &VariableType::Boolean),
                Ok(false.into())
            );
        });
    }

    #[test]
    fn serialize_boolean() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            let boolean: Value<&'static str> = true.into();
            assert_eq!(
                boolean.export(context.log, &""),
                Ok(SerializableValue::Boolean(true))
            );

            let boolean: Value<&'static str> = false.into();
            assert_eq!(
                boolean.export(context.log, &""),
                Ok(SerializableValue::Boolean(false))
            );
        });
    }

    #[test]
    fn deserialize_number() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                serde_yaml::from_str::<SerializableValue>("42")
                    .unwrap()
                    .into_value(context, &"", &VariableType::Scalar("Number")),
                Ok(Float::new(42.0).unwrap().into())
            );
        });
    }

    #[test]
    fn serialize_number() {
        let value = SerializableValue::Scalar(Scalar::from(Float::new(42.0).unwrap()));

        assert_eq!(serde_yaml::to_string(&value).unwrap(), "42.0\n");
    }

    #[test]
    fn deserialize_struct() {
        let mut log = Vec::new();

        let module = Module::load(
            &mut log,
            "test_module.ccm",
            r#"struct MyStruct { value: Number = 1 }"#,
        )
        .unwrap();

        assert!(log.is_empty());

        ExecutionContext::new(&mut Runtime::from(module), |context| {
            let struct_def = r#"
                   type: MyStruct
                   members:
                     value: 42"#;

            let structure = serde_yaml::from_str::<SerializableValue>(struct_def)
                .unwrap()
                .into_value(context, &"", &VariableType::Struct("MyStruct"));

            assert_eq!(
                structure.unwrap(),
                run_expression(
                    context,
                    &Expression::parse("MyStruct { value = 42 }").unwrap().1
                )
                .unwrap()
            );

            let struct_def = r#"
                   type: MyStruct
                   members:
                     value: default"#;

            let structure = serde_yaml::from_str::<SerializableValue>(struct_def)
                .unwrap()
                .into_value(context, &"", &VariableType::Struct("MyStruct"));

            assert_eq!(
                structure.unwrap(),
                run_expression(
                    context,
                    &Expression::parse("MyStruct { ..default }").unwrap().1
                )
                .unwrap()
            );

            assert!(serde_yaml::from_str::<SerializableValue>(struct_def)
                .unwrap()
                .into_value(context, &"", &VariableType::Struct("OtherStruct"))
                .is_err());

            let struct_def = r#"
                   type: MyStruct
                   members:
                     wrong_value: 42"#;
            assert!(serde_yaml::from_str::<SerializableValue>(struct_def)
                .unwrap()
                .into_value(context, &"", &VariableType::Struct("MyStruct"))
                .is_err());

            let struct_def = r#"
                   type: MyStruct
                   members:
                     value: true"#;
            assert!(serde_yaml::from_str::<SerializableValue>(struct_def)
                .unwrap()
                .into_value(context, &"", &VariableType::Struct("MyStruct"))
                .is_err());
        });
    }

    #[test]
    fn serialize_struct() {
        let mut log = Vec::new();

        let module = Module::load(
            &mut log,
            "test_module.ccm",
            r#"struct MyStruct { value: Number }"#,
        )
        .unwrap();

        assert!(log.is_empty());

        ExecutionContext::new(&mut Runtime::from(module), |context| {
            let value: Value<&'static str> = run_expression(
                context,
                &Expression::parse("MyStruct { value = 42 }").unwrap().1,
            )
            .unwrap();

            assert_eq!(
                value.export(context.log, &""),
                Ok(SerializableValue::Struct {
                    ty: "MyStruct".to_string(),
                    members: HashMap::from([(
                        "value".to_string(),
                        SerializableValue::Scalar(Scalar::from(Float::new(42.0).unwrap()))
                    )])
                })
            );
        });
    }

    #[test]
    fn deserialize_list() {
        let list_def = r#"[1, true, "some text"]"#;

        ExecutionContext::new(&mut Runtime::default(), |context| {
            let list = serde_yaml::from_str::<SerializableValue>(list_def).unwrap();

            let list = list.into_value(context, &"", &VariableType::List);

            assert_eq!(
                list.unwrap(),
                run_expression(
                    context,
                    &Expression::parse("[1, true, \"some text\"]").unwrap().1
                )
                .unwrap()
            );
        });
    }

    #[test]
    fn serialize_list() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            let value: Value<&'static str> = List::from([
                Float::new(1.0).unwrap().into(),
                true.into(),
                SString::from("some text".to_string()).into(),
            ])
            .into();
            assert_eq!(
                value.export(context.log, &""),
                Ok(SerializableValue::List(vec![
                    SerializableValue::Scalar(Scalar::from(Float::new(1.0).unwrap())),
                    SerializableValue::Boolean(true),
                    SerializableValue::String("some text".to_string())
                ]))
            );
        });
    }

    #[test]
    fn deserialize_string() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                serde_yaml::from_str::<SerializableValue>("text")
                    .unwrap()
                    .into_value(context, &"", &VariableType::String),
                Ok(SString::from("text").into())
            );
        });
    }

    #[test]
    fn serialize_string() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            let value: Value<&'static str> = SString::from("This is a test".to_string()).into();
            assert_eq!(
                value.export(context.log, &""),
                Ok(SerializableValue::String("This is a test".to_string()))
            );
        });
    }

    #[test]
    fn deserialize_measurement() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                serde_yaml::from_str::<SerializableValue>("42mm")
                    .unwrap()
                    .into_value(context, &"", &VariableType::Scalar("Length")),
                Ok(Scalar::try_from(Length::new::<millimeter>(42.0))
                    .unwrap()
                    .into())
            );
        });
    }

    #[test]
    fn serialize_measurement() {
        let value =
            SerializableValue::Scalar(Scalar::try_from(Length::new::<millimeter>(42.0)).unwrap());

        assert_eq!(serde_yaml::to_string(&value).unwrap(), "0.042 m\n");
    }

    #[test]
    fn deserialize_default() {
        ExecutionContext::new(&mut Runtime::default(), |context| {
            assert_eq!(
                serde_yaml::from_str::<SerializableValue>("default")
                    .unwrap()
                    .into_value(context, &"", &VariableType::Scalar("Length")),
                Ok(DefaultValue.into())
            );
        });
    }

    #[test]
    fn serialize_default() {
        let value = SerializableValue::Default;

        // It's supposed to fail.
        assert!(serde_yaml::to_string(&value).is_err());
    }
}
