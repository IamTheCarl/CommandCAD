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
    execution::{types::StructDefinition, ExecutionContext, Failure},
    parsing::VariableType,
    Span,
};

use super::{
    structures::validate_assignment_type, DefaultValue, List, Measurement, Object, OperatorResult,
    SString, Structure, Value,
};

// TODO add the ability to deserialize Default and Measurements. You should not be able to serialize these values.
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
    #[serde(
        untagged //,
        // deserialize_with = "SerializableValue::parse_measurement",
        // serialize_with = "SerializableValue::serialize_measurement"
    )]
    Measurement(Measurement),
    #[serde(untagged)]
    String(String),
}

impl SerializableValue {
    // pub fn parse_measurement<'de, D>(deserializer: D) -> Result<Measurement, D::Error>
    // where
    //     D: Deserializer<'de>,
    // {
    //     use serde::de::Error;

    // 	let n = deserializer.deserialize_any(visitor)

    //      if let Ok(s) = String::deserialize(deserializer) {
    //     //     Measurement::from_str(&s).map_err(|error| D::Error::custom(format!("{:?}", error)))
    //     // } else {
    //     //     let n = RawNumber::deserialize(deserializer)?;
    //     //     let n = Number::new(n).map_err(|error| D::Error::custom(format!("{:?}", error)))?;

    //     //     Ok(Measurement::from_number(n))
    //     // }
    // }

    // pub fn serialize_measurement<S>(
    //     measurement: &Measurement,
    //     serializer: S,
    // ) -> Result<S::Ok, S::Error>
    // where
    //     S: Serializer,
    // {
    //     todo!()
    // }

    pub fn into_value_without_type_check<'a, S: Span>(
        self,
        context: &mut ExecutionContext<'a, S>,
        span: &S,
    ) -> OperatorResult<S, Value<'a, S>> {
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
                    .downcast_ref::<StructDefinition<'a, S>>(span)?
                    .definition;

                let mut failures = Vec::new();
                let mut table = HashMap::with_capacity(initalizer.members.len());

                for member in initalizer.members.iter() {
                    if let Some(value) = values.remove(member.name.as_str()) {
                        match value.into_value(context, span, &member.ty.ty) {
                            Ok(value) => {
                                match validate_assignment_type(context, member, span, value) {
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
                let mut collected_values: Vec<Value<'a, S>> = Vec::with_capacity(values.len());

                for value in values {
                    let value = value.into_value_without_type_check(context, span)?;
                    collected_values.push(value);
                }

                Ok(List::from(collected_values).into())
            }
            Self::String(value) => Ok(SString::from(value).into()),
            Self::Measurement(measurement) => Ok(measurement.into()),
        }
    }

    pub fn into_value<'a, S: Span>(
        self,
        context: &mut ExecutionContext<'a, S>,
        span: &S,
        ty: &VariableType<S>,
    ) -> OperatorResult<S, Value<'a, S>> {
        if self.matches_type(ty) {
            self.into_value_without_type_check(context, span)
        } else {
            Err(Failure::CannotConvertFromTo(
                span.clone(),
                self.type_name(),
                ty.name(),
            ))
        }
    }

    fn matches_type<S: Span>(&self, ty: &VariableType<S>) -> bool {
        match (self, ty) {
            (Self::Boolean(_), VariableType::Boolean) => true,
            (Self::Struct { ty: s_ty, .. }, VariableType::Struct(v_ty)) => v_ty.as_str() == s_ty,
            (Self::List(_), VariableType::List) => true,
            (Self::String(_), VariableType::String) => true,
            (Self::Measurement(measurement), ty) => measurement.matches_type(ty),
            (Self::Default, _) => true,
            _ => false,
        }
    }

    fn type_name(&self) -> Cow<'static, str> {
        match self {
            SerializableValue::Default => "Default".into(),
            SerializableValue::Boolean(_) => "Boolean".into(),
            SerializableValue::Struct { ty, members: _ } => format!("struct {}", ty).into(),
            SerializableValue::List(_) => "List".into(),
            SerializableValue::String(_) => "String".into(),
            SerializableValue::Measurement(measurement) => {
                <Measurement as Object<&'static str>>::type_name(measurement)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use common_data_types::Number;
    use uom::si::{f64::Length, length::millimeter};

    use crate::script::{
        execution::{
            expressions::run_expression,
            types::{Measurement, Object, SString},
            Module, ModuleScope,
        },
        parsing::Expression,
    };

    use super::*;

    #[test]
    fn deserialize_boolean() {
        let mut context = ExecutionContext::default();

        assert_eq!(
            serde_yaml::from_str::<SerializableValue>("true")
                .unwrap()
                .into_value(&mut context, &"", &VariableType::Boolean),
            Ok(true.into())
        );
        assert_eq!(
            serde_yaml::from_str::<SerializableValue>("false")
                .unwrap()
                .into_value(&mut context, &"", &VariableType::Boolean),
            Ok(false.into())
        );
    }

    #[test]
    fn serialize_boolean() {
        let context = ExecutionContext::default();

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
    }

    #[test]
    fn deserialize_number() {
        let mut context = ExecutionContext::default();

        assert_eq!(
            serde_yaml::from_str::<SerializableValue>("42")
                .unwrap()
                .into_value(&mut context, &"", &VariableType::Measurement("Number")),
            Ok(Number::new(42.0).unwrap().into())
        );
    }

    #[test]
    fn serialize_number() {
        let value = SerializableValue::Measurement(Measurement::from(Number::new(42.0).unwrap()));

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

        let module_scope = ModuleScope::new(&module);
        let mut context = ExecutionContext::new(module_scope);

        let struct_def = r#"
                   type: MyStruct
                   members:
                     value: 42"#;

        let structure = serde_yaml::from_str::<SerializableValue>(struct_def)
            .unwrap()
            .into_value(&mut context, &"", &VariableType::Struct("MyStruct"));

        assert_eq!(
            structure.unwrap(),
            run_expression(
                &mut context,
                Box::leak(Box::new(
                    Expression::parse("MyStruct { value = 42 }").unwrap().1
                ))
            )
            .unwrap()
        );

        let struct_def = r#"
                   type: MyStruct
                   members:
                     value: default"#;

        let structure = serde_yaml::from_str::<SerializableValue>(struct_def)
            .unwrap()
            .into_value(&mut context, &"", &VariableType::Struct("MyStruct"));

        assert_eq!(
            structure.unwrap(),
            run_expression(
                &mut context,
                Box::leak(Box::new(
                    Expression::parse("MyStruct { ..default }").unwrap().1
                ))
            )
            .unwrap()
        );

        assert!(serde_yaml::from_str::<SerializableValue>(struct_def)
            .unwrap()
            .into_value(&mut context, &"", &VariableType::Struct("OtherStruct"))
            .is_err());

        let struct_def = r#"
                   type: MyStruct
                   members:
                     wrong_value: 42"#;
        assert!(serde_yaml::from_str::<SerializableValue>(struct_def)
            .unwrap()
            .into_value(&mut context, &"", &VariableType::Struct("MyStruct"))
            .is_err());

        let struct_def = r#"
                   type: MyStruct
                   members:
                     value: true"#;
        assert!(serde_yaml::from_str::<SerializableValue>(struct_def)
            .unwrap()
            .into_value(&mut context, &"", &VariableType::Struct("MyStruct"))
            .is_err());
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

        let module_scope = ModuleScope::new(&module);
        let mut context = ExecutionContext::new(module_scope);

        let value: Value<&'static str> = run_expression(
            &mut context,
            Box::leak(Box::new(
                Expression::parse("MyStruct { value = 42 }").unwrap().1,
            )),
        )
        .unwrap();

        assert_eq!(
            value.export(context.log, &""),
            Ok(SerializableValue::Struct {
                ty: "MyStruct".to_string(),
                members: HashMap::from([(
                    "value".to_string(),
                    SerializableValue::Measurement(Measurement::from(Number::new(42.0).unwrap()))
                )])
            })
        );
    }

    #[test]
    fn deserialize_list() {
        let list_def = r#"[1, true, "some text"]"#;

        let mut context = ExecutionContext::default();

        let list = serde_yaml::from_str::<SerializableValue>(list_def).unwrap();

        let list = list.into_value(&mut context, &"", &VariableType::List);

        assert_eq!(
            list.unwrap(),
            run_expression(
                &mut context,
                Box::leak(Box::new(
                    Expression::parse("[1, true, \"some text\"]").unwrap().1
                ))
            )
            .unwrap()
        );
    }

    #[test]
    fn serialize_list() {
        let context = ExecutionContext::default();

        let value: Value<&'static str> = List::from([
            Number::new(1.0).unwrap().into(),
            true.into(),
            SString::from("some text".to_string()).into(),
        ])
        .into();
        assert_eq!(
            value.export(context.log, &""),
            Ok(SerializableValue::List(vec![
                SerializableValue::Measurement(Measurement::from(Number::new(1.0).unwrap())),
                SerializableValue::Boolean(true),
                SerializableValue::String("some text".to_string())
            ]))
        );
    }

    #[test]
    fn deserialize_string() {
        let mut context = ExecutionContext::default();

        assert_eq!(
            serde_yaml::from_str::<SerializableValue>("text")
                .unwrap()
                .into_value(&mut context, &"", &VariableType::String),
            Ok(SString::from("text").into())
        );
    }

    #[test]
    fn serialize_string() {
        let context = ExecutionContext::default();

        let value: Value<&'static str> = SString::from("This is a test".to_string()).into();
        assert_eq!(
            value.export(context.log, &""),
            Ok(SerializableValue::String("This is a test".to_string()))
        );
    }

    #[test]
    fn deserialize_measurement() {
        let mut context = ExecutionContext::default();

        assert_eq!(
            serde_yaml::from_str::<SerializableValue>("42mm")
                .unwrap()
                .into_value(&mut context, &"", &VariableType::Measurement("Length")),
            Ok(Measurement::try_from(Length::new::<millimeter>(42.0))
                .unwrap()
                .into())
        );
    }

    #[test]
    fn serialize_measurement() {
        let value = SerializableValue::Measurement(
            Measurement::try_from(Length::new::<millimeter>(42.0)).unwrap(),
        );

        assert_eq!(serde_yaml::to_string(&value).unwrap(), "0.042 m\n");
    }

    #[test]
    fn deserialize_default() {
        let mut context = ExecutionContext::default();

        assert_eq!(
            serde_yaml::from_str::<SerializableValue>("default")
                .unwrap()
                .into_value(&mut context, &"", &VariableType::Measurement("Length")),
            Ok(DefaultValue.into())
        );
    }

    #[test]
    fn serialize_default() {
        let value = SerializableValue::Default;

        // It's supposed to fail.
        assert!(serde_yaml::to_string(&value).is_err());
    }
}
