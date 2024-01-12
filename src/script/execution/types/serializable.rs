use std::{borrow::Cow, collections::HashMap};

use serde::{Deserialize, Serialize};

use crate::script::{
    execution::{types::StructDefinition, ExecutionContext},
    parsing::VariableType,
    LogMessage, Span,
};

use super::{
    number::{RawNumber, UnwrapNotNan},
    List, Number, SString, Structure, Value,
};

#[derive(Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case", untagged)]
pub enum SerializableValue {
    Boolean(bool),
    Number(RawNumber),
    Struct {
        #[serde(rename = "type")]
        ty: String,
        members: HashMap<String, SerializableValue>,
    },
    List(Vec<SerializableValue>),
    String(String),
}

impl SerializableValue {
    pub fn into_value_without_type_check<'a, S: Span>(
        self,
        context: &mut ExecutionContext<'a, S>,
        span: &S,
    ) -> std::result::Result<Value<'a, S>, ()> {
        match self {
            Self::Boolean(value) => Ok((value).into()),
            Self::Number(value) => Number::new(value)
                .unwrap_not_nan(&mut context.log, span)
                .map_err(|_| ()),
            Self::Struct {
                ty,
                members: mut values,
            } => {
                let initalizer = context
                    .stack
                    .get_variable_str(&mut context.log, span, ty.as_ref())
                    .map_err(|_| ())?
                    .downcast_ref::<StructDefinition<'a, S>>(&mut context.log, span)
                    .map_err(|_| ())?
                    .definition;

                let mut failed = false;
                let mut table = HashMap::with_capacity(initalizer.members.len());

                for member in initalizer.members.iter() {
                    if let Some(value) = values.remove(member.name.as_str()) {
                        match value.into_value(context, span, &member.ty) {
                            Ok(value) => {
                                table.insert(member.name.to_string(), value);
                            }
                            Err(_) => {
                                // They should have already logged their failure.
                                failed = true;
                            }
                        }
                    } else {
                        context.log.push(LogMessage::StructMissingAssignment(
                            span.clone(),
                            member.name.clone(),
                        ));
                        failed = true;
                    }
                }

                if failed {
                    Err(())
                } else {
                    Ok(Structure::new(initalizer.name.clone(), table).into())
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
        }
    }

    pub fn into_value<'a, S: Span>(
        self,
        context: &mut ExecutionContext<'a, S>,
        span: &S,
        ty: &VariableType<S>,
    ) -> std::result::Result<Value<'a, S>, ()> {
        if self.matches_type(ty) {
            self.into_value_without_type_check(context, span)
        } else {
            context.log.push(LogMessage::CannotConvertFromTo(
                span.clone(),
                self.type_name(),
                ty.name(),
            ));
            Err(())
        }
    }

    fn matches_type<S: Span>(&self, ty: &VariableType<S>) -> bool {
        match (self, ty) {
            (Self::Boolean(_), VariableType::Boolean) => true,
            (Self::Number(_), VariableType::Number) => true,
            (Self::Struct { ty: s_ty, .. }, VariableType::Struct(v_ty)) => v_ty.as_str() == s_ty,
            (Self::List(_), VariableType::List) => true,
            (Self::String(_), VariableType::String) => true,
            _ => false,
        }
    }

    fn type_name(&self) -> Cow<'static, str> {
        match self {
            SerializableValue::Boolean(_) => "Boolean".into(),
            SerializableValue::Number(_) => "Number".into(),
            SerializableValue::Struct { ty, members: _ } => format!("struct {}", ty).into(),
            SerializableValue::List(_) => "List".into(),
            SerializableValue::String(_) => "String".into(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::script::{
        execution::{
            expressions::run_expression,
            types::{Object, SString},
            ModuleScope, Stack,
        },
        module::Module,
        parsing::Expression,
        RuntimeLog,
    };

    use super::*;

    #[test]
    fn deserialize_boolean() {
        let mut context = ExecutionContext::<&str> {
            log: Default::default(),
            stack: Default::default(),
        };

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
        let mut context = ExecutionContext::<&str> {
            log: Default::default(),
            stack: Default::default(),
        };

        let boolean: Value<&'static str> = true.into();
        assert_eq!(
            boolean.export(&mut context.log, &""),
            Ok(SerializableValue::Boolean(true))
        );

        let boolean: Value<&'static str> = false.into();
        assert_eq!(
            boolean.export(&mut context.log, &""),
            Ok(SerializableValue::Boolean(false))
        );
    }

    #[test]
    fn deserialize_number() {
        let mut context = ExecutionContext::<&str> {
            log: Default::default(),
            stack: Default::default(),
        };

        assert_eq!(
            serde_yaml::from_str::<SerializableValue>("42")
                .unwrap()
                .into_value(&mut context, &"", &VariableType::Number),
            Ok(Number::new(42.0).unwrap().into())
        );
    }

    #[test]
    fn serialize_number() {
        let mut context = ExecutionContext::<&str> {
            log: Default::default(),
            stack: Default::default(),
        };

        let value: Value<&'static str> = Number::new(42.0).unwrap().into();
        assert_eq!(
            value.export(&mut context.log, &""),
            Ok(SerializableValue::Number(42.0))
        );
    }

    #[test]
    fn deserialize_struct() {
        let mut log = RuntimeLog::default();

        let module = Module::load(
            &mut log,
            "test_module.ccm",
            r#"struct MyStruct { value: Number }"#,
        )
        .unwrap();

        assert!(!log.containes_any_error());

        let module_scope = ModuleScope::new(&module);
        let mut context = ExecutionContext {
            stack: Stack::new(module_scope),
            log,
        };

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
                &Expression::parse("struct MyStruct { value = 42 }")
                    .unwrap()
                    .1
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
        let mut log = RuntimeLog::default();

        let module = Module::load(
            &mut log,
            "test_module.ccm",
            r#"struct MyStruct { value: Number }"#,
        )
        .unwrap();

        assert!(!log.containes_any_error());

        let module_scope = ModuleScope::new(&module);
        let mut context = ExecutionContext {
            stack: Stack::new(module_scope),
            log,
        };

        let value: Value<&'static str> = run_expression(
            &mut context,
            &Expression::parse("struct MyStruct { value = 42 }")
                .unwrap()
                .1,
        )
        .unwrap();

        assert_eq!(
            value.export(&mut context.log, &""),
            Ok(SerializableValue::Struct {
                ty: "MyStruct".to_string(),
                members: HashMap::from([("value".to_string(), SerializableValue::Number(42.0))])
            })
        );
    }

    #[test]
    fn deserialize_list() {
        let list_def = r#"[1, true, "some text"]"#;

        let mut context = ExecutionContext::<&str> {
            log: Default::default(),
            stack: Default::default(),
        };

        let list = serde_yaml::from_str::<SerializableValue>(list_def).unwrap();

        let list = list.into_value(&mut context, &"", &VariableType::List);

        assert_eq!(
            list.unwrap(),
            run_expression(
                &mut context,
                &Expression::parse("[1, true, \"some text\"]").unwrap().1
            )
            .unwrap()
        );
    }

    #[test]
    fn serialize_list() {
        let mut context = ExecutionContext::<&str> {
            log: Default::default(),
            stack: Default::default(),
        };

        let value: Value<&'static str> = List::from([
            Number::new(1.0).unwrap().into(),
            true.into(),
            SString::from("some text".to_string()).into(),
        ])
        .into();
        assert_eq!(
            value.export(&mut context.log, &""),
            Ok(SerializableValue::List(vec![
                SerializableValue::Number(1.0),
                SerializableValue::Boolean(true),
                SerializableValue::String("some text".to_string())
            ]))
        );
    }

    #[test]
    fn deserialize_string() {
        let mut context = ExecutionContext::<&str> {
            log: Default::default(),
            stack: Default::default(),
        };

        assert_eq!(
            serde_yaml::from_str::<SerializableValue>("text")
                .unwrap()
                .into_value(&mut context, &"", &VariableType::String),
            Ok(SString::from("text").into())
        );
    }

    #[test]
    fn serialize_string() {
        let mut context = ExecutionContext::<&str> {
            log: Default::default(),
            stack: Default::default(),
        };

        let value: Value<&'static str> = SString::from("This is a test".to_string()).into();
        assert_eq!(
            value.export(&mut context.log, &""),
            Ok(SerializableValue::String("This is a test".to_string()))
        );
    }
}
