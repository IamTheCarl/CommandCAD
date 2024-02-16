use std::{collections::HashMap, rc::Rc};

use crate::script::{
    execution::{
        expressions::{run_expression, run_trailer},
        ExecutionContext, Failure,
    },
    parsing::{self, StructInitialization, VariableType},
    RuntimeLog, Span,
};

use super::{serializable::SerializableValue, NamedObject, Object, OperatorResult, Value};

#[derive(Debug, Clone, PartialEq)]
pub struct Structure<'a, S: Span> {
    type_name: S,
    members: Rc<HashMap<String, Value<'a, S>>>,
}

impl<'a, S: Span> Object<'a, S> for Structure<'a, S> {
    fn matches_type(&self, ty: &VariableType<S>) -> bool {
        if let VariableType::Struct(type_name) = ty {
            type_name.as_str() == self.type_name.as_str()
        } else {
            false
        }
    }

    fn attribute(
        &self,
        _log: &mut RuntimeLog<S>,
        _span: &S,
        name: &S,
    ) -> OperatorResult<S, Value<'a, S>> {
        let key = name.as_str();

        if let Some(value) = self.members.get(key) {
            Ok(value.clone())
        } else {
            Err(Failure::UnknownAttribute(name.clone()))
        }
    }

    fn export(&self, log: &mut RuntimeLog<S>, span: &S) -> OperatorResult<S, SerializableValue> {
        let mut members = HashMap::with_capacity(self.members.len());

        for (key, item) in self.members.iter() {
            let serializable = item.export(log, span)?;
            members.insert(key.clone(), serializable);
        }

        Ok(SerializableValue::Struct {
            ty: self.type_name.to_string(),
            members,
        })
    }
}

impl<'a, S: Span> NamedObject for Structure<'a, S> {
    fn static_type_name() -> &'static str {
        "struct"
    }
}

impl<'a, S: Span, const N: usize> From<(S, [(String, Value<'a, S>); N])> for Structure<'a, S> {
    fn from((type_name, value): (S, [(String, Value<'a, S>); N])) -> Self {
        Self {
            type_name,
            members: Rc::new(HashMap::from(value)),
        }
    }
}

impl<'a, S: Span> Structure<'a, S> {
    pub fn new(type_name: S, table: HashMap<String, Value<'a, S>>) -> Self {
        Self {
            type_name,
            members: Rc::new(table),
        }
    }

    pub fn initalization(
        context: &mut ExecutionContext<'a, S>,
        initalization: &StructInitialization<S>,
    ) -> OperatorResult<S, Value<'a, S>> {
        enum Inheritance<'a, S: Span> {
            Structure(Structure<'a, S>),
            Default,
        }

        let type_name = initalization.name.clone();

        // We're going to run through this whole struct and collect all the errors at once.
        let mut failures = Vec::new();

        // Figure out our inheritance, if any.
        let inheritance = if let Some(trailer) = &initalization.inheritance {
            let trailer_result = run_trailer(context, trailer)?;

            match trailer_result {
                Value::Structure(structure) => {
                    if structure.type_name.as_str() == type_name.as_str() {
                        Some(Inheritance::Structure(structure))
                    } else {
                        return Err(Failure::StructWrongInheritanceType(
                            trailer.get_span().clone(),
                            type_name,
                            structure.type_name.clone(),
                        ));
                    }
                }
                Value::Default(_span) => Some(Inheritance::Default),
                _ => {
                    return Err(Failure::ExpectedGot(
                        trailer.get_span().clone(),
                        "Struct or Default".into(),
                        trailer_result.type_name(),
                    ));
                }
            }
        } else {
            None
        };

        let struct_source = context
            .stack
            .get_variable(&initalization.name)?
            .downcast_ref::<StructDefinition<S>>(&initalization.name)?
            .definition;

        let mut members = HashMap::with_capacity(struct_source.members.len());

        for member in struct_source.members.iter() {
            let name = member.name.as_str();

            let assignment_expression =
                initalization
                    .assignments
                    .iter()
                    .find_map(|(assignment_name, assignment)| {
                        if assignment_name.as_str() == name {
                            Some(assignment)
                        } else {
                            None
                        }
                    });

            let value = if let Some(expression) = assignment_expression {
                match run_expression(context, expression) {
                    Ok(value) => {
                        if value.matches_type(&member.ty) {
                            value
                        } else {
                            failures.push(Failure::ExpectedGot(
                                expression.get_span().clone(),
                                member.ty.name(),
                                value.type_name().into(),
                            ));
                            continue;
                        }
                    }
                    Err(failure) => {
                        failures.push(failure);
                        continue;
                    }
                }
            } else if let Some(inheritance) = inheritance.as_ref() {
                match inheritance {
                    Inheritance::Structure(inheritance) => {
                        if let Some(value) = inheritance.members.get(member.name.as_str()) {
                            value.clone()
                        } else {
                            failures.push(Failure::StructMissingAssignment(
                                initalization.get_span().clone(),
                                member.name.clone(),
                            ));
                            continue;
                        }
                    }
                    Inheritance::Default => {
                        if let Some(default) = &member.default_value {
                            match Value::from_litteral(context, default) {
                                Ok(value) => value,
                                Err(failure) => {
                                    failures.push(failure);
                                    continue;
                                }
                            }
                        } else {
                            failures.push(Failure::NoDefault(
                                initalization.get_span().clone(),
                                member.name.clone(),
                            ));
                            continue;
                        }
                    }
                }
            } else {
                failures.push(Failure::StructMissingAssignment(
                    initalization.get_span().clone(),
                    member.name.clone(),
                ));
                continue;
            };

            members.insert(member.name.to_string(), value);
        }

        // Make sure there aren't extra assignments.
        for (name, _expression) in initalization.assignments.iter() {
            if struct_source
                .members
                .iter()
                .find(|member| member.name.as_str() == name.as_str())
                .is_none()
            {
                failures.push(Failure::StructExcessAssignment(name.clone()));
            }
        }

        if failures.is_empty() {
            Ok(Structure {
                type_name,
                members: Rc::new(members),
            }
            .into())
        } else {
            Err(Failure::StructConstruction(
                initalization.get_span().clone(),
                failures,
            ))
        }
    }
}

#[derive(Clone)]
pub struct StructDefinition<'a, S: Span> {
    pub definition: &'a parsing::Struct<S>,
}

impl<'a, S: Span> Object<'a, S> for StructDefinition<'a, S> {
    fn matches_type(&self, _ty: &VariableType<S>) -> bool {
        false
    }
}

impl<'a, S: Span> std::fmt::Debug for StructDefinition<'a, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StructDefinition").finish()
    }
}

impl<'a, S: Span> PartialEq for StructDefinition<'a, S> {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl<'a, S: Span> NamedObject for StructDefinition<'a, S> {
    fn static_type_name() -> &'static str {
        "StructDefinition"
    }
}

#[cfg(test)]
mod test {
    use crate::script::{
        execution::{
            expressions::run_expression, types::Number, ExecutionContext, ModuleScope, Stack,
        },
        module::Module,
        parsing::Expression,
    };

    use super::*;

    #[test]
    fn struct_initalization() {
        let mut log = Vec::new();

        let module = Module::load(
            &mut log,
            "test_module.ccm",
            r#"struct EmptyStruct {}
          struct DefaultStruct { value: Number = 42 }
            struct TwoPartStruct { value: Number = 1, other: Number = 2 }"#,
        )
        .unwrap();

        assert!(log.is_empty());

        let module_scope = ModuleScope::new(&module);
        let mut context = ExecutionContext {
            stack: Stack::new(module_scope),
            log: Default::default(),
        };

        context.stack.new_variable(
            &"two_part_struct",
            Structure::from((
                "TwoPartStruct",
                [
                    ("value".into(), Number::new(3.0).unwrap().into()),
                    ("other".into(), Number::new(4.0).unwrap().into()),
                ],
            ))
            .into(),
        );

        assert_eq!(
            run_expression(
                &mut context,
                &Expression::parse("struct EmptyStruct {}").unwrap().1
            ),
            Ok(Structure::from(("EmptyStruct", [])).into())
        );

        assert_eq!(
            run_expression(
                &mut context,
                &Expression::parse("struct DefaultStruct { value = 24 }")
                    .unwrap()
                    .1
            ),
            Ok(Structure::from((
                "DefaultStruct",
                [("value".into(), Number::new(24.0).unwrap().into())]
            ))
            .into())
        );
        assert_eq!(
            run_expression(
                &mut context,
                &Expression::parse("struct DefaultStruct { ..default }")
                    .unwrap()
                    .1
            ),
            Ok(Structure::from((
                "DefaultStruct",
                [("value".into(), Number::new(42.0).unwrap().into())]
            ))
            .into())
        );
        assert_eq!(
            run_expression(
                &mut context,
                &Expression::parse("struct TwoPartStruct { ..default }")
                    .unwrap()
                    .1
            ),
            Ok(Structure::from((
                "TwoPartStruct",
                [
                    ("value".into(), Number::new(1.0).unwrap().into()),
                    ("other".into(), Number::new(2.0).unwrap().into())
                ]
            ))
            .into())
        );
        assert_eq!(
            run_expression(
                &mut context,
                &Expression::parse("struct TwoPartStruct { value = 3, ..default }")
                    .unwrap()
                    .1
            ),
            Ok(Structure::from((
                "TwoPartStruct",
                [
                    ("value".into(), Number::new(3.0).unwrap().into()),
                    ("other".into(), Number::new(2.0).unwrap().into())
                ]
            ))
            .into())
        );
        assert_eq!(
            run_expression(
                &mut context,
                &Expression::parse("struct TwoPartStruct { ..two_part_struct }")
                    .unwrap()
                    .1
            ),
            Ok(Structure::from((
                "TwoPartStruct",
                [
                    ("value".into(), Number::new(3.0).unwrap().into()),
                    ("other".into(), Number::new(4.0).unwrap().into())
                ]
            ))
            .into())
        );
        assert_eq!(
            run_expression(
                &mut context,
                &Expression::parse("struct TwoPartStruct { value = 5, ..two_part_struct }")
                    .unwrap()
                    .1
            ),
            Ok(Structure::from((
                "TwoPartStruct",
                [
                    ("value".into(), Number::new(5.0).unwrap().into()),
                    ("other".into(), Number::new(4.0).unwrap().into())
                ]
            ))
            .into())
        );
        assert_eq!(
            run_expression(
                &mut context,
                &Expression::parse("struct TwoPartStruct { ..struct EmptyStruct {} }")
                    .unwrap()
                    .1
            ),
            Err(Failure::StructWrongInheritanceType(
                "struct",
                "TwoPartStruct",
                "EmptyStruct"
            ))
        );
        assert_eq!(
            run_expression(
                &mut context,
                &Expression::parse("struct EmptyStruct { bogus = 24 }")
                    .unwrap()
                    .1
            ),
            Err(Failure::StructConstruction(
                "struct",
                vec![Failure::StructExcessAssignment("bogus")]
            ))
        );
        assert_eq!(
            run_expression(
                &mut context,
                &Expression::parse("struct DefaultStruct { value = false }")
                    .unwrap()
                    .1
            ),
            Err(Failure::StructConstruction(
                "struct",
                vec![Failure::ExpectedGot(
                    "false",
                    "Number".into(),
                    "Boolean".into()
                )]
            )),
        );
    }
}
