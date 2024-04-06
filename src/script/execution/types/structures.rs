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

use std::{collections::HashMap, rc::Rc};

use crate::script::{
    execution::{
        expressions::{run_expression, run_trailer},
        validate_assignment_type, ExecutionContext, Failure,
    },
    logging::RuntimeLog,
    parsing::{self, StructInitialization, Trailer, VariableType},
    Span,
};

use super::{serializable::SerializableValue, NamedObject, Object, OperatorResult, Value};

#[derive(Debug, PartialEq, Clone)]
pub struct Structure<S: Span> {
    ty: Rc<parsing::StructDefinition<S>>,
    pub members: Rc<HashMap<String, Value<S>>>,
}

impl<S: Span> Structure<S> {
    pub fn name(&self) -> &str {
        self.ty.name.as_str()
    }
}

impl<S: Span> Object<S> for Structure<S> {
    fn matches_type(
        &self,
        ty: &VariableType<S>,
        _log: &mut dyn RuntimeLog<S>,
        _variable_name_span: &S,
    ) -> OperatorResult<S, bool> {
        Ok(if let VariableType::Struct(type_name) = ty {
            type_name.as_str() == self.ty.name.as_str()
        } else {
            false
        })
    }

    fn attribute(
        &self,
        _log: &mut dyn RuntimeLog<S>,
        _span: &S,
        name: &S,
    ) -> OperatorResult<S, Value<S>> {
        let key = name.as_str();

        if let Some(value) = self.members.get(key) {
            Ok(value.clone())
        } else {
            Err(Failure::UnknownAttribute(name.clone()))
        }
    }

    fn export(
        &self,
        log: &mut dyn RuntimeLog<S>,
        span: &S,
    ) -> OperatorResult<S, SerializableValue> {
        let mut members = HashMap::with_capacity(self.members.len());

        for (key, item) in self.members.iter() {
            let serializable = item.export(log, span)?;
            members.insert(key.clone(), serializable);
        }

        Ok(SerializableValue::Struct {
            ty: self.ty.name.to_string(),
            members,
        })
    }
}

impl<S: Span> NamedObject for Structure<S> {
    fn static_type_name() -> &'static str {
        "struct"
    }
}

impl<S: Span> Structure<S> {
    pub fn new(ty: Rc<parsing::StructDefinition<S>>, table: HashMap<String, Value<S>>) -> Self {
        Self {
            ty,
            members: Rc::new(table),
        }
    }

    #[cfg(test)]
    pub fn from_array<const N: usize>(
        ty: Rc<parsing::StructDefinition<S>>,
        values: [(String, Value<S>); N],
    ) -> Self {
        Self {
            ty,
            members: Rc::new(HashMap::from(values)),
        }
    }

    pub fn initalization(
        context: &mut ExecutionContext<S>,
        struct_definition: &Trailer<S>,
        initalization: &StructInitialization<S>,
    ) -> OperatorResult<S, Value<S>> {
        enum Inheritance<'a, S: Span> {
            Structure(&'a Structure<S>),
            Default,
            None,
        }

        fn build_struct<S: Span>(
            context: &mut ExecutionContext<S>,
            struct_definition: &Rc<parsing::StructDefinition<S>>,
            initalization: &StructInitialization<S>,
            inheritance: Inheritance<S>,
        ) -> OperatorResult<S, Value<S>> {
            // We're going to run through this whole struct and collect all the errors at once.
            let mut failures = Vec::new();

            let mut members = HashMap::with_capacity(struct_definition.members.len());

            for member in struct_definition.members.iter() {
                let name = member.name.as_str();

                let assignment =
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

                let value = if let Some(expression) = assignment {
                    match run_expression(context, expression) {
                        Ok(value) => match validate_assignment_type(
                            context,
                            member,
                            expression.get_span(),
                            value,
                            expression.get_span(),
                        ) {
                            Ok(value) => value,
                            Err(failure) => {
                                failures.push(failure);
                                continue;
                            }
                        },
                        Err(failure) => {
                            failures.push(failure);
                            continue;
                        }
                    }
                } else {
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
                            if let Some(default) = &member.ty.default_value {
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
                        Inheritance::None => {
                            failures.push(Failure::StructMissingAssignment(
                                initalization.get_span().clone(),
                                member.name.clone(),
                            ));
                            continue;
                        }
                    }
                };

                members.insert(member.name.to_string(), value);
            }

            // Make sure there aren't extra assignments.
            for (name, _expression) in initalization.assignments.iter() {
                if !struct_definition
                    .members
                    .iter()
                    .any(|member| member.name.as_str() == name.as_str())
                {
                    failures.push(Failure::StructExcessAssignment(name.clone()));
                }
            }

            if failures.is_empty() {
                Ok(Structure {
                    ty: Rc::clone(struct_definition),
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

        let struct_evaluated_definition = run_trailer(context, struct_definition)?
            .downcast::<StructDefinition<S>>(struct_definition.get_span())?
            .definition;

        // Figure out our inheritance, if any.
        if let Some(trailer) = &initalization.inheritance {
            let trailer_result = run_trailer(context, trailer)?;

            match trailer_result {
                Value::Structure(structure) => {
                    if structure.ty == struct_evaluated_definition {
                        build_struct(
                            context,
                            &struct_evaluated_definition,
                            initalization,
                            Inheritance::Structure(&structure),
                        )
                    } else {
                        return Err(Failure::StructWrongInheritanceType(
                            trailer.get_span().clone(),
                            struct_evaluated_definition.name.clone(),
                            structure.ty.name.clone(),
                        ));
                    }
                }
                Value::Default(_span) => build_struct(
                    context,
                    &struct_evaluated_definition,
                    initalization,
                    Inheritance::Default,
                ),
                _ => {
                    return Err(Failure::ExpectedGot(
                        trailer.get_span().clone(),
                        "Struct or Default".into(),
                        trailer_result.type_name(),
                    ));
                }
            }
        } else {
            build_struct(
                context,
                &struct_evaluated_definition,
                initalization,
                Inheritance::None,
            )
        }
    }
}

#[derive(Clone)]
pub struct StructDefinition<S: Span> {
    pub definition: Rc<parsing::StructDefinition<S>>,
}

impl<S: Span> Object<S> for StructDefinition<S> {
    fn matches_type(
        &self,
        _ty: &VariableType<S>,
        _log: &mut dyn RuntimeLog<S>,
        _variable_name_span: &S,
    ) -> OperatorResult<S, bool> {
        // You cannot assign struct definitions to variables or pass them between functions.
        Ok(false)
    }
}

impl<S: Span> std::fmt::Debug for StructDefinition<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StructDefinition").finish()
    }
}

impl<S: Span> PartialEq for StructDefinition<S> {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl<S: Span> NamedObject for StructDefinition<S> {
    fn static_type_name() -> &'static str {
        "StructDefinition"
    }
}

#[cfg(test)]
mod test {
    use crate::script::{
        execution::{expressions::run_expression, ExecutionContext, Module},
        parsing::Expression,
        Runtime,
    };
    use common_data_types::Number;

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

        let mut runtime = Runtime::from(module);
        ExecutionContext::new(&mut runtime, |context| {
            let two_part_struct = context
                .stack
                .get_variable(&"TwoPartStruct")
                .unwrap()
                .downcast_ref::<StructDefinition<&'static str>>(&"")
                .unwrap()
                .definition
                .clone();
            let empty_struct = context
                .stack
                .get_variable(&"EmptyStruct")
                .unwrap()
                .downcast_ref::<StructDefinition<&'static str>>(&"")
                .unwrap()
                .definition
                .clone();
            let default_struct = context
                .stack
                .get_variable(&"DefaultStruct")
                .unwrap()
                .downcast_ref::<StructDefinition<&'static str>>(&"")
                .unwrap()
                .definition
                .clone();

            context.stack.new_variable(
                &"two_part_struct",
                Structure::from_array(
                    two_part_struct.clone(),
                    [
                        ("value".into(), Number::new(3.0).unwrap().into()),
                        ("other".into(), Number::new(4.0).unwrap().into()),
                    ],
                )
                .into(),
            );

            assert_eq!(
                run_expression(context, &Expression::parse("EmptyStruct {}").unwrap().1),
                Ok(Structure::from_array(empty_struct, []).into())
            );

            assert_eq!(
                run_expression(
                    context,
                    &Expression::parse("DefaultStruct { value = 24 }").unwrap().1
                ),
                Ok(Structure::from_array(
                    default_struct.clone(),
                    [("value".into(), Number::new(24.0).unwrap().into())]
                )
                .into())
            );
            assert_eq!(
                run_expression(
                    context,
                    &Expression::parse("DefaultStruct { ..default }").unwrap().1
                ),
                Ok(Structure::from_array(
                    default_struct.clone(),
                    [("value".into(), Number::new(42.0).unwrap().into())]
                )
                .into())
            );
            assert_eq!(
                run_expression(
                    context,
                    &Expression::parse("DefaultStruct { value = default }")
                        .unwrap()
                        .1
                ),
                Ok(Structure::from_array(
                    default_struct,
                    [("value".into(), Number::new(42.0).unwrap().into())]
                )
                .into())
            );
            assert_eq!(
                run_expression(
                    context,
                    &Expression::parse("TwoPartStruct { ..default }").unwrap().1
                ),
                Ok(Structure::from_array(
                    two_part_struct.clone(),
                    [
                        ("value".into(), Number::new(1.0).unwrap().into()),
                        ("other".into(), Number::new(2.0).unwrap().into())
                    ]
                )
                .into())
            );
            assert_eq!(
                run_expression(
                    context,
                    &Expression::parse("TwoPartStruct { value = 3, ..default }")
                        .unwrap()
                        .1
                ),
                Ok(Structure::from_array(
                    two_part_struct.clone(),
                    [
                        ("value".into(), Number::new(3.0).unwrap().into()),
                        ("other".into(), Number::new(2.0).unwrap().into())
                    ]
                )
                .into())
            );
            assert_eq!(
                run_expression(
                    context,
                    &Expression::parse("TwoPartStruct { ..two_part_struct }")
                        .unwrap()
                        .1
                ),
                Ok(Structure::from_array(
                    two_part_struct.clone(),
                    [
                        ("value".into(), Number::new(3.0).unwrap().into()),
                        ("other".into(), Number::new(4.0).unwrap().into())
                    ]
                )
                .into())
            );
            assert_eq!(
                run_expression(
                    context,
                    &Expression::parse("TwoPartStruct { value = 5, ..two_part_struct }")
                        .unwrap()
                        .1
                ),
                Ok(Structure::from_array(
                    two_part_struct,
                    [
                        ("value".into(), Number::new(5.0).unwrap().into()),
                        ("other".into(), Number::new(4.0).unwrap().into())
                    ]
                )
                .into())
            );
            assert_eq!(
                run_expression(
                    context,
                    &Expression::parse("TwoPartStruct { ..EmptyStruct {} }")
                        .unwrap()
                        .1
                ),
                Err(Failure::StructWrongInheritanceType(
                    "EmptyStruct",
                    "TwoPartStruct",
                    "EmptyStruct"
                ))
            );
            assert_eq!(
                run_expression(
                    context,
                    &Expression::parse("EmptyStruct { bogus = 24 }").unwrap().1
                ),
                Err(Failure::StructConstruction(
                    "{",
                    vec![Failure::StructExcessAssignment("bogus")]
                ))
            );
            assert_eq!(
                run_expression(
                    context,
                    &Expression::parse("DefaultStruct { value = false }")
                        .unwrap()
                        .1
                ),
                Err(Failure::StructConstruction(
                    "{",
                    vec![Failure::ExpectedGot(
                        "false",
                        "Number".into(),
                        "Boolean".into()
                    )]
                )),
            );
        });
    }
}
