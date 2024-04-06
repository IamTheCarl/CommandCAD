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

use anyhow::{bail, Result};

mod parsing;
pub use parsing::Span;

use crate::script::{
    execution::{
        types::{Solid, UserFunction},
        Module,
    },
    parsing::FunctionSignature,
};

pub use crate::script::execution::types::{Scalar, SerializableValue};

mod execution;
pub use execution::{print_all_supported_units, Failure};
use execution::{
    types::{Object, Sketch},
    GlobalResources,
};

use self::execution::{ExecutionContext, Stack};

pub mod logging;

#[derive(Default)]
pub struct Runtime<S: Span> {
    pub global_resources: GlobalResources,
    pub stack: Stack<S>,
    pub root_span: S,
}

impl<S: Span> From<Module<S>> for Runtime<S> {
    fn from(module: Module<S>) -> Self {
        let root_span = module.get_span().clone();
        let stack = Stack::new(module);

        Runtime {
            global_resources: GlobalResources::default(),
            stack,
            root_span,
        }
    }
}

impl<S: Span> Runtime<S> {
    pub fn load(file_name: impl Into<String>, code: S) -> Result<Self> {
        let mut validation_log = Vec::new();

        let file_name = file_name.into();
        let module = Module::load(&mut validation_log, file_name, code)?;

        if !validation_log.is_empty() {
            // TODO there's got to be a better way to present validation errors.
            bail!("Module failed validation: {:?}", validation_log);
        }

        Ok(Self::from(module))
    }

    pub fn run_sketch(
        &mut self,
        name: &str,
        arguments: Vec<SerializableValue>,
    ) -> std::result::Result<(), Failure<S>> {
        let root_span = self.root_span.clone();
        ExecutionContext::new(self, |context| {
            let mut argument_values = Vec::with_capacity(arguments.len());
            for argument in arguments {
                let value = argument.into_value_without_type_check(context, &root_span)?;
                argument_values.push(value);
            }

            let sketch = context.stack.get_variable_str(&root_span, name)?;

            let sketch = sketch.downcast_ref::<UserFunction<S>>(&root_span)?.clone();

            if matches!(
                sketch.source.signature,
                FunctionSignature::Sketch { arguments: _ }
            ) {
                // TODO attaching a span to a user function would be useful for debug purposes.
                let result = sketch.call(context, &root_span, argument_values, &[])?;

                result.downcast::<Sketch>(&root_span)?;

                log::warn!(
                    "Sketches currently cannot be serialized, so no output will be provied."
                );

                Ok(())
            } else {
                Err(Failure::ExpectedGot(
                    root_span.clone(),
                    "sketch".into(),
                    sketch.source.signature.to_string().into(),
                ))
            }
        })
    }

    pub fn run_solid(
        &mut self,
        name: &str,
        arguments: Vec<SerializableValue>,
    ) -> std::result::Result<Solid, Failure<S>> {
        let root_span = self.root_span.clone();
        ExecutionContext::new(self, |context| {
            let mut argument_values = Vec::with_capacity(arguments.len());
            for argument in arguments {
                let value = argument.into_value_without_type_check(context, &root_span)?;
                argument_values.push(value);
            }

            let solid = context.stack.get_variable_str(&root_span, name)?;

            let solid = solid.downcast_ref::<UserFunction<S>>(&root_span)?.clone();

            if matches!(
                solid.source.signature,
                FunctionSignature::Solid { arguments: _ }
            ) {
                // TODO attaching a span to a user function would be useful for debug purposes.
                let result = solid.call(context, &root_span, argument_values, &[])?;

                let solid = result.downcast::<Solid>(&root_span)?;
                Ok(solid)
            } else {
                Err(Failure::ExpectedGot(
                    root_span.clone(),
                    "solid".into(),
                    solid.source.signature.to_string().into(),
                ))
            }
        })
    }

    pub fn run_task(
        &mut self,
        name: &str,
        arguments: Vec<SerializableValue>,
    ) -> std::result::Result<SerializableValue, Failure<S>> {
        let root_span = self.root_span.clone();
        ExecutionContext::new(self, |context| {
            let mut argument_values = Vec::with_capacity(arguments.len());
            for argument in arguments {
                let value = argument.into_value_without_type_check(context, &root_span)?;
                argument_values.push(value);
            }

            let task = context.stack.get_variable_str(&root_span, name)?;

            let task = task.downcast_ref::<UserFunction<S>>(&root_span)?.clone();

            if matches!(
                task.source.signature,
                FunctionSignature::Task {
                    return_type: _,
                    arguments: _
                }
            ) {
                // TODO attaching a span to a user function would be useful for debug purposes.
                let result = task.call(context, &root_span, argument_values, &[])?;

                let result = result.export(context.log, &root_span)?;

                Ok(result)
            } else {
                Err(Failure::ExpectedGot(
                    root_span.clone(),
                    "task".into(),
                    task.source.signature.to_string().into(),
                ))
            }
        })
    }
}

#[cfg(test)]
mod test {
    use std::borrow::Cow;

    use common_data_types::Number;
    use uom::si::{
        f64::Length,
        length::{meter, millimeter},
    };

    use crate::script::execution::types::Scalar;

    use super::*;

    #[test]
    fn run_sketch() {
        let mut runtime = Runtime::load(
            "root_module",
            "sketch my_sketch(input: Length = 50m) { new_sketch([]) }",
        )
        .unwrap();

        assert!(matches!(
            runtime.run_sketch("my_sketch", vec![]),
            Err(Failure::MissingArguments(_))
        ));

        runtime
            .run_sketch("my_sketch", vec![SerializableValue::Default])
            .unwrap();

        runtime
            .run_sketch(
                "my_sketch",
                vec![SerializableValue::Scalar(
                    Scalar::try_from(Length::new::<meter>(10.0)).unwrap(),
                )],
            )
            .unwrap();

        let mut runtime =
            Runtime::load("root_module", "function my_sketch() -> Number { 2 }").unwrap();

        assert!(matches!(
            runtime.run_sketch("my_sketch", vec![]),
            Err(Failure::ExpectedGot(
                _,
                Cow::Borrowed("sketch"),
                _, // Cow::Borrowed("function() -> Number")
            ))
        ));
    }

    #[test]
    fn run_solid() {
        let mut runtime = Runtime::load(
            "root_module",
            "solid my_solid(input: Length = 1cm) { new_sketch(Circle { center = vec2(0m, 0m), radius = input }).sweep(global_xy_plane(), vec3(0cm, 0cm, 1cm)) }",
        )
        .unwrap();

        assert!(matches!(
            runtime.run_solid("my_solid", vec![]),
            Err(Failure::MissingArguments(_))
        ));

        runtime
            .run_solid("my_solid", vec![SerializableValue::Default])
            .unwrap();

        runtime
            .run_solid(
                "my_solid",
                vec![SerializableValue::Scalar(
                    Scalar::try_from(Length::new::<millimeter>(10.0)).unwrap(),
                )],
            )
            .unwrap();

        let mut runtime =
            Runtime::load("root_module", "function my_solid() -> Number { 2 }").unwrap();

        assert!(matches!(
            runtime.run_solid("my_solid", vec![]),
            Err(Failure::ExpectedGot(
                _,
                Cow::Borrowed("solid"),
                _, // Cow::Borrowed("function() -> Number")
            ))
        ));
    }

    #[test]
    fn run_task() {
        let mut runtime = Runtime::load(
            "root_module",
            "task my_task(input: Number = 50) -> Number { input }",
        )
        .unwrap();

        assert!(matches!(
            runtime.run_task("my_task", vec![]),
            Err(Failure::MissingArguments(_))
        ));

        assert_eq!(
            runtime.run_task("my_task", vec![SerializableValue::Default]),
            Ok(SerializableValue::Scalar(Number::new(50.0).unwrap().into()))
        );

        assert_eq!(
            runtime.run_task(
                "my_task",
                vec![SerializableValue::Scalar(Number::new(22.0).unwrap().into())]
            ),
            Ok(SerializableValue::Scalar(Number::new(22.0).unwrap().into()))
        );

        let mut runtime =
            Runtime::load("root_module", "sketch my_sketch() { new_sketch([]) }").unwrap();

        assert!(matches!(
            runtime.run_task("my_sketch", vec![]),
            Err(Failure::ExpectedGot(
                _,
                Cow::Borrowed("task"),
                _, // Cow::Borrowed("sketch()")
            ))
        ));
    }
}
