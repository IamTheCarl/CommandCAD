use crate::script::{
    execution::{
        types::{function::IntoBuiltinFunction, NamedObject, Object, Structure},
        ExecutionContext, Failure,
    },
    parsing::VariableType,
    Span,
};

use fj::core::{
    objects::Cycle as FornjotCycle,
    operations::{build::BuildCycle, insert::Insert},
    storage::Handle,
};

use super::{circle::unwrap_circle, polygon::unwrap_polygon};

pub fn register_globals<'a, S: Span>(context: &mut ExecutionContext<'a, S>) {
    context.stack.new_variable_str(
        "new_cycle",
        (|context: &mut ExecutionContext<'a, S>, span: &S, configuration: Structure<'a, S>| {
            match configuration.name() {
                "Circle" => {
                    let (center, radius) = unwrap_circle(context, span, configuration)?;

                    let circle = FornjotCycle::circle(
                        center,
                        radius,
                        &mut context.global_resources.fornjot_services,
                    );
                    let circle = circle.insert(&mut context.global_resources.fornjot_services);

                    Ok(Cycle { handle: circle }.into())
                }
                "Polygon" => {
                    let points = unwrap_polygon(context, span, configuration)?;

                    let polygone = FornjotCycle::polygon(
                        points,
                        &mut context.global_resources.fornjot_services,
                    );
                    let polygon = polygone.insert(&mut context.global_resources.fornjot_services);

                    Ok(Cycle { handle: polygon }.into())
                }
                // "RawCycle" => {
                //     todo!() // TODO I want to be able to build a region from half-edges.
                // }
                _ => Err(Failure::ExpectedGot(
                    span.clone(),
                    "Empty, Circle, or Polygon".into(),
                    configuration.name().to_string().into(),
                )),
            }
        })
        .into_builtin_function()
        .into(),
    )
}

#[derive(Clone)]
pub struct Cycle {
    pub handle: Handle<FornjotCycle>,
}

impl<'a, S: Span> Object<'a, S> for Cycle {
    fn matches_type(&self, ty: &VariableType<S>) -> bool {
        matches!(ty, VariableType::Cycle)
    }
}

impl NamedObject for Cycle {
    fn static_type_name() -> &'static str {
        "Cycle"
    }
}

impl PartialEq for Cycle {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl std::fmt::Debug for Cycle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Cycle").finish()
    }
}

#[cfg(test)]
mod test {
    use crate::script::{
        execution::{expressions::run_expression, types::Value},
        parsing::Expression,
    };

    use super::*;

    #[test]
    fn construct_circle() {
        let mut context = ExecutionContext::<&'static str>::default();

        assert!(matches!(
            run_expression(
                &mut context,
                &Expression::parse(
                    "new_cycle(struct Circle { center = [1mm, 2mm], radius = 3mm })"
                )
                .unwrap()
                .1,
            ),
            Ok(Value::Cycle(_))
        ));
    }

    #[test]
    fn construct_polygon() {
        let mut context = ExecutionContext::<&'static str>::default();

        assert!(matches!(
            run_expression(
                &mut context,
                &Expression::parse(
                    "new_cycle(struct Polygon { points = [[0m, 0m], [0m, 1m], [1m, 1m], [1m, 0m]] })"
                )
                .unwrap()
                .1,
            ),
            Ok(Value::Cycle(_))
        ));
    }
}
