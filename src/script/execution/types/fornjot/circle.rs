use std::rc::Rc;

use fj::math::{Point, Scalar};
use parsing::Span;

use crate::script::{
    execution::{
        types::{List, Measurement, OperatorResult, StructDefinition, Structure},
        ExecutionContext,
    },
    parsing::{self, MemberVariable, VariableType},
};

use super::{point_from_list, scalar_from_measurement};

pub fn register_globals<S: Span>(context: &mut ExecutionContext<'_, S>) {
    context.stack.new_variable_str(
        "Circle",
        StructDefinition {
            definition: Box::leak(Box::new(parsing::Struct {
                name: S::from_str("Circle"),
                members: vec![
                    MemberVariable {
                        name: S::from_str("center"),
                        ty: VariableType::List,
                        constraints: None,
                        default_value: None,
                    },
                    MemberVariable {
                        name: S::from_str("radius"),
                        ty: VariableType::Measurement(S::from_str("Length")),
                        constraints: None,
                        default_value: None,
                    },
                ],
            })),
        }
        .into(),
    );
}

/// Unwraps a structure to be made into a circle (assumes you have already checked that the structure is a circle type)
pub fn unwrap_circle<S: Span>(
    context: &ExecutionContext<S>,
    span: &S,
    circle: Structure<S>,
) -> OperatorResult<S, (Point<2>, Scalar)> {
    let mut members = Rc::unwrap_or_clone(circle.members);
    let center = members.remove("center").unwrap();
    let center = center.downcast::<List<S>>(span)?;
    let center = point_from_list::<S, 2>(
        span,
        context.global_resources.convert_to_fornjot_units,
        center,
    )?;

    let radius = members.remove("radius").unwrap();
    let radius = radius.downcast::<Measurement>(span)?;
    let radius = scalar_from_measurement(
        span,
        context.global_resources.convert_to_fornjot_units,
        &radius,
    )?;

    Ok((center, radius))
}
