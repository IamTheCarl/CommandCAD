use std::rc::Rc;

use fj::math::Point;
use parsing::Span;

use crate::script::{
    execution::{
        types::{List, OperatorResult, StructDefinition, Structure},
        ExecutionContext, Failure,
    },
    parsing::{self, MemberVariable, VariableType},
};

use super::{point_from_list, unpack_dynamic_length_list};

pub fn register_globals<S: Span>(context: &mut ExecutionContext<'_, S>) {
    context.stack.new_variable_str(
        "Polygon",
        StructDefinition {
            definition: Box::leak(Box::new(parsing::Struct {
                name: S::from_str("Polygon"),
                members: vec![MemberVariable {
                    name: S::from_str("points"),
                    ty: VariableType::List,
                    constraints: None,
                    default_value: None,
                }],
            })),
        }
        .into(),
    );
}

/// Unwraps a structure to be made into a polygon (assumes you have already checked that the structure is a polygon type)
pub fn unwrap_polygon<S: Span>(
    context: &ExecutionContext<S>,
    span: &S,
    polygon: Structure<S>,
) -> OperatorResult<S, Vec<Point<2>>> {
    let mut members = Rc::unwrap_or_clone(polygon.members);
    let provided_points = members
        .remove("points")
        .unwrap()
        .downcast::<List<S>>(span)?;
    let mut fornjot_points = Vec::with_capacity(provided_points.len());

    for (index, point) in
        unpack_dynamic_length_list::<S, List<S>>(span, provided_points)?.enumerate()
    {
        let point = point_from_list::<S, 2>(
            span,
            context.global_resources.convert_to_fornjot_units,
            point,
        )
        .map_err(|failure| Failure::ListElementFailure(span.clone(), index, Box::new(failure)))?;

        fornjot_points.push(point);
    }

    Ok(fornjot_points)
}
