use crate::script::{
    execution::{expressions::run_trailer, ControlFlow, ExecutionContext, ExecutionResult},
    parsing::{self, VariableType},
    LogMessage, RuntimeLog, Span,
};

use super::{NamedObject, Number, Object, Value};

#[derive(Debug, Clone, PartialEq)]
pub struct Range {
    pub lower_bound: Option<Number>,
    pub upper_bound_is_inclusive: bool,
    pub upper_bound: Option<Number>,
}

impl<'a, S: Span> Object<'a, S> for Range {
    fn matches_type(&self, ty: &VariableType<S>) -> bool {
        matches!(ty, VariableType::Range)
    }

    fn iterate(
        &self,
        log: &mut RuntimeLog<S>,
        span: &S,
    ) -> ExecutionResult<'a, S, Box<dyn Iterator<Item = Value<'a, S>> + '_>> {
        match (
            self.lower_bound,
            self.upper_bound,
            self.upper_bound_is_inclusive,
        ) {
            (None, None, false) => {
                log.push(LogMessage::CannotConvertFromTo(
                    span.clone(),
                    "..".into(),
                    "iterator".into(),
                ));
                Err(ControlFlow::Failure)
            }
            (Some(_lower_bound), None, false) => {
                log.push(LogMessage::CannotConvertFromTo(
                    span.clone(),
                    "lower_bound..".into(),
                    "iterator".into(),
                ));
                Err(ControlFlow::Failure)
            }
            (None, Some(_upper_bound), false) => {
                log.push(LogMessage::CannotConvertFromTo(
                    span.clone(),
                    "..upper_bound".into(),
                    "iterator".into(),
                ));
                Err(ControlFlow::Failure)
            }
            (None, Some(_upper_bound), true) => {
                log.push(LogMessage::CannotConvertFromTo(
                    span.clone(),
                    "..=upper_bound".into(),
                    "iterator".into(),
                ));
                Err(ControlFlow::Failure)
            }
            (Some(lower_bound), Some(upper_bound), false) => {
                let lower_bound = lower_bound.trunc() as isize;
                let upper_bound = upper_bound.trunc() as isize;
                Ok(Box::new(
                    (lower_bound..upper_bound).map(|index| Number::new(index as _).unwrap().into()),
                ))
            }
            (Some(lower_bound), Some(upper_bound), true) => {
                let lower_bound = lower_bound.trunc() as isize;
                let upper_bound = upper_bound.trunc() as isize;
                Ok(Box::new(
                    (lower_bound..=upper_bound)
                        .map(|index| Number::new(index as _).unwrap().into()),
                ))
            }
            (_, None, true) => unreachable!(), // Inclusive ranges without an upper bound are illegal to construct.
        }
    }
}

impl Range {
    pub fn from_parsed<'a, S: Span>(
        context: &mut ExecutionContext<'a, S>,
        range: &parsing::Range<S>,
    ) -> ExecutionResult<'a, S, Self> {
        let lower_bound = if let Some(lower_bound) = &range.lower_bound {
            let span = lower_bound.get_span();
            Some(run_trailer(context, lower_bound)?.downcast(&mut context.log, span)?)
        } else {
            None
        };

        let upper_bound_is_inclusive = range.upper_bound_is_inclusive;

        let upper_bound = if let Some(upper_bound) = &range.upper_bound {
            let span = upper_bound.get_span();

            Some(run_trailer(context, upper_bound)?.downcast(&mut context.log, span)?)
        } else {
            None
        };

        if upper_bound_is_inclusive && upper_bound.is_none() {
            context
                .log
                .push(LogMessage::MissingUpperBound(range.get_span().clone()));
            return Err(ControlFlow::Failure);
        }

        Ok(Self {
            lower_bound,
            upper_bound_is_inclusive,
            upper_bound,
        })
    }
}

impl NamedObject for Range {
    fn static_type_name() -> &'static str {
        "Range"
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn iterate() {
        let mut context = ExecutionContext {
            log: Default::default(),
            stack: Default::default(),
        };

        assert!(
            Range::from_parsed(&mut context, &parsing::Range::parse("..").unwrap().1)
                .unwrap()
                .iterate(&mut context.log, &"span")
                .is_err()
        );

        assert!(
            Range::from_parsed(&mut context, &parsing::Range::parse("1..").unwrap().1)
                .unwrap()
                .iterate(&mut context.log, &"span")
                .is_err()
        );

        assert!(
            Range::from_parsed(&mut context, &parsing::Range::parse("..1").unwrap().1)
                .unwrap()
                .iterate(&mut context.log, &"span")
                .is_err()
        );
        assert!(
            Range::from_parsed(&mut context, &parsing::Range::parse("..=1").unwrap().1)
                .unwrap()
                .iterate(&mut context.log, &"span")
                .is_err()
        );
        assert!(
            Range::from_parsed(&mut context, &parsing::Range::parse("..=1").unwrap().1)
                .unwrap()
                .iterate(&mut context.log, &"span")
                .is_err()
        );
        assert_eq!(
            Range::from_parsed(&mut context, &parsing::Range::parse("1..5").unwrap().1)
                .unwrap()
                .iterate(&mut context.log, &"span")
                .unwrap()
                .collect::<Vec<_>>(),
            [
                Number::new(1.0).unwrap().into(),
                Number::new(2.0).unwrap().into(),
                Number::new(3.0).unwrap().into(),
                Number::new(4.0).unwrap().into(),
            ]
        );
        assert_eq!(
            Range::from_parsed(&mut context, &parsing::Range::parse("1..=5").unwrap().1)
                .unwrap()
                .iterate(&mut context.log, &"span")
                .unwrap()
                .collect::<Vec<_>>(),
            [
                Number::new(1.0).unwrap().into(),
                Number::new(2.0).unwrap().into(),
                Number::new(3.0).unwrap().into(),
                Number::new(4.0).unwrap().into(),
                Number::new(5.0).unwrap().into(),
            ]
        );
    }
}
