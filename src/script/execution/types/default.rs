use crate::script::{parsing::VariableType, Span};

use super::{NamedObject, Object};

#[derive(Debug, Clone, PartialEq)]
pub struct Default;

impl<S: Span> Object<'_, S> for Default {
    fn matches_type(&self, _ty: &VariableType<S>) -> bool {
        false
    }
}

impl NamedObject for Default {
    fn static_type_name() -> &'static str {
        "Default"
    }
}
