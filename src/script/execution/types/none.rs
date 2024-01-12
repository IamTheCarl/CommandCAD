use crate::script::{parsing::VariableType, Span};

use super::{NamedObject, Object};

#[derive(Debug, Clone, PartialEq)]
pub struct NoneType;

impl<S: Span> Object<'_, S> for NoneType {
    fn matches_type(&self, _ty: &VariableType<S>) -> bool {
        false
    }
}

impl NamedObject for NoneType {
    fn static_type_name() -> &'static str {
        "None"
    }
}
