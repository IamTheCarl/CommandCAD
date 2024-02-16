use crate::script::{parsing::VariableType, Span};

use super::{NamedObject, Object};

#[derive(Debug, Clone, PartialEq)]
pub struct DefaultValue;

impl<S: Span> Object<'_, S> for DefaultValue {
    fn matches_type(&self, _ty: &VariableType<S>) -> bool {
        false
    }
}

impl NamedObject for DefaultValue {
    fn static_type_name() -> &'static str {
        "Default"
    }
}
