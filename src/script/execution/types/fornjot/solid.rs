use fj::core::{objects::Solid as FornjotSolid, storage::Handle};

use crate::script::{
    execution::{
        types::{NamedObject, Object},
        ExecutionContext,
    },
    parsing::VariableType,
    Span,
};

pub fn register_globals<S: Span>(_context: &mut ExecutionContext<'_, S>) {}

#[derive(Clone)]
pub struct Solid {
    pub handle: Handle<FornjotSolid>,
}

impl<'a, S: Span> Object<'a, S> for Solid {
    fn matches_type(&self, ty: &VariableType<S>) -> bool {
        matches!(ty, VariableType::Sketch)
    }

    // TODO we need a way to get the faces of the solid, and from the faces, get the surfaces.
}

impl NamedObject for Solid {
    fn static_type_name() -> &'static str {
        "Solid"
    }
}

impl From<Handle<FornjotSolid>> for Solid {
    fn from(handle: Handle<FornjotSolid>) -> Self {
        Self { handle }
    }
}

impl PartialEq for Solid {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl std::fmt::Debug for Solid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Solid").finish()
    }
}
