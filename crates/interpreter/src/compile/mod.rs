mod expressions;
mod statements;

use std::{path::PathBuf, sync::Arc};
use type_sitter::{IncorrectKind, Node};

pub use expressions::*;
pub use statements::*;

mod nodes {
    include!(concat!(env!("OUT_DIR"), "/nodes.rs"));
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct AstNode<N> {
    reference: SourceReference,
    pub node: N,
}

impl<N> AstNode<N> {
    fn new<'n>(file: &Arc<PathBuf>, value: &impl Node<'n>, node: N) -> Self {
        Self {
            reference: SourceReference {
                file: file.clone(),
                range: value.range(),
            },
            node,
        }
    }

    fn into_box(self) -> AstNode<Box<N>> {
        AstNode {
            reference: self.reference,
            node: Box::new(self.node),
        }
    }
}

trait Parse<'t, N: 't>: Sized {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: N,
    ) -> Result<AstNode<Self>, Error<'t, 'i>>;
}

impl<'t, N: Node<'t> + 't> Parse<'t, N> for String {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: N,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let text = &input[value.byte_range()];
        Ok(AstNode::new(file, &value, text.to_string()))
    }
}

/// Represents an error while compiling.
/// I would have preferred to use the `thiserror` crate, but
/// the non static lifetime of IncorrectKind ruined any opportunity of that.
#[derive(Debug, Eq, PartialEq)]
pub enum Error<'t, 'i> {
    IncorrectKind(IncorrectKind<'t>),
    InvalidUnit(InvalidUnitError<'t, 'i>),
    ParseInt(ParseIntError<'t>),
}

impl<'t> From<IncorrectKind<'t>> for Error<'t, '_> {
    fn from(value: IncorrectKind<'t>) -> Self {
        Self::IncorrectKind(value)
    }
}

impl<'t> From<ParseIntError<'t>> for Error<'t, '_> {
    fn from(value: ParseIntError<'t>) -> Self {
        Self::ParseInt(value)
    }
}

/// Compiles a full document. Panics if there are any issues.
/// This is for testing only.
pub(crate) fn full_compile(file: impl Into<PathBuf>, input: &str) -> AstNode<Expression> {
    let test_file = Arc::new(file.into());
    let mut tree = new_parser();
    let root = tree.parse(input, None).unwrap();
    crate::compile(&test_file, input, &root).unwrap()
}
