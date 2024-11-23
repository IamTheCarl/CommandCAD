use nodes::{Boolean, List, SourceFile};
use type_sitter::{IncorrectKind, Node, NodeResult};

mod nodes {
    include!(concat!(env!("OUT_DIR"), "/nodes.rs"));
}

use nodes::anon_unions::{
    BinaryExpression_Boolean_ClosureDefinition_Default_DictionaryConstruction_If_List_Parenthesis_Path_ProceduralBlock_Scalar_SignedInteger_String_StructDefinition_UnaryExpression_UnsignedInteger_Void as EType,
    False_True as BooleanValue,
};

pub enum Expression {
    BinaryExpression(()),
    Boolean(bool),
    ClosureDefinition(()),
    Default,
    DictionaryConstruction(()),
    If(()),
    List(Vec<Expression>),
    Parenthesis(Box<Expression>),
    Path(()),
    ProceduralBlock(()),
    Scalar(()),
    SignedInteger(i64),
    String(String),
    StructDefinition(()),
    UnaryExpression(()),
    UnsignedInteger(u64),
    Void,
}

impl<'t> TryFrom<Boolean<'t>> for Expression {
    type Error = IncorrectKind<'t>;

    fn try_from(value: Boolean<'t>) -> Result<Self, Self::Error> {
        Ok(Self::Boolean(matches!(
            value.child()?,
            BooleanValue::True(_)
        )))
    }
}

impl<'t> TryFrom<List<'t>> for Expression {
    type Error = IncorrectKind<'t>;

    fn try_from(value: List<'t>) -> Result<Self, Self::Error> {
        let mut cursor = value.walk();

        let mut list = Vec::new();
        for expression in value.expressions(&mut cursor) {
            let expression = expression?;
            let expression = Self::try_from(expression)?;
            list.push(expression);
        }

        Ok(Self::List(list))
    }
}
impl<'t> TryFrom<nodes::Parenthesis<'t>> for Expression {
    type Error = IncorrectKind<'t>;

    fn try_from(value: nodes::Parenthesis<'t>) -> Result<Self, Self::Error> {
        let expression = value.expression()?;
        Self::try_from(expression)
    }
}

impl<'t> TryFrom<nodes::Expression<'t>> for Expression {
    type Error = IncorrectKind<'t>;

    fn try_from(value: nodes::Expression<'t>) -> Result<Self, Self::Error> {
        match value.child()? {
            EType::BinaryExpression(binary_expression) => todo!(),
            EType::Boolean(value) => Self::try_from(value),
            EType::ClosureDefinition(closure_definition) => todo!(),
            EType::Default(_) => Ok(Self::Default),
            EType::DictionaryConstruction(dictionary_construction) => todo!(),
            EType::If(_) => todo!(),
            EType::List(list) => Self::try_from(list),
            EType::Parenthesis(parenthesis) => Self::try_from(parenthesis),
            EType::Path(path) => todo!(),
            EType::ProceduralBlock(procedural_block) => todo!(),
            EType::Scalar(scalar) => todo!(),
            EType::SignedInteger(signed_integer) => todo!(),
            EType::String(_) => todo!(),
            EType::StructDefinition(struct_definition) => todo!(),
            EType::UnaryExpression(unary_expression) => todo!(),
            EType::UnsignedInteger(unsigned_integer) => todo!(),
            EType::Void(void) => Ok(Self::Void),
        }
    }
}

pub fn new() -> type_sitter::Parser<nodes::SourceFile<'static>> {
    type_sitter::Parser::new(&tree_sitter_command_cad_model::language())
        .expect("Error loading CommandCadModel grammar")
}

pub fn compile<'t>(tree: &'t type_sitter::Tree<SourceFile<'static>>) -> NodeResult<'t, Expression> {
    let root = tree.root_node()?;

    Expression::try_from(root.expression()?)
}
