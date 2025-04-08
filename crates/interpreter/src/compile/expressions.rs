use std::{path::PathBuf, sync::Arc};

use common_data_types::{ConversionFactor, Dimension, Float, RawFloat};
use nodes::SourceFile;
use tree_sitter::Range;
use type_sitter::{HasChild, Node};
use unwrap_enum::EnumAs;

use super::{nodes, AstNode, Error, Parse, Statement};

#[derive(Debug, Eq, PartialEq)]
pub struct InvalidUnitError<'t, 'i> {
    pub name: &'i str,
    pub node: nodes::anon_unions::Identifier_UnitQuote<'t>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct ParseIntError<'t> {
    pub error: std::num::ParseIntError,
    pub node: nodes::Integer<'t>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct ParseNumberError<'t> {
    pub error: std::num::ParseIntError,
    pub node: nodes::Number<'t>,
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct SourceReference {
    pub file: Arc<PathBuf>,
    pub range: Range,
}

#[derive(Debug, Hash, Eq, PartialEq, EnumAs)]
pub enum Expression {
    BinaryExpression(AstNode<Box<BinaryExpression>>),
    Boolean(AstNode<bool>),
    ClosureDefinition(AstNode<Box<ClosureDefinition>>),
    Default(AstNode<()>),
    DictionaryConstruction(AstNode<DictionaryConstruction>),
    If(AstNode<IfExpression>),
    List(AstNode<Vec<AstNode<Expression>>>),
    Parenthesis(AstNode<Box<Expression>>),
    Path(AstNode<IdentityPath>),
    ProceduralBlock(AstNode<ProceduralBlock>),
    Scalar(AstNode<Scalar>),
    SignedInteger(AstNode<i64>),
    String(AstNode<String>),
    StructDefinition(AstNode<StructDefinition>),
    UnaryExpression(AstNode<Box<UnaryExpression>>),
    UnsignedInteger(AstNode<u64>),
    Void(AstNode<()>),
}

impl<'t> Parse<'t, nodes::anon_unions::Path_StructDefinition_Void<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::anon_unions::Path_StructDefinition_Void<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        match value {
            nodes::anon_unions::Path_StructDefinition_Void::Path(path) => Ok(AstNode::new(
                file,
                &path,
                Self::Path(IdentityPath::parse(file, input, path)?),
            )),
            nodes::anon_unions::Path_StructDefinition_Void::StructDefinition(struct_definition) => {
                Ok(AstNode::new(
                    file,
                    &struct_definition,
                    Self::StructDefinition(StructDefinition::parse(
                        file,
                        input,
                        struct_definition,
                    )?),
                ))
            }
            nodes::anon_unions::Path_StructDefinition_Void::Void(void) => Ok(AstNode::new(
                file,
                &value,
                Self::Void(AstNode::new(file, &void, ())),
            )),
        }
    }
}

impl<'t> Parse<'t, nodes::BinaryExpression<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::BinaryExpression<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        Ok(AstNode::new(
            file,
            &value,
            Self::BinaryExpression(
                BinaryExpression::parse(file, input, value).map(|o| o.into_box())?,
            ),
        ))
    }
}

impl<'t> Parse<'t, nodes::Boolean<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        _input: &'i str,
        value: nodes::Boolean<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        type ChildType<'t> = <nodes::Boolean<'t> as HasChild<'t>>::Child;
        let child = value.child()?;

        Ok(AstNode::new(
            file,
            &value,
            Self::Boolean(AstNode::new(
                file,
                &child,
                matches!(child, ChildType::True(_)),
            )),
        ))
    }
}

impl<'t> Parse<'t, nodes::ClosureDefinition<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::ClosureDefinition<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        Ok(AstNode::new(
            file,
            &value,
            Self::ClosureDefinition(ClosureDefinition::parse(file, input, value)?.into_box()),
        ))
    }
}

impl<'t> Parse<'t, nodes::DictionaryConstruction<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::DictionaryConstruction<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        Ok(AstNode::new(
            file,
            &value,
            Self::DictionaryConstruction(DictionaryConstruction::parse(file, input, value)?),
        ))
    }
}

impl<'t> Parse<'t, nodes::If<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::If<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        Ok(AstNode::new(
            file,
            &value,
            Self::If(IfExpression::parse(file, input, value)?),
        ))
    }
}

impl<'t> Parse<'t, nodes::List<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::List<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let mut cursor = value.walk();

        let mut list = Vec::new();
        for expression in value.expressions(&mut cursor) {
            let expression = expression?;
            let expression = Self::parse(file, input, expression)?;
            list.push(expression);
        }

        Ok(AstNode::new(
            file,
            &value,
            Self::List(AstNode::new(file, &value, list)),
        ))
    }
}

impl<'t> Parse<'t, nodes::Parenthesis<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::Parenthesis<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let expression = value.expression()?;
        Ok(AstNode::new(
            file,
            &value,
            Self::Parenthesis(Self::parse(file, input, expression).map(|e| e.into_box())?),
        ))
    }
}

impl<'t> Parse<'t, nodes::Path<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::Path<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        Ok(AstNode::new(
            file,
            &value,
            Self::Path(IdentityPath::parse(file, input, value)?),
        ))
    }
}

impl<'t> Parse<'t, nodes::ProceduralBlock<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::ProceduralBlock<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        Ok(AstNode::new(
            file,
            &value,
            Self::ProceduralBlock(ProceduralBlock::parse(file, input, value)?),
        ))
    }
}

impl<'t> Parse<'t, nodes::Scalar<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::Scalar<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        Ok(AstNode::new(
            file,
            &value,
            Self::Scalar(Scalar::parse(file, input, value)?),
        ))
    }
}

impl<'t> Parse<'t, nodes::SignedInteger<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::SignedInteger<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let number = value.value()?;
        let integer = match number.child()? {
            nodes::anon_unions::BaseTen_Binary_Hex_Octal::Hex(value) => {
                let text = &input[value.byte_range()][2..];
                i64::from_str_radix(text, 16)
            }
            nodes::anon_unions::BaseTen_Binary_Hex_Octal::BaseTen(value) => {
                let text = &input[value.byte_range()];
                i64::from_str_radix(text, 10)
            }
            nodes::anon_unions::BaseTen_Binary_Hex_Octal::Octal(value) => {
                let text = &input[value.byte_range()][2..];
                i64::from_str_radix(text, 8)
            }
            nodes::anon_unions::BaseTen_Binary_Hex_Octal::Binary(value) => {
                let text = &input[value.byte_range()][2..];
                i64::from_str_radix(text, 2)
            }
        }
        .map_err(|error| ParseIntError {
            error,
            node: number,
        })?;

        Ok(AstNode::new(
            file,
            &value,
            Self::SignedInteger(AstNode::new(file, &value, integer)),
        ))
    }
}

impl<'t> Parse<'t, nodes::String<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::String<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        static ESCAPE_SEQUENCES: &[(&str, &str)] = &[("\"", "\\\""), ("\n", "\\n"), ("\\", "\\\\")];
        let raw_text = &input[value.byte_range()];
        let raw_text = &raw_text[1..raw_text.len() - 1];

        let mut sequence_iter = ESCAPE_SEQUENCES.iter();
        let (replace, find) = sequence_iter.next().unwrap(); // Should never fail since we static initalized that array.

        let mut processed_text = raw_text.replace(find, replace);

        for (replace, find) in sequence_iter {
            processed_text = processed_text.replace(find, replace);
        }

        Ok(AstNode::new(
            file,
            &value,
            Self::String(AstNode::new(file, &value, processed_text)),
        ))
    }
}

impl<'t> Parse<'t, nodes::StructDefinition<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::StructDefinition<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        Ok(AstNode::new(
            file,
            &value,
            Self::StructDefinition(StructDefinition::parse(file, input, value)?),
        ))
    }
}

impl<'t> Parse<'t, nodes::UnaryExpression<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::UnaryExpression<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        Ok(AstNode::new(
            file,
            &value,
            Self::UnaryExpression(
                UnaryExpression::parse(file, input, value).map(|n| n.into_box())?,
            ),
        ))
    }
}

impl<'t> Parse<'t, nodes::UnsignedInteger<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::UnsignedInteger<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let number = value.value()?;
        let integer = match number.child()? {
            nodes::anon_unions::BaseTen_Binary_Hex_Octal::Hex(value) => {
                let text = &input[value.byte_range()][2..];
                u64::from_str_radix(text, 16)
            }
            nodes::anon_unions::BaseTen_Binary_Hex_Octal::BaseTen(value) => {
                let text = &input[value.byte_range()];
                u64::from_str_radix(text, 10)
            }
            nodes::anon_unions::BaseTen_Binary_Hex_Octal::Octal(value) => {
                let text = &input[value.byte_range()][2..];
                u64::from_str_radix(text, 8)
            }
            nodes::anon_unions::BaseTen_Binary_Hex_Octal::Binary(value) => {
                let text = &input[value.byte_range()][2..];
                u64::from_str_radix(text, 2)
            }
        }
        .map_err(|error| ParseIntError {
            error,
            node: number,
        })?;

        Ok(AstNode::new(
            file,
            &value,
            Self::UnsignedInteger(AstNode::new(file, &value, integer)),
        ))
    }
}

impl<'t> Parse<'t, nodes::Expression<'t>> for Expression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::Expression<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        type ChildType<'t> = <nodes::Expression<'t> as HasChild<'t>>::Child;

        match value.child()? {
            ChildType::BinaryExpression(binary_expression) => {
                Self::parse(file, input, binary_expression)
            }
            ChildType::Boolean(value) => Self::parse(file, input, value),
            ChildType::ClosureDefinition(closure_definition) => {
                Self::parse(file, input, closure_definition)
            }
            ChildType::Default(default) => Ok(AstNode::new(
                file,
                &default,
                Self::Default(AstNode::new(file, &default, ())),
            )),
            ChildType::DictionaryConstruction(dictionary_construction) => {
                Self::parse(file, input, dictionary_construction)
            }
            ChildType::If(if_expression) => Self::parse(file, input, if_expression),
            ChildType::List(list) => Self::parse(file, input, list),
            ChildType::Parenthesis(parenthesis) => Self::parse(file, input, parenthesis),
            ChildType::Path(path) => Self::parse(file, input, path),
            ChildType::ProceduralBlock(procedural_block) => {
                Self::parse(file, input, procedural_block)
            }
            ChildType::Scalar(scalar) => Self::parse(file, input, scalar),
            ChildType::SignedInteger(signed_integer) => Self::parse(file, input, signed_integer),
            ChildType::String(string) => Self::parse(file, input, string),
            ChildType::StructDefinition(struct_definition) => {
                Self::parse(file, input, struct_definition)
            }
            ChildType::UnaryExpression(unary_expression) => {
                Self::parse(file, input, unary_expression)
            }
            ChildType::UnsignedInteger(unsigned_integer) => {
                Self::parse(file, input, unsigned_integer)
            }
            ChildType::Void(void) => Ok(AstNode::new(
                file,
                &value,
                Self::Void(AstNode::new(file, &void, ())),
            )),
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum UnaryExpressionOperation {
    Add,
    Sub,
    Not,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct UnaryExpression {
    pub operation: AstNode<UnaryExpressionOperation>,
    pub expression: AstNode<Expression>,
}

impl<'t> Parse<'t, nodes::UnaryExpression<'t>> for UnaryExpression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::UnaryExpression<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let operation = value.op()?;

        let operation = match operation {
            nodes::anon_unions::Not_Add_Sub::Not(not) => {
                AstNode::new(file, &not, UnaryExpressionOperation::Not)
            }
            nodes::anon_unions::Not_Add_Sub::Add(add) => {
                AstNode::new(file, &add, UnaryExpressionOperation::Add)
            }
            nodes::anon_unions::Not_Add_Sub::Sub(sub) => {
                AstNode::new(file, &sub, UnaryExpressionOperation::Sub)
            }
        };

        let expression = Expression::parse(file, input, value.expression()?)?;

        Ok(AstNode::new(
            file,
            &value,
            Self {
                operation,
                expression,
            },
        ))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum BinaryExpressionOperation {
    NotEq,
    And,
    AndAnd,
    Mul,
    MulMul,
    Add,
    Sub,
    DotDot,
    DotDotEq,
    Div,
    DivDiv,
    Lt,
    LtLt,
    LtEq,
    EqEq,
    Gt,
    GtEq,
    GtGt,
    BitXor,
    Or,
    OrOr,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct BinaryExpression {
    pub operation: AstNode<BinaryExpressionOperation>,
    pub a: AstNode<Expression>,
    pub b: AstNode<Expression>,
}

impl<'t> Parse<'t, nodes::BinaryExpression<'t>> for BinaryExpression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::BinaryExpression<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        use nodes::anon_unions::NotEq_And_AndAnd_Mul_MulMul_Add_Sub_DotDot_DotDotEq_Div_DivDiv_Lt_LtLt_LtEq_EqEq_Gt_GtEq_GtGt_BitXor_Or_OrOr as Operation;

        let operation = value.op()?;

        let operation = match operation {
            Operation::NotEq(not_eq) => {
                AstNode::new(file, &not_eq, BinaryExpressionOperation::NotEq)
            }
            Operation::And(and) => AstNode::new(file, &and, BinaryExpressionOperation::And),
            Operation::AndAnd(and_and) => {
                AstNode::new(file, &and_and, BinaryExpressionOperation::AndAnd)
            }
            Operation::Mul(mul) => AstNode::new(file, &mul, BinaryExpressionOperation::Mul),
            Operation::MulMul(mul_mul) => {
                AstNode::new(file, &mul_mul, BinaryExpressionOperation::MulMul)
            }
            Operation::Add(add) => AstNode::new(file, &add, BinaryExpressionOperation::Add),
            Operation::Sub(sub) => AstNode::new(file, &sub, BinaryExpressionOperation::Sub),
            Operation::DotDot(dot_dot) => {
                AstNode::new(file, &dot_dot, BinaryExpressionOperation::DotDot)
            }
            Operation::DotDotEq(dot_dot_eq) => {
                AstNode::new(file, &dot_dot_eq, BinaryExpressionOperation::DotDotEq)
            }
            Operation::Div(div) => AstNode::new(file, &div, BinaryExpressionOperation::Div),
            Operation::DivDiv(div_div) => {
                AstNode::new(file, &div_div, BinaryExpressionOperation::DivDiv)
            }
            Operation::Lt(lt) => AstNode::new(file, &lt, BinaryExpressionOperation::Lt),
            Operation::LtLt(lt_lt) => AstNode::new(file, &lt_lt, BinaryExpressionOperation::LtLt),
            Operation::LtEq(lt_eq) => AstNode::new(file, &lt_eq, BinaryExpressionOperation::LtEq),
            Operation::EqEq(eq_eq) => AstNode::new(file, &eq_eq, BinaryExpressionOperation::EqEq),
            Operation::Gt(gt) => AstNode::new(file, &gt, BinaryExpressionOperation::Gt),
            Operation::GtEq(gt_eq) => AstNode::new(file, &gt_eq, BinaryExpressionOperation::GtEq),
            Operation::GtGt(gt_gt) => AstNode::new(file, &gt_gt, BinaryExpressionOperation::GtGt),
            Operation::BitXor(bit_xor) => {
                AstNode::new(file, &bit_xor, BinaryExpressionOperation::BitXor)
            }
            Operation::Or(or) => AstNode::new(file, &or, BinaryExpressionOperation::Or),
            Operation::OrOr(or_or) => AstNode::new(file, &or_or, BinaryExpressionOperation::OrOr),
        };

        let a = value.a()?;
        let a = Expression::parse(file, input, a)?;

        let b = value.b()?;
        let b = Expression::parse(file, input, b)?;

        Ok(AstNode::new(file, &value, Self { operation, a, b }))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct IdentityPath {
    pub path: Vec<AstNode<String>>,
}

impl<'t> Parse<'t, nodes::Path<'t>> for IdentityPath {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::Path<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let mut cursor = value.walk();
        let mut path = Vec::new();

        for ident in value.identifiers(&mut cursor) {
            let ident = ident?;
            let text = String::parse(file, input, ident)?;

            path.push(text);
        }

        Ok(AstNode::new(file, &value, Self { path }))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct Scalar {
    pub dimension: Dimension,
    pub value: Float,
}

impl<'t> Parse<'t, nodes::Scalar<'t>> for Scalar {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::Scalar<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        const IDENTITY_CONVERSION_FACTOR: ConversionFactor = ConversionFactor {
            constant: 0.0,
            coefficient: 1.0,
            dimension: Dimension::zero(),
        };

        // Get the conversion factor using the unit name.
        let conversion_factor = if let Some(unit_name) = value.unit() {
            let unit_name = unit_name?;

            let unit_name_str = match unit_name {
                nodes::anon_unions::Identifier_UnitQuote::Identifier(_identifier) => {
                    &input[unit_name.byte_range()]
                }
                nodes::anon_unions::Identifier_UnitQuote::UnitQuote(_unit_quote) => {
                    let original = &input[unit_name.byte_range()];
                    &original[1..original.len() - 1]
                }
            };

            if let Some(conversion_factor) = units::get_conversion_factor(unit_name_str) {
                conversion_factor
            } else {
                // TODO try and list similar names to what they user may have meant.
                // We don't know what this is.
                return Err(Error::InvalidUnit(InvalidUnitError {
                    name: unit_name_str,
                    node: unit_name,
                }));
            }
        } else {
            // If a unit is not specified, we assume the zero dimension and no conversion.
            &IDENTITY_CONVERSION_FACTOR
        };

        let whole_node = value.whole()?;
        let whole = &input[whole_node.byte_range()];
        let whole: u64 = whole.parse().map_err(|error| ParseNumberError {
            error,
            node: whole_node,
        })?;

        let (fraction, fraction_len): (u64, _) = if let Some(fractional_node) = value.fractional() {
            let fraction_str = &input[fractional_node.byte_range()];
            let fraction = fraction_str.parse().map_err(|error| ParseNumberError {
                error,
                node: whole_node,
            })?;

            (fraction, fraction_str.len())
        } else {
            // Faction is not present. We assume zero.
            (0, 1)
        };

        let scale_value: f64 =
            whole as RawFloat + (fraction as RawFloat / (10 as RawFloat).powi(fraction_len as _));
        let scale_value = Float::new(scale_value).expect("Float was NaN");
        let scale_value = conversion_factor.convert_to_base_unit(scale_value);

        let dimension = conversion_factor.dimension;

        Ok(AstNode::new(
            file,
            &value,
            Self {
                dimension,
                value: scale_value,
            },
        ))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct ProceduralBlock {
    pub statements: Vec<AstNode<Statement>>,
}

impl<'t> Parse<'t, nodes::ProceduralBlock<'t>> for ProceduralBlock {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::ProceduralBlock<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let mut cursor = value.walk();
        let mut statements = Vec::new();

        for statement in value.statements(&mut cursor) {
            let statement = statement?;
            let statement = Statement::parse(file, input, statement)?;

            statements.push(statement);
        }

        Ok(AstNode::new(file, &value, Self { statements }))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct IfExpression {
    pub condition: AstNode<Box<Expression>>,
    pub on_true: AstNode<ProceduralBlock>,
    pub on_false: Option<AstNode<ProceduralBlock>>,
}

impl<'t> Parse<'t, nodes::If<'t>> for IfExpression {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::If<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let condition = value.condition()?;
        let condition = Expression::parse(file, input, condition)?.into_box();

        let on_true = ProceduralBlock::parse(file, input, value.on_true()?)?;

        let on_false = if let Some(on_false) = value.on_false() {
            let on_false = on_false?;
            let on_false = ProceduralBlock::parse(file, input, on_false)?;
            Some(on_false)
        } else {
            None
        };

        Ok(AstNode::new(
            file,
            &value,
            IfExpression {
                condition,
                on_true,
                on_false,
            },
        ))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct DictionaryMemberAssignment {
    pub name: AstNode<String>,
    pub assignment: AstNode<Expression>,
}

impl<'t> Parse<'t, nodes::DictionaryMemberAssignment<'t>> for DictionaryMemberAssignment {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::DictionaryMemberAssignment<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let name = value.name()?;
        let name = String::parse(file, input, name)?;
        let assignment = Expression::parse(file, input, value.assignment()?)?;

        Ok(AstNode::new(file, &value, Self { name, assignment }))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct DictionaryConstruction {
    pub assignments: Vec<AstNode<DictionaryMemberAssignment>>,
}

impl<'t> Parse<'t, nodes::DictionaryConstruction<'t>> for DictionaryConstruction {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::DictionaryConstruction<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        dbg!(&value);
        let mut assignments = Vec::new();
        let mut cursor = value.walk();
        let assignments_iter = value.assignmentss(&mut cursor);

        for assignment in assignments_iter {
            dbg!(&assignment);
            let assignment = assignment?;

            // Skip the commas.
            if let Some(assignment) = assignment.as_dictionary_member_assignment() {
                let assignment = DictionaryMemberAssignment::parse(file, input, assignment)?;
                assignments.push(assignment);
            }
        }

        let node = Self { assignments };
        Ok(AstNode::new(file, &value, node))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct ClosureDefinition {
    pub argument: AstNode<Expression>,
    pub captures: Vec<AstNode<String>>,
    pub returns: AstNode<Expression>,
    pub expression: AstNode<Expression>,
}

impl<'t> Parse<'t, nodes::ClosureDefinition<'t>> for ClosureDefinition {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::ClosureDefinition<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let argument = value.argument()?;
        let argument = Expression::parse(file, input, argument)?;

        let captures = value.captured_variables()?;
        let mut cursor = captures.walk();
        let mut identities = Vec::new();

        for identity in captures.identifiers(&mut cursor) {
            let identity = identity?;

            let text = String::parse(file, input, identity)?;

            identities.push(text);
        }

        let returns = value.argument()?;
        let returns = Expression::parse(file, input, returns)?;

        let expression = value.expression()?;
        let expression = Expression::parse(file, input, expression)?;

        Ok(AstNode::new(
            file,
            &value,
            Self {
                argument,
                captures: identities,
                returns,
                expression,
            },
        ))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct StructMember {
    pub name: AstNode<String>,
    pub ty: AstNode<IdentityPath>,
    pub default: Option<AstNode<Expression>>,
}

impl<'t> Parse<'t, nodes::StructMember<'t>> for StructMember {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::StructMember<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let name = value.name()?;
        let name = String::parse(file, input, name)?;

        let ty = value.type_path()?;
        let ty = IdentityPath::parse(file, input, ty)?;

        let default = if let Some(default) = value.default() {
            let default = default?;
            Some(Expression::parse(file, input, default)?)
        } else {
            None
        };

        Ok(AstNode::new(file, &value, Self { name, ty, default }))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct StructDefinition {
    pub members: Vec<AstNode<StructMember>>,
    pub variadic: bool,
}

impl<'t> Parse<'t, nodes::StructDefinition<'t>> for StructDefinition {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: nodes::StructDefinition<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let mut cursor = value.walk();

        let mut members = Vec::new();
        for member in value.memberss(&mut cursor) {
            let member = member?;
            if let nodes::anon_unions::Comma_StructMember::StructMember(member) = member {
                let member = StructMember::parse(file, input, member)?;
                members.push(member);
            }
        }

        let mut cursor = value.walk();

        // There can only ever be one.
        let final_member = value.final_elements(&mut cursor).next();

        let variadic = match final_member {
            Some(member) => {
                let member = member?;
                match member {
                    nodes::anon_unions::Comma_StructMember_VaradicDots::Comma(_comma) => false,
                    nodes::anon_unions::Comma_StructMember_VaradicDots::StructMember(member) => {
                        let member = StructMember::parse(file, input, member)?;
                        members.push(member);
                        false
                    }
                    nodes::anon_unions::Comma_StructMember_VaradicDots::VaradicDots(_) => true,
                }
            }
            None => false,
        };

        Ok(AstNode::new(file, &value, Self { members, variadic }))
    }
}

pub fn new_parser() -> type_sitter::Parser<nodes::SourceFile<'static>> {
    type_sitter::Parser::new(&tree_sitter_command_cad_model::language())
        .expect("Error loading CommandCadModel grammar")
}

pub fn compile<'t, 'i>(
    file: &Arc<PathBuf>,
    input: &'i str,
    tree: &'t type_sitter::Tree<SourceFile<'static>>,
) -> Result<AstNode<Expression>, Error<'t, 'i>> {
    let root = tree.root_node()?;

    Expression::parse(file, input, root.expression()?)
}

#[cfg(test)]
mod test {
    use crate::compile::full_compile;

    use super::*;

    #[test]
    fn binary_expression() {
        let root = full_compile("test_file.ccm", "2i + 3i");

        let binary_expression = root.node.as_binaryexpression().unwrap();
        let operation_reference = binary_expression.node.operation.reference.clone();

        let a = &binary_expression.node.a;
        let a_reference = a.reference.clone();

        let b = &binary_expression.node.b;
        let b_reference = b.reference.clone();

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::BinaryExpression(AstNode {
                    reference: binary_expression.reference.clone(),
                    node: Box::new(BinaryExpression {
                        operation: AstNode {
                            reference: operation_reference,
                            node: BinaryExpressionOperation::Add
                        },
                        a: AstNode {
                            reference: a_reference,
                            node: Expression::SignedInteger(AstNode {
                                reference: a.node.as_signedinteger().unwrap().reference.clone(),
                                node: 2
                            },)
                        },
                        b: AstNode {
                            reference: b_reference,
                            node: Expression::SignedInteger(AstNode {
                                reference: b.node.as_signedinteger().unwrap().reference.clone(),
                                node: 3
                            })
                        }
                    })
                })
            }
        );
    }

    #[test]
    fn boolean_true() {
        let root = full_compile("test_file.ccm", "true");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::Boolean(AstNode {
                    reference: root.node.as_boolean().unwrap().reference.clone(),
                    node: true
                })
            }
        );
    }

    #[test]
    fn boolean_false() {
        let root = full_compile("test_file.ccm", "false");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::Boolean(AstNode {
                    reference: root.node.as_boolean().unwrap().reference.clone(),
                    node: false
                })
            }
        );
    }

    #[test]
    fn closure_definition() {
        let root = full_compile("test_file.ccm", "()[this, that] -> () {}");
        let closure = root.node.as_closuredefinition().unwrap();
        let closure_reference = closure.reference.clone();
        let argument = &closure.node.argument;
        let argument_reference = argument.reference.clone();
        let argument_void_reference = argument.node.as_void().unwrap().reference.clone();

        let captures = &closure.node.captures;
        let this_reference = captures[0].reference.clone();
        let that_reference = captures[1].reference.clone();

        let returns = &closure.node.returns;
        let returns_reference = returns.reference.clone();
        let returns_void_reference = returns.node.as_void().unwrap().reference.clone();

        let expression = &closure.node.expression;
        let expression_reference = expression.reference.clone();
        let block_reference = expression
            .node
            .as_proceduralblock()
            .unwrap()
            .reference
            .clone();

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::ClosureDefinition(AstNode {
                    reference: closure_reference,
                    node: Box::new(ClosureDefinition {
                        argument: AstNode {
                            reference: argument_reference,
                            node: Expression::Void(AstNode {
                                reference: argument_void_reference,
                                node: ()
                            })
                        },
                        captures: vec![
                            AstNode {
                                reference: this_reference,
                                node: "this".to_string()
                            },
                            AstNode {
                                reference: that_reference,
                                node: "that".to_string()
                            }
                        ],
                        returns: AstNode {
                            reference: returns_reference,
                            node: Expression::Void(AstNode {
                                reference: returns_void_reference,
                                node: ()
                            })
                        },
                        expression: AstNode {
                            reference: expression_reference,
                            node: Expression::ProceduralBlock(AstNode {
                                reference: block_reference,
                                node: ProceduralBlock { statements: vec![] }
                            })
                        }
                    })
                })
            }
        );
    }

    #[test]
    fn default() {
        let root = full_compile("test_file.ccm", "default");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::Default(AstNode {
                    reference: root.node.as_default().unwrap().reference.clone(),
                    node: ()
                })
            }
        );
    }

    #[test]
    fn dictionary_construction() {
        let root = full_compile("test_file.ccm", "(a = true, b = false)");
        let construction_expression = root.node.as_dictionaryconstruction().unwrap();
        assert_eq!(construction_expression.node.assignments.len(), 2);
        let a = &construction_expression.node.assignments[0];
        let b = &construction_expression.node.assignments[1];

        assert_eq!(
            a.node.name,
            AstNode {
                reference: a.node.name.reference.clone(),
                node: "a".to_string()
            }
        );
        assert_eq!(
            a.node.assignment,
            AstNode {
                reference: a.node.assignment.reference.clone(),
                node: Expression::Boolean(AstNode {
                    reference: a.node.assignment.reference.clone(),
                    node: true
                })
            }
        );

        assert_eq!(
            b.node.name,
            AstNode {
                reference: b.node.name.reference.clone(),
                node: "b".to_string()
            }
        );
        assert_eq!(
            b.node.assignment,
            AstNode {
                reference: b.node.assignment.reference.clone(),
                node: Expression::Boolean(AstNode {
                    reference: b.node.assignment.reference.clone(),
                    node: false
                })
            }
        );
    }

    #[test]
    fn if_expression() {
        let root = full_compile("test_file.ccm", "if true {}");
        let if_expression = root.node.as_if().unwrap();
        let if_reference = if_expression.reference.clone();
        let condition = &if_expression.node.condition;
        let condition_reference = condition.reference.clone();
        let boolean_reference = condition.node.as_boolean().unwrap().reference.clone();
        let true_block_reference = if_expression.node.on_true.reference.clone();

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::If(AstNode {
                    reference: if_reference,
                    node: IfExpression {
                        condition: AstNode {
                            reference: condition_reference,
                            node: Expression::Boolean(AstNode {
                                reference: boolean_reference,
                                node: true
                            })
                        }
                        .into_box(),
                        on_true: AstNode {
                            reference: true_block_reference,
                            node: ProceduralBlock { statements: vec![] }
                        },
                        on_false: None,
                    }
                })
            }
        );
    }

    #[test]
    fn if_else_expression() {
        let root = full_compile("test_file.ccm", "if true {} else {}");
        let if_expression = root.node.as_if().unwrap();
        let if_reference = if_expression.reference.clone();
        let condition = &if_expression.node.condition;
        let condition_reference = condition.reference.clone();
        let boolean_reference = condition.node.as_boolean().unwrap().reference.clone();
        let true_block_reference = if_expression.node.on_true.reference.clone();
        let false_block_reference = if_expression
            .node
            .on_false
            .as_ref()
            .unwrap()
            .reference
            .clone();

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::If(AstNode {
                    reference: if_reference,
                    node: IfExpression {
                        condition: AstNode {
                            reference: condition_reference,
                            node: Expression::Boolean(AstNode {
                                reference: boolean_reference,
                                node: true
                            })
                        }
                        .into_box(),
                        on_true: AstNode {
                            reference: true_block_reference,
                            node: ProceduralBlock { statements: vec![] }
                        },
                        on_false: Some(AstNode {
                            reference: false_block_reference,
                            node: ProceduralBlock { statements: vec![] }
                        }),
                    }
                })
            }
        );
    }

    #[test]
    fn list() {
        let root = full_compile("test_file.ccm", "[1i, 2i, 3i, 4i, 5i]");

        let list = root.node.as_list().unwrap();
        let list_reference = list.reference.clone();
        let element_1_reference = list.node[0].reference.clone();
        let integer_1_reference = list.node[0]
            .node
            .as_signedinteger()
            .unwrap()
            .reference
            .clone();
        let element_2_reference = list.node[1].reference.clone();
        let integer_2_reference = list.node[1]
            .node
            .as_signedinteger()
            .unwrap()
            .reference
            .clone();
        let element_3_reference = list.node[2].reference.clone();
        let integer_3_reference = list.node[2]
            .node
            .as_signedinteger()
            .unwrap()
            .reference
            .clone();
        let element_4_reference = list.node[3].reference.clone();
        let integer_4_reference = list.node[3]
            .node
            .as_signedinteger()
            .unwrap()
            .reference
            .clone();
        let element_5_reference = list.node[4].reference.clone();
        let integer_5_reference = list.node[4]
            .node
            .as_signedinteger()
            .unwrap()
            .reference
            .clone();

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::List(AstNode {
                    reference: list_reference,
                    node: vec![
                        AstNode {
                            reference: element_1_reference,
                            node: Expression::SignedInteger(AstNode {
                                reference: integer_1_reference,
                                node: 1,
                            })
                        },
                        AstNode {
                            reference: element_2_reference,
                            node: Expression::SignedInteger(AstNode {
                                reference: integer_2_reference,
                                node: 2,
                            })
                        },
                        AstNode {
                            reference: element_3_reference,
                            node: Expression::SignedInteger(AstNode {
                                reference: integer_3_reference,
                                node: 3,
                            })
                        },
                        AstNode {
                            reference: element_4_reference,
                            node: Expression::SignedInteger(AstNode {
                                reference: integer_4_reference,
                                node: 4,
                            })
                        },
                        AstNode {
                            reference: element_5_reference,
                            node: Expression::SignedInteger(AstNode {
                                reference: integer_5_reference,
                                node: 5,
                            })
                        },
                    ]
                })
            }
        );
    }

    #[test]
    fn parenthesis() {
        let root = full_compile("test_file.ccm", "(5i)");
        let parenthesis = root.node.as_parenthesis().unwrap();
        let parenthesis_reference = parenthesis.reference.clone();
        let integer = parenthesis.node.as_signedinteger().unwrap();
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::Parenthesis(
                    AstNode {
                        reference: parenthesis_reference,
                        node: Expression::SignedInteger(AstNode {
                            reference: integer.reference.clone(),
                            node: 5
                        })
                    }
                    .into_box()
                )
            }
        );
    }

    #[test]
    fn local_path() {
        let root = full_compile("test_file.ccm", "this.thang");
        let path = root.node.as_path().unwrap();
        let this = &path.node.path[0];
        let thang = &path.node.path[1];

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::Path(AstNode {
                    reference: path.reference.clone(),
                    node: IdentityPath {
                        path: vec![
                            AstNode {
                                reference: this.reference.clone(),
                                node: "this".into()
                            },
                            AstNode {
                                reference: thang.reference.clone(),
                                node: "thang".into()
                            }
                        ]
                    }
                })
            }
        );
    }

    #[test]
    fn argument_path() {
        let root = full_compile("test_file.ccm", "@.this.thang");
        let path = root.node.as_path().unwrap();
        let this = &path.node.path[0];
        let thang = &path.node.path[1];

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::Path(AstNode {
                    reference: path.reference.clone(),
                    node: IdentityPath {
                        path: vec![
                            AstNode {
                                reference: this.reference.clone(),
                                node: "this".into()
                            },
                            AstNode {
                                reference: thang.reference.clone(),
                                node: "thang".into()
                            }
                        ]
                    }
                })
            }
        );
    }

    #[test]
    fn procedural_block() {
        // An unimpressive test. The more in-depth testing gets done in statements.rs
        let root = full_compile("test_file.ccm", "{}");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::ProceduralBlock(AstNode {
                    reference: root.node.as_proceduralblock().unwrap().reference.clone(),
                    node: ProceduralBlock { statements: vec![] }
                })
            }
        );
    }

    #[test]
    fn scalar_no_decimal() {
        let root = full_compile("test_file.ccm", "0");

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::Scalar(AstNode {
                    reference: root.node.as_scalar().unwrap().reference.clone(),
                    node: Scalar {
                        dimension: Dimension::zero(),
                        value: Float::new(0.0).unwrap()
                    }
                })
            }
        );
    }

    #[test]
    fn scalar_no_unit() {
        let root = full_compile("test_file.ccm", "0.0");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::Scalar(AstNode {
                    reference: root.node.as_scalar().unwrap().reference.clone(),
                    node: Scalar {
                        dimension: Dimension::zero(),
                        value: Float::new(0.0).unwrap()
                    }
                })
            }
        );
    }

    #[test]
    fn scalar_with_unit() {
        let root = full_compile("test_file.ccm", "0.0mm");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::Scalar(AstNode {
                    reference: root.node.as_scalar().unwrap().reference.clone(),
                    node: Scalar {
                        dimension: Dimension::length(),
                        value: Float::new(0.0).unwrap()
                    }
                })
            }
        );
    }

    #[test]
    fn scalar_unit_conversion() {
        // Test conversion factor
        let root = full_compile("test_file.ccm", "1cm");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::Scalar(AstNode {
                    reference: root.node.as_scalar().unwrap().reference.clone(),
                    node: Scalar {
                        dimension: Dimension::length(),
                        value: Float::new(0.01).unwrap()
                    }
                })
            }
        );
    }

    #[test]
    fn scalar_quoted_unit() {
        // Test conversion factor
        let root = full_compile("test_file.ccm", "1'm^2'");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::Scalar(AstNode {
                    reference: root.node.as_scalar().unwrap().reference.clone(),
                    node: Scalar {
                        dimension: Dimension::area(),
                        value: Float::new(1.0).unwrap()
                    }
                })
            }
        );
    }

    #[test]
    fn signed_integer() {
        let root = full_compile("test_file.ccm", "5i");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::SignedInteger(AstNode {
                    reference: root.node.as_signedinteger().unwrap().reference.clone(),
                    node: 5
                })
            }
        );
    }

    #[test]
    fn signed_integer_hex() {
        let root = full_compile("test_file.ccm", "0x5i");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::SignedInteger(AstNode {
                    reference: root.node.as_signedinteger().unwrap().reference.clone(),
                    node: 0x5
                })
            }
        );
    }

    #[test]
    fn signed_integer_octal() {
        let root = full_compile("test_file.ccm", "0o5i");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::SignedInteger(AstNode {
                    reference: root.node.as_signedinteger().unwrap().reference.clone(),
                    node: 0o5
                })
            }
        );
    }

    #[test]
    fn signed_integer_binary() {
        let root = full_compile("test_file.ccm", "0b1010i");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::SignedInteger(AstNode {
                    reference: root.node.as_signedinteger().unwrap().reference.clone(),
                    node: 0b1010
                })
            }
        );
    }

    #[test]
    fn string() {
        let root = full_compile("test_file.ccm", "\"Some text\\n\"");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::String(AstNode {
                    reference: root.node.as_string().unwrap().reference.clone(),
                    node: "Some text\n".into()
                })
            }
        );
    }

    #[test]
    fn struct_definition() {
        let root = full_compile(
            "test_file.ccm",
            "( one: std.Constraint, two: std.Constraint = a, ... )",
        );
        let struct_definition = root.node.as_structdefinition().unwrap();
        let members = &struct_definition.node.members;
        let one = &members[0];
        let two = &members[1];
        let two_default = two.node.default.as_ref().unwrap();
        let two_default_path = two_default.node.as_path().unwrap();

        assert!(struct_definition.node.variadic);
        assert_eq!(members.len(), 2);
        assert_eq!(two_default_path.node.path.len(), 1);

        assert_eq!(
            *one,
            AstNode {
                reference: one.reference.clone(),
                node: StructMember {
                    name: AstNode {
                        reference: one.node.name.reference.clone(),
                        node: "one".into()
                    },
                    ty: AstNode {
                        reference: one.node.ty.reference.clone(),
                        node: IdentityPath {
                            path: vec![
                                AstNode {
                                    reference: one.node.ty.node.path[0].reference.clone(),
                                    node: "std".into(),
                                },
                                AstNode {
                                    reference: one.node.ty.node.path[1].reference.clone(),
                                    node: "Constraint".into(),
                                }
                            ]
                        }
                    },
                    default: None
                }
            }
        );
        assert_eq!(
            *two,
            AstNode {
                reference: two.reference.clone(),
                node: StructMember {
                    name: AstNode {
                        reference: two.node.name.reference.clone(),
                        node: "two".into()
                    },
                    ty: AstNode {
                        reference: two.node.ty.reference.clone(),
                        node: IdentityPath {
                            path: vec![
                                AstNode {
                                    reference: two.node.ty.node.path[0].reference.clone(),
                                    node: "std".into(),
                                },
                                AstNode {
                                    reference: two.node.ty.node.path[1].reference.clone(),
                                    node: "Constraint".into(),
                                }
                            ]
                        }
                    },
                    default: Some(AstNode {
                        reference: two_default.reference.clone(),
                        node: Expression::Path(AstNode {
                            reference: two_default_path.reference.clone(),
                            node: IdentityPath {
                                path: vec![AstNode {
                                    reference: two_default_path.node.path[0].reference.clone(),
                                    node: "a".into()
                                }]
                            }
                        })
                    })
                }
            }
        );
    }

    #[test]
    fn unary_expression() {
        let root = full_compile("test_file.ccm", "-5i");
        let unary_expression = root.node.as_unaryexpression().unwrap();
        let unary_expression_reference = unary_expression.reference.clone();
        let expression_reference = unary_expression.node.expression.reference.clone();
        let integer_reference = unary_expression
            .node
            .expression
            .node
            .as_signedinteger()
            .unwrap()
            .reference
            .clone();
        let operation_reference = unary_expression.node.operation.reference.clone();

        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::UnaryExpression(AstNode {
                    reference: unary_expression_reference,
                    node: Box::new(UnaryExpression {
                        expression: AstNode {
                            reference: expression_reference,
                            node: Expression::SignedInteger(AstNode {
                                reference: integer_reference,
                                node: 5
                            })
                        },
                        operation: AstNode {
                            reference: operation_reference,
                            node: UnaryExpressionOperation::Sub
                        }
                    })
                })
            }
        );
    }

    #[test]
    fn unsigned_integer() {
        let root = full_compile("test_file.ccm", "5u");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::UnsignedInteger(AstNode {
                    reference: root.node.as_unsignedinteger().unwrap().reference.clone(),
                    node: 5
                })
            }
        );
    }

    #[test]
    fn unsigned_integer_hex() {
        let root = full_compile("test_file.ccm", "0x5u");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::UnsignedInteger(AstNode {
                    reference: root.node.as_unsignedinteger().unwrap().reference.clone(),
                    node: 0x5
                })
            }
        );
    }

    #[test]
    fn unsigned_integer_octal() {
        let root = full_compile("test_file.ccm", "0o5u");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::UnsignedInteger(AstNode {
                    reference: root.node.as_unsignedinteger().unwrap().reference.clone(),
                    node: 0o5
                })
            }
        );
    }

    #[test]
    fn unsigned_integer_binary() {
        let root = full_compile("test_file.ccm", "0b1010u");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::UnsignedInteger(AstNode {
                    reference: root.node.as_unsignedinteger().unwrap().reference.clone(),
                    node: 0b1010
                })
            }
        );
    }

    #[test]
    fn void() {
        let root = full_compile("test_file.ccm", "()");
        assert_eq!(
            root,
            AstNode {
                reference: root.reference.clone(),
                node: Expression::Void(AstNode {
                    reference: root.node.as_void().unwrap().reference.clone(),
                    node: ()
                })
            }
        );
    }
}
