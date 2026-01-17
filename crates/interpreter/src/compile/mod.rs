mod expressions;
mod formula;

use common_data_types::{ConversionFactor, Dimension, Float, RawFloat};
use imstr::ImString;
use std::{path::PathBuf, sync::Arc};
use type_sitter::{HasChild, IncorrectKind, Node};

pub use expressions::*;

pub mod nodes {
    include!(concat!(env!("OUT_DIR"), "/nodes.rs"));
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct AstNode<N> {
    pub reference: SourceReference,
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

impl<'t, N: Node<'t> + 't> Parse<'t, N> for ImString {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        input: &'i str,
        value: N,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        let text = &input[value.byte_range()];
        Ok(AstNode::new(file, &value, ImString::from(text.to_string())))
    }
}

impl<'t> Parse<'t, nodes::Boolean<'t>> for bool {
    fn parse<'i>(
        file: &Arc<PathBuf>,
        _input: &'i str,
        value: nodes::Boolean<'t>,
    ) -> Result<AstNode<Self>, Error<'t, 'i>> {
        type ChildType<'t> = <nodes::Boolean<'t> as HasChild<'t>>::Child;
        let child = value.child()?;

        Ok(AstNode::new(
            file,
            &child,
            matches!(child, ChildType::True(_)),
        ))
    }
}

impl<'t> Parse<'t, nodes::SignedInteger<'t>> for i64 {
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

        Ok(AstNode::new(file, &value, integer))
    }
}

impl<'t> Parse<'t, nodes::UnsignedInteger<'t>> for u64 {
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

        Ok(AstNode::new(file, &value, integer))
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
            whole as RawFloat + (fraction as RawFloat / (10.0 as RawFloat).powi(fraction_len as _));
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

/// Represents an error while compiling.
/// I would have preferred to use the `thiserror` crate, but
/// the non static lifetime of IncorrectKind ruined any opportunity of that.
#[derive(Debug, Eq, PartialEq)]
pub enum Error<'t, 'i> {
    IncorrectKind(IncorrectKind<'t>),
    InvalidUnit(InvalidUnitError<'t, 'i>),
    ParseInt(ParseIntError<'t>),
    ParseNumber(ParseNumberError<'t>),
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

impl<'t> From<ParseNumberError<'t>> for Error<'t, '_> {
    fn from(value: ParseNumberError<'t>) -> Self {
        Self::ParseNumber(value)
    }
}

/// Compiles a full document. Panics if there are any issues.
/// This is for testing only.
#[cfg(test)]
pub(crate) fn full_compile(input: &str) -> AstNode<Expression> {
    let test_file = Arc::new(PathBuf::from("test.ccm"));
    let mut tree = new_parser();
    let root = tree.parse(input, None).unwrap();
    crate::compile(&test_file, input, &root).unwrap()
}
