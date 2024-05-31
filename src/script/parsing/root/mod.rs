/*
 * Copyright 2024 James Carl
 * AGPL-3.0-only or AGPL-3.0-or-later
 *
 * This file is part of Command Cad.
 *
 * Command CAD is free software: you can redistribute it and/or modify it under the terms of
 * the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License along with this
 * program. If not, see <https://www.gnu.org/licenses/>.
 */

use nom::{
    branch::alt,
    combinator::{all_consuming, map},
    multi::many0,
    sequence::delimited,
};

pub use self::import::Import;

use super::{space0, Function, Span, StructDefinition, VResult};

mod import;

#[derive(Debug, Eq, PartialEq)]
pub struct FileAST<S: Span> {
    pub root_elements: Vec<RootElement<S>>,
}

impl<S: Span> FileAST<S> {
    pub fn parse(input: S) -> VResult<S, Self> {
        all_consuming(map(
            many0(delimited(space0, RootElement::parse, space0)),
            |root_elements| FileAST { root_elements },
        ))(input)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum RootElement<S: Span> {
    Import(Import<S>),
    Struct(StructDefinition<S>),
    Function(Function<S>),
}

impl<S: Span> RootElement<S> {
    fn parse(input: S) -> VResult<S, Self> {
        alt((
            map(Import::parse, Self::Import),
            map(StructDefinition::parse, Self::Struct),
            map(Function::parse_as_sketch, Self::Function),
            map(Function::parse_as_solid, Self::Function),
            map(Function::parse_as_task, Self::Function),
            map(Function::parse_as_function, Self::Function),
        ))(input)
    }
}

#[cfg(test)]
mod test {

    use crate::script::parsing::{
        blocks::CallableBlock, Block, FunctionSignature, NamedBlock, VariableType,
    };

    use super::*;

    #[test]
    fn root_element() {
        // Import(Import<S>),
        assert_eq!(
            RootElement::parse("import path::to::module;"),
            Ok((
                "",
                RootElement::Import(Import {
                    path: vec!["path", "to", "module"],
                    external: false
                })
            ))
        );

        // Struct(Struct<S>),
        assert_eq!(
            RootElement::parse("struct MyStruct {}"),
            Ok((
                "",
                RootElement::Struct(StructDefinition {
                    name: "MyStruct",
                    members: vec![]
                })
            ))
        );

        // Sketch(Sketch<S>),
        assert_eq!(
            RootElement::parse("sketch my_sketch() {}"),
            Ok((
                "",
                RootElement::Function(Function {
                    starting_span: "sketch",
                    named_block: NamedBlock {
                        name: "my_sketch",
                        callable: CallableBlock {
                            parameter_span: "(",
                            parameters: vec![],
                            block: Block { statements: vec![] }
                        }
                    },
                    signature: FunctionSignature::Sketch { arguments: vec![] },
                })
            ))
        );

        // Solid(Solid<S>),
        assert_eq!(
            RootElement::parse("solid my_solid() {}"),
            Ok((
                "",
                RootElement::Function(Function {
                    starting_span: "solid",
                    named_block: NamedBlock {
                        name: "my_solid",
                        callable: CallableBlock {
                            parameter_span: "(",
                            parameters: vec![],
                            block: Block { statements: vec![] }
                        }
                    },
                    signature: FunctionSignature::Solid { arguments: vec![] },
                })
            ))
        );

        // Function(NamedBlock<S>),
        assert_eq!(
            RootElement::parse("function my_function() -> Length {}"),
            Ok((
                "",
                RootElement::Function(Function {
                    starting_span: "function",
                    named_block: NamedBlock {
                        name: "my_function",
                        callable: CallableBlock {
                            parameter_span: "(",
                            parameters: vec![],
                            block: Block { statements: vec![] }
                        }
                    },
                    signature: FunctionSignature::Function {
                        return_type: Box::new(VariableType::Scalar("Length")),
                        arguments: vec![]
                    },
                })
            ))
        );
    }

    #[test]
    fn import() {
        assert_eq!(
            Import::parse("import path::to::module;"),
            Ok((
                "",
                Import {
                    path: vec!["path", "to", "module"],
                    external: false
                }
            ))
        );
        assert_eq!(
            Import::parse("import extern path::to::module;"),
            Ok((
                "",
                Import {
                    path: vec!["path", "to", "module"],
                    external: true
                }
            ))
        );
    }

    #[test]
    fn file_ast() {
        assert_eq!(
            FileAST::parse(
                r#"
            /// My Struct
            struct MyStruct {}
    
            /* My Sketch */
            sketch my_sketch() {}
            solid my_solid() {}
            function my_function() -> Length {}
            task my_task() -> Length {}
"#
            ),
            Ok((
                "",
                FileAST {
                    root_elements: vec![
                        RootElement::Struct(StructDefinition {
                            name: "MyStruct",
                            members: vec![]
                        }),
                        RootElement::Function(Function {
                            starting_span: "sketch",
                            named_block: NamedBlock {
                                name: "my_sketch",
                                callable: CallableBlock {
                                    parameter_span: "(",
                                    parameters: vec![],
                                    block: Block { statements: vec![] }
                                }
                            },
                            signature: FunctionSignature::Sketch { arguments: vec![] },
                        }),
                        RootElement::Function(Function {
                            starting_span: "solid",
                            named_block: NamedBlock {
                                name: "my_solid",
                                callable: CallableBlock {
                                    parameter_span: "(",
                                    parameters: vec![],
                                    block: Block { statements: vec![] }
                                }
                            },
                            signature: FunctionSignature::Solid { arguments: vec![] },
                        }),
                        RootElement::Function(Function {
                            starting_span: "function",
                            named_block: NamedBlock {
                                name: "my_function",
                                callable: CallableBlock {
                                    parameter_span: "(",
                                    parameters: vec![],
                                    block: Block { statements: vec![] }
                                }
                            },
                            signature: FunctionSignature::Function {
                                return_type: Box::new(VariableType::Scalar("Length")),
                                arguments: vec![]
                            },
                        }),
                        RootElement::Function(Function {
                            starting_span: "task",
                            named_block: NamedBlock {
                                name: "my_task",
                                callable: CallableBlock {
                                    parameter_span: "(",
                                    parameters: vec![],
                                    block: Block { statements: vec![] }
                                }
                            },
                            signature: FunctionSignature::Task {
                                return_type: Box::new(VariableType::Scalar("Length")),
                                arguments: vec![]
                            },
                        }),
                    ]
                }
            ))
        );
    }
}
