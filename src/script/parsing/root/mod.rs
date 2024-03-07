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

pub use self::{import::Import, sketch::Sketch, solid::Solid, task::Task};

use super::{space0, Function, Span, StructDefinition, VResult};

mod import;
mod sketch;
mod solid;
mod task;

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
    Sketch(Sketch<S>),
    Solid(Solid<S>),
    Task(Task<S>),
    Function(Function<S>),
}

impl<S: Span> RootElement<S> {
    fn parse(input: S) -> VResult<S, Self> {
        alt((
            map(Import::parse, Self::Import),
            map(StructDefinition::parse, Self::Struct),
            map(Sketch::parse, Self::Sketch),
            map(Solid::parse, Self::Solid),
            map(Task::parse, Self::Task),
            map(Function::parse, Self::Function),
        ))(input)
    }
}

#[cfg(test)]
mod test {

    use std::rc::Rc;

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
                RootElement::Sketch(Sketch {
                    starting_span: "sketch",
                    named_block: NamedBlock {
                        name: "my_sketch",
                        callable: CallableBlock {
                            parameter_span: "(",
                            parameters: vec![],
                            block: Block { statements: vec![] }
                        }
                    },
                    signature: Rc::new(FunctionSignature::Sketch { arguments: vec![] }),
                })
            ))
        );

        // Solid(Solid<S>),
        assert_eq!(
            RootElement::parse("solid my_solid() {}"),
            Ok((
                "",
                RootElement::Solid(Solid {
                    starting_span: "solid",
                    named_block: NamedBlock {
                        name: "my_solid",
                        callable: CallableBlock {
                            parameter_span: "(",
                            parameters: vec![],
                            block: Block { statements: vec![] }
                        }
                    },
                    signature: Rc::new(FunctionSignature::Solid { arguments: vec![] }),
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
                    signature: Rc::new(FunctionSignature::Function {
                        return_type: Box::new(VariableType::Measurement("Length")),
                        arguments: vec![]
                    }),
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
                        RootElement::Sketch(Sketch {
                            starting_span: "sketch",
                            named_block: NamedBlock {
                                name: "my_sketch",
                                callable: CallableBlock {
                                    parameter_span: "(",
                                    parameters: vec![],
                                    block: Block { statements: vec![] }
                                }
                            },
                            signature: Rc::new(FunctionSignature::Sketch { arguments: vec![] }),
                        }),
                        RootElement::Solid(Solid {
                            starting_span: "solid",
                            named_block: NamedBlock {
                                name: "my_solid",
                                callable: CallableBlock {
                                    parameter_span: "(",
                                    parameters: vec![],
                                    block: Block { statements: vec![] }
                                }
                            },
                            signature: Rc::new(FunctionSignature::Solid { arguments: vec![] }),
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
                            signature: Rc::new(FunctionSignature::Function {
                                return_type: Box::new(VariableType::Measurement("Length")),
                                arguments: vec![]
                            }),
                        }),
                        RootElement::Task(Task {
                            starting_span: "task",
                            named_block: NamedBlock {
                                name: "my_task",
                                callable: CallableBlock {
                                    parameter_span: "(",
                                    parameters: vec![],
                                    block: Block { statements: vec![] }
                                }
                            },
                            signature: Rc::new(FunctionSignature::Task {
                                return_type: Box::new(VariableType::Measurement("Length")),
                                arguments: vec![]
                            }),
                        }),
                    ]
                }
            ))
        );
    }
}
