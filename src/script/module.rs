use std::collections::HashMap;

use anyhow::{anyhow, Result};

use super::{execution::Failure, parsing};
use parsing::Span;

const RESERVED_KEYWORDS: &[&str] = &[
    "let", "import", "extern", "struct", "sketch", "widget", "function", "return", "if", "else",
    "match", "for", "while", "loop", "in", "break", "continue", "struct", "true", "false",
    "default", "self",
];

fn is_reserved_keyword<S: Span>(word: &S) -> Option<&'static str> {
    RESERVED_KEYWORDS
        .iter()
        .find(|keyword| word.as_str() == **keyword)
        .copied()
}

#[derive(Debug, Eq, PartialEq)]
enum CallableReference {
    Struct(usize),
    Function(usize),
    Sketch(usize),
    Widget(usize),
}

#[derive(Debug, Eq, PartialEq)]
pub struct Module<S: Span> {
    pub(super) file_name: String,
    pub(super) root_elements: RootElements<S>,
}

impl<S: Span> Module<S> {
    pub fn load(
        log: &mut Vec<Failure<S>>,
        file_name: impl Into<String>,
        code: impl Into<S>,
    ) -> Result<Self> {
        let file_name = file_name.into();
        let code = code.into();

        let root_elements = Self::load_ast(code)?;
        let mut callables = HashMap::new();

        // TODO Validation should have its own message types and not be considered a runtime failure.
        // It should probably be moved into parsing, whenever the parser overhaul happens.
        Self::check_for_name_conflicts(
            log,
            &root_elements,
            &mut callables,
            || root_elements.structs.iter(),
            |structure| structure.name.clone(),
            |index| root_elements.structs[index].name.clone(),
            CallableReference::Struct,
        );
        Self::check_for_name_conflicts(
            log,
            &root_elements,
            &mut callables,
            || root_elements.functions.iter(),
            |function| function.named_block.name.clone(),
            |index| root_elements.functions[index].named_block.name.clone(),
            CallableReference::Function,
        );
        Self::check_for_name_conflicts(
            log,
            &root_elements,
            &mut callables,
            || root_elements.sketches.iter(),
            |sketch| sketch.named_block.name.clone(),
            |index| root_elements.sketches[index].named_block.name.clone(),
            CallableReference::Sketch,
        );
        Self::check_for_name_conflicts(
            log,
            &root_elements,
            &mut callables,
            || root_elements.widgets.iter(),
            |widget| widget.named_block.name.clone(),
            |index| root_elements.widgets[index].named_block.name.clone(),
            CallableReference::Widget,
        );

        let module = Self {
            file_name,
            root_elements,
        };

        module.validate(log);

        Ok(module)
    }

    fn load_ast(code: S) -> Result<RootElements<S>> {
        let (_, ast) = parsing::FileAST::parse(code)
            .map_err(|error| anyhow!("Failed to parse file: {:?}", error))?;

        let mut imports = Vec::new();
        let mut structs = Vec::new();
        let mut functions = Vec::new();
        let mut sketches = Vec::new();
        let mut widgets = Vec::new();

        for element in ast.root_elements.into_iter() {
            match element {
                parsing::RootElement::Import(import) => imports.push(import),
                parsing::RootElement::Struct(sstruct) => structs.push(sstruct),
                parsing::RootElement::Sketch(sketch) => sketches.push(sketch),
                parsing::RootElement::Widget(widget) => widgets.push(widget),
                parsing::RootElement::Function(function) => functions.push(function),
            }
        }

        Ok(RootElements {
            imports,
            structs,
            functions,
            sketches,
            widgets,
        })
    }

    fn check_for_name_conflicts<T, I, FI, FN, FS, FBR>(
        log: &mut Vec<Failure<S>>,
        root_elements: &RootElements<S>,
        callables: &mut HashMap<String, CallableReference>,
        get_iter: FI,
        get_name: FN,
        get_span: FS,
        build_reference: FBR,
    ) where
        I: Iterator<Item = T>,
        FI: FnOnce() -> I,
        FN: Fn(&T) -> S,
        FS: Fn(usize) -> S,
        FBR: Fn(usize) -> CallableReference,
    {
        for (index, item) in get_iter().enumerate() {
            if let Some(old) = callables.insert(get_name(&item).to_string(), build_reference(index))
            {
                let old_span = match old {
                    CallableReference::Struct(index) => root_elements.structs[index].name.clone(),
                    CallableReference::Function(index) => {
                        root_elements.functions[index].named_block.name.clone()
                    }
                    CallableReference::Sketch(index) => {
                        root_elements.sketches[index].named_block.name.clone()
                    }
                    CallableReference::Widget(index) => {
                        root_elements.widgets[index].named_block.name.clone()
                    }
                };
                let new_span = get_span(index);

                log.push(Failure::DuplicateGlobal(old_span, new_span));
            }
        }
    }

    /// Validation is checking for errors that don't require the context of execution.
    fn validate(&self, log: &mut Vec<Failure<S>>) {
        self.root_elements.validate(log);
    }
}

#[derive(Debug, Eq, PartialEq)]
pub(super) struct RootElements<S: Span> {
    pub(super) imports: Vec<parsing::Import<S>>,
    pub(super) structs: Vec<parsing::Struct<S>>,
    pub(super) functions: Vec<parsing::Function<S>>,
    pub(super) sketches: Vec<parsing::Sketch<S>>,
    pub(super) widgets: Vec<parsing::Widget<S>>,
}

impl<S: Span> RootElements<S> {
    /// Validation is checking for errors that don't require the context of execution.1
    fn validate(&self, log: &mut Vec<Failure<S>>) {
        // There is no validation to be done for imports.

        for structure in self.structs.iter() {
            Self::validate_struct(log, structure);
        }
        for function in self.functions.iter() {
            Self::validate_named_block(log, &function.named_block);
        }
        for sketch in self.sketches.iter() {
            Self::validate_named_block(log, &sketch.named_block);
        }
        for widget in self.widgets.iter() {
            Self::validate_named_block(log, &widget.named_block);
        }
    }

    fn validate_struct(log: &mut Vec<Failure<S>>, structure: &parsing::Struct<S>) {
        // Name should not be a reserved keyword.
        if let Some(keyword) = is_reserved_keyword(&structure.name) {
            log.push(Failure::ReservedKeyword(structure.name.clone(), keyword));
        }

        // None of the members should have a reserved keyword for a name.
        for member in structure.members.iter() {
            if let Some(keyword) = is_reserved_keyword(&member.name) {
                log.push(Failure::ReservedKeyword(member.name.clone(), keyword));
            }
        }
    }

    fn validate_named_block(log: &mut Vec<Failure<S>>, block: &parsing::NamedBlock<S>) {
        // Name should not be a reserved keyword.
        if let Some(keyword) = is_reserved_keyword(&block.name) {
            log.push(Failure::ReservedKeyword(block.name.clone(), keyword));
        }

        // Parameter names should not be reserved keywords.
        for parameter in block.parameters.iter() {
            if let Some(keyword) = is_reserved_keyword(&parameter.name) {
                log.push(Failure::ReservedKeyword(parameter.name.clone(), keyword));
            }
        }

        Self::validate_block(log, &block.block);
    }

    fn validate_block(log: &mut Vec<Failure<S>>, block: &parsing::Block<S>) {
        let mut core_iter = block.statements.iter().peekable();
        let statement_iter = std::iter::from_fn(|| {
            core_iter
                .next()
                .map(|next| (next, core_iter.peek().is_none()))
        });

        for (statement, is_last) in statement_iter {
            // Variables should not contain keywords.
            if let Some(parsing::Statement::Assign(assignment)) = statement.get() {
                for to_assign in assignment.to_assign.iter() {
                    if let Some(keyword) = is_reserved_keyword(&to_assign.name) {
                        log.push(Failure::ReservedKeyword(to_assign.name.clone(), keyword));
                    }
                }
            }

            // Some statements require that they have a semicolon behind them, unless they are the last line.
            // Enforce that.
            if let parsing::BlockStatement::Open(statement) = statement {
                if !is_last {
                    if let Some(span) = match statement {
                        parsing::Statement::Expression(spanable) => Some(spanable.get_span()),
                        parsing::Statement::Assign(spanable) => Some(spanable.get_span()),
                        parsing::Statement::Return(spanable) => Some(spanable.get_span()),
                        parsing::Statement::Break(spanable) => Some(spanable.get_span()),
                        parsing::Statement::Continue(spanable) => Some(spanable.get_span()),

                        _ => None,
                    } {
                        log.push(Failure::UnclosedStatement(span.clone()));
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const TEST_AST_CODE: &str = r#"
            import path::to::module;
            struct MyStruct {}
            sketch my_sketch() {}
            widget my_widget() {}
            function my_function() -> Length {}
"#;

    #[test]
    fn load_module() {
        let mut log = Vec::new();
        assert!(Module::<&str>::load(&mut log, "my_module.ccm", "").is_ok());
    }

    #[test]
    fn load_ast() {
        let root: RootElements<&str> = Module::load_ast(TEST_AST_CODE).unwrap();

        assert_eq!(
            root.imports,
            [parsing::Import {
                path: vec!["path", "to", "module"],
                external: false
            }]
        );

        assert_eq!(
            root.structs,
            [parsing::Struct {
                starting_span: "struct",
                name: "MyStruct",
                members: vec![]
            }]
        );
        assert_eq!(
            root.sketches,
            [parsing::Sketch {
                starting_span: "sketch",
                named_block: parsing::NamedBlock {
                    name: "my_sketch",
                    parameter_span: "(",
                    parameters: vec![],
                    block: parsing::Block { statements: vec![] }
                },
            }]
        );
        assert_eq!(
            root.widgets,
            [parsing::Widget {
                starting_span: "widget",
                named_block: parsing::NamedBlock {
                    name: "my_widget",
                    parameter_span: "(",
                    parameters: vec![],
                    block: parsing::Block { statements: vec![] }
                },
            }]
        );
        assert_eq!(
            root.functions,
            [parsing::Function {
                starting_span: "function",
                named_block: parsing::NamedBlock {
                    name: "my_function",
                    parameter_span: "(",
                    parameters: vec![],
                    block: parsing::Block { statements: vec![] }
                },
                return_type: parsing::VariableType::Measurement("Length")
            }]
        );
    }

    fn validate(code: &'static str) -> Vec<Failure<&str>> {
        let mut log = Vec::new();
        Module::load(&mut log, "my_module.ccs", code).unwrap(); // Even if validation "fails" we get the validation log back.

        log
    }

    #[test]
    fn validate_block_empty() {
        assert_eq!(validate("widget MyWidget() {}"), []);
    }

    #[test]
    fn validate_block_tail_expressions() {
        assert_eq!(validate("widget MyWidget() { let a = b; }"), []);
        assert_eq!(validate("widget MyWidget() { let a = b }"), []);
        assert_eq!(validate("widget MyWidget() { let a = b; let c = d }"), []);
        assert_eq!(
            validate("widget MyWidget() { let a = b let c = d }"),
            [Failure::UnclosedStatement("let"),]
        );
    }

    #[test]
    fn validate_assignment_keyword_resurvation() {
        assert_eq!(
            validate("widget MyWidget() { let break = b; }"),
            [Failure::ReservedKeyword("break", "break")]
        );

        assert_eq!(validate("widget MyWidget() { let break_beat = b; }"), []);
    }

    #[test]
    fn validate_named_block_keyword_resurvation() {
        assert_eq!(
            validate("widget break() { }"),
            [Failure::ReservedKeyword("break", "break")]
        );
    }

    #[test]
    fn validate_parameter_names_are_not_keywords() {
        assert_eq!(validate("widget MyWidget() { }"), []);

        assert_eq!(validate("widget MyWidget(okay: Length) { }"), []);

        assert_eq!(
            validate("widget MyWidget(break: Length) { }"),
            [Failure::ReservedKeyword("break", "break")]
        );
    }

    #[test]
    fn validate_struct_name_not_keyword() {
        assert_eq!(validate("struct MyStruct { }"), []);
        assert_eq!(
            validate("struct break { }"),
            [Failure::ReservedKeyword("break", "break")]
        );
    }

    #[test]
    fn validate_struct_member_not_keyword() {
        assert_eq!(validate("struct MyStruct { }"), []);
        assert_eq!(validate("struct MyStruct { okay: Length }"), []);
        assert_eq!(
            validate("struct MyStruct { break: Length }"),
            [Failure::ReservedKeyword("break", "break")]
        );
    }

    #[test]
    fn validate_no_duplicate_global_names() {
        assert_eq!(validate("struct MyThing1 {} function MyThing2() -> Length {} sketch MyThing3() {} widget MyThing4() {}"), []);
        assert_eq!(validate("struct MyThing {} function MyThing() -> Length {} sketch MyThing() {} widget MyThing() {}"), [ 
	    Failure::DuplicateGlobal("MyThing", "MyThing"),
	    Failure::DuplicateGlobal("MyThing", "MyThing"),
	    Failure::DuplicateGlobal("MyThing", "MyThing"),
	]);
    }
}