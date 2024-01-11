use std::collections::HashMap;

use anyhow::{anyhow, bail, Result};
use nom_locate::LocatedSpan;

use super::parsing;
use parsing::Span;

const RESERVED_KEYWORDS: &[&str] = &[
    "let", "import", "extern", "struct", "sketch", "widget", "function", "return", "if", "else",
    "match", "for", "while", "loop", "in", "break", "continue", "struct", "true", "false",
    "default",
];

fn is_reserved_keyword<S: Span>(word: &S) -> Option<&'static str> {
    for keyword in RESERVED_KEYWORDS {
        if matches!(word.compare(keyword), nom::CompareResult::Ok) {
            return Some(keyword);
        }
    }

    None
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
    file_name: String,
    root_elements: RootElements<S>,
    callables: HashMap<String, CallableReference>,
}

impl<S: Span> Module<S> {
    pub fn load(
        log: &mut ValidationLog<S>,
        file_name: impl Into<String>,
        code: impl Into<S>,
    ) -> Result<Self> {
        let file_name = file_name.into();
        let code = code.into();

        let root_elements = Self::load_ast(code)?;
        let mut callables = HashMap::new();

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

        dbg!(&callables);

        let module = Self {
            file_name,
            root_elements,
            callables,
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
        log: &mut ValidationLog<S>,
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
            if let Some(old) =
                callables.insert(dbg!(get_name(&item).to_string()), build_reference(index))
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

                log.push(ValidationMessage::DuplicateGlobal(old_span, new_span));
            }
        }
    }

    /// Validation is checking for errors that don't require the context of execution.
    fn validate(&self, log: &mut ValidationLog<S>) {
        self.root_elements.validate(log);
    }
}

#[derive(Debug, Eq, PartialEq)]
struct RootElements<S: Span> {
    imports: Vec<parsing::Import<S>>,
    structs: Vec<parsing::Struct<S>>,
    functions: Vec<parsing::Function<S>>,
    sketches: Vec<parsing::Sketch<S>>,
    widgets: Vec<parsing::Widget<S>>,
}

impl<S: Span> RootElements<S> {
    /// Validation is checking for errors that don't require the context of execution.1
    fn validate(&self, log: &mut ValidationLog<S>) {
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

    fn validate_struct(log: &mut ValidationLog<S>, structure: &parsing::Struct<S>) {
        // Name should not be a reserved keyword.
        if let Some(keyword) = is_reserved_keyword(&structure.name) {
            log.push(ValidationMessage::ReservedKeyword(
                structure.name.clone(),
                keyword,
            ));
        }

        // None of the members should have a reserved keyword for a name.
        for member in structure.members.iter() {
            if let Some(keyword) = is_reserved_keyword(&member.name) {
                log.push(ValidationMessage::ReservedKeyword(
                    member.name.clone(),
                    keyword,
                ));
            }
        }
    }

    fn validate_named_block(log: &mut ValidationLog<S>, block: &parsing::NamedBlock<S>) {
        // Name should not be a reserved keyword.
        if let Some(keyword) = is_reserved_keyword(&block.name) {
            log.push(ValidationMessage::ReservedKeyword(
                block.name.clone(),
                keyword,
            ));
        }

        // Parameter names should not be reserved keywords.
        for parameter in block.parameters.iter() {
            if let Some(keyword) = is_reserved_keyword(&parameter.name) {
                log.push(ValidationMessage::ReservedKeyword(
                    parameter.name.clone(),
                    keyword,
                ));
            }
        }

        Self::validate_block(log, &block.block);
    }

    fn validate_block(log: &mut ValidationLog<S>, block: &parsing::Block<S>) {
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
                    for part in to_assign.path.parts.iter() {
                        if let Some(keyword) = is_reserved_keyword(part) {
                            log.push(ValidationMessage::ReservedKeyword(part.clone(), keyword));
                        }
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
                        log.push(ValidationMessage::UnclosedStatement(span));
                    }
                }
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq, Default)]
pub struct ValidationLog<S> {
    entries: Vec<ValidationMessage<S>>,
}

impl<S: Span> ValidationLog<S> {
    fn push(&mut self, message: ValidationMessage<S>) {
        self.entries.push(message);
    }

    fn containes_error(&self) -> bool {
        for message in self.entries.iter() {
            if matches!(message.log_level(), ValidationLogLevel::Error) {
                return true;
            }
        }

        false
    }
}

#[derive(Debug, Eq, PartialEq)]
enum ValidationMessage<S> {
    UnclosedStatement(S),
    ReservedKeyword(S, &'static str),
    DuplicateGlobal(S, S),
}

impl<S> ValidationMessage<S> {
    fn log_level(&self) -> ValidationLogLevel {
        match self {
            Self::UnclosedStatement(_) => ValidationLogLevel::Error,
            Self::ReservedKeyword(_, _) => ValidationLogLevel::Error,
            Self::DuplicateGlobal(_, _) => ValidationLogLevel::Error,
        }
    }
}

impl<S: Span + FormatSpan> std::fmt::Display for ValidationMessage<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnclosedStatement(span) => {
                write!(
                    f,
                    "{}: Non-tail expressions must be closed with a semicolon",
                    span.format_span()
                )
            }
            Self::ReservedKeyword(span, keyword) => {
                write!(
                    f,
                    "{}: Keyword `{}` is reserved",
                    span.format_span(),
                    keyword
                )
            }
            Self::DuplicateGlobal(first_instance, second_instance) => {
                write!(
                    f,
                    "Duplicate global {}:{}",
                    first_instance.format_span(),
                    second_instance.format_span()
                )
            }
        }
    }
}

pub trait FormatSpan {
    fn format_span(&self) -> String;
}

impl<'a> FormatSpan for &'a str {
    fn format_span(&self) -> String {
        format!("`{}`", self)
    }
}
impl<'a> FormatSpan for LocatedSpan<&'a str> {
    fn format_span(&self) -> String {
        format!("[Line:{}]", self.location_line())
    }
}
impl FormatSpan for imstr::ImString {
    fn format_span(&self) -> String {
        format!("`{}`", self)
    }
}
impl FormatSpan for LocatedSpan<imstr::ImString> {
    fn format_span(&self) -> String {
        format!("[Line:{}]", self.location_line())
    }
}

enum ValidationLogLevel {
    Warning,
    Error,
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
        let mut log = ValidationLog::default();
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
                    parameters: vec![],
                    block: parsing::Block { statements: vec![] }
                },
                return_type: parsing::VariableType::Length
            }]
        );
    }

    fn validate(code: &str) -> ValidationLog<&str> {
        let mut log = ValidationLog::default();
        Module::<&str>::load(&mut log, "my_module.ccs", code).unwrap(); // Even if validation "fails" we get the validation log back.

        log
    }

    #[test]
    fn validate_block_empty() {
        assert_eq!(validate("widget MyWidget() {}").entries, []);
    }

    #[test]
    fn validate_block_tail_expressions() {
        assert_eq!(validate("widget MyWidget() { let a = b; }").entries, []);
        assert_eq!(validate("widget MyWidget() { let a = b }").entries, []);
        assert_eq!(
            validate("widget MyWidget() { let a = b; let c = d }").entries,
            []
        );
        assert_eq!(
            validate("widget MyWidget() { let a = b let c = d }").entries,
            [ValidationMessage::UnclosedStatement("let"),]
        );
    }

    #[test]
    fn validate_assignment_keyword_resurvation() {
        assert_eq!(
            validate("widget MyWidget() { let break = b; }").entries,
            [ValidationMessage::ReservedKeyword("break", "break")]
        );
    }

    #[test]
    fn validate_named_block_keyword_resurvation() {
        assert_eq!(
            validate("widget break() { }").entries,
            [ValidationMessage::ReservedKeyword("break", "break")]
        );
    }

    #[test]
    fn validate_parameter_names_are_not_keywords() {
        assert_eq!(validate("widget MyWidget() { }").entries, []);

        assert_eq!(validate("widget MyWidget(okay: Length) { }").entries, []);

        assert_eq!(
            validate("widget MyWidget(break: Length) { }").entries,
            [ValidationMessage::ReservedKeyword("break", "break")]
        );
    }

    #[test]
    fn validate_struct_name_not_keyword() {
        assert_eq!(validate("struct MyStruct { }").entries, []);
        assert_eq!(
            validate("struct break { }").entries,
            [ValidationMessage::ReservedKeyword("break", "break")]
        );
    }

    #[test]
    fn validate_struct_member_not_keyword() {
        assert_eq!(validate("struct MyStruct { }").entries, []);
        assert_eq!(validate("struct MyStruct { okay: Length }").entries, []);
        assert_eq!(
            validate("struct MyStruct { break: Length }").entries,
            [ValidationMessage::ReservedKeyword("break", "break")]
        );
    }

    #[test]
    fn validate_no_duplicate_global_names() {
        assert_eq!(validate("struct MyThing1 {} function MyThing2() -> Length {} sketch MyThing3() {} widget MyThing4() {}").entries, []);
        assert_eq!(validate("struct MyThing {} function MyThing() -> Length {} sketch MyThing() {} widget MyThing() {}").entries, [ 
	    ValidationMessage::DuplicateGlobal("MyThing", "MyThing"),
	    ValidationMessage::DuplicateGlobal("MyThing", "MyThing"),
	    ValidationMessage::DuplicateGlobal("MyThing", "MyThing"),
	]);
    }
}
