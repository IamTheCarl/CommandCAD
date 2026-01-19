use std::{collections::HashMap, path::PathBuf, sync::Arc};

mod arguments;
use anyhow::{anyhow, Context, Result};
use arguments::Arguments;
use ariadne::{Cache, Label, Report, ReportKind, Source};
use clap::Parser as _;
use reedline::{DefaultHinter, DefaultPrompt, Reedline, Signal};
use type_sitter::Node as _;

use crate::arguments::Commands;

use interpreter::{
    build_prelude,
    compile::{compile, iter_raw_nodes},
    execute_expression,
    execution::values::BuiltinCallableDatabase,
    new_parser,
    values::{Object, Style, Value},
    ExecutionContext, ImString, LogMessage, Parser, RuntimeLog, SourceReference, StackScope,
    StackTrace,
};

fn main() {
    let arguments = Arguments::parse();

    match arguments.command {
        Commands::Repl => repl(),
    }
}

#[derive(Debug)]
struct StderrLog;

impl RuntimeLog for StderrLog {
    fn push_message(&self, message: LogMessage) {
        let level_char = match message.level {
            interpreter::LogLevel::Info => 'I',
            interpreter::LogLevel::Warning => 'W',
        };

        eprintln!("{}: {}: {}", level_char, message.origin, message);
    }
}

struct ReplFileCache<'i>(Source<&'i str>);

impl<'i> Cache<Arc<PathBuf>> for ReplFileCache<'i> {
    type Storage = &'i str;

    fn fetch(
        &mut self,
        _id: &Arc<PathBuf>,
    ) -> Result<&Source<Self::Storage>, impl std::fmt::Debug> {
        self.0.fetch(&())
    }

    fn display<'a>(&self, _id: &'a Arc<PathBuf>) -> Option<impl std::fmt::Display + 'a> {
        self.0.display(&())
    }
}

fn repl() {
    let mut line_editor = Reedline::create().with_hinter(Box::new(
        DefaultHinter::default().with_style(
            nu_ansi_term::Style::new()
                .italic()
                .fg(nu_ansi_term::Color::DarkGray),
        ),
    ));
    let prompt = DefaultPrompt::default();
    println!("Welcome to REPL mode. Press Ctrl-C or Ctrl-D to exit");

    let repl_file = Arc::new(PathBuf::from("repl.ccm"));
    let mut parser = new_parser();

    let database = BuiltinCallableDatabase::new();
    let prelude = build_prelude(&database);

    loop {
        let sig = line_editor.read_line(&prompt);
        match sig {
            Ok(Signal::Success(input)) => {
                if let Err(error) =
                    run_line(&mut parser, &prelude, &database, &repl_file, input.as_str())
                {
                    eprintln!("Failed to run line: {error}");
                }
            }
            Ok(Signal::CtrlD) | Ok(Signal::CtrlC) => {
                println!("\nAborted!");
                break;
            }
            x => {
                println!("Event: {:?}", x);
            }
        }
    }
}

fn build_syntax_errors<'t>(
    tree: &'t interpreter::compile::RootTree,
    repl_file: &'t Arc<PathBuf>,
    span: SourceReference,
) -> Option<Report<'t, SourceReference>> {
    let mut report_builder = Report::build(ReportKind::Error, span);
    report_builder.set_message("Syntax issues found while parsing");

    let mut has_syntax_issues = false;

    for node in iter_raw_nodes(tree) {
        if node.is_missing() {
            let kind = node.raw().kind();
            report_builder.add_label(
                Label::new(SourceReference {
                    file: repl_file.clone(),
                    range: node.range(),
                })
                .with_message(format!("Missing expected node `{kind}`")),
            );

            has_syntax_issues = true;
        }

        if node.is_error() {
            report_builder.add_label(
                Label::new(SourceReference {
                    file: repl_file.clone(),
                    range: node.range(),
                })
                .with_message("Could not parse node"),
            );

            has_syntax_issues = true;
        }
    }

    if has_syntax_issues {
        Some(report_builder.finish())
    } else {
        Option::None
    }
}

fn run_line(
    parser: &mut Parser,
    prelude: &HashMap<ImString, Value>,
    database: &BuiltinCallableDatabase,
    repl_file: &Arc<PathBuf>,
    input: &str,
) -> Result<()> {
    let tree = parser
        .parse(input, None)
        .map_err(|error| anyhow!("Failed to parse input: {error:?}"))?;
    let root =
        compile(&repl_file, input, &tree).map_err(|error| anyhow!("Failed to compile: {error}"))?;

    let log = StderrLog;
    let context = ExecutionContext {
        log: &log as &dyn RuntimeLog,
        stack_trace: &StackTrace::top(root.reference.clone()),
        stack: &StackScope::top(&prelude),
        database: &database,
    };

    if let Some(report) = build_syntax_errors(&tree, repl_file, root.reference.clone()) {
        report
            .eprint(ReplFileCache(Source::from(input)))
            .context("Failed to format syntax error message")?;
    }

    let result = execute_expression(&context, &root);

    match result {
        Ok(result) => {
            let mut output = String::new();
            result
                .format(&context, &mut output, Style::Default, None)
                .context("Failed to write output to display")?;

            println!("{output}");
        }
        Err(error) => {
            let report = error.report();
            report
                .eprint(ReplFileCache(Source::from(input)))
                .context("Failed to format error message")?;
        }
    }

    Ok(())
}
