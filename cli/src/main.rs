use std::{
    collections::HashMap,
    path::PathBuf,
    sync::{Arc, Mutex},
};

mod arguments;
use anyhow::{anyhow, bail, Context, Result};
use arguments::Arguments;
use ariadne::{Cache, Label, Report, ReportKind, Source};
use clap::Parser as _;
use git2::Repository;
use reedline::{DefaultHinter, DefaultPrompt, Reedline, Signal};
use tempfile::TempDir;
use type_sitter::Node as _;

use crate::arguments::Commands;

use interpreter::{
    build_prelude,
    compile::{compile, iter_raw_nodes},
    execute_expression,
    execution::values::BuiltinCallableDatabase,
    new_parser, run_file,
    values::{Object, Style, Value},
    ExecutionContext, ExecutionFileCache, ImString, LogMessage, Parser, RuntimeLog,
    SourceReference, StackScope, StackTrace, Store,
};

fn main() {
    let arguments = Arguments::parse();

    let result = match arguments.command {
        Commands::Repl => repl(),
        Commands::File { file } => process_file(file),
    };

    if let Err(error) = result {
        println!("Fatal error: {error:?}");
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

    fn collect_syntax_errors<'t>(
        &self,
        input: &str,
        tree: &'t interpreter::compile::RootTree,
        file: &'t Arc<PathBuf>,
        span: SourceReference,
    ) {
        if let Some(report) = build_syntax_errors(&tree, file, span) {
            if let Err(error) = report.eprint(ReplFileCache(Source::from(input))) {
                eprintln!("Failed to print syntax error message: {error}");
            }
        }
    }
}

fn process_file(file: PathBuf) -> Result<()> {
    if !file.exists() {
        bail!("File does not exist");
    }

    if file.is_dir() {
        bail!("File is a directory");
    }

    let database = BuiltinCallableDatabase::new();
    let prelude = build_prelude(&database).context("Failed to build prelude")?;

    let parent = file
        .parent()
        .context("Could not get parent directory of file")?;

    let store_directory = match Repository::discover(parent) {
        Ok(repository) => {
            let git_directory = repository.path();
            let project_directory = git_directory
                .parent()
                .context("Failed to get parent directory of .git")?;
            project_directory.join(".ccad/store")
        }
        Err(error) => {
            eprintln!("Failed to discover project directory (is this project in a git repository?): {error}");
            eprintln!("Current directory will be used for the store.");
            PathBuf::from("./.ccad/store")
        }
    };
    std::fs::create_dir_all(&store_directory).context("Failed to create store directory")?;

    let store = Store::new(store_directory);
    let log = StderrLog;
    let files = Mutex::new(HashMap::new());

    let context = ExecutionContext {
        log: &log as &dyn RuntimeLog,
        stack_trace: &StackTrace::bootstrap(),
        stack: &StackScope::top(&prelude),
        database: &database,
        store: &store,
        file_cache: &files,
    };

    let result = run_file(&context, file);
    match result {
        Ok(result) => {
            let mut output = String::new();
            result
                .format(&context, &mut output, Style::Default, None)
                .context("Failed to write output to display")?;

            println!("{output}");
        }
        Err(error) => {
            let file_cache = context.file_cache.lock().expect("File cache was poisoned");

            let report = error.report();
            report
                .eprint(ExecutionFileCache(&*file_cache))
                .context("Failed to format error message")?;
        }
    }

    Ok(())
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

fn repl() -> Result<()> {
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
    let prelude = build_prelude(&database).context("Failed to build prelude")?;

    let store_directory = TempDir::new().unwrap();
    let store = Store::new(store_directory.path());

    println!("Store is located at {:?}", store_directory.path());
    println!("Store will be deleted on exit.");

    loop {
        let sig = line_editor.read_line(&prompt);
        match sig {
            Ok(Signal::Success(input)) => {
                if let Err(error) = run_line(
                    &mut parser,
                    &prelude,
                    &database,
                    &store,
                    &repl_file,
                    input.as_str(),
                ) {
                    eprintln!("Failed to run line: {error}");
                }
            }
            Ok(Signal::CtrlD) | Ok(Signal::CtrlC) => {
                println!("\nAborted!");
                break Ok(());
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
    store: &Store,
    repl_file: &Arc<PathBuf>,
    input: &str,
) -> Result<()> {
    let tree = parser
        .parse(input, None)
        .map_err(|error| anyhow!("Failed to parse input: {error:?}"))?;
    let root =
        compile(&repl_file, input, &tree).map_err(|error| anyhow!("Failed to compile: {error}"))?;

    let log = StderrLog;
    let files = Mutex::new(HashMap::new());

    let context = ExecutionContext {
        log: &log as &dyn RuntimeLog,
        stack_trace: &StackTrace::top(root.reference.clone()),
        stack: &StackScope::top(&prelude),
        database: &database,
        store,
        file_cache: &files,
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
