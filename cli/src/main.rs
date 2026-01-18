use std::{collections::HashMap, path::PathBuf, sync::Arc};

mod arguments;
use anyhow::{anyhow, Context, Result};
use arguments::Arguments;
use ariadne::{Cache, Source};
use clap::Parser as _;
use reedline::{DefaultPrompt, Reedline, Signal};

use crate::arguments::Commands;

use interpreter::{
    build_prelude, compile, execute_expression,
    execution::values::BuiltinCallableDatabase,
    new_parser,
    values::{Object, Style, Value},
    ExecutionContext, ImString, LogMessage, Parser, RuntimeLog, StackScope, StackTrace,
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
    let mut line_editor = Reedline::create();
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

fn run_line(
    parser: &mut Parser,
    prelude: &HashMap<ImString, Value>,
    database: &BuiltinCallableDatabase,
    repl_file: &Arc<PathBuf>,
    input: &str,
) -> Result<()> {
    let root = parser
        .parse(input, None)
        .map_err(|error| anyhow!("Failed to parse input: {error:?}"))?;
    let root =
        compile(&repl_file, input, &root).map_err(|error| anyhow!("Failed to compile: {error}"))?;

    let log = StderrLog;
    let context = ExecutionContext {
        log: &log as &dyn RuntimeLog,
        stack_trace: &StackTrace::top(root.reference.clone()),
        stack: &StackScope::top(&prelude),
        database: &database,
    };

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
