mod arguments;
use arguments::Arguments;
use clap::Parser as _;
use reedline::{DefaultPrompt, Reedline, Signal};

use crate::arguments::Commands;

fn main() {
    let arguments = Arguments::parse();

    match arguments.command {
        Commands::Repl => repl(),
    }
}

fn repl() {
    let mut line_editor = Reedline::create();
    let prompt = DefaultPrompt::default();
    println!("Welcome to REPL mode. Press Ctrl-C or Ctrl-D to exit");

    loop {
        let sig = line_editor.read_line(&prompt);
        match sig {
            Ok(Signal::Success(buffer)) => {
                println!("We processed: {}", buffer);
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
