use clap::{Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct Arguments {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Enter read-eval-print loop
    Repl,

    /// Evaluate a single file and whatever local dependencies it may reference
    File { file: PathBuf },
}
