use std::path::PathBuf;

#[derive(clap::Parser)]
#[command(name = "formatter")]
#[command(bin_name = "formatter")]
/// Format a Command CAD script
pub struct RunArgs {
    #[arg(long)]
    /// Path to script to format. Leave unspecified to use standard input.
    pub input: Option<PathBuf>,

    #[arg(long)]
    /// Path to output file. Leave unspecified to use standard output.
    pub output: Option<PathBuf>,
}
