use clap::builder::{PossibleValuesParser, TypedValueParser};
use std::{fmt::Display, path::PathBuf, str::FromStr};

use clap::Parser;

#[derive(Parser)]
#[command(name = "ccad")]
#[command(bin_name = "ccad")]
pub enum Command {
    Run(RunArgs),
}

#[derive(Clone)]
pub enum OutputFormat {
    Yaml,
    Json,
}

impl Default for OutputFormat {
    fn default() -> Self {
        Self::Json
    }
}

impl Display for OutputFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OutputFormat::Yaml => write!(f, "yaml"),
            OutputFormat::Json => write!(f, "json"),
        }
    }
}

impl FromStr for OutputFormat {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "yaml" => Ok(Self::Yaml),
            "json" => Ok(Self::Json),
            _ => Err(()),
        }
    }
}

#[derive(clap::Args)]
#[command(version, about, long_about = None)]
pub struct RunArgs {
    pub script: PathBuf,
    pub task_name: String,

    #[arg(long, default_value_t = OutputFormat::default(),
          value_parser = PossibleValuesParser::new(["json", "yaml"])
	  .map(|s| s.parse::<OutputFormat>().unwrap()),)]
    pub output_format: OutputFormat,

    #[arg(last = true)]
    pub arguments: Vec<String>,
}
