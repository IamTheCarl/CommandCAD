use clap::builder::{PossibleValuesParser, TypedValueParser};
use std::{fmt::Display, path::PathBuf, str::FromStr};

use clap::Parser;

#[derive(Parser)]
#[command(name = "ccad")]
#[command(bin_name = "ccad")]
/// Create fabricatable 3D and 2D models from code and generate output with commands.
pub enum Command {
    Run(RunArgs),
    Sketch(SketchArgs),
    Form(FormArgs),
}

#[derive(Clone)]
pub enum TaskOutputFormat {
    Yaml,
    Json,
}

impl Default for TaskOutputFormat {
    fn default() -> Self {
        Self::Json
    }
}

impl Display for TaskOutputFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TaskOutputFormat::Yaml => write!(f, "yaml"),
            TaskOutputFormat::Json => write!(f, "json"),
        }
    }
}

impl FromStr for TaskOutputFormat {
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
#[command(version, about)]
/// Run a task and write the output to a file, or stdout.
pub struct RunArgs {
    pub script: PathBuf,
    pub task_name: String,

    #[arg(long, default_value_t = TaskOutputFormat::default(),
          value_parser = PossibleValuesParser::new(["json", "yaml"])
	  .map(|s| s.parse::<TaskOutputFormat>().unwrap()),)]
    pub output_format: TaskOutputFormat,

    #[arg(last = true)]
    pub arguments: Vec<String>,
}

#[derive(clap::Args)]
#[command(version, about)]
/// Build a 2D sketch.
pub struct SketchArgs {
    pub script: PathBuf,
    pub sketch_name: String,

    #[arg(last = true)]
    pub arguments: Vec<String>,
}

#[derive(Clone)]
pub enum SolidOutputFormat {
    ThreeMF,
    Stl,
    Obj,
}

impl Default for SolidOutputFormat {
    fn default() -> Self {
        Self::Stl
    }
}

impl Display for SolidOutputFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SolidOutputFormat::ThreeMF => write!(f, "3mf"),
            SolidOutputFormat::Stl => write!(f, "stl"),
            SolidOutputFormat::Obj => write!(f, "obj"),
        }
    }
}

impl FromStr for SolidOutputFormat {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_lowercase().as_str() {
            "3mf" => Ok(Self::ThreeMF),
            "stl" => Ok(Self::Stl),
            "obj" => Ok(Self::Obj),
            _ => Err(()),
        }
    }
}

#[derive(clap::Args)]
#[command(version, about)]
/// Form a 3D solid.
pub struct FormArgs {
    pub script: PathBuf,
    pub solid_name: String,

    // FIXME Fornjot needs modification to output to stdout, so we're just taking a file path for now.
    /// File to write solid to (3MF, STL, or STL)
    pub output_file: PathBuf,

    #[arg(last = true)]
    pub arguments: Vec<String>,
}
