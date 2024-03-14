/*
 * Copyright 2024 James Carl
 * AGPL-3.0-only or AGPL-3.0-or-later
 *
 * This file is part of Command Cad.
 *
 * Command CAD is free software: you can redistribute it and/or modify it under the terms of
 * the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License along with this
 * program. If not, see <https://www.gnu.org/licenses/>.
 */

use clap::builder::{NonEmptyStringValueParser, PossibleValuesParser, TypedValueParser};
use std::{fmt::Display, path::PathBuf, str::FromStr};

use clap::Parser;

#[derive(Clone, Debug)]
pub enum OutputTarget {
    Stdout,
    File(PathBuf),
}

impl Display for OutputTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Stdout => write!(f, "[stdout]"),
            Self::File(path) => write!(f, "{:?}", path),
        }
    }
}

impl Default for OutputTarget {
    fn default() -> Self {
        Self::Stdout
    }
}

impl FromStr for OutputTarget {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "-" => Ok(Self::Stdout),
            "[stdout]" => Ok(Self::Stdout),
            _ => Ok(Self::File(PathBuf::from(s))),
        }
    }
}

#[derive(Parser)]
#[command(name = "ccad")]
#[command(bin_name = "ccad")]
/// Create fabricatable 3D and 2D models from code and generate output with commands.
pub enum Command {
    Run(RunArgs),
    Sketch(SketchArgs),
    Form(FormArgs),
    ListUnits(ListUnitsArgs),
}

#[derive(Clone, Debug)]
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
    /// Path to script to run
    pub script: PathBuf,

    /// Name of the task within the script to be ran
    pub task_name: String,

    /// The format to output the result in (is automatically derived if outputting to a file, or defaults to json if outputting to stdout)
    #[arg(long, default_value = Option::None,
          value_parser = PossibleValuesParser::new(["json", "yaml"])
	  .map(|s| s.parse::<TaskOutputFormat>().unwrap()),)]
    pub output_format: Option<TaskOutputFormat>,

    /// Path to target output file. Set to - or leave blank to output to stdout
    #[arg(long, default_value_t = OutputTarget::default(),
	  value_parser = NonEmptyStringValueParser::new().map(|s| s.parse::<OutputTarget>().unwrap()))]
    pub output: OutputTarget,

    /// Arguments to be passed to the task function
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

#[derive(Clone, Debug)]
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

    /// File to write solid to (3MF, STL, or STL)
    #[arg(long, default_value = Option::None,
          value_parser = PossibleValuesParser::new(["3mf", "stl", "obj"])
	  .map(|s| s.parse::<SolidOutputFormat>().unwrap()),)]
    pub output_format: Option<SolidOutputFormat>,

    pub tolerance: Option<String>,

    /// Path to target output file. Set to - or leave blank to output to stdout
    #[arg(long, default_value_t = OutputTarget::default(),
	  value_parser = NonEmptyStringValueParser::new().map(|s| s.parse::<OutputTarget>().unwrap()))]
    pub output: OutputTarget,

    #[arg(last = true)]
    pub arguments: Vec<String>,
}

#[derive(clap::Args)]
#[command(version, about)]
/// List builtin units of measurement
pub struct ListUnitsArgs {
    /// Optional filter. Will only print results containing the keyword.
    pub search: Option<String>,
}
