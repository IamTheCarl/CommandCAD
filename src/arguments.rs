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
