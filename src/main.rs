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

use std::{
    fs::{self, File},
    io::{Seek, Write},
    ops::Deref,
    path::PathBuf,
    str::FromStr,
};

use anyhow::{anyhow, bail, Context, Result};
use arguments::{ListUnitsArgs, OutputTarget, SolidOutputFormat, TaskOutputFormat};
use atty::Stream;
use clap::Parser;

mod arguments;
// mod package;
mod script;

use fj_core::algorithms::{
    approx::Tolerance, bounding_volume::BoundingVolume, triangulate::Triangulate,
};
use fj_export::{export_3mf, export_obj, export_stl};
use fj_math::{Aabb, Point, Scalar as FornjotScalar};
use script::{Failure, Runtime, SerializableValue};
use tempfile::SpooledTempFile;
use uom::si::{f64::Length, length::millimeter};

use crate::script::{print_all_supported_units, Scalar};

fn main() {
    stderrlog::new()
        .show_level(true)
        .verbosity(3)
        .init()
        .unwrap();
    let command = arguments::Command::parse();

    match command {
        arguments::Command::Run(run_args) => run(run_args),
        arguments::Command::Sketch(sketch_args) => sketch(sketch_args),
        arguments::Command::Form(form_args) => form(form_args),
        arguments::Command::ListUnits(list_args) => list_units(list_args),
    }
}

fn list_units(list_args: ListUnitsArgs) {
    let mut output = std::io::stdout();
    if let Err(error) = print_all_supported_units(&mut output, list_args.search.as_deref()) {
        log::error!("Output failure: {:?}", error);
    }
}

fn run(run_args: arguments::RunArgs) {
    if let Err(error) = trampoline(
        run_args.script,
        run_args.arguments,
        |runtime, arguments| runtime.run_task(&run_args.task_name, arguments),
        |value, _runtime| {
            let (mut output, format): (Box<dyn Write>, TaskOutputFormat) = match run_args.output {
                OutputTarget::Stdout => {
                    let format = run_args.output_format.unwrap_or_default();

                    (Box::new(std::io::stdout()), format)
                }
                OutputTarget::File(path) => {
                    let format = if let Some(output_format) = run_args.output_format {
                        output_format
                    } else {
                        path.extension()
                            .and_then(|ext| ext.to_str())
                            .and_then(|s| TaskOutputFormat::from_str(s).ok())
                            .ok_or(anyhow!("Could not infer output format from file name"))?
                    };

                    (Box::new(File::create(path)?), format)
                }
            };

            match format {
                TaskOutputFormat::Yaml => serde_yaml::to_writer(&mut output, &value)
                    .context("Failed to serialize results")?,
                TaskOutputFormat::Json => serde_json::to_writer(&mut output, &value)
                    .context("Failed to serialize results")?,
            };

            writeln!(&mut output)?;

            Ok(())
        },
    ) {
        log::error!("Failed to run task: {:?}", error);
    }
}

fn sketch(sketch_args: arguments::SketchArgs) {
    if let Err(error) = trampoline(
        sketch_args.script,
        sketch_args.arguments,
        |runtime, arguments| runtime.run_sketch(&sketch_args.sketch_name, arguments),
        |_value, _runtime| Ok(()), // Sketches are not yet serializable.
    ) {
        log::error!("Failed to build sketch: {:?}", error);
    }
}

fn form(form_args: arguments::FormArgs) {
    if let Err(error) = trampoline(
        form_args.script,
        form_args.arguments,
        |runtime, arguments| runtime.run_solid(&form_args.solid_name, arguments),
        |solid, runtime| {
            let tolerance = match form_args.tolerance {
                Some(tolerance) => {
                    let tolerance = Scalar::from_str(&tolerance)?;
                    let tolerance: Length = tolerance
                        .try_into()
                        .map_err(|_| anyhow!("Failed to parse tolerance as a length"))?;

                    Tolerance::from_scalar(tolerance.get::<millimeter>())?
                }
                None => {
                    // Compute a default tolerance derived from the bounding box.
                    let aabb = runtime
                        .global_resources(|global_resources| {
                            solid
                                .handle
                                .deref()
                                .aabb(&global_resources.fornjot_core.layers.geometry)
                        })
                        .unwrap_or(Aabb {
                            min: Point::origin(),
                            max: Point::origin(),
                        });

                    // Find the smallest face.
                    let mut min_extent = FornjotScalar::MAX;
                    for extent in aabb.size().components {
                        if extent > FornjotScalar::ZERO && extent < min_extent {
                            min_extent = extent;
                        }
                    }

                    // Our smallest face will be divided into 1000 parts.
                    let tolerance = min_extent / FornjotScalar::from_f64(1000.0);
                    Tolerance::from_scalar(tolerance)?
                }
            };

            let mesh = runtime.global_resources_mut(|global_resources| {
                (solid.handle.deref(), tolerance).triangulate(&mut global_resources.fornjot_core)
            });

            match form_args.output {
                OutputTarget::Stdout => {
                    let format = form_args.output_format.unwrap_or_default();
                    let mut output = std::io::stdout();

                    let is_binary_format = match format {
                        SolidOutputFormat::ThreeMF => true,
                        SolidOutputFormat::Stl => true,
                        SolidOutputFormat::Obj => false,
                    };

                    if is_binary_format && atty::is(Stream::Stdout) {
                        bail!("Refusing to output binary data to terminal.");
                    }

                    match format {
                        SolidOutputFormat::ThreeMF => {
                            // We'll use a bout a megabyte of memory before buffering to the filesystem.
                            let mut tempfile = SpooledTempFile::new(1024 * 1024);

                            export_3mf(&mesh, &mut tempfile)?;
                            tempfile.seek(std::io::SeekFrom::Start(0))?;

                            std::io::copy(&mut tempfile, &mut output)?;
                        }
                        SolidOutputFormat::Stl => export_stl(&mesh, &mut output)?,
                        SolidOutputFormat::Obj => export_obj(&mesh, &mut output)?,
                    }
                }
                OutputTarget::File(path) => {
                    let format = if let Some(output_format) = form_args.output_format {
                        output_format
                    } else {
                        path.extension()
                            .and_then(|ext| ext.to_str())
                            .and_then(|s| SolidOutputFormat::from_str(s).ok())
                            .ok_or(anyhow!("Could not infer output format from file name"))?
                    };

                    let mut output = File::create(path)?;

                    match format {
                        SolidOutputFormat::ThreeMF => export_3mf(&mesh, &mut output)?,
                        SolidOutputFormat::Stl => export_stl(&mesh, &mut output)?,
                        SolidOutputFormat::Obj => export_obj(&mesh, &mut output)?,
                    }
                }
            };

            Ok(())
        },
    ) {
        log::error!("Failed to build solid: {:?}", error);
    }
}

fn trampoline<A, S, R>(
    script: PathBuf,
    arguments: Vec<String>,
    runtime_action: A,
    serialize_action: S,
) -> Result<()>
where
    A: FnOnce(&mut Runtime, Vec<SerializableValue>) -> std::result::Result<R, Failure>,
    S: FnOnce(R, &mut Runtime) -> Result<()>,
{
    fn parse_argument(argument: &str) -> Result<SerializableValue> {
        match serde_json::from_str(argument) {
            Ok(value) => Ok(value),
            Err(json_error) => match serde_yaml::from_str(argument) {
                Ok(value) => Ok(value),
                Err(yaml_error) => Err(anyhow!(
                    "Could not parse as Json or Yaml.\nJson Error: {:?}\n Yaml Error: {:?}",
                    json_error,
                    yaml_error
                )),
            },
        }
    }

    let module_name = script
        .file_name()
        .context("Script file does not have a name")?
        .to_str()
        .context("Script name could not be UTF8 encoded")?;

    let code = fs::read_to_string(&script).context("Failed to read script into memory")?;

    let mut runtime =
        script::Runtime::load((module_name, code)).context("Failed to load runtime")?;

    let mut unpacked_arguments = Vec::with_capacity(arguments.len());

    for (index, argument) in arguments.into_iter().enumerate() {
        let argument = parse_argument(&argument)
            .with_context(|| format!("Failed to parse function argument {}", index))?;

        unpacked_arguments.push(argument);
    }

    let result = runtime_action(&mut runtime, unpacked_arguments);

    match result {
        Ok(result) => {
            serialize_action(result, &mut runtime)?;
        }
        Err(failure) => {
            log::error!("{}", failure);
        }
    }

    Ok(())
}
