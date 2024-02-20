use std::{fs, io::Write, ops::Deref, path::PathBuf};

use anyhow::{anyhow, Context, Result};
use clap::Parser;

mod arguments;
// mod package;
mod script;

use fj::{
    core::algorithms::{approx::Tolerance, triangulate::Triangulate},
    export::export,
};
use script::{Failure, Runtime, SerializableValue};

fn main() {
    stderrlog::new().init().unwrap();
    let command = arguments::Command::parse();

    match command {
        arguments::Command::Run(run_args) => run(run_args),
        arguments::Command::Sketch(sketch_args) => sketch(sketch_args),
        arguments::Command::Form(form_args) => form(form_args),
    }
}

fn run(run_args: arguments::RunArgs) {
    if let Err(error) = trampoline(
        run_args.script,
        run_args.arguments,
        |runtime, arguments| runtime.run_function(&run_args.task_name, arguments),
        |value, mut output| {
            match run_args.output_format {
                arguments::TaskOutputFormat::Yaml => serde_yaml::to_writer(&mut output, &value)
                    .context("Failed to serialize results")?,
                arguments::TaskOutputFormat::Json => serde_json::to_writer(&mut output, &value)
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
        |_value, _output| Ok(()), // Sketches are not yet serializable.
    ) {
        log::error!("Failed to build sketch: {:?}", error);
    }
}

fn form(form_args: arguments::FormArgs) {
    if let Err(error) = trampoline(
        form_args.script,
        form_args.arguments,
        |runtime, arguments| runtime.run_solid(&form_args.solid_name, arguments),
        |solid, _output| {
            // TODO load tolerance from arguments. It should be a measurement type.
            // TODO we can automatically derive this value from the bounding box of the model.
            let tolerance = Tolerance::from_scalar(0.1).unwrap();

            let mesh = (solid.handle.deref(), tolerance).triangulate();

            export(&mesh, &form_args.output_file)?;

            Ok(())
        },
    ) {
        log::error!("Failed to build sketch: {:?}", error);
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
    S: FnOnce(R, &mut dyn Write) -> Result<()>,
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

    for message in runtime.log().messages.iter() {
        match message.log_level() {
            script::LogLevel::Warning => log::warn!("{}", message),
        }
    }

    // TODO support writing to file.
    let mut output_stream = std::io::stdout();

    match result {
        Ok(result) => {
            serialize_action(result, &mut output_stream)?;
        }
        Err(failure) => {
            log::error!("{}", failure);
        }
    }

    Ok(())
}
