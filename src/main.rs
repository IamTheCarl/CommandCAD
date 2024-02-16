use std::{fs, io::Write};

use anyhow::{anyhow, Context, Result};
use clap::Parser;

mod arguments;
// mod package;
mod script;

use script::SerializableValue;

fn main() {
    stderrlog::new().init().unwrap();
    let command = arguments::Command::parse();

    match command {
        arguments::Command::Run(run_args) => run(run_args),
    }
}

fn run(run_args: arguments::RunArgs) {
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

    fn run_trampoline(run_args: arguments::RunArgs) -> Result<()> {
        let module_name = run_args
            .script
            .file_name()
            .context("Script file does not have a name")?
            .to_str()
            .context("Script name could not be UTF8 encoded")?;

        let code =
            fs::read_to_string(&run_args.script).context("Failed to read script into memory")?;

        let mut runtime =
            script::Runtime::load((module_name, code)).context("Failed to load runtime")?;

        let mut arguments = Vec::with_capacity(run_args.arguments.len());

        for (index, argument) in run_args.arguments.into_iter().enumerate() {
            let argument = parse_argument(&argument)
                .with_context(|| format!("Failed to parse function argument {}", index))?;

            arguments.push(argument);
        }

        let result = runtime.run_function(&run_args.task_name, arguments);

        for message in runtime.log().messages.iter() {
            match message.log_level() {
                script::LogLevel::Warning => log::warn!("{}", message),
            }
        }

        let mut output_stream = std::io::stdout();

        match result {
            Ok(result) => {
                match run_args.output_format {
                    arguments::OutputFormat::Yaml => {
                        serde_yaml::to_writer(&mut output_stream, &result)
                            .context("Failed to serialize results")?
                    }
                    arguments::OutputFormat::Json => {
                        serde_json::to_writer(&mut output_stream, &result)
                            .context("Failed to serialize results")?
                    }
                };

                writeln!(output_stream)?;
            }
            Err(failure) => {
                log::error!("{}", failure);
            }
        }

        Ok(())
    }

    if let Err(error) = run_trampoline(run_args) {
        log::error!("Failed to run script: {:?}", error);
    }
}
