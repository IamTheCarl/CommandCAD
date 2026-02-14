use std::{
    fs,
    io::{self, Read, Write},
};

use anyhow::{Context, Result};
use clap::Parser as _;

mod arguments;

fn main() {
    if let Err(error) = trampoline() {
        eprintln!("Fatal error: {error:?}")
    }
}

fn trampoline() -> Result<()> {
    let args = arguments::RunArgs::parse();

    let mut input: Box<dyn Read> = if let Some(input) = args.input {
        Box::new(fs::File::open(input).context("Failed to open input file")?)
    } else {
        Box::new(std::io::stdin())
    };

    let mut input_str = String::new();
    input
        .read_to_string(&mut input_str)
        .context("Failed to read input file")?;
    let input = input_str;

    todo!()
}
