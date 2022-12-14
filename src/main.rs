#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;
extern crate strum_macros;

mod parser;
mod semantics;
mod translation;
mod translators;

use std::{fs, path::Path, process::exit};

use crate::parser::{ButterParser, Rule};
use crate::translation::Translator;
use clap::Parser as ClapParser;
use pest::Parser;

/// Compiler for butter templates.
#[derive(ClapParser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Template file to compile.
    #[arg(short, long)]
    template: String,
}

fn main() {
    let args = Args::parse();

    println!("Parsing file {}", args.template);
    let template_path = Path::new(&args.template);

    let tmpl_contents = fs::read_to_string(template_path)
        .expect(format!("Couldn't read the template at {}", &args.template).as_str());

    println!("Contents:\n{}", tmpl_contents);

    let parser = ButterParser::parse(Rule::main, tmpl_contents.as_str());
    let mut main_pairs = parser.expect("Failed to parse template.");

    println!("Parsed: {:?}", &main_pairs);

    let maybe_package = semantics::interpret_package(template_path, main_pairs.next().unwrap());
    if maybe_package.is_err() {
        println!("Package error: {}", maybe_package.err().unwrap());
        exit(1);
    }
    let package = maybe_package.unwrap();
    println!("Package:\n{:?}", package);

    let translation = translators::Rust::translate(&package);
    println!("Translation:\n{}", translation);
}
