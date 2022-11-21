#[macro_use]
extern crate pest_derive;

mod parser;

use std::fs;

use clap::Parser as ClapParser;
use pest::Parser;
use crate::parser::{ButterParser, Rule};


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

    let tmpl_contents = fs::read_to_string(&args.template)
        .expect(format!("Couldn't read the template at {}", &args.template).as_str());
    
    println!("Contents:\n{}", tmpl_contents);

    let parser = ButterParser::parse(Rule::main, tmpl_contents.as_str());
    let pairs = parser.expect("Failed to parse template.");

    println!("Parsed: {:?}", pairs);
}
