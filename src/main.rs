use std::fs;

use clap::Parser;

/// Simple program to greet a person
#[derive(Parser, Debug)]
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
}
