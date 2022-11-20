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
}
