mod parser;
mod transformer;
mod translator;
mod types;

use std::fs::read_to_string;

use chumsky::Parser;
use parser::{parse_program, parse_tokens};
use transformer::transform;
use translator::translate;

fn main() {
	let source_file = match std::env::args().nth(1) {
		Some(file) => file,
		None => {
			eprintln!("usage: chun [source file]");
			return;
		}
	};
	let source = read_to_string(&source_file).expect("file should exist");
	let res = parse_tokens().parse(source);
	let tokens = match res {
		Ok(tokens) => tokens,
		Err(errs) => {
			eprintln!("Tokenization error:");
			eprintln!("{:#?}", errs);
			return;
		}
	};
	let res = parse_program().parse(tokens);
	let ast = match res {
		Ok(ast) => ast,
		Err(errs) => {
			eprintln!("Parsing error:");
			eprintln!("{errs:#?}",);
			return;
		}
	};
	let program = match transform(&ast) {
		Ok(prog) => prog,
		Err(errs) => {
			eprintln!("Semantic issue: {errs:#?}");
			return;
		}
	};
	let il = translate(&program);
	println!("{}", il);
}
