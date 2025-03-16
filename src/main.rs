mod parser;
mod transformer;
mod translator;

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
			eprintln!("{:#?}", errs);
			return;
		}
	};
	let (ast, ast_errors) = parse_program().parse_recovery(tokens);
	if !ast_errors.is_empty() {
		for err in ast_errors {
			eprintln!("{err:#?}");
		}
	}
	let ast = match ast {
		Some(ast) => ast,
		None => return,
	};
	if let Err(issues) = transform(&ast) {
		eprintln!("Semantic issue: {:#?}", issues);
		return;
	}
	// println!("{:#?}", ast);
	let il = translate(&ast);
	println!("{}", il);
}
