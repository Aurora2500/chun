mod parser;
mod translator;
mod validator;

use std::fs::read_to_string;

use chumsky::Parser;
use parser::{parse_program, parse_tokens};
use translator::translate;

fn main() {
	let source_file = match std::env::args().nth(1) {
		Some(file) => file,
		None => {
			eprintln!("usage: chun [source file]");
			return;
		}
	};
	let source = read_to_string(source_file).expect("file should exist");
	let res = parse_tokens().parse(source);
	let tokens = match res {
		Ok(tokens) => tokens,
		Err(_errs) => {
			return;
		}
	};
	let ast = parse_program().parse(tokens);
	let ast = match ast {
		Ok(ast) => ast,
		Err(_errs) => return,
	};
	let il = translate(&ast);
	println!("{}", il);
}
