#![allow(unused)]
use crate::{
	lower::lower_to_llvm,
	parser::{lexer::lex, parse_module},
	passes::pass_all,
};

mod ast;
mod diagnostics;
mod hir;
mod lower;
mod parser;
mod passes;
mod span;
mod tokens;
mod types;

static SRC: &'static str = r##"

extern {
	#[symbol = "InitWindow"]
	fn init_window(width: i32, height: i32, title: cstr);
	#[symbol = "CloseWindow"]
	fn close_window();
	#[symbol = "WindowShouldClose"]
	fn window_should_close() -> bool;
	#[symbol = "BeginDrawing"]
	fn begin_drawing();
	#[symbol = "EndDrawing"]
	fn end_drawing();
}

fn main() {
	init_window(800, 600, "woah chun!");
	defer close_window();
	while !window_should_close() {
		begin_drawing();
		defer end_drawing();
	}
}
"##;

fn main() {
	let ast = parse_module(SRC);

	let hir = pass_all(ast);
	let llvm = lower_to_llvm(&hir);
	println!("{llvm}");
}
