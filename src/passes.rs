pub mod hir;
pub mod symbols;
mod types;

use crate::{
	ast::UntypedModule,
	hir::HIR,
	passes::{hir::hir_pass, symbols::symbol_pass, types::type_pass},
	span::Span,
};

pub fn pass_all(ast: UntypedModule) -> HIR {
	let resolved = symbol_pass(&ast);
	let typed = type_pass(&resolved);
	let hir = hir_pass(&typed);
	hir
}
