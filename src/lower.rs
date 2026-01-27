use crate::{
	hir::{HIR, HIRBasicBlock, HIRConst, HIRFn, HIRStmt, HIRSymbol, HIRTerminator},
	passes::symbols::Symbol,
};
use std::fmt::{Display, Formatter, Write};

pub fn lower_to_llvm(hir: &HIR) -> String {
	let mut out = String::new();
	for c in &hir.consts {
		lower_const(&mut out, c);
	}
	for f in &hir.fns {
		lower_fn(&mut out, f);
	}
	out
}

pub fn lower_const(out: &mut String, cnst: &HIRConst) {
	match cnst {
		HIRConst::Str { id, value } => {
			writeln!(
				out,
				"@str.{} = internal constant [{} x i8] c\"\\{}00\";",
				id,
				value.len() + 1,
				value,
			);
		}
	}
}

pub fn lower_fn(out: &mut String, f: &HIRFn) {
	write!(out, "define i32 @{}(", f.name);
	for param in &f.params {}
	writeln!(out, ") nounwind {{");
	for bb in &f.segments {
		lower_bb(out, bb);
	}
	writeln!(out, "}}");
}

fn lower_bb(out: &mut String, bb: &HIRBasicBlock) {
	writeln!(out, "bb{}:", bb.id);
	for stmt in &bb.stmts {
		lower_stmt(out, stmt);
	}
	lower_terminator(out, &bb.terminator);
}

fn lower_stmt(out: &mut String, stmt: &HIRStmt) {
	match stmt {
		crate::hir::HIRStmt::Assignment { to, val } => {
			write!(out, "    {} = ", Refer(to),);
			writeln!(out, "");
		}
	}
}

fn lower_terminator(out: &mut String, terminator: &HIRTerminator) {
	match terminator {
		crate::hir::HIRTerminator::Return { value } => {
			writeln!(out, "    ret void");
		}
		crate::hir::HIRTerminator::Jump { to } => {
			writeln!(out, "    br label bb{to}");
		}
		crate::hir::HIRTerminator::Branch { cond, then, else_ } => {
			let cond = match cond {
				crate::hir::HIRValue::Symbol(hirsymbol) => match hirsymbol {
					crate::hir::HIRSymbol::LocalTemp(_) => "%tmp",
					crate::hir::HIRSymbol::Global(_) => "@tmp",
					crate::hir::HIRSymbol::Symbol(symbol) => todo!(),
				},
				crate::hir::HIRValue::Int { value, type_ } => todo!(),
				crate::hir::HIRValue::Float { value, type_ } => todo!(),
				crate::hir::HIRValue::Bool { value, type_ } => {
					if *value {
						"1"
					} else {
						"0"
					}
				}
			};
			writeln!(out, "    br i1 {cond}, label bb{then}, label bb{else_}");
		}
	}
}

pub struct Refer<T>(T);

impl Display for Refer<&HIRSymbol> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match &self.0 {
			HIRSymbol::LocalTemp(i) => write!(f, "%tmp{i}"),
			HIRSymbol::Global(i) => write!(f, "@data{i}"),
			HIRSymbol::Symbol(Symbol { id, kind, original }) => match kind {
				crate::passes::symbols::SymbolKind::Local => write!(f, "%{}", original.name),
				crate::passes::symbols::SymbolKind::Function => write!(f, "@{}", original.name),
				crate::passes::symbols::SymbolKind::Struct => todo!(),
				crate::passes::symbols::SymbolKind::Enum => todo!(),
				crate::passes::symbols::SymbolKind::EnumValue => todo!(),
				crate::passes::symbols::SymbolKind::Union => todo!(),
				crate::passes::symbols::SymbolKind::UnionVariant => todo!(),
			},
		}
	}
}
