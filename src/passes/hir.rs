use std::{collections::HashMap, mem, process::exit};

use ecow::EcoString;

use crate::{
	ast::{
		Statement, TypedBlock, TypedDefinition, TypedFnDef, TypedModule, TypedStatement,
		typed::TypedExpr,
	},
	hir::{
		HIR, HIRBasicBlock, HIRConst, HIRFn, HIROp, HIRScalar, HIRStmt, HIRSymbol, HIRTerminator,
		HIRValue,
	},
	passes::symbols::Symbol,
};

pub fn hir_pass(ast: &TypedModule) -> HIR {
	let mut t = Transformer::new();
	dbg!(t.lower_module(ast))
}

impl Transformer {
	fn lower_module(&mut self, ast: &TypedModule) -> HIR {
		let TypedModule {
			definitions:
				TypedDefinition {
					structs,
					enums,
					unions,
					fn_defs,
					fn_decls,
				},
		} = ast;

		let fns = fn_defs.into_iter().map(|f| self.lower_fn(f)).collect();

		let consts = self
			.anon_strs
			.iter()
			.enumerate()
			.map(|(id, value)| HIRConst::Str {
				id: id as u64,
				value: value.clone(),
			})
			.collect();

		HIR { fns, consts }
	}

	fn lower_fn(&mut self, fndef: &TypedFnDef) -> HIRFn {
		let TypedFnDef {
			attrs,
			name,
			params,
			return_type,
			body,
			..
		} = fndef;
		let mut cfg = CFG::new();
		let mut scope = Scope::new();
		cfg.lower_block(self, &mut scope, body, Destination::Return);

		HIRFn {
			params: vec![],
			segments: cfg.segments,
		}
	}
}

enum Destination {
	None,
	Return,
	Symbol(HIRSymbol),
}

impl CFG {
	fn lower_block<'ast>(
		&mut self,
		trans: &mut Transformer,
		scope: &mut Scope<'_, 'ast>,
		block: &'ast TypedBlock,
		dest: Destination,
	) {
		let TypedBlock { stmts, tail, .. } = block;
		for stmt in stmts {
			self.lower_stmt(trans, scope, stmt);
		}
		for deferred_op in scope.defers.iter().rev() {
			_ = self.lower_expr(trans, &mut scope.sub(), deferred_op);
		}
	}

	fn lower_stmt<'ast>(
		&mut self,
		trans: &mut Transformer,
		scope: &mut Scope<'_, 'ast>,
		stmt: &'ast TypedStatement,
	) {
		match stmt {
			Statement::Expr { expr, .. } => {
				self.lower_expr(trans, scope, expr);
			}
			Statement::Let {
				binding,
				type_,
				val,
				..
			} => {}
			Statement::ErrDefer { span, op } => todo!(),
			Statement::Defer { span, op } => {
				scope.add_defer(op);
			}
		}
	}

	fn lower_expr(
		&mut self,
		trans: &mut Transformer,
		scope: &mut Scope,
		expr: &TypedExpr,
	) -> Option<HIRValue> {
		match expr {
			TypedExpr::Binding { binding, .. } => {
				Some(HIRValue::Symbol(HIRSymbol::Symbol(binding.clone())))
			}
			TypedExpr::StrLiteral { span, value, type_ } => {
				trans.push_str_const(value.clone());
				return Some(HIRValue::Symbol(HIRSymbol::GlobalTemp(0)));
			}
			TypedExpr::IntLiteral {
				span,
				value,
				int_value,
				suffix,
				type_,
			} => Some(HIRValue::Int {
				value: int_value.clone(),
				type_: HIRScalar::I32,
			}),
			TypedExpr::FloatLiteral {
				span,
				value,
				suffix,
				type_,
			} => todo!(),
			TypedExpr::BoolLiteral { span, value, type_ } => todo!(),
			TypedExpr::Unit { span, type_ } => None,
			TypedExpr::NamedStructLiteral {
				span,
				name,
				fields,
				type_,
			} => todo!(),
			TypedExpr::Block(block) => todo!(),
			TypedExpr::If {
				span,
				cond,
				then,
				else_if,
				else_,
				type_,
			} => None,
			TypedExpr::For {
				span,
				iterator,
				body,
				type_,
			} => todo!(),
			TypedExpr::While {
				span,
				cond,
				body,
				type_,
			} => {
				let cond_bb = self.get_id();
				self.terminate(HIRTerminator::Jump { to: cond_bb }, cond_bb);
				let cond = self.lower_expr(trans, scope, cond).expect("handle");
				let body_bb = self.get_id();
				let exit_bb = self.get_id();
				self.terminate(
					HIRTerminator::Branch {
						cond,
						then: body_bb,
						else_: exit_bb,
					},
					body_bb,
				);
				let mut body_scope = scope.sub();
				self.lower_block(trans, &mut body_scope, body, Destination::None);
				self.terminate(HIRTerminator::Jump { to: cond_bb }, exit_bb);

				None
			}
			TypedExpr::Loop(block) => todo!(),
			TypedExpr::Match {
				span,
				on,
				arms,
				type_,
			} => todo!(),
			TypedExpr::Binop {
				span,
				lhs,
				op,
				rhs,
				type_,
			} => todo!(),
			TypedExpr::Unop {
				span,
				op,
				on,
				type_,
			} => todo!(),
			TypedExpr::Group { span, inner, type_ } => todo!(),
			TypedExpr::CallLike {
				span,
				func,
				args,
				type_,
			} => {
				let HIRValue::Symbol(func) = self.lower_expr(trans, scope, func).expect("handle")
				else {
					panic!("not handled")
				};
				let args = args
					.iter()
					.map(|a| self.lower_expr(trans, scope, a).expect("handle"))
					.collect();
				let ret_sym = HIRSymbol::LocalTemp(self.get_id());
				self.curr.push(HIRStmt::Assignment {
					to: ret_sym.clone(),
					val: HIROp::Call { func, args },
				});
				Some(HIRValue::Symbol(ret_sym))
			}
			TypedExpr::Index {
				span,
				val,
				at,
				type_,
			} => todo!(),
			TypedExpr::TupleIndex {
				span,
				val,
				idx,
				type_,
			} => todo!(),
			TypedExpr::FieldAccess {
				span,
				val,
				field,
				type_,
			} => todo!(),
			TypedExpr::MethodCall {
				span,
				val,
				method,
				args,
				type_,
			} => todo!(),
			TypedExpr::Deref { span, ptr, type_ } => todo!(),
		}
	}
}

struct CFG {
	id: u64,
	segments: Vec<HIRBasicBlock>,
	curr: Vec<HIRStmt>,
	curr_id: u64,
}

impl CFG {
	fn new() -> Self {
		Self {
			id: 1,
			segments: vec![],
			curr: vec![],
			curr_id: 0,
		}
	}

	fn get_id(&mut self) -> u64 {
		let id = self.id;
		self.id += 1;
		id
	}

	fn terminate(&mut self, terminator: HIRTerminator, new_id: u64) {
		let stmts = mem::take(&mut self.curr);
		let id = self.curr_id;
		self.curr_id = new_id;
		self.segments.push(HIRBasicBlock {
			id,
			stmts,
			terminator,
		});
	}
}

struct Scope<'parent, 'ast> {
	parent: Option<&'parent Self>,
	defers: Vec<&'ast TypedExpr>,
	drops: Vec<Symbol>,
}

impl<'ast> Scope<'static, 'ast> {
	fn new() -> Self {
		Self {
			parent: None,
			defers: vec![],
			drops: vec![],
		}
	}
}

impl<'parent, 'ast> Scope<'parent, 'ast> {
	fn add_defer(&mut self, op: &'ast TypedExpr) {
		self.defers.push(op);
	}

	fn add_drop(&mut self, symbol: Symbol) {
		self.drops.push(symbol);
	}

	fn sub<'curr: 'parent>(&'curr self) -> Scope<'curr, 'ast> {
		Self {
			parent: Some(self),
			defers: vec![],
			drops: vec![],
		}
	}
}

struct Transformer {
	anon_strs: Vec<EcoString>,
}

impl Transformer {
	fn new() -> Self {
		Self { anon_strs: vec![] }
	}

	fn push_str_const(&mut self, str: EcoString) {
		self.anon_strs.push(str);
	}
}
