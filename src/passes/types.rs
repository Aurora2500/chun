use std::{collections::HashMap, sync::Arc};

use crate::{
	ast::{
		FnParam, FnParamNormal, ResolvedBlock, ResolvedDefinition, ResolvedFnDecl, ResolvedFnDef,
		ResolvedFnParam, ResolvedModule, ResolvedStatement, TypeAst, TypedBlock, TypedDefinition,
		TypedFnDecl, TypedFnDef, TypedFnParam, TypedModule, TypedStatement, resolved::ResolvedExpr,
		typed::TypedExpr,
	},
	passes::symbols::{Symbol, SymbolId},
	types::{
		Type,
		prelude::{fun, named, optional, ptr, unit, unitype},
	},
};

pub fn type_pass(ast: &ResolvedModule) -> TypedModule {
	let mut inf = Inferencer::new();
	inf.top_pass(ast);
	inf.infer(ast)
}

impl Inferencer {
	fn top_pass(&mut self, ast: &ResolvedModule) {
		for fndef in ast.definitions.fn_defs.iter() {
			let params = fndef
				.params
				.iter()
				.map(|p| match p {
					crate::ast::FnParam::Receiver(fn_param_receiver) => todo!(),
					crate::ast::FnParam::Normal(FnParamNormal { annotation, .. }) => {
						self.infer_annot(annotation)
					}
				})
				.collect();
			let ret = fndef
				.return_annotation
				.as_ref()
				.map_or_else(unit, |a| self.infer_annot(&a));

			self.type_symbol(&fndef.name, fun(params, ret));
		}

		for fndecl in ast.definitions.fn_decls.iter() {
			let params = fndecl
				.params
				.iter()
				.map(|p| match p {
					crate::ast::FnParam::Receiver(fn_param_receiver) => todo!(),
					crate::ast::FnParam::Normal(FnParamNormal { annotation, .. }) => {
						self.infer_annot(annotation)
					}
				})
				.collect();
			let ret = fndecl
				.ret_annot
				.as_ref()
				.map_or_else(unit, |a| self.infer_annot(&a));

			self.type_symbol(&fndecl.name, fun(params, ret));
		}
	}

	fn infer(&mut self, ast: &ResolvedModule) -> TypedModule {
		let ResolvedModule {
			definitions:
				ResolvedDefinition {
					structs,
					enums,
					unions,
					fn_defs,
					fn_decls,
				},
		} = ast;

		let structs = structs.iter().map(|s| todo!()).collect();
		let enums = enums.iter().map(|e| todo!()).collect();
		let unions = unions.iter().map(|u| todo!()).collect();
		let fn_defs = fn_defs.iter().map(|f| self.infer_fndef(f)).collect();
		let fn_decls = fn_decls.iter().map(|f| self.infer_fndecl(f)).collect();

		let definitions = TypedDefinition {
			structs,
			enums,
			unions,
			fn_defs,
			fn_decls,
		};
		TypedModule { definitions }
	}

	pub fn infer_fndecl(&mut self, ast: &ResolvedFnDecl) -> TypedFnDecl {
		let ResolvedFnDecl {
			span,
			attrs,
			publicity,
			name,
			params,
			ret_annot,
			ret_type,
			type_,
		} = ast.clone();

		let params = params.into_iter().map(|p| self.infer_fn_param(p)).collect();
		let ret_type = ret_annot
			.as_ref()
			.map_or_else(unit, |a| self.infer_annot(a));
		let type_ = self.lookup_symbol(&name).unwrap();

		TypedFnDecl {
			span,
			attrs,
			publicity,
			name,
			params,
			ret_annot,
			ret_type,
			type_,
		}
	}

	pub fn infer_fndef(&mut self, ast: &ResolvedFnDef) -> TypedFnDef {
		let ResolvedFnDef {
			span,
			attrs,
			name,
			params,
			return_annotation,
			body,
			..
		} = ast.clone();

		let params = params.into_iter().map(|p| self.infer_fn_param(p)).collect();
		let body = self.infer_block(body);

		TypedFnDef {
			span,
			attrs,
			name,
			params,
			return_type: return_annotation
				.as_ref()
				.map_or_else(unit, |a| self.infer_annot(a)),
			return_annotation,
			body,
		}
	}

	pub fn infer_fn_param(&mut self, ast: ResolvedFnParam) -> TypedFnParam {
		match ast {
			FnParam::Receiver(fn_param_receiver) => todo!(),
			FnParam::Normal(FnParamNormal {
				span,
				attrs,
				binding,
				annotation,
				type_,
			}) => FnParam::Normal(FnParamNormal {
				span,
				attrs,
				binding,
				type_: self.infer_annot(&annotation),
				annotation,
			}),
		}
	}

	fn infer_annot(&mut self, ast: &TypeAst) -> Arc<Type> {
		match ast {
			TypeAst::Named(type_ast_named) => {
				named(type_ast_named.name.path.last().unwrap().name.clone())
			}
			TypeAst::Ptr { span, pointee } => ptr(self.infer_annot(&pointee)),
			TypeAst::Optional { span, inner } => optional(self.infer_annot(&inner)),
			TypeAst::Sliced { span, item } => todo!(),
			TypeAst::Array { span, len, item } => todo!(),
			TypeAst::Wildcard { span } => todo!(),
		}
	}

	fn infer_block(&mut self, block: ResolvedBlock) -> TypedBlock {
		let ResolvedBlock { span, stmts, tail } = block;

		let stmts = stmts
			.into_iter()
			.map(|stmt| self.infer_stmt(stmt))
			.collect();

		let tail = tail.map(|t| self.infer_expr(t));

		TypedBlock { span, stmts, tail }
	}

	fn infer_stmt(&mut self, stmt: ResolvedStatement) -> TypedStatement {
		match stmt {
			crate::ast::Statement::Expr { span, expr } => TypedStatement::Expr {
				span,
				expr: self.infer_expr(expr),
			},
			crate::ast::Statement::Let {
				span,
				binding,
				annotation,
				type_,
				val,
			} => todo!(),
			crate::ast::Statement::ErrDefer { span, op } => todo!(),
			crate::ast::Statement::Defer { span, op } => TypedStatement::Defer {
				span,
				op: self.infer_expr(op),
			},
		}
	}

	fn infer_expr(&mut self, expr: ResolvedExpr) -> TypedExpr {
		match expr {
			ResolvedExpr::Binding { span, binding } => {
				let type_ = self.lookup_symbol(&binding).expect("not handling this yet");
				TypedExpr::Binding {
					span,
					binding,
					type_,
				}
			}
			ResolvedExpr::StrLiteral { span, value } => TypedExpr::StrLiteral {
				span,
				value,
				type_: named("str".into()),
			},
			ResolvedExpr::IntLiteral {
				span,
				value,
				int_value,
				suffix,
			} => {
				let type_ = match &suffix {
					Some(s) => named(s.clone()),
					None => self.unbound(),
				};
				TypedExpr::IntLiteral {
					span,
					value,
					int_value,
					suffix,
					type_,
				}
			}
			ResolvedExpr::FloatLiteral {
				span,
				value,
				suffix,
			} => todo!(),
			ResolvedExpr::BoolLiteral { span, value } => todo!(),
			ResolvedExpr::Unit { span } => TypedExpr::Unit {
				span,
				type_: unit(),
			},
			ResolvedExpr::TupleLiteral { span, values } => todo!(),
			ResolvedExpr::NamedStructLiteral { span, name, fields } => todo!(),
			ResolvedExpr::Block(block) => todo!(),
			ResolvedExpr::If {
				span,
				cond,
				then,
				else_if,
				else_,
			} => todo!(),
			ResolvedExpr::For {
				span,
				iterator,
				body,
			} => todo!(),
			ResolvedExpr::While { span, cond, body } => {
				let cond = self.infer_expr(*cond);
				let body = self.infer_block(*body);
				TypedExpr::While {
					span,
					cond: Box::new(cond),
					body: Box::new(body),
					type_: self.unbound(),
				}
			}
			ResolvedExpr::Loop(block) => todo!(),
			ResolvedExpr::Match { span, on, arms } => todo!(),
			ResolvedExpr::Binop { span, lhs, op, rhs } => todo!(),
			ResolvedExpr::Unop { span, op, on } => todo!(),
			ResolvedExpr::Group { span, inner } => todo!(),
			ResolvedExpr::CallLike { span, func, args } => {
				let func = self.infer_expr(*func);
				let fn_type = func.get_type();
				let args = args.into_iter().map(|a| self.infer_expr(a)).collect();
				TypedExpr::CallLike {
					span,
					func: Box::new(func),
					args,
					type_: self.unbound(),
				}
			}
			ResolvedExpr::Index { span, val, at } => todo!(),
			ResolvedExpr::TupleIndex { span, val, idx } => todo!(),
			ResolvedExpr::FieldAccess { span, val, field } => todo!(),
			ResolvedExpr::MethodCall {
				span,
				val,
				method,
				args,
			} => todo!(),
			ResolvedExpr::Deref { span, ptr } => todo!(),
		}
	}
}

#[derive(Default)]
struct Substitutions {
	map: HashMap<u64, Arc<Type>>,
}

#[derive(Default)]
struct Inferencer {
	uni_id: u64,
	subst: Substitutions,
	symbols: HashMap<SymbolId, Arc<Type>>,
}

impl Inferencer {
	fn new() -> Self {
		Self::default()
	}

	fn lookup_symbol(&self, sym: &Symbol) -> Option<Arc<Type>> {
		self.symbols.get(&sym.id).map(Arc::clone)
	}

	fn type_symbol(&mut self, sym: &Symbol, ty: Arc<Type>) {
		self.symbols.insert(sym.id, ty);
	}

	fn unbound(&mut self) -> Arc<Type> {
		let t = unitype(self.uni_id);
		self.uni_id += 1;
		t
	}
}
