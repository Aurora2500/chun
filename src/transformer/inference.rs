// does inference using algorithm W

use std::collections::HashMap;

use crate::types::{
	ast::{self},
	MonoType, Primitive,
};

use super::{error::SemanticError, Ctx};

type Unifying = i32;

#[derive(Debug, Default, Clone, Copy)]
pub struct UniCount(Unifying);

impl UniCount {
	pub fn new_uni(&mut self) -> UniType {
		let u = UniType::Uni(self.0);
		self.0 += 1;
		u
	}
}

#[derive(Debug, Clone)]
pub enum UniType {
	Uni(Unifying),
	Mono(MonoType),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Constraint {
	Integral,
	Floating,
	Numeric,
}

fn compatible_constraints(a: Constraint, b: Constraint) -> bool {
	match (a, b) {
		(x, y) if x == y => true,
		(Constraint::Numeric, _) | (_, Constraint::Numeric) => true,
		_ => false,
	}
}

#[derive(Debug, Default, Clone)]
pub struct Subst {
	substitutions: HashMap<Unifying, UniType>,
	constraints: HashMap<Unifying, UniType>,
}

impl Subst {
	pub fn new() -> Self {
		Self::default()
	}

	pub fn apply(&self, ty: &UniType) -> UniType {
		match ty {
			UniType::Uni(x) => self
				.substitutions
				.get(x)
				.cloned()
				.unwrap_or(UniType::Uni(*x)),
			x => x.clone(),
		}
	}

	pub fn unify(&mut self, a: &UniType, b: &UniType) -> Option<UniType> {
		let a = self.apply(a);
		let b = self.apply(b);

		match (a, b) {
			(UniType::Uni(x), UniType::Uni(y)) => {
				let (x, y) = if x < y { (x, y) } else { (y, x) };
				self.substitutions.insert(x, UniType::Uni(y));
				return Some(UniType::Uni(y));
			}
			(UniType::Uni(x), ty) | (ty, UniType::Uni(x)) => {
				self.substitutions.insert(x, ty.clone());
				return Some(ty);
			}
			(UniType::Mono(a), UniType::Mono(b)) => {
				if a == b {
					return Some(UniType::Mono(a));
				} else {
					return None;
				}
			}
		}
	}

	pub fn monomorph<'a: 'b, 'b>(&'a self, mut ty: &'b UniType) -> Option<MonoType> {
		loop {
			match ty {
				UniType::Mono(x) => return Some(x.clone()),
				UniType::Uni(t) => match self.substitutions.get(&t) {
					None => return None,
					Some(t) => ty = t,
				},
			}
		}
	}
}

#[derive(Debug, Default, Clone)]
struct Inferencer<'a> {
	vars: HashMap<&'a str, UniType>,
	subst: Subst,
}

impl<'a> Inferencer<'a> {
	fn new() -> Self {
		Self::default()
	}

	fn read_sig(&mut self, sig: &ast::FnSignature<'a>) {
		for param in sig.params.iter() {
			self.vars
				.insert(param.name, UniType::Mono(param.r#type.clone()));
		}
	}

	pub fn unify_expr(
		&mut self,
		expr: &ast::Expr<'a, UniType>,
		ctx: &Ctx<'a>,
		errs: &mut Vec<SemanticError<'a>>,
	) -> Option<UniType> {
		use ast::ExprVariant;
		let ast::Expr {
			expr,
			r#type: expr_ty,
		} = expr;
		match expr {
			ExprVariant::Var(v) => {
				let existing_v = self
					.vars
					.get(v)
					.expect("Should have caught unreferenced variables by now");
				return self.subst.unify(existing_v, expr_ty);
			}
			ExprVariant::Literal(x) => {
				return Some(expr_ty.clone());
			}
			ExprVariant::Binop { op, lhs, rhs } => {
				let lhs_ty = self.unify_expr(lhs, ctx, errs)?;
				let rhs_ty = self.unify_expr(rhs, ctx, errs)?;
				if let Some(res_ty) = self.subst.unify(&lhs_ty, &rhs_ty) {
					self.subst.unify(expr_ty, &rhs_ty);
					Some(res_ty)
				} else {
					errs.push(SemanticError::IncompatibleTypes {
						a: lhs_ty,
						b: rhs_ty,
					});
					None
				}
			}
			ExprVariant::Unop { op, expr } => {
				let ty = self.unify_expr(expr, ctx, errs);
				if let Some(ty) = &ty {
					self.subst.unify(expr_ty, ty);
				}
				return ty;
			}
			ExprVariant::FnCall { func, params } => {
				let sig = ctx.existing_funcs.get(func)?;
				for (p, s) in params.iter().zip(sig.params.iter()) {
					let sig_ty = UniType::Mono(s.r#type.clone());
					let p_ty = self.unify_expr(p, ctx, errs);
					if let Some(p_ty) = p_ty {
						if self.subst.unify(&sig_ty, &p_ty).is_none() {
							errs.push(SemanticError::IncompatibleTypes { a: sig_ty, b: p_ty });
						}
					} else {
					}
				}
				let ret_ty = UniType::Mono(sig.return_type.clone());
				let sub_res = self.subst.unify(expr_ty, &ret_ty);
				if sub_res.is_none() {
					errs.push(SemanticError::IncompatibleTypes {
						a: expr_ty.clone(),
						b: ret_ty,
					});
				}
				sub_res
			}
		}
	}

	pub fn unify_stmt(
		&mut self,
		stmt: &ast::Stmt<'a, UniType>,
		ctx: &mut Ctx<'a>,
		errs: &mut Vec<SemanticError<'a>>,
	) {
		use ast::Stmt;
		match stmt {
			Stmt::Binding {
				ident,
				r#type,
				value,
			} => {
				let expr_ty = match self.unify_expr(value, ctx, errs) {
					Some(t) => t,
					None => return,
				};
				self.vars.insert(&ident, r#type.clone());
				self.subst.unify(r#type, &expr_ty);
			}
			Stmt::Expr(expr) => {
				self.unify_expr(expr, ctx, errs);
			}
			_ => todo!(),
		}
	}
}

fn monomorph_expr<'a>(
	expr: &ast::Expr<'a, UniType>,
	infr: &mut Inferencer<'a>,
	errs: &mut Vec<SemanticError<'a>>,
) -> Option<ast::Expr<'a>> {
	let ast::Expr {
		expr,
		r#type: expr_ty,
	} = expr;
	let r#type = infr.subst.monomorph(expr_ty)?;

	Some(ast::Expr {
		r#type,
		expr: match expr {
			ast::ExprVariant::Literal(x) => ast::ExprVariant::Literal(*x),
			ast::ExprVariant::Var(v) => ast::ExprVariant::Var(v),
			ast::ExprVariant::Unop { op, expr } => {
				let expr = monomorph_expr(expr, infr, errs)?;
				ast::ExprVariant::Unop {
					op: *op,
					expr: Box::new(expr),
				}
			}
			ast::ExprVariant::Binop { op, lhs, rhs } => {
				let lhs = monomorph_expr(lhs, infr, errs);
				let rhs = monomorph_expr(rhs, infr, errs);
				ast::ExprVariant::Binop {
					op: *op,
					lhs: Box::new(lhs?),
					rhs: Box::new(rhs?),
				}
			}
			ast::ExprVariant::FnCall { func, params } => {
				let params = params
					.iter()
					.map(|e| monomorph_expr(e, infr, errs))
					.collect::<Option<_>>()?;
				ast::ExprVariant::FnCall { func, params }
			}
		},
	})
}

fn monomorph_stmt<'a>(
	stmt: &ast::Stmt<'a, UniType>,
	infr: &mut Inferencer<'a>,
	errs: &mut Vec<SemanticError<'a>>,
) -> Option<ast::Stmt<'a>> {
	use ast::Stmt;
	Some(match stmt {
		Stmt::Binding {
			ident,
			r#type,
			value,
		} => {
			let r#type = infr.subst.monomorph(r#type)?;
			let value = monomorph_expr(value, infr, errs)?;
			Stmt::Binding {
				ident,
				r#type,
				value,
			}
		}
		Stmt::Expr(expr) => {
			let expr = monomorph_expr(expr, infr, errs)?;
			Stmt::Expr(expr)
		}
		_ => todo!(),
	})
}

pub(super) fn transform_to_monotype<'a>(
	prog: ast::Program<'a, UniType>,
	ctx: Ctx<'a>,
	errs: &mut Vec<SemanticError<'a>>,
) -> ast::Program<'a> {
	let mut new_prog = vec![];

	for ast::FnDef { sig, body } in prog.0 {
		let mut infr = Inferencer::new();
		infr.read_sig(&sig);
		let body = match body {
			ast::FnBody::Lambda(expr) => {
				infr.unify_expr(&expr, &ctx, errs);
				let expr = match monomorph_expr(&expr, &mut infr, errs) {
					Some(e) => e,
					None => continue,
				};
				new_prog.push(ast::FnDef {
					sig,
					body: ast::FnBody::Lambda(expr),
				});
				continue;
			}
			ast::FnBody::Block(b) => b,
		};
		let mut scope_ctx = ctx.clone();

		for stmt in body.iter() {
			infr.unify_stmt(stmt, &mut scope_ctx, errs);
		}

		let body = body
			.iter()
			.map(|stmt| monomorph_stmt(stmt, &mut infr, errs))
			.collect::<Option<_>>();
		if let Some(body) = body {
			new_prog.push(ast::FnDef {
				sig,
				body: ast::FnBody::Block(body),
			});
		}
	}

	ast::Program(new_prog)
}
