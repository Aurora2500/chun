pub mod ast;
pub mod error;
pub mod types;

use std::{
	collections::{hash_map::Entry, HashMap, HashSet},
	ops::Range,
};

use error::SemanticError;

use crate::parser;

#[derive(Default, Clone)]
struct Ctx<'a> {
	existing_funcs: HashMap<&'a str, parser::FnSignature>,
	variables: HashSet<&'a str>,
}

fn transform_expr(
	ast: (&parser::Expr, Range<usize>),
	ctx: &Ctx,
	errs: &mut Vec<SemanticError>,
) -> Option<ast::Expr> {
	//TODO: fix types
	let t = types::Primitive::Numeric(types::Numeric::Integral(types::Integral::I32));
	match ast.0 {
		parser::Expr::Literal(x) => {
			return Some(ast::Expr {
				r#type: t,
				expr: ast::ExprVariant::Literal(*x),
			});
		}
		parser::Expr::Var(v) => {
			if ctx.variables.contains(v.as_str()) {
				return Some(ast::Expr {
					r#type: t,
					expr: ast::ExprVariant::Var(v.clone()),
				});
			} else {
				return None;
			}
		}
		parser::Expr::FnCall { func, params } => {
			if !ctx.existing_funcs.contains_key(func.0.as_str()) {
				errs.push(SemanticError::RefNonExisting);
				return None;
			};
			let params: Option<Vec<_>> = params
				.iter()
				.map(|(expr, span)| transform_expr((expr, span.clone()), ctx, errs))
				.collect();
			let params = match params {
				Some(x) => x,
				None => return None,
			};
			return Some(ast::Expr {
				r#type: t,
				expr: ast::ExprVariant::FnCall {
					func: func.0.clone(),
					params,
				},
			});
		}
		parser::Expr::Unop { op, expr } => {
			let expr = (&expr.0, expr.1.clone());
			return transform_expr(expr, ctx, errs).map(|expr| ast::Expr {
				r#type: t,
				expr: ast::ExprVariant::Unop {
					op: *op,
					expr: Box::new(expr),
				},
			});
		}
		parser::Expr::Binop { op, lhs, rhs } => {
			let lhs = (&lhs.0, lhs.1.clone());
			let rhs = (&rhs.0, rhs.1.clone());
			let lhs = transform_expr(lhs, ctx, errs);
			let rhs = transform_expr(rhs, ctx, errs);
			return match (lhs, rhs) {
				(Some(lhs), Some(rhs)) => Some(ast::Expr {
					r#type: t,
					expr: ast::ExprVariant::Binop {
						op: *op,
						lhs: Box::new(lhs),
						rhs: Box::new(rhs),
					},
				}),
				_ => None,
			};
		}
	};
}

fn transform_stmt(
	stmt: &parser::Stmt,
	ctx: &Ctx,
	errs: &mut Vec<SemanticError>,
) -> Option<ast::Stmt> {
	match stmt {
		parser::Stmt::Binding {
			ident,
			r#type,
			value,
		} => {
			//TODO: type handling
			let value = transform_expr((&value.0, value.1.clone()), ctx, errs);
			let value = match value {
				Some(x) => x,
				None => return None,
			};
			return Some(ast::Stmt::Binding {
				ident: ident.0.clone(),
				r#type: types::Primitive::Numeric(types::Numeric::Integral(types::Integral::I32)),
				value,
			});
		}
		parser::Stmt::Expr(expr) => {
			return transform_expr((&expr.0, expr.1.clone()), ctx, errs).map(ast::Stmt::Expr);
		}
		_ => return todo!(),
	}
}

fn transform_fnparam(param: &parser::FnParam) -> ast::FnParam {
	ast::FnParam {
		name: param.name.0.clone(),
		r#type: types::Primitive::Numeric(types::Numeric::Integral(types::Integral::I32)),
	}
}

fn transform_fnsig(sig: &parser::FnSignature) -> ast::FnSignature {
	ast::FnSignature {
		ident: sig.ident.0.clone(),
		params: sig.params.iter().map(|x| transform_fnparam(&x.0)).collect(),
	}
}

pub fn transform(ast: &parser::Program) -> Result<ast::Program, Vec<SemanticError>> {
	let mut ctx = Ctx::default();
	let mut errs = Vec::new();
	let mut prog = vec![];

	for tl in ast.0.iter() {
		let signature = match tl {
			parser::TopLevelDef::Extern(func) => func,
			parser::TopLevelDef::Fn(parser::FnDef { sig, body: _ }) => sig,
		};
		let entry = ctx.existing_funcs.entry(&signature.ident.0);
		if matches! { entry,  Entry::Occupied(_) } {
			errs.push(SemanticError::DuplicateFunc {
				name: (*entry.key()).to_owned(),
			});
			continue;
		} else {
			entry.insert_entry(signature.clone());
		}
	}

	for tl in ast.0.iter() {
		let fndef = match &tl {
			parser::TopLevelDef::Fn(f) => f,
			parser::TopLevelDef::Extern(_) => continue,
		};
		let mut scope_ctx = ctx.clone();
		for (param, _) in fndef.sig.params.iter() {
			scope_ctx.variables.insert(&param.name.0);
		}
		let sig = transform_fnsig(&fndef.sig);
		let body = match &fndef.body.0 {
			parser::FnBody::Lambda(expr) => {
				let expr = (&expr.0, expr.1.clone());
				let body = transform_expr(expr, &scope_ctx, &mut errs);
				if let Some(body) = body {
					prog.push(ast::FnDef {
						sig,
						body: ast::FnBody::Lambda(body),
					});
				}
				continue;
			}
			parser::FnBody::Block(body) => body,
		};
		let body: Option<Vec<ast::Stmt>> = body
			.iter()
			.map(|(stmt, _)| {
				let new_stmt = transform_stmt(stmt, &scope_ctx, &mut errs);
				if let parser::Stmt::Binding { ident, .. } = stmt {
					scope_ctx.variables.insert(&ident.0);
				}
				new_stmt
			})
			.collect();
		if let Some(body) = body {
			prog.push(ast::FnDef {
				sig,
				body: ast::FnBody::Block(body),
			})
		}
	}
	println!("{errs:#?}");
	println!("{prog:#?}");
	if errs.is_empty() {
		Ok(ast::Program(prog))
	} else {
		Err(errs)
	}
}
