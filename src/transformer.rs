pub mod error;
pub mod inference;

use std::{
	collections::{hash_map::Entry, HashMap, HashSet},
	ops::Range,
};

use error::SemanticError;
use inference::{transform_to_monotype, UniCount, UniType};

use crate::{
	parser,
	types::{ast, parse_primitive, MonoType},
};

#[derive(Default, Clone)]
struct Ctx<'a> {
	existing_funcs: HashMap<&'a str, ast::FnSignature<'a>>,
	variables: HashSet<&'a str>,
	univar_count: UniCount,
}

impl Ctx<'_> {
	pub fn new_uni(&mut self) -> UniType {
		self.univar_count.new_uni()
	}
}

fn transform_expr<'a>(
	ast: (&'a parser::Expr, Range<usize>),
	ctx: &mut Ctx,
	errs: &mut Vec<SemanticError>,
) -> Option<ast::Expr<'a, UniType>> {
	//TODO: fix types
	match ast.0 {
		parser::Expr::Literal(x) => {
			return Some(ast::Expr {
				r#type: ctx.new_uni(),
				expr: ast::ExprVariant::Literal(*x),
			});
		}
		parser::Expr::Var(v) => {
			if ctx.variables.contains(v.as_str()) {
				return Some(ast::Expr {
					r#type: ctx.new_uni(),
					expr: ast::ExprVariant::Var(v),
				});
			} else {
				return None;
			}
		}
		parser::Expr::FnCall { func, params } => {
			if !ctx.existing_funcs.contains_key(func.0.as_str()) {
				errs.push(SemanticError::RefNonExistingFunc {
					span: func.1.clone(),
				});
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
				r#type: ctx.new_uni(),
				expr: ast::ExprVariant::FnCall {
					func: &func.0,
					params,
				},
			});
		}
		parser::Expr::Unop { op, expr } => {
			let expr = (&expr.0, expr.1.clone());
			return transform_expr(expr, ctx, errs).map(|expr| ast::Expr {
				r#type: ctx.new_uni(),
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
					r#type: ctx.new_uni(),
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

fn transform_stmt<'a>(
	stmt: &'a parser::Stmt,
	ctx: &mut Ctx,
	errs: &mut Vec<SemanticError<'a>>,
) -> Option<ast::Stmt<'a, UniType>> {
	match stmt {
		parser::Stmt::Binding {
			ident: (ident, _),
			r#type,
			value,
		} => {
			let value = transform_expr((&value.0, value.1.clone()), ctx, errs)?;
			let r#type = match r#type {
				Some((ty, span)) => match parse_primitive(ty, span.clone()) {
					Ok(x) => UniType::Mono(MonoType::Primitive(x)),
					Err(e) => {
						errs.push(e);
						return None;
					}
				},
				None => ctx.new_uni(),
			};
			return Some(ast::Stmt::Binding {
				ident,
				value,
				r#type,
			});
		}
		parser::Stmt::Assignment {
			ident: (ident, _),
			expr,
		} => {
			let value = transform_expr((&expr.0, expr.1.clone()), ctx, errs)?;
			return Some(ast::Stmt::Assignment {
				ident,
				value,
				r#type: ctx.new_uni(),
			});
		}
		parser::Stmt::Expr(expr) => {
			return transform_expr((&expr.0, expr.1.clone()), ctx, errs).map(ast::Stmt::Expr);
		}
		parser::Stmt::If { condition, block } => {
			let cond = transform_expr((&condition.0, condition.1.clone()), ctx, errs)?;
			let block = block
				.iter()
				.map(|stmt| transform_stmt(&stmt.0, ctx, errs))
				.collect::<Option<_>>()?;
			return Some(ast::Stmt::If { cond, block });
		}
		parser::Stmt::While { condition, block } => {
			let cond = transform_expr((&condition.0, condition.1.clone()), ctx, errs)?;
			let block = block
				.iter()
				.map(|stmt| transform_stmt(&stmt.0, ctx, errs))
				.collect::<Option<_>>()?;
			return Some(ast::Stmt::While { cond, block });
		}
		//TODO: other expressions
		_ => todo!(),
	}
}

fn transform_fnparam(param: &parser::FnParam) -> Result<ast::FnParam, SemanticError> {
	let r#type = parse_primitive(&param.r#type.0, param.r#type.1.clone())?;
	Ok(ast::FnParam {
		name: &param.name.0,
		r#type: MonoType::Primitive(r#type),
	})
}

fn transform_fnsig<'a>(
	sig: &'a parser::FnSignature,
	errs: &mut Vec<SemanticError<'a>>,
) -> Option<ast::FnSignature<'a>> {
	let params = sig
		.params
		.iter()
		.map(|x| match transform_fnparam(&x.0) {
			Ok(p) => Some(p),
			Err(e) => {
				errs.push(e);
				None
			}
		})
		.collect::<Option<_>>()?;
	let return_type = match &sig.return_type {
		Some((ty, span)) => match parse_primitive(&ty, span.clone()) {
			Ok(ty) => Some(MonoType::Primitive(ty)),
			Err(e) => {
				errs.push(e);
				None
			}
		},
		None => Some(MonoType::Unit),
	}?;

	Some(ast::FnSignature {
		ident: &sig.ident.0,
		params,
		return_type,
	})
}

fn transform_to_uni_type<'a>(
	ast: &'a parser::Program,
	ctx: &mut Ctx<'a>,
	errs: &mut Vec<SemanticError<'a>>,
) -> ast::Program<'a, UniType> {
	let mut prog = vec![];
	for tl in ast.0.iter() {
		let fndef = match &tl {
			parser::TopLevelDef::Fn(f) => f,
			parser::TopLevelDef::Extern(_) => continue,
		};
		let mut scope_ctx = ctx.clone();
		for (param, _) in fndef.sig.params.iter() {
			scope_ctx.variables.insert(&param.name.0);
		}
		let sig = transform_fnsig(&fndef.sig, errs);
		let sig = match sig {
			None => continue,
			Some(x) => x,
		};
		let body = match &fndef.body.0 {
			parser::FnBody::Lambda(expr) => {
				let expr = (&expr.0, expr.1.clone());
				let body = transform_expr(expr, &mut scope_ctx, errs);
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
		let body = body
			.iter()
			.map(|(stmt, _)| {
				let new_stmt = transform_stmt(stmt, &mut scope_ctx, errs);
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
	ast::Program(prog)
}

pub fn transform(ast: &parser::Program) -> Result<ast::Program, Vec<SemanticError>> {
	let mut ctx = Ctx::default();
	let mut errs = Vec::new();

	for tl in ast.0.iter() {
		let signature = match tl {
			parser::TopLevelDef::Extern(func) => func,
			parser::TopLevelDef::Fn(parser::FnDef { sig, body: _ }) => sig,
		};
		let entry = ctx.existing_funcs.entry(&signature.ident.0);
		if matches! { entry,  Entry::Occupied(_) } {
			errs.push(SemanticError::DuplicateFunc {
				name: (*entry.key()),
			});
			continue;
		} else {
			let signature = transform_fnsig(signature, &mut errs);
			if let Some(signature) = signature {
				entry.insert_entry(signature);
			}
		}
	}

	let prog = transform_to_uni_type(&ast, &mut ctx, &mut errs);
	let prog = transform_to_monotype(prog, ctx, &mut errs);

	if errs.is_empty() {
		Ok(prog)
	} else {
		Err(errs)
	}
}
