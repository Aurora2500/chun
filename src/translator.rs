use std::fmt::Display;

use crate::parser::{Expr, FnBody, Program, Stmt, TopLevelDef};

#[derive(Debug, Clone, Copy)]
enum TempVar<'a> {
	Intermediate(u8),
	Named(&'a str),
	Constant(i32),
}

impl Display for TempVar<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Intermediate(x) => write!(f, "%temp_{}", x),
			Self::Named(x) => write!(f, "%var_{}", x),
			Self::Constant(x) => write!(f, "{}", x),
		}
	}
}

impl<'a> TempVar<'a> {
	fn new(id: u8) -> Self {
		Self::Intermediate(id)
	}

	fn from_var(name: &'a str) -> Self {
		Self::Named(name)
	}
}

#[derive(Debug, Default)]
struct Ctx {
	temp_count: u8,
}

impl Ctx {
	fn new_intermediate(&mut self) -> TempVar<'static> {
		let tv = TempVar::new(self.temp_count);
		self.temp_count += 1;
		return tv;
	}
}

fn translate_expr<'a>(
	expr: &'a Expr,
	immediate: Option<TempVar<'a>>,
	ctx: &mut Ctx,
	il: &mut String,
) -> TempVar<'a> {
	match expr {
		Expr::Literal(x) => {
			if let Some(var) = immediate {
				il.push_str(&format!("\t{var} = w copy {x}\n"));
			}
			TempVar::Constant(*x)
		}
		Expr::Var(x) => {
			let old_var = TempVar::from_var(&x);
			if let Some(var) = immediate {
				il.push_str(&format!("\t{var} = w copy {old_var}"));
			}
			old_var
		}
		Expr::Add { lhs, rhs } => {
			let lhs_var = translate_expr(&lhs.0, None, ctx, il);
			let rhs_var = translate_expr(&rhs.0, None, ctx, il);
			let var = if let Some(var) = immediate {
				var
			} else {
				ctx.new_intermediate()
			};
			il.push_str(&format!("\t{var} = w add {lhs_var}, {rhs_var}\n"));

			var
		}
		Expr::FnCall { func, params } => {
			let param_vars: Vec<TempVar<'a>> = params
				.iter()
				.map(|expr| translate_expr(&expr.0, None, ctx, il))
				.collect();
			let var = if let Some(var) = immediate {
				var
			} else {
				ctx.new_intermediate()
			};
			il.push_str(&format!("\t{var} = w call ${} (", func.0));
			for v in param_vars {
				il.push_str(&format!("w {v}, "));
			}
			il.push_str(")\n");
			var
		}
	}
}

fn translate_stmt(stmt: &Stmt, ctx: &mut Ctx, il: &mut String) {
	match stmt {
		Stmt::Expr(expr) => {
			translate_expr(&expr.0, None, ctx, il);
		}
		Stmt::Binding {
			ident,
			r#type: _,
			value,
		} => {
			let new_var = TempVar::from_var(&ident.0);
			translate_expr(&value.0, Some(new_var), ctx, il);
		}
	}
}

pub fn translate(ast: &Program) -> String {
	let mut il = String::new();

	for top_level_def in ast.0.iter() {
		let func = match top_level_def {
			TopLevelDef::Fn(func) => func,
			TopLevelDef::Extern(_) => continue,
		};
		let mut ctx = Ctx::default();

		il.push_str("export function w $");
		il.push_str(&func.ident.0);
		il.push_str("(");
		for param in func.params.iter() {
			il.push_str("w %var_");
			il.push_str(&param.0.name.0);
			il.push_str(", ");
		}
		il.push_str(") {\n");
		il.push_str("@entry\n");
		match &func.body.0 {
			FnBody::Lambda(expr) => {
				let var = translate_expr(&expr.0, None, &mut ctx, &mut il);
				il.push_str(&format!("\tret {}\n", var));
			}
			FnBody::Block(stmts) => {
				for stmt in stmts {
					translate_stmt(&stmt.0, &mut ctx, &mut il);
				}
				il.push_str("\tret 0\n");
			}
		}
		il.push_str("}\n\n");
	}

	il
}
