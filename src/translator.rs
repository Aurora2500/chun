use std::fmt::Display;

use crate::{
	parser::{Binop, Unop},
	transformer::{
		ast::{self, Expr, ExprVariant, FnBody, Stmt},
		types::{Float, Integral, Numeric, Primitive},
	},
};

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

fn primitive_to_ty(p: Primitive) -> &'static str {
	match p {
		Primitive::Numeric(num) => match num {
			Numeric::Integral(int) => match int {
				Integral::I32 | Integral::U32 => "w",
				Integral::I64 | Integral::U64 => "l",
			},
			Numeric::Float(float) => match float {
				Float::Single => "s",
				Float::Double => "d",
			},
		},
	}
}

fn translate_expr<'a>(
	expr: &'a Expr,
	immediate: Option<TempVar<'a>>,
	ctx: &mut Ctx,
	il: &mut String,
) -> TempVar<'a> {
	let Expr { r#type, expr } = expr;
	let val_type = primitive_to_ty(*r#type);
	match expr {
		ExprVariant::Literal(x) => {
			if let Some(var) = immediate {
				il.push_str(&format!("\t{var} = {val_type} copy {x}\n"));
			}
			TempVar::Constant(*x)
		}
		ExprVariant::Var(x) => {
			let old_var = TempVar::from_var(&x);
			if let Some(var) = immediate {
				il.push_str(&format!("\t{var} = {val_type} copy {old_var}"));
			}
			old_var
		}
		ExprVariant::Unop { op, expr } => {
			let expr = translate_expr(&expr, None, ctx, il);
			let var = if let Some(var) = immediate {
				var
			} else {
				ctx.new_intermediate()
			};
			let op = match *op {
				Unop::Neg => "neg",
				_ => todo!(),
			};
			il.push_str(&format!("\t{var} = {val_type} {op} {expr}\n"));
			var
		}
		ExprVariant::Binop { op, lhs, rhs } => {
			let lhs_var = translate_expr(&lhs, None, ctx, il);
			let rhs_var = translate_expr(&rhs, None, ctx, il);
			let var = if let Some(var) = immediate {
				var
			} else {
				ctx.new_intermediate()
			};
			let op = match *op {
				Binop::Add => "add",
				Binop::Sub => "sub",
				Binop::Mul => "mul",
				Binop::Div => "div",
				Binop::Rem => "rem",
				_ => todo!(),
			};
			il.push_str(&format!("\t{var} = {val_type} {op} {lhs_var}, {rhs_var}\n"));
			var
		}
		ExprVariant::FnCall { func, params } => {
			let param_vars: Vec<(TempVar<'a>, Primitive)> = params
				.iter()
				.map(|expr| (translate_expr(&expr, None, ctx, il), expr.r#type))
				.collect();
			let var = if let Some(var) = immediate {
				var
			} else {
				ctx.new_intermediate()
			};
			il.push_str(&format!("\t{var} = {val_type} call ${} (", func));
			for (param, param_ty) in param_vars {
				let param_ty = primitive_to_ty(param_ty);
				il.push_str(&format!("{param_ty} {param}, "));
			}
			il.push_str(")\n");
			var
		}
	}
}

fn translate_stmt(stmt: &Stmt, ctx: &mut Ctx, il: &mut String) {
	match stmt {
		Stmt::Expr(expr) => {
			translate_expr(&expr, None, ctx, il);
		}
		Stmt::Binding {
			ident,
			r#type: _,
			value,
		} => {
			let new_var = TempVar::from_var(&ident);
			translate_expr(&value, Some(new_var), ctx, il);
		}
		_ => todo!(),
	}
}

pub fn translate(ast: &ast::Program) -> String {
	let mut il = String::new();

	for func in ast.0.iter() {
		let mut ctx = Ctx::default();

		il.push_str("export function w $");
		il.push_str(&func.sig.ident);
		il.push_str("(");
		for param in func.sig.params.iter() {
			il.push_str("w %var_");
			il.push_str(&param.name);
			il.push_str(", ");
		}
		il.push_str(") {\n");
		il.push_str("@entry\n");
		match &func.body {
			FnBody::Lambda(expr) => {
				let var = translate_expr(&expr, None, &mut ctx, &mut il);
				il.push_str(&format!("\tret {}\n", var));
			}
			FnBody::Block(stmts) => {
				for stmt in stmts {
					translate_stmt(&stmt, &mut ctx, &mut il);
				}
				il.push_str("\tret 0\n");
			}
		}
		il.push_str("}\n\n");
	}

	il
}
