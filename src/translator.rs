use std::fmt::Display;

use crate::{
	parser::{Binop, Unop},
	types::{
		ast::{self, Expr, ExprVariant, FnBody, Stmt},
		MonoType, Primitive,
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

struct Block(u8);

impl Display for Block {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "@block_{}", self.0)
	}
}

#[derive(Debug, Default)]
struct Ctx {
	temp_count: u8,
	block_count: u8,
}

impl Ctx {
	fn new_intermediate(&mut self) -> TempVar<'static> {
		let tv = TempVar::new(self.temp_count);
		self.temp_count += 1;
		tv
	}

	fn new_block(&mut self) -> Block {
		let b = Block(self.block_count);
		self.block_count += 1;
		b
	}
}

fn mono_to_ty(m: MonoType) -> Option<&'static str> {
	match m {
		MonoType::Primitive(p) => Some(match p {
			Primitive::I32 | Primitive::U32 => "w",
			Primitive::I64 | Primitive::U64 => "l",
			Primitive::F32 => "s",
			Primitive::F64 => "d",
		}),
		_ => None,
	}
}

fn assert_primitive(m: MonoType) -> Primitive {
	match m {
		MonoType::Primitive(p) => p,
		MonoType::Unit => panic!("assert monotype to primitive failed"),
	}
}

fn translate_expr<'a>(
	expr: &'a Expr,
	immediate: Option<TempVar<'a>>,
	ctx: &mut Ctx,
	il: &mut String,
) -> Option<TempVar<'a>> {
	let Expr { r#type, expr } = expr;
	let val_type = mono_to_ty(r#type.clone());
	match expr {
		ExprVariant::Literal(x) => {
			let val_type = val_type.unwrap();
			if let Some(var) = immediate {
				il.push_str(&format!("\t{var} = {val_type} copy {x}\n"));
			}
			Some(TempVar::Constant(*x))
		}
		ExprVariant::Var(x) => {
			let val_type = val_type.unwrap();
			let old_var = TempVar::from_var(&x);
			if let Some(var) = immediate {
				il.push_str(&format!("\t{var} = {val_type} copy {old_var}"));
			}
			Some(old_var)
		}
		ExprVariant::Unop { op, expr } => {
			let val_type = val_type.unwrap();
			let expr = translate_expr(&expr, None, ctx, il).expect("Unit used as value");
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
			Some(var)
		}
		ExprVariant::Binop { op, lhs, rhs } => {
			let val_type = val_type.unwrap();
			let lhs_var = translate_expr(&lhs, None, ctx, il).expect("unit used as value");
			let rhs_var = translate_expr(&rhs, None, ctx, il).expect("unit used as value");
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
			Some(var)
		}
		ExprVariant::FnCall { func, params } => {
			let param_vars: Vec<(Option<TempVar<'a>>, Primitive)> = params
				.iter()
				.map(|expr| {
					(
						translate_expr(&expr, None, ctx, il),
						assert_primitive(expr.r#type.clone()),
					)
				})
				.collect();
			let var = if let Some(var) = immediate {
				var
			} else {
				ctx.new_intermediate()
			};
			il.push('\t');
			if let Some(val_type) = val_type {
				il.push_str(&format!("{var} = {val_type} "));
			}
			il.push_str(&format!("call ${} (", func));
			for (param, param_ty) in param_vars {
				let param_ty = mono_to_ty(MonoType::Primitive(param_ty))
					.expect("param type shouldn't be unit");
				let param = param.expect("param should be referable");
				il.push_str(&format!("{param_ty} {param}, "));
			}
			il.push_str(")\n");
			if val_type.is_some() {
				Some(var)
			} else {
				None
			}
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
		Stmt::Assignment {
			ident,
			r#type: _,
			value,
		} => {
			let new_var = TempVar::from_var(&ident);
			translate_expr(&value, Some(new_var), ctx, il);
		}
		Stmt::If { cond, block } => {
			let if_block = ctx.new_block();
			let merge_block = ctx.new_block();

			let cond = translate_expr(cond, None, ctx, il).expect("needs a tempvar");
			il.push_str(&format!("\tjnz {cond}, {if_block}, {merge_block}\n"));
			il.push_str(&format!("{if_block}\n"));
			for stmt in block {
				translate_stmt(stmt, ctx, il);
			}
			il.push_str(&format!("{merge_block}\n"));
		}
		Stmt::While { cond, block } => {
			let test_block = ctx.new_block();
			let loop_block = ctx.new_block();
			let merge_block = ctx.new_block();

			il.push_str(&format!("{test_block}\n"));
			let cond = translate_expr(cond, None, ctx, il).expect("needs a tempvar");
			il.push_str(&format!("\tjnz {cond}, {loop_block}, {merge_block}\n"));
			il.push_str(&format!("{loop_block}\n"));
			for stmt in block {
				translate_stmt(stmt, ctx, il);
			}
			il.push_str(&format!("\tjmp {test_block}\n"));
			il.push_str(&format!("{merge_block}\n"));
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
				il.push_str("\tret");
				if let Some(var) = var {
					il.push_str(&format!(" {var}"));
				}
				il.push('\n');
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
