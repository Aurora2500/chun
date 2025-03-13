use crate::parser::{Expr, FnBody, Program, Stmt};

#[derive(Debug, Clone)]
enum TempVar<'a> {
	Intermediate(u8),
	Named(&'a str),
}

impl<'a> TempVar<'a> {
	fn new(id: u8) -> Self {
		Self::Intermediate(id)
	}

	fn from_var(name: &'a str) -> Self {
		Self::Named(name)
	}

	fn refer(&self, il: &mut String) {
		match self {
			Self::Intermediate(x) => {
				il.push_str(&std::format!("%temp_{}", x));
			}
			Self::Named(x) => {
				il.push_str(&std::format!("%var_{}", x));
			}
		}
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

fn translate_expr<'a>(expr: &'a Expr, ctx: &mut Ctx, il: &mut String) -> TempVar<'a> {
	match expr {
		Expr::Literal(x) => {
			let temp = ctx.new_intermediate();
			il.push_str("\t");
			temp.refer(il);
			il.push_str(&std::format!(" = w copy {}\n", x));
			return temp;
		}
		Expr::Var(x) => TempVar::from_var(&x),
		Expr::Add { lhs, rhs } => {
			let lhs_var = translate_expr(lhs, ctx, il);
			let rhs_var = translate_expr(rhs, ctx, il);
			let temp = ctx.new_intermediate();
			il.push_str("\t");
			temp.refer(il);
			il.push_str(" = w add ");
			lhs_var.refer(il);
			il.push_str(", ");
			rhs_var.refer(il);
			il.push_str("\n");

			return temp;
		}
		Expr::FnCall { func, params } => {
			let param_vars: Vec<TempVar<'a>> = params
				.iter()
				.map(|expr| translate_expr(expr, ctx, il))
				.collect();
			let temp = ctx.new_intermediate();
			il.push_str("\t");
			temp.refer(il);
			il.push_str(" = w call $");
			il.push_str(func);
			il.push_str(" (");
			for v in param_vars {
				il.push_str("w ");
				v.refer(il);
				il.push_str(", ");
			}
			il.push_str(")\n");
			return temp;
		}
	}
}

fn translate_stmt(stmt: &Stmt, ctx: &mut Ctx, il: &mut String) {
	match stmt {
		Stmt::Expr(expr) => {
			translate_expr(expr, ctx, il);
		}
		Stmt::Binding { ident, value } => {
			let expr_var = translate_expr(value, ctx, il);
			let new_var = TempVar::from_var(ident);
			il.push_str("\t");
			new_var.refer(il);
			il.push_str(" = w copy ");
			expr_var.refer(il);
			il.push_str("\n");
		}
	}
}

pub fn translate(ast: &Program) -> String {
	let mut il = String::new();

	for func in ast.0.iter() {
		let mut ctx = Ctx::default();

		il.push_str("export function w $");
		il.push_str(&func.ident);
		il.push_str("(");
		for param in func.params.iter() {
			il.push_str("w %var_");
			il.push_str(&param.name);
			il.push_str(", ");
		}
		il.push_str(") {\n");
		il.push_str("@entry\n");
		match &func.body {
			FnBody::Lambda(expr) => {
				let var = translate_expr(expr, &mut ctx, &mut il);
				il.push_str("\tret ");
				var.refer(&mut il);
				il.push_str("\n");
			}
			FnBody::Block(stmts) => {
				for stmt in stmts {
					translate_stmt(stmt, &mut ctx, &mut il);
				}
				il.push_str("\tret 0\n");
			}
		}
		il.push_str("}\n");
	}

	il
}
