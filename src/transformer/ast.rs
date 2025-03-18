use crate::parser::{Binop, Unop};

use super::types::Primitive;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Expr {
	pub r#type: Primitive,
	pub expr: ExprVariant,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprVariant {
	Var(String),
	Literal(i32),
	Unop {
		op: Unop,
		expr: Box<Expr>,
	},
	Binop {
		op: Binop,
		lhs: Box<Expr>,
		rhs: Box<Expr>,
	},
	FnCall {
		func: String,
		params: Vec<Expr>,
	},
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
	Binding {
		ident: String,
		r#type: Primitive,
		value: Expr,
	},
	Expr(Expr),
	Return(Option<Expr>),
	While {
		cond: Expr,
		block: Vec<Stmt>,
	},
	If {
		cond: Expr,
		block: Vec<Stmt>,
	},
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnParam {
	pub name: String,
	pub r#type: Primitive,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FnBody {
	Lambda(Expr),
	Block(Vec<Stmt>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnSignature {
	pub ident: String,
	pub params: Vec<FnParam>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnDef {
	pub sig: FnSignature,
	pub body: FnBody,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Program(pub Vec<FnDef>);
