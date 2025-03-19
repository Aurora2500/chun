use crate::parser::{Binop, Unop};

use super::MonoType;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Expr<'a, Ty = MonoType> {
	pub r#type: Ty,
	pub expr: ExprVariant<'a, Ty>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprVariant<'a, Ty = MonoType> {
	Var(&'a str),
	Integral(i64),
	Boolean(bool),
	Unop {
		op: Unop,
		expr: Box<Expr<'a, Ty>>,
	},
	Binop {
		op: Binop,
		lhs: Box<Expr<'a, Ty>>,
		rhs: Box<Expr<'a, Ty>>,
	},
	FnCall {
		func: &'a str,
		params: Vec<Expr<'a, Ty>>,
	},
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt<'a, Ty = MonoType> {
	Binding {
		ident: &'a str,
		r#type: Ty,
		value: Expr<'a, Ty>,
	},
	Assignment {
		ident: &'a str,
		r#type: Ty,
		value: Expr<'a, Ty>,
	},
	Expr(Expr<'a, Ty>),
	Return(Option<Expr<'a, Ty>>),
	While {
		cond: Expr<'a, Ty>,
		block: Vec<Stmt<'a, Ty>>,
	},
	If {
		cond: Expr<'a, Ty>,
		block: Vec<Stmt<'a, Ty>>,
	},
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnParam<'a> {
	pub name: &'a str,
	pub r#type: MonoType,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FnBody<'a, Ty = MonoType> {
	Lambda(Expr<'a, Ty>),
	Block(Vec<Stmt<'a, Ty>>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnSignature<'a> {
	pub ident: &'a str,
	pub params: Vec<FnParam<'a>>,
	pub return_type: MonoType,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnDef<'a, Ty = MonoType> {
	pub sig: FnSignature<'a>,
	pub body: FnBody<'a, Ty>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Program<'a, Ty = MonoType>(pub Vec<FnDef<'a, Ty>>);
