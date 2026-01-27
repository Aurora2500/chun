use ecow::EcoString;
use num_bigint::BigInt;

use crate::{ast::Binop, passes::symbols::Symbol, tokens::FloatLiteral};

#[derive(Debug, Clone)]
pub struct HIR {
	pub consts: Vec<HIRConst>,
	pub fns: Vec<HIRFn>,
}

#[derive(Debug, Clone)]
pub enum HIRConst {
	Str { id: u64, value: EcoString },
}

#[derive(Debug, Clone)]
pub struct HIRExternFn {}

#[derive(Debug, Clone)]
pub struct HIRFn {
	pub name: EcoString,
	pub params: Vec<HIRParam>,
	pub segments: Vec<HIRBasicBlock>,
}

#[derive(Debug, Clone)]
pub struct HIRParam {}

#[derive(Debug, Clone)]
pub struct HIRBasicBlock {
	pub id: u64,
	pub stmts: Vec<HIRStmt>,
	pub terminator: HIRTerminator,
}

#[derive(Debug, Clone)]
pub enum HIRTerminator {
	Return {
		value: Option<HIRValue>,
	},
	Jump {
		to: u64,
	},
	Branch {
		cond: HIRValue,
		then: u64,
		else_: u64,
	},
}

#[derive(Debug, Clone)]
pub enum HIRStmt {
	Assignment { to: HIRSymbol, val: HIROp },
}

#[derive(Debug, Clone)]
pub enum HIRSymbol {
	LocalTemp(u64),
	Global(u64),
	Symbol(Symbol),
}

#[derive(Debug, Clone)]
pub enum HIRValue {
	Symbol(HIRSymbol),
	Int {
		value: BigInt,
		type_: HIRScalar,
	},
	Float {
		value: FloatLiteral,
		type_: HIRScalar,
	},
	Bool {
		value: bool,
		type_: HIRScalar,
	},
}

#[derive(Debug, Clone)]
pub enum HIROp {
	IntLiteral {
		value: BigInt,
	},
	Binop {
		lhs: HIRSymbol,
		op: Binop,
		rhs: HIRSymbol,
	},
	Call {
		func: HIRSymbol,
		args: Vec<HIRValue>,
	},
}

pub enum HIRType {
	Struct { name: EcoString, members: HIRScalar },
	Scalar(HIRScalar),
}

#[derive(Debug, Clone)]
pub enum HIRScalar {
	Bool,
	I8,
	U8,
	I16,
	U16,
	I32,
	U32,
	I64,
	U64,
	ISize,
	USize,
	Ptr,
}
