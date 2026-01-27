use std::sync::Arc;

use ecow::EcoString;
use num_bigint::BigInt;

use crate::{
	ast::{Binop, Pattern, SpanName, TypedBlock, Unop},
	passes::symbols::Symbol,
	span::Span,
	types::{Type, prelude::unit},
};

#[derive(Debug, Clone)]
pub enum TypedExpr {
	Binding {
		span: Span,
		binding: Symbol,
		type_: Arc<Type>,
	},
	// literals
	StrLiteral {
		span: Span,
		value: EcoString,
		type_: Arc<Type>,
	},
	IntLiteral {
		span: Span,
		value: EcoString,
		int_value: BigInt,
		suffix: Option<EcoString>,
		type_: Arc<Type>,
	},
	FloatLiteral {
		span: Span,
		value: EcoString,
		suffix: Option<EcoString>,
		type_: Arc<Type>,
	},
	BoolLiteral {
		span: Span,
		value: bool,
		type_: Arc<Type>,
	},
	Unit {
		span: Span,
		type_: Arc<Type>,
	},

	// compound literals
	NamedStructLiteral {
		span: Span,
		name: Symbol,
		fields: Vec<TypedExprStructField>,
		type_: Arc<Type>,
	},
	// control flow
	Block(Box<TypedBlock>),
	If {
		span: Span,
		cond: Box<Self>,
		then: Box<TypedBlock>,
		else_if: Vec<(Self, TypedBlock)>,
		else_: Option<Box<TypedBlock>>,
		type_: Arc<Type>,
	},
	For {
		span: Span,
		iterator: Box<Self>,
		body: Box<TypedBlock>,
		type_: Arc<Type>,
	},
	While {
		span: Span,
		cond: Box<Self>,
		body: Box<TypedBlock>,
		type_: Arc<Type>,
	},
	Loop(Box<TypedBlock>),
	Match {
		span: Span,
		on: Box<Self>,
		arms: Vec<MatchArm>,
		type_: Arc<Type>,
	},

	// common Expressions
	Binop {
		span: Span,
		lhs: Box<Self>,
		op: Binop,
		rhs: Box<Self>,
		type_: Arc<Type>,
	},
	Unop {
		span: Span,
		op: Unop,
		on: Box<Self>,
		type_: Arc<Type>,
	},
	Group {
		span: Span,
		inner: Box<Self>,
		type_: Arc<Type>,
	},
	CallLike {
		span: Span,
		func: Box<Self>,
		args: Vec<Self>,
		type_: Arc<Type>,
	},
	Index {
		span: Span,
		val: Box<Self>,
		at: Box<Self>,
		type_: Arc<Type>,
	},
	TupleIndex {
		span: Span,
		val: Box<Self>,
		idx: u64,
		type_: Arc<Type>,
	},
	FieldAccess {
		span: Span,
		val: Box<Self>,
		field: SpanName,
		type_: Arc<Type>,
	},
	MethodCall {
		span: Span,
		val: Box<Self>,
		method: SpanName,
		args: Vec<Self>,
		type_: Arc<Type>,
	},
	Deref {
		span: Span,
		ptr: Box<Self>,
		type_: Arc<Type>,
	},
}

#[derive(Debug, Clone)]
pub struct TypedExprStructField {
	pub span: Span,
	pub field: SpanName,
	pub value: TypedExpr,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
	binding: Pattern<()>,
	guard: Option<TypedExpr>,
	to: TypedExpr,
}

impl TypedExpr {
	pub fn get_type(&self) -> Arc<Type> {
		match self {
			Self::Binding { type_, .. }
			| Self::StrLiteral { type_, .. }
			| Self::IntLiteral { type_, .. }
			| Self::FloatLiteral { type_, .. }
			| Self::BoolLiteral { type_, .. }
			| Self::Unit { type_, .. }
			// | Self::TupleLiteral { type_, .. }
			| Self::NamedStructLiteral { type_, .. }
			| Self::If { type_, .. }
			| Self::For { type_, .. }
			| Self::While { type_, .. }
			| Self::Match { type_, .. }
			| Self::Binop { type_, .. }
			| Self::Unop { type_, .. }
			| Self::Group { type_, .. }
			| Self::CallLike { type_, .. }
			| Self::Index { type_, .. }
			| Self::TupleIndex { type_, .. }
			| Self::FieldAccess { type_, .. }
			| Self::MethodCall { type_, .. }
			| Self::Deref { type_, .. } => type_.clone(),
			Self::Block(block) | Self::Loop(block) => {
				block.tail.as_ref().map(Self::get_type).unwrap_or_else(unit)
			}
		}
	}
}
