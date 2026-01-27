use ecow::EcoString;
use num_bigint::BigInt;

use crate::{
	ast::{Binop, Pattern, ResolvedBlock, SpanName, Unop},
	passes::symbols::Symbol,
	span::Span,
};

#[derive(Debug, Clone)]
pub enum ResolvedExpr {
	Binding {
		span: Span,
		binding: Symbol,
	},
	// literals
	StrLiteral {
		span: Span,
		value: EcoString,
	},
	IntLiteral {
		span: Span,
		value: EcoString,
		int_value: BigInt,
		suffix: Option<EcoString>,
	},
	FloatLiteral {
		span: Span,
		value: EcoString,
		suffix: Option<EcoString>,
	},
	BoolLiteral {
		span: Span,
		value: bool,
	},
	Unit {
		span: Span,
	},

	// compound literals
	TupleLiteral {
		span: Span,
		values: Vec<Self>,
	},
	NamedStructLiteral {
		span: Span,
		name: Symbol,
		fields: Vec<ResolvedExprStructField>,
	},
	// control flow
	Block(Box<ResolvedBlock>),
	If {
		span: Span,
		cond: Box<Self>,
		then: Box<ResolvedBlock>,
		else_if: Vec<(Self, ResolvedBlock)>,
		else_: Option<Box<ResolvedBlock>>,
	},
	For {
		span: Span,
		iterator: Box<Self>,
		body: Box<ResolvedBlock>,
	},
	While {
		span: Span,
		cond: Box<Self>,
		body: Box<ResolvedBlock>,
	},
	Loop(Box<ResolvedBlock>),
	Match {
		span: Span,
		on: Box<Self>,
		arms: Vec<MatchArm>,
	},

	// common Expressions
	Binop {
		span: Span,
		lhs: Box<Self>,
		op: Binop,
		rhs: Box<Self>,
	},
	Unop {
		span: Span,
		op: Unop,
		on: Box<Self>,
	},
	Group {
		span: Span,
		inner: Box<Self>,
	},
	CallLike {
		span: Span,
		func: Box<Self>,
		args: Vec<Self>,
	},
	Index {
		span: Span,
		val: Box<Self>,
		at: Box<Self>,
	},
	TupleIndex {
		span: Span,
		val: Box<Self>,
		idx: u64,
	},
	FieldAccess {
		span: Span,
		val: Box<Self>,
		field: SpanName,
	},
	MethodCall {
		span: Span,
		val: Box<Self>,
		method: SpanName,
		args: Vec<Self>,
	},
	Deref {
		span: Span,
		ptr: Box<Self>,
	},
}

#[derive(Debug, Clone)]
pub struct ResolvedExprStructField {
	pub span: Span,
	pub field: SpanName,
	pub value: ResolvedExpr,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
	binding: Pattern<()>,
	guard: Option<ResolvedExpr>,
	to: ResolvedExpr,
}
