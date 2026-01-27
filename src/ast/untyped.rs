use ecow::EcoString;
use num_bigint::BigInt;

use crate::{
	ast::{Binop, Block, Pattern, SimplePath, SpanName, Unop, UntypedBlock},
	span::{HasSpan, Span},
};

#[derive(Debug, Clone)]
pub enum UntypedExpr {
	Binding {
		span: Span,
		binding: SimplePath,
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
		name: SimplePath,
		fields: Vec<UntypedExprStructField>,
	},
	// control flow
	Block(Box<UntypedBlock>),
	If {
		span: Span,
		cond: Box<Self>,
		then: Box<UntypedBlock>,
		else_if: Vec<(UntypedExpr, UntypedBlock)>,
		else_: Option<Box<UntypedBlock>>,
	},
	For {
		span: Span,
		iterator: Box<Self>,
		body: Box<UntypedBlock>,
	},
	While {
		span: Span,
		cond: Box<Self>,
		body: Box<UntypedBlock>,
	},
	Loop(Box<UntypedBlock>),
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
pub enum UntypedExprStructField {
	Shorthand {
		span: Span,
		field: SpanName,
	},
	Normal {
		span: Span,
		field: SpanName,
		value: UntypedExpr,
	},
}

#[derive(Debug, Clone)]
pub struct MatchArm {
	binding: Pattern<()>,
	guard: Option<UntypedExpr>,
	to: UntypedExpr,
}

impl HasSpan for UntypedExpr {
	fn span(&self) -> Span {
		match self {
			Self::Binding { span, .. }
			| Self::StrLiteral { span, .. }
			| Self::IntLiteral { span, .. }
			| Self::FloatLiteral { span, .. }
			| Self::BoolLiteral { span, .. }
			| Self::Unit { span }
			| Self::TupleLiteral { span, .. }
			| Self::NamedStructLiteral { span, .. }
			| Self::If { span, .. }
			| Self::For { span, .. }
			| Self::While { span, .. }
			| Self::Match { span, .. }
			| Self::Binop { span, .. }
			| Self::Unop { span, .. }
			| Self::Group { span, .. }
			| Self::CallLike { span, .. }
			| Self::Index { span, .. }
			| Self::TupleIndex { span, .. }
			| Self::FieldAccess { span, .. }
			| Self::MethodCall { span, .. }
			| Self::Deref { span, .. } => *span,
			Self::Block(block) | Self::Loop(block) => block.span,
		}
	}
}

impl UntypedExpr {
	pub fn is_block_expr(&self) -> bool {
		match self {
			Self::Block(_)
			| Self::Match { .. }
			| Self::If { .. }
			| Self::For { .. }
			| Self::While { .. }
			| Self::Loop(_) => true,
			Self::Binding { .. }
			| Self::StrLiteral { .. }
			| Self::IntLiteral { .. }
			| Self::FloatLiteral { .. }
			| Self::BoolLiteral { .. }
			| Self::Unit { .. }
			| Self::TupleLiteral { .. }
			| Self::NamedStructLiteral { .. }
			| Self::Binop { .. }
			| Self::Unop { .. }
			| Self::Group { .. }
			| Self::CallLike { .. }
			| Self::Index { .. }
			| Self::TupleIndex { .. }
			| Self::FieldAccess { .. }
			| Self::MethodCall { .. }
			| Self::Deref { .. } => false,
		}
	}
}
