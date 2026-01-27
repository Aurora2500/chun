pub mod resolved;
pub mod typed;
pub mod untyped;

use std::{marker::PhantomData, sync::Arc};

use crate::{
	ast::{resolved::ResolvedExpr, typed::TypedExpr, untyped::UntypedExpr},
	passes::symbols::Symbol,
	span::{HasSpan, Span},
	tokens::Token,
	types::Type,
};
use ecow::EcoString;

pub type UntypedModule = Module<Vec<UntypedDefinition>>;
pub type ResolvedModule = Module<ResolvedDefinition>;
pub type TypedModule = Module<TypedDefinition>;

#[derive(Debug, Clone)]
pub struct Module<Definitions> {
	pub definitions: Definitions,
}

#[derive(Debug, Clone)]
pub enum Attr {
	Simple { value: SimplePath },
	Eq { key: SimplePath, value: UntypedExpr },
}

#[derive(Debug, Clone)]
pub struct Attrs {
	pub attrs: Vec<Attr>,
}

#[derive(Debug, Clone)]
pub struct ResolvedDefinition {
	pub structs: Vec<ResolvedStructDef>,
	pub enums: Vec<ResolvedEnumDef>,
	pub unions: Vec<ResolvedUnionDef>,
	pub fn_defs: Vec<ResolvedFnDef>,
	pub fn_decls: Vec<ResolvedFnDecl>,
	// pub impls: Vec<ResolvedImpl>,
}

#[derive(Debug, Clone)]
pub struct TypedDefinition {
	pub structs: Vec<TypedStructDef>,
	pub enums: Vec<TypedEnumDef>,
	pub unions: Vec<TypedUnionDef>,
	pub fn_defs: Vec<TypedFnDef>,
	pub fn_decls: Vec<TypedFnDecl>,
}

pub type UntypedDefinition = TopLevelDef<(), UntypedExpr, SpanName>;

#[derive(Debug, Clone)]
pub enum TopLevelDef<T, Expr, Sym> {
	StructDef(StructDef<T, Sym>),
	EnumDef(EnumDef<T, Expr, Sym>),
	UnionDef(UnionDef<T, Sym>),
	ImplBlock(ImplBlock<T, Expr, Sym>),
	ExternBlock(ExternBlock<T, Sym>),
	FnDef(FnDef<T, Expr, Sym>),
}

pub type UntypedGenericParams = GenericParams<()>;

#[derive(Debug, Clone)]
pub struct GenericParams<T> {
	pub span: Span,
	pub params: Vec<GenericParam<T>>,
}

pub type UntypedGenericParam = GenericParam<()>;

#[derive(Debug, Clone)]
pub struct GenericParam<T> {
	pub span: Span,
	pub phantom: bool,
	pub binding: SpanName,
	pub default: Option<TypeAst>,
	pub type_: T,
}

pub type UntypedStructDef = StructDef<(), SpanName>;
pub type ResolvedStructDef = StructDef<(), Symbol>;
pub type TypedStructDef = StructDef<Arc<Type>, Symbol>;

#[derive(Debug, Clone)]
pub struct StructDef<T, Sym> {
	pub span: Span,
	pub public: bool,
	pub name: Sym,
	pub generics: Option<GenericParams<T>>,
	pub kind: StructDefKind<T>,
}

#[derive(Debug, Clone)]
pub enum StructDefKind<T> {
	Unit,
	Tuple { items: Vec<StructTupleItem<T>> },
	Struct { fields: Vec<StructField<T>> },
}

#[derive(Debug, Clone)]
pub struct StructTupleItem<T> {
	pub span: Span,
	pub public: bool,
	pub annot: TypeAst,
	pub type_: T,
}

#[derive(Debug, Clone)]
pub struct StructField<T> {
	pub span: Span,
	pub public: bool,
	pub name: SpanName,
	pub annot: TypeAst,
	pub type_: T,
}

pub type UntypedEnumDef = EnumDef<(), UntypedExpr, SpanName>;
pub type ResolvedEnumDef = EnumDef<(), ResolvedExpr, Symbol>;
pub type TypedEnumDef = EnumDef<Arc<Type>, TypedExpr, Symbol>;

#[derive(Debug, Clone)]
pub struct EnumDef<T, Expr, Sym> {
	pub span: Span,
	pub name: Sym,
	pub repr: Option<TypeAst>,
	pub type_: T,
	pub values: Vec<EnumDefValue<Expr, Sym>>,
}

pub type UntypedEnumDefValue = EnumDefValue<UntypedExpr, SpanName>;
pub type ResolvedEnumDefValue = EnumDefValue<UntypedExpr, Symbol>;

#[derive(Debug, Clone)]
pub struct EnumDefValue<Expr, Sym> {
	pub name: Sym,
	pub value: Option<Expr>,
}

pub type UntypedUnionDef = UnionDef<(), SpanName>;
pub type ResolvedUnionDef = UnionDef<(), Symbol>;
pub type TypedUnionDef = UnionDef<Arc<Type>, Symbol>;

#[derive(Debug, Clone)]
pub struct UnionDef<T, Sym> {
	pub span: Span,
	pub name: Sym,
	pub tag: Option<TypeAst>,
	pub variants: Vec<UnionVariant<T>>,
}

#[derive(Debug, Clone)]
pub struct UnionVariant<T> {
	pub name: SpanName,
	pub kind: UnionVariantKind<T>,
}

#[derive(Debug, Clone)]
pub enum UnionVariantKind<T> {
	UnitVariant,
	TupleVariant(UnionTupleVariant<T>),
}

#[derive(Debug, Clone)]
pub struct UnionTupleVariant<T> {
	pub span: Span,
	pub fields: Vec<T>,
}

pub type UntypedImplBlock = ImplBlock<(), UntypedExpr, SpanName>;

#[derive(Debug, Clone)]
pub struct ImplBlock<T, Expr, Sym> {
	pub span: Span,
	pub generics: Option<ImplBlockGenerics>,
	pub impler: TypeAst,
	pub methods: Vec<FnDef<T, Expr, Sym>>,
}

#[derive(Debug, Clone)]
pub struct ImplBlockGenerics {
	pub items: Vec<ImplBlockGeneric>,
}

#[derive(Debug, Clone)]
pub struct ImplBlockGeneric {
	pub span: Span,
	pub binding: SpanName,
}

pub type UntypedExternBlock = ExternBlock<(), SpanName>;
pub type ResolvedExternBlock = ExternBlock<(), Symbol>;

#[derive(Debug, Clone)]
pub struct ExternBlock<T, Sym> {
	pub items: Vec<FnDecl<T, Sym>>,
}

pub type UntypedFnDecl = FnDecl<(), SpanName>;
pub type ResolvedFnDecl = FnDecl<(), Symbol>;
pub type TypedFnDecl = FnDecl<Arc<Type>, Symbol>;

#[derive(Debug, Clone)]
pub struct FnDecl<T, Sym> {
	pub span: Span,
	pub attrs: Attrs,
	pub publicity: bool,
	pub name: Sym,
	pub params: Vec<FnParam<T, Sym>>,
	pub ret_annot: Option<TypeAst>,
	pub ret_type: T,
	pub type_: T,
}

pub type UntypedFnDef = FnDef<(), UntypedExpr, SpanName>;
pub type ResolvedFnDef = FnDef<(), ResolvedExpr, Symbol>;
pub type TypedFnDef = FnDef<Arc<Type>, TypedExpr, Symbol>;

#[derive(Debug, Clone)]
pub struct FnDef<T, Expr, Sym> {
	pub span: Span,
	pub attrs: Attrs,
	pub name: Sym,
	pub params: Vec<FnParam<T, Sym>>,
	pub return_annotation: Option<TypeAst>,
	pub return_type: T,
	pub body: Block<T, Expr, Sym>,
}

pub type ResolvedFnParam = FnParam<(), Symbol>;
pub type TypedFnParam = FnParam<Arc<Type>, Symbol>;

#[derive(Debug, Clone)]
pub enum FnParam<T, Sym> {
	Receiver(FnParamReceiver<T>),
	Normal(FnParamNormal<T, Sym>),
}

#[derive(Debug, Clone)]
pub struct FnParamReceiver<T> {
	pub span: Span,
	pub attrs: Attr,
	pub type_: T,
}

#[derive(Debug, Clone)]
pub struct FnParamNormal<T, Sym> {
	pub span: Span,
	pub attrs: Attrs,
	pub binding: Sym,
	pub annotation: TypeAst,
	pub type_: T,
}

#[derive(Debug, Clone)]
pub enum TypeAst {
	Named(TypeAstNamed),
	Ptr {
		// *T
		span: Span,
		pointee: Box<Self>,
	},
	Optional {
		// ?T
		span: Span,
		inner: Box<Self>,
	},
	Sliced {
		// []T
		span: Span,
		item: Box<Self>,
	},
	Array {
		// [n]T
		span: Span,
		len: u64,
		item: Box<Self>,
	},
	Wildcard {
		// _
		span: Span,
	},
}

#[derive(Debug, Clone)]
pub struct TypeAstNamed {
	pub span: Span,
	pub name: SimplePath,
	pub type_vars: Vec<TypeAst>,
}

pub type UntypedStatement = Statement<(), UntypedExpr, SpanName>;
pub type ResolvedStatement = Statement<(), ResolvedExpr, Symbol>;
pub type TypedStatement = Statement<Arc<Type>, TypedExpr, Symbol>;

#[derive(Debug, Clone)]
pub enum Statement<T, Expr, Sym> {
	Expr {
		span: Span,
		expr: Expr,
	},
	Let {
		span: Span,
		binding: Sym,
		annotation: Option<TypeAst>,
		type_: T,
		val: Expr,
	},
	ErrDefer {
		span: Span,
		op: Expr,
	},
	Defer {
		span: Span,
		op: Expr,
	},
}

#[derive(Debug, Clone)]
pub enum Pattern<T> {
	Bindin {
		span: Span,
		name: EcoString,
		mutable: bool,
		type_: T,
	},
	Tuple {
		span: Span,
		bindings: Vec<Pattern<T>>,
		type_: T,
	},
	NamedTuple {
		span: Span,
		name: SimplePath,
		bindings: Vec<Pattern<T>>,
		type_: T,
	},
	NamedStruct {
		span: Span,
		name: SimplePath,
		fields: Vec<PatternStructField<T>>,
		type_: T,
	},
	Disjunction {
		span: Span,
		variants: Vec<Pattern<T>>,
		type_: T,
	},
	Wildcard,
}

#[derive(Debug, Clone)]
pub enum PatternStructField<T> {
	Direct {
		span: Span,
		binding: EcoString,
		type_: T,
	},
	Mapped {
		span: Span,
		field: SpanName,
		to: Pattern<T>,
		type_: T,
	},
	Elipsis {
		span: Span,
	},
}

pub type UntypedBlock = Block<(), UntypedExpr, SpanName>;
pub type ResolvedBlock = Block<(), ResolvedExpr, Symbol>;
pub type TypedBlock = Block<Arc<Type>, TypedExpr, Symbol>;

#[derive(Debug, Clone)]
pub struct Block<T, Expr, Sym> {
	pub span: Span,
	pub stmts: Vec<Statement<T, Expr, Sym>>,
	pub tail: Option<Expr>,
}

impl<T, Expr, Sym> HasSpan for Block<T, Expr, Sym> {
	fn span(&self) -> Span {
		self.span
	}
}

#[derive(Debug, Clone, Copy)]
pub enum Binop {
	// arithmetic
	Add,
	Sub,
	Mul,
	Div,
	Mod,

	// Bitwise
	BitAnd,
	BitOr,
	BitXor,
	BitShiftLeft,
	BitShiftRight,

	// Logical
	And,
	Or,
	Eq,
	Uneq,
	Lt,
	Le,
	Gt,
	Ge,
}

#[derive(Debug, Clone)]
pub enum Unop {
	Neg,
	Not,
	Ref,
}

#[derive(Clone, Copy)]
pub struct BinopInfo {
	pub prec: usize,
	pub assoc: Assoc,
	pub op: Binop,
}

impl BinopInfo {
	pub fn next_prec(self) -> usize {
		self.prec
			+ match self.assoc {
				Assoc::Left => 1,
				Assoc::Right => 0,
			}
	}
}

#[derive(Clone, Copy)]
pub enum Assoc {
	Left,
	Right,
}

pub fn infix_table(token: &Token) -> Option<BinopInfo> {
	match token {
		Token::EqEq => Some(BinopInfo {
			prec: 10,
			assoc: Assoc::Left,
			op: Binop::Eq,
		}),
		Token::Uneq => Some(BinopInfo {
			prec: 10,
			assoc: Assoc::Left,
			op: Binop::Uneq,
		}),
		Token::Lt => Some(BinopInfo {
			prec: 15,
			assoc: Assoc::Left,
			op: Binop::Le,
		}),
		Token::LtEq => Some(BinopInfo {
			prec: 15,
			assoc: Assoc::Left,
			op: Binop::Lt,
		}),
		Token::Gt => Some(BinopInfo {
			prec: 15,
			assoc: Assoc::Left,
			op: Binop::Ge,
		}),
		Token::GtEq => Some(BinopInfo {
			prec: 15,
			assoc: Assoc::Left,
			op: Binop::Gt,
		}),
		Token::AmpAmp => Some(BinopInfo {
			prec: 20,
			assoc: Assoc::Left,
			op: Binop::And,
		}),
		Token::VbarVbar => Some(BinopInfo {
			prec: 20,
			assoc: Assoc::Left,
			op: Binop::Or,
		}),
		Token::Amp => Some(BinopInfo {
			prec: 25,
			assoc: Assoc::Left,
			op: Binop::BitAnd,
		}),
		Token::Vbar => Some(BinopInfo {
			prec: 25,
			assoc: Assoc::Left,
			op: Binop::BitOr,
		}),
		Token::Caret => Some(BinopInfo {
			prec: 25,
			assoc: Assoc::Left,
			op: Binop::BitXor,
		}),
		Token::LtLt => Some(BinopInfo {
			prec: 28,
			assoc: Assoc::Left,
			op: Binop::BitShiftLeft,
		}),
		Token::GtGt => Some(BinopInfo {
			prec: 28,
			assoc: Assoc::Left,
			op: Binop::BitShiftRight,
		}),
		Token::Plus => Some(BinopInfo {
			prec: 30,
			assoc: Assoc::Left,
			op: Binop::Add,
		}),
		Token::Minus => Some(BinopInfo {
			prec: 30,
			assoc: Assoc::Left,
			op: Binop::Sub,
		}),
		Token::Star => Some(BinopInfo {
			prec: 35,
			assoc: Assoc::Left,
			op: Binop::Mul,
		}),
		Token::Slash => Some(BinopInfo {
			prec: 35,
			assoc: Assoc::Left,
			op: Binop::Div,
		}),
		Token::Percent => Some(BinopInfo {
			prec: 35,
			assoc: Assoc::Left,
			op: Binop::Mod,
		}),
		_ => None,
	}
}

pub static PREFIX_PREC: usize = 100;

pub fn prefix_table(token: &Token) -> Option<Unop> {
	match token {
		Token::Amp => Some(Unop::Ref),
		Token::Minus => Some(Unop::Neg),
		Token::Bang => Some(Unop::Not),
		_ => None,
	}
}

#[derive(Debug, Clone)]
pub struct SimplePath {
	pub span: Span,
	pub path: Vec<SpanName>,
}

#[derive(Debug, Clone)]
pub struct SpanName {
	pub span: Span,
	pub name: EcoString,
}
