use std::borrow::Cow;

use ecow::EcoString;
use num_bigint::BigInt;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
	// literals
	LitInt {
		value: EcoString,
		int_value: BigInt,
		suffix: Option<EcoString>,
	},
	LitFloat {
		value: EcoString,
		float_value: FloatLiteral,
		suffix: Option<EcoString>,
	},
	LitStr(EcoString),
	Ident(EcoString),
	True,
	False,
	Wildcard,

	// item keywords
	Module,
	Fn,
	Struct,
	Enum,
	Union,
	Trait,
	Impl,
	Extern,
	Use,
	Macro,

	// control keywords
	Let,
	If,
	Else,
	For,
	In,
	While,
	Loop,
	Break,
	Continue,
	Match,
	Defer,
	ErrDefer,

	// modifier keywords
	Phantom,
	Mut,
	Pub,
	As,
	Where,

	// delimiters
	OParen,
	CParen,
	OSquareBracket,
	CSquareBracket,
	OCurlyBracket,
	CCurlyBracket,

	// ops
	Plus,
	Minus,
	Star,
	Slash,
	Percent,
	LtLt,
	GtGt,
	Amp,
	AmpAmp,
	Vbar,
	VbarVbar,
	Caret,
	Qmark,
	Bang,
	Dot,
	Elipsis,
	Comma,
	Arrow,
	FatArrow,
	Colon,
	Semicolon,
	Scope,
	Hash,

	Eq,
	PlusEq,
	MinusEq,
	StarEq,
	SlashEq,
	PercentEq,
	AmpEq,
	VbarEq,
	CaretEq,
	AmpAmpEq,
	VbarVbarEq,

	Lt,
	LtEq,
	Gt,
	GtEq,
	EqEq,
	Uneq,
	// special EOF to not have to deal with Option<Token>
	EOF,
}

pub enum DelimKind {
	Paren,
	SquareBracket,
	CurlyBracket,
}

pub enum TokenTree {
	Token(Token),
	Group(DelimKind, Vec<TokenTree>),
}

impl Token {
	fn as_print_symbol(&self) -> &'static str {
		match self {
			Token::LitInt { .. } => "integer",
			Token::LitFloat { .. } => "float",
			Token::LitStr(_) => "string",
			Token::Ident(_) => "identifier",
			Token::True => "'true'",
			Token::False => "'false'",
			Token::Wildcard => "'_'",
			Token::Module => "'mod'",
			Token::Fn => "'fn'",
			Token::Struct => "'struct'",
			Token::Enum => "'enum",
			Token::Union => "'union'",
			Token::Trait => "'trait'",
			Token::Impl => "'impl'",
			Token::Extern => "'extern'",
			Token::Use => "'use'",
			Token::Macro => "'macro'",
			Token::Let => "'let'",
			Token::If => "'if'",
			Token::Else => "'if'",
			Token::For => "'for'",
			Token::In => "'in'",
			Token::While => "'while'",
			Token::Loop => "'loop'",
			Token::Break => "'break'",
			Token::Continue => "'continue'",
			Token::Match => "'match'",
			Token::Defer => "'defer'",
			Token::ErrDefer => "'errdefer'",
			Token::Phantom => "'phantom'",
			Token::Mut => "'mut'",
			Token::Pub => "'pub'",
			Token::As => "'as'",
			Token::Where => "'where'",
			Token::OParen => "'('",
			Token::CParen => "')'",
			Token::OSquareBracket => "'['",
			Token::CSquareBracket => "']'",
			Token::OCurlyBracket => "'{'",
			Token::CCurlyBracket => "'}'",
			Token::Plus => "'+'",
			Token::Minus => "'-'",
			Token::Star => "'*'",
			Token::Slash => "'/'",
			Token::Percent => "'%'",
			Token::LtLt => "'<<'",
			Token::GtGt => "'>>'",
			Token::Amp => "'&'",
			Token::AmpAmp => "'&&'",
			Token::VbarVbar => "'||'",
			Token::Vbar => "'|'",
			Token::Caret => "'^'",
			Token::Qmark => "'?'",
			Token::Bang => "'!'",
			Token::Dot => "'.'",
			Token::Elipsis => "'..'",
			Token::Comma => "','",
			Token::Arrow => "'->'",
			Token::FatArrow => "'=>'",
			Token::Colon => "':'",
			Token::Semicolon => "';'",
			Token::Scope => "'::'",
			Token::Hash => "'#'",
			Token::Eq => "'='",
			Token::PlusEq => "'+='",
			Token::MinusEq => "'-='",
			Token::StarEq => "'*='",
			Token::SlashEq => "'/='",
			Token::PercentEq => "'%='",
			Token::AmpEq => "'&='",
			Token::VbarEq => "'|='",
			Token::CaretEq => "'^='",
			Token::AmpAmpEq => "'&&='",
			Token::VbarVbarEq => "'||='",
			Token::Lt => "'<'",
			Token::LtEq => "'<='",
			Token::Gt => "'>'",
			Token::GtEq => "'>='",
			Token::EqEq => "'=='",
			Token::Uneq => "'!='",
			Token::EOF => "end of file",
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct FloatLiteral(f64);

impl Eq for FloatLiteral {}

impl Ord for FloatLiteral {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		self.0.partial_cmp(&other.0).expect("FloatLiteral Ord")
	}
}

impl FloatLiteral {
	pub fn new(x: f64) -> Self {
		if !x.is_nan() {
			Self(x)
		} else {
			panic!("FloatLiteral must not be NaN")
		}
	}
}
