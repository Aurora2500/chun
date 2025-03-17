pub mod error;

use std::ops::Range;

use chumsky::{
	prelude::{choice, end, just, take_until, todo},
	recursive::recursive,
	select,
	text::{digits, ident, keyword, newline, TextParser},
	Parser, Stream,
};

use error::ParserError;

pub type Spanned<T> = (T, Range<usize>);

pub struct SpanVec<T>(Vec<Spanned<T>>);

impl<'a, T: Clone + 'a + 'static>
	Into<Stream<'a, T, Range<usize>, Box<dyn Iterator<Item = (T, Range<usize>)> + 'a>>>
	for SpanVec<T>
{
	fn into(self) -> Stream<'a, T, Range<usize>, Box<dyn Iterator<Item = (T, Range<usize>)>>> {
		let end = self.0.last().map_or(0, |x| x.1.end);
		Stream::from_iter(end..end, Box::new(self.0.into_iter()))
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
	Literal(i32),
	Ident(String),
	Fn,
	Extern,
	Return,
	While,
	If,
	Let,
	FatArrow,
	Colon,
	Semicolon,
	Comma,
	Assign,
	OpenBracket,
	CloseBracket,
	OpenParen,
	CloseParen,
	Plus,
	Minus,
	Star,
	Div,
	Rem,
	Inc,
	Dec,
	Eq,
	Ineq,
	Lt,
	Leq,
	Gt,
	Geq,
	LogicAnd,
	LogicOr,
	Xor,
	Not,
}

macro_rules! span_keyword {
	($kw:expr, $token: expr) => {
		keyword($kw).map_with_span(|_, span| ($token, span))
	};
}

macro_rules! span_just {
	($sym:expr, $token: expr) => {
		just($sym).map_with_span(|_, span| ($token, span))
	};
}

pub fn parse_tokens() -> impl Parser<char, SpanVec<Token>, Error = ParserError<char>> {
	// let single_line = just("//").then(take_until(newline())).ignored();
	// let multi_line = just("/*").then(take_until(just("*/"))).ignored();
	choice((
		choice((
			span_keyword!("fn", Token::Fn),
			span_keyword!("let", Token::Let),
			span_keyword!("extern", Token::Extern),
			span_keyword!("return", Token::Return),
			span_keyword!("if", Token::If),
			span_keyword!("while", Token::While),
		)),
		span_just!("=>", Token::FatArrow),
		choice((
			span_just!("==", Token::Eq),
			span_just!("!=", Token::Ineq),
			span_just!("<=", Token::Leq),
			span_just!(">=", Token::Geq),
			span_just!("&&", Token::LogicAnd),
			span_just!("||", Token::LogicOr),
			span_just!('^', Token::Xor),
			span_just!('!', Token::Not),
			span_just!('<', Token::Lt),
			span_just!('>', Token::Gt),
			span_just!("++", Token::Inc),
			span_just!("--", Token::Dec),
			span_just!('+', Token::Plus),
			span_just!('-', Token::Minus),
			span_just!('*', Token::Star),
			span_just!('/', Token::Div),
			span_just!('%', Token::Rem),
		)),
		span_just!(':', Token::Colon),
		span_just!(';', Token::Semicolon),
		span_just!(',', Token::Comma),
		span_just!('=', Token::Assign),
		span_just!('{', Token::OpenBracket),
		span_just!('}', Token::CloseBracket),
		span_just!('(', Token::OpenParen),
		span_just!(')', Token::CloseParen),
		ident().map_with_span(|x, span| (Token::Ident(x), span)),
		digits(10)
			.from_str::<i32>()
			.unwrapped()
			.map_with_span(|x, span| (Token::Literal(x), span)),
	))
	.padded()
	// .padded_by(single_line.or(multi_line))
	.repeated()
	.map(SpanVec)
	.then_ignore(end())
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Binop {
	Add,
	Sub,
	Mul,
	Div,
	Rem,
	Eq,
	Uneq,
	Lt,
	Leq,
	Gt,
	Geq,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
	Var(String),
	Literal(i32),
	Binop {
		op: Binop,
		lhs: Box<Spanned<Expr>>,
		rhs: Box<Spanned<Expr>>,
	},
	FnCall {
		func: Spanned<String>,
		params: Vec<Spanned<Expr>>,
	},
}

macro_rules! binary_op {
	($atom:expr; $($from:expr => $to:expr),+ $(,)?) => {
			$atom.clone().then(choice((
				$(
					just($from).to($to),
				)+
				))
				.then($atom)
				.repeated()
			)
			.foldl(|lhs, (op, rhs)| {
				let span = lhs.1.start..rhs.1.end;
				(
					Expr::Binop {
						op,
						lhs: Box::new(lhs),
						rhs: Box::new(rhs),
					},
					span,
				)
			})
	};
}

fn parse_expr() -> impl Parser<Token, Spanned<Expr>, Error = ParserError<Token>> {
	recursive(|expr| {
		let lit = select! { |span|
			Token::Literal(x) => (Expr::Literal(x), span),
			Token::Ident(x) => (Expr::Var(x), span),
		};

		let fncall = select! {|span| Token::Ident(x) => (x, span)}
			.then(
				expr.clone()
					.separated_by(just(Token::Comma))
					.allow_trailing()
					.delimited_by(just(Token::OpenParen), just(Token::CloseParen)),
			)
			.map_with_span(|(func, params), span: Range<usize>| {
				(Expr::FnCall { func, params }, span)
			});
		let unary = choice((fncall.clone(), lit));
		let mul = binary_op!(unary;
			Token::Star => Binop::Mul,
			Token::Div => Binop::Div,
			Token::Rem => Binop::Rem,
		);
		let add = binary_op!(mul; Token::Plus => Binop::Add, Token::Minus => Binop::Sub);
		choice((add, fncall, lit))
	})
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
	Binding {
		ident: Spanned<String>,
		r#type: Option<Spanned<String>>,
		value: Box<Spanned<Expr>>,
	},
	Expr(Box<Spanned<Expr>>),
	Return(Box<Spanned<Expr>>),
	While {
		condition: Box<Spanned<Expr>>,
		block: Vec<Spanned<Stmt>>,
	},
	If {
		condition: Box<Spanned<Expr>>,
		block: Vec<Spanned<Stmt>>,
	},
}

fn parse_stmt() -> impl Parser<Token, Spanned<Stmt>, Error = ParserError<Token>> {
	let binding = just(Token::Let)
		.ignore_then(select! {|span| Token::Ident(x) => (x, span)})
		.then_ignore(just(Token::Colon))
		.then(select!(|span| Token::Ident(x) => (x, span)).or_not())
		.then_ignore(just(Token::Assign))
		.then(parse_expr())
		.map_with_span(|((ident, r#type), value), span| {
			(
				Stmt::Binding {
					ident,
					r#type,
					value: Box::new(value),
				},
				span,
			)
		});

	let expr = parse_expr().map_with_span(|expr, span| (Stmt::Expr(Box::new(expr)), span));

	choice((binding, expr)).then_ignore(just(Token::Semicolon))
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnParam {
	pub name: Spanned<String>,
	pub r#type: Spanned<String>,
}

fn parse_params() -> impl Parser<Token, Vec<Spanned<FnParam>>, Error = ParserError<Token>> {
	let ident = select! {|span| Token::Ident(x) => (x, span)};
	ident
		.clone()
		.then_ignore(just(Token::Colon))
		.then(ident)
		.map_with_span(|(name, r#type), span| (FnParam { name, r#type }, span))
		.separated_by(just(Token::Comma))
		.allow_trailing()
		.delimited_by(just(Token::OpenParen), just(Token::CloseParen))
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FnBody {
	Lambda(Box<Spanned<Expr>>),
	Block(Vec<Spanned<Stmt>>),
}

fn parse_fnbody() -> impl Parser<Token, Spanned<FnBody>, Error = ParserError<Token>> {
	let block = parse_stmt()
		.repeated()
		.delimited_by(just(Token::OpenBracket), just(Token::CloseBracket))
		.map_with_span(|x, span| (FnBody::Block(x), span));
	let lambda = just(Token::FatArrow)
		.ignore_then(parse_expr())
		.then_ignore(just(Token::Semicolon))
		.map_with_span(|expr, span| (FnBody::Lambda(Box::new(expr)), span));
	choice((block, lambda))
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnSignature {
	pub ident: Spanned<String>,
	pub params: Vec<Spanned<FnParam>>,
}

fn parse_fn_signature() -> impl Parser<Token, FnSignature, Error = ParserError<Token>> {
	just(Token::Fn)
		.ignore_then(select! {|span| Token::Ident(x) => (x, span)})
		.then(parse_params())
		.map(|(ident, params)| FnSignature { ident, params })
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnDef {
	pub ident: Spanned<String>,
	pub params: Vec<Spanned<FnParam>>,
	pub body: Spanned<FnBody>,
}

fn parse_fn_def() -> impl Parser<Token, FnDef, Error = ParserError<Token>> {
	parse_fn_signature()
		.then(parse_fnbody())
		.map(|(FnSignature { ident, params }, body)| FnDef {
			ident,
			params,
			body,
		})
}

fn parse_externfn() -> impl Parser<Token, FnSignature, Error = ParserError<Token>> {
	just(Token::Extern).ignore_then(parse_fn_signature())
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TopLevelDef {
	Fn(FnDef),
	Extern(FnSignature),
}

fn parse_topleveldef() -> impl Parser<Token, TopLevelDef, Error = ParserError<Token>> {
	choice((
		parse_fn_def().map(TopLevelDef::Fn),
		parse_externfn().map(TopLevelDef::Extern),
	))
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Program(pub Vec<TopLevelDef>);

pub fn parse_program() -> impl Parser<Token, Program, Error = ParserError<Token>> {
	parse_topleveldef()
		.repeated()
		.then_ignore(end())
		.map(Program)
}
