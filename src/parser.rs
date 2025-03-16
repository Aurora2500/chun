pub mod error;

use std::ops::Range;

use chumsky::{
	prelude::{choice, end, just},
	recursive::recursive,
	select,
	text::{digits, ident, keyword, TextParser},
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
	Let,
	FatArrow,
	Colon,
	Semicolon,
	Comma,
	Eq,
	OpenBracket,
	CloseBracket,
	OpenParen,
	CloseParen,
	Plus,
}

pub fn parse_tokens() -> impl Parser<char, SpanVec<Token>, Error = ParserError<char>> {
	choice((
		keyword("fn").map_with_span(|_, span| (Token::Fn, span)),
		keyword("let").map_with_span(|_, span| (Token::Let, span)),
		keyword("extern").map_with_span(|_, span| (Token::Extern, span)),
		just("=>").map_with_span(|_, span| (Token::FatArrow, span)),
		just(':').map_with_span(|_, span| (Token::Colon, span)),
		just(';').map_with_span(|_, span| (Token::Semicolon, span)),
		just(',').map_with_span(|_, span| (Token::Comma, span)),
		just('=').map_with_span(|_, span| (Token::Eq, span)),
		just('{').map_with_span(|_, span| (Token::OpenBracket, span)),
		just('}').map_with_span(|_, span| (Token::CloseBracket, span)),
		just('(').map_with_span(|_, span| (Token::OpenParen, span)),
		just(')').map_with_span(|_, span| (Token::CloseParen, span)),
		just('+').map_with_span(|_, span| (Token::Plus, span)),
		ident().map_with_span(|x, span| (Token::Ident(x), span)),
		digits(10)
			.from_str::<i32>()
			.unwrapped()
			.map_with_span(|x, span| (Token::Literal(x), span)),
	))
	.padded()
	.repeated()
	.map(SpanVec)
	.then_ignore(end())
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
	Var(String),
	Literal(i32),
	Add {
		lhs: Box<Spanned<Expr>>,
		rhs: Box<Spanned<Expr>>,
	},
	FnCall {
		func: Spanned<String>,
		params: Vec<Spanned<Expr>>,
	},
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
			.map_with_span(|(func, params), span| (Expr::FnCall { func, params }, span));
		let unary = choice((fncall.clone(), lit));
		let add = unary
			.then_ignore(just(Token::Plus))
			.then(expr)
			.map_with_span(|(lhs, rhs), span| {
				(
					Expr::Add {
						lhs: Box::new(lhs),
						rhs: Box::new(rhs),
					},
					span,
				)
			});
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
}

fn parse_stmt() -> impl Parser<Token, Spanned<Stmt>, Error = ParserError<Token>> {
	let binding = just(Token::Let)
		.ignore_then(select! {|span| Token::Ident(x) => (x, span)})
		.then_ignore(just(Token::Colon))
		.then(select!(|span| Token::Ident(x) => (x, span)).or_not())
		.then_ignore(just(Token::Eq))
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
