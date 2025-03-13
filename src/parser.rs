use chumsky::{
	error::Simple,
	prelude::{choice, end, filter, just},
	recursive::recursive,
	select,
	text::{digits, ident, keyword, TextParser},
	Parser,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
	Literal(i32),
	Ident(String),
	Fn,
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

pub fn parse_tokens() -> impl Parser<char, Vec<Token>, Error = Simple<char>> {
	choice((
		keyword("fn").to(Token::Fn),
		keyword("let").to(Token::Let),
		just("=>").to(Token::FatArrow),
		just(':').to(Token::Colon),
		just(';').to(Token::Semicolon),
		just(',').to(Token::Comma),
		just('=').to(Token::Eq),
		just('{').to(Token::OpenBracket),
		just('}').to(Token::CloseBracket),
		just('(').to(Token::OpenParen),
		just(')').to(Token::CloseParen),
		just('+').to(Token::Plus),
		ident().map(Token::Ident),
		digits(10).map(|digits: String| {
			Token::Literal(digits.parse().expect("should have only matched digits"))
		}),
	))
	.padded()
	.repeated()
	.then_ignore(end())
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
	Var(String),
	Literal(i32),
	Add { lhs: Box<Expr>, rhs: Box<Expr> },
	FnCall { func: String, params: Vec<Expr> },
}

fn parse_expr() -> impl Parser<Token, Expr, Error = Simple<Token>> {
	recursive(|expr| {
		let lit = select! {Token::Literal(x) => Expr::Literal(x)};
		let fncall = select! {Token::Ident(x) => x}
			.then(
				expr.clone()
					.separated_by(just(Token::Comma))
					.allow_trailing()
					.delimited_by(just(Token::OpenParen), just(Token::CloseParen)),
			)
			.map(|(func, params)| Expr::FnCall { func, params });
		let var = select! {Token::Ident(x) => Expr::Var(x)};

		let unary = choice((lit, fncall.clone(), var));
		let add = unary
			.then_ignore(just(Token::Plus))
			.then(expr)
			.map(|(lhs, rhs)| Expr::Add {
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
			});
		choice((add, fncall, lit, var))
	})
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
	Binding { ident: String, value: Box<Expr> },
	Expr(Box<Expr>),
}

fn parse_stmt() -> impl Parser<Token, Stmt, Error = Simple<Token>> {
	let binding = just(Token::Let)
		.ignore_then(select! {Token::Ident(x) => x})
		.then_ignore(just(Token::Colon))
		.then_ignore(filter(|x| matches!(x, Token::Ident(_))))
		.then_ignore(just(Token::Eq))
		.then(parse_expr())
		.map(|(ident, value)| Stmt::Binding {
			ident,
			value: Box::new(value),
		});

	binding
		.or(parse_expr().map(|expr| Stmt::Expr(Box::new(expr))))
		.then_ignore(just(Token::Semicolon))
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnParam {
	pub name: String,
	pub r#type: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FnBody {
	Lambda(Box<Expr>),
	Block(Vec<Stmt>),
}

fn parse_fnbody() -> impl Parser<Token, FnBody, Error = Simple<Token>> {
	let block = parse_stmt()
		.repeated()
		.delimited_by(just(Token::OpenBracket), just(Token::CloseBracket))
		.map(FnBody::Block);
	let lambda = just(Token::FatArrow)
		.ignore_then(parse_expr())
		.then_ignore(just(Token::Semicolon))
		.map(|expr| FnBody::Lambda(Box::new(expr)));
	choice((block, lambda))
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnDef {
	pub ident: String,
	pub params: Vec<FnParam>,
	pub body: FnBody,
}

fn parse_fn_def() -> impl Parser<Token, FnDef, Error = Simple<Token>> {
	let ident = select! {Token::Ident(x) => x};
	let params = ident
		.clone()
		.then_ignore(just(Token::Colon))
		.then(ident.clone())
		.map(|(name, r#type)| FnParam { name, r#type })
		.separated_by(just(Token::Comma))
		.allow_trailing()
		.delimited_by(just(Token::OpenParen), just(Token::CloseParen));

	just(Token::Fn)
		.ignore_then(ident.clone())
		.then(params)
		.then(parse_fnbody())
		.map(|((ident, params), body)| FnDef {
			ident,
			params,
			body,
		})
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Program(pub Vec<FnDef>);

pub fn parse_program() -> impl Parser<Token, Program, Error = Simple<Token>> {
	parse_fn_def().repeated().then_ignore(end()).map(Program)
}
