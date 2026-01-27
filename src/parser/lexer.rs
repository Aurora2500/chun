use std::{collections::hash_map::Values, ops::Add};

use ecow::EcoString;
use num_bigint::BigInt;

use crate::{
	span::Span,
	tokens::{FloatLiteral, Token},
};

pub fn lex(src: &'_ str) -> Box<[(Span, Token)]> {
	Lexer::new(src).collect::<Box<[_]>>()
}

#[derive(Debug, Clone)]
struct Lexer<'src> {
	src: &'src str,
	pos: usize,
}

impl<'src> Iterator for Lexer<'src> {
	type Item = (Span, Token);

	fn next(&mut self) -> Option<Self::Item> {
		self.skip_whitespace_and_comments();

		if let Some(t) = self.try_lex_symbol() {
			return Some(t);
		}

		match self.peek()? {
			'0'..='9' => self.lex_number(),
			'"' => self.lex_str_literal(),
			c if (c.is_alphabetic() || c == '_') => self.lex_kw_ident(),
			_ => todo!(),
		}
	}
}

static SYMBOLS: &[(&str, Token)] = &[
	("<<", Token::LtLt),
	(">>", Token::GtGt),
	("<=", Token::LtEq),
	(">=", Token::GtEq),
	("<", Token::Lt),
	(">", Token::Gt),
	("==", Token::EqEq),
	("!=", Token::Uneq),
	("+=", Token::PlusEq),
	("-=", Token::MinusEq),
	("*=", Token::StarEq),
	("/=", Token::SlashEq),
	("%=", Token::PercentEq),
	("&=", Token::AmpEq),
	("|=", Token::VbarEq),
	("^=", Token::CaretEq),
	("&&=", Token::AmpAmpEq),
	("||=", Token::VbarVbarEq),
	("->", Token::Arrow),
	("=>", Token::FatArrow),
	("+", Token::Plus),
	("-", Token::Minus),
	("*", Token::Star),
	("/", Token::Slash),
	("%", Token::Percent),
	("&", Token::Amp),
	("&&", Token::AmpAmp),
	("||", Token::VbarVbar),
	("?", Token::Qmark),
	("!", Token::Bang),
	("|", Token::Vbar),
	("^", Token::Caret),
	("..", Token::Elipsis),
	(".", Token::Dot),
	(",", Token::Comma),
	("::", Token::Scope),
	(":", Token::Colon),
	("#", Token::Hash),
	(";", Token::Semicolon),
	("=", Token::Eq),
	("(", Token::OParen),
	(")", Token::CParen),
	("[", Token::OSquareBracket),
	("]", Token::CSquareBracket),
	("{", Token::OCurlyBracket),
	("}", Token::CCurlyBracket),
];

impl<'src> Lexer<'src> {
	fn new(src: &'src str) -> Self {
		Self { src, pos: 0 }
	}

	fn peek(&self) -> Option<char> {
		self.rest().chars().next()
	}

	fn peek2(&self) -> Option<char> {
		self.rest().chars().nth(1)
	}

	fn rest(&self) -> &'src str {
		&self.src[self.pos..]
	}

	fn skip_whitespace_and_comments(&mut self) {
		'l: loop {
			match self.peek() {
				Some(ws) if ws.is_whitespace() => {
					//whitespace
					let i = self
						.rest()
						.find(|c: char| !c.is_whitespace())
						.unwrap_or_else(|| self.rest().len());
					self.pos += i;
					continue 'l;
				}
				Some(c) if c == '/' => match self.src.chars().nth(1) {
					// single comment
					Some('/') => {
						let i = self.rest().find('\n').unwrap_or_else(|| self.rest().len());
						self.pos += i;
						continue 'l;
					}
					// multi line comment
					Some('*') => {
						todo!("multi line comments not implemented")
					}
					_ => return,
				},
				_ => return,
			}
		}
	}

	fn lex_number(&mut self) -> Option<(Span, Token)> {
		if self.peek() == Some('0') {
			match self.peek2() {
				Some('x' | 'X') => self.lex_number_radix(16),
				Some('o' | 'O') => self.lex_number_radix(8),
				Some('b' | 'B') => self.lex_number_radix(2),
				_ => self.lex_decimal_number(),
			}
		} else {
			self.lex_decimal_number()
		}
	}

	fn lex_number_radix(&mut self, radix: u32) -> Option<(Span, Token)> {
		let start = self.pos;
		let mut value = EcoString::new();
		value.push_str(&self.rest()[..2]);
		self.pos += 2;
		loop {
			match self.peek() {
				Some(c) if c.is_digit(radix) => {
					self.pos += 1;
					value.push(c);
				}
				Some('_') => {
					self.pos += 1;
				}
				_ => {
					break;
				}
			}
		}
		let value_int = BigInt::parse_bytes(value.as_str()[2..].as_bytes(), radix)
			.expect("lex_number_radix parse bigint");
		let suffix_len = self
			.rest()
			.find(|c: char| !matches!(c, 'a'..='z'| 'A'..='Z' |'0'..='9'))
			.unwrap_or(self.rest().len());
		let suffix = if suffix_len > 0 {
			Some(EcoString::from(&self.rest()[..suffix_len]))
		} else {
			None
		};
		self.pos += suffix_len;

		let end = self.pos;
		Some((
			Span::new(start, end),
			Token::LitInt {
				value,
				int_value: value_int,
				suffix,
			},
		))
	}

	fn lex_decimal_number(&mut self) -> Option<(Span, Token)> {
		let start = self.pos;

		let mut value = EcoString::new();
		if self.peek() == Some('-') {
			value.push('-');
			self.pos += 1;
		}

		let mut is_float = false;
		loop {
			match self.peek() {
				Some(c @ '0'..='9') => {
					value.push(c);
					self.pos += 1;
				}
				Some('_') => {
					self.pos += 1;
				}
				_ => {
					break;
				}
			}
		}

		// decimal part
		if self.peek() == Some('.') && matches!(self.peek2(), Some('0'..='9')) {
			is_float = true;
			value.push('.');
			self.pos += 1;
			loop {
				match self.peek() {
					Some(c @ '0'..='9') => {
						value.push(c);
						self.pos += 1;
					}
					Some('_') => {
						self.pos += 1;
					}
					_ => {
						break;
					}
				}
			}
		}

		// exponent
		if matches!(self.peek(), Some('e' | 'E'))
			& matches!(self.peek2(), Some('+' | '-' | '0'..='9'))
		{
			is_float = true;
			self.pos += 1;
			match self.peek() {
				Some(c @ ('+' | '-')) => {
					self.pos += 1;
					value.push(c);
				}
				_ => {}
			}
			loop {
				match self.peek() {
					Some(c @ '0'..='9') => {
						value.push(c);
						self.pos += 1;
					}
					Some('_') => {
						self.pos += 1;
					}
					_ => {
						break;
					}
				}
			}
		}

		// suffix
		let suffix_len = self
			.rest()
			.find(|c: char| !matches!(c, 'a'..='z'| 'A'..='Z' |'0'..='9'))
			.unwrap_or(self.rest().len());
		let suffix = if suffix_len > 0 {
			Some(EcoString::from(&self.rest()[..suffix_len]))
		} else {
			None
		};

		let end = self.pos;
		Some((
			Span::new(start, end),
			if is_float {
				let x = value.parse::<f64>().expect("lex_decimal_number float");
				let value_float = FloatLiteral::new(x);
				Token::LitFloat {
					value,
					float_value: value_float,
					suffix,
				}
			} else {
				let value_int =
					BigInt::parse_bytes(value.as_bytes(), 10).expect("lex_decimal_number int");
				Token::LitInt {
					value,
					int_value: value_int,
					suffix,
				}
			},
		))
	}

	fn lex_str_literal(&mut self) -> Option<(Span, Token)> {
		let mut content = EcoString::new();
		let start = self.pos;
		self.pos += 1;
		let mut escaping = false;
		for (pos, c) in self.rest().char_indices() {
			match escaping {
				true => todo!("escaped strings not handled yet"),
				false => {
					if c == '"' {
						self.pos += pos + 1;
						return Some((Span::new(start, self.pos), Token::LitStr(content)));
					}
					if c == '\\' {
						escaping = true;
					}

					content.push(c);
				}
			}
		}
		None
	}

	fn try_lex_symbol(&mut self) -> Option<(Span, Token)> {
		for (op, tok) in SYMBOLS.iter() {
			if self.rest().starts_with(op) {
				let start = self.pos;
				let end = start + op.len();
				self.pos = end;
				return Some((Span::new(start, end), tok.clone()));
			}
		}

		None
	}

	fn lex_kw_ident(&mut self) -> Option<(Span, Token)> {
		let len = self
			.rest()
			.find(|c: char| !(c.is_alphanumeric() || c == '_'))
			.unwrap_or_else(|| self.rest().len());
		let start = self.pos;
		let end = start + len;
		let tok = &self.src[start..end];
		self.pos = end;
		let tok = match tok {
			"mod" => Token::Module,
			"fn" => Token::Fn,
			"struct" => Token::Struct,
			"enum" => Token::Enum,
			"union" => Token::Union,
			"trait" => Token::Trait,
			"impl" => Token::Impl,
			"extern" => Token::Extern,
			"use" => Token::Use,
			"macro" => Token::Macro,
			"let" => Token::Let,
			"if" => Token::If,
			"else" => Token::Else,
			"for" => Token::For,
			"in" => Token::In,
			"while" => Token::While,
			"loop" => Token::Loop,
			"break" => Token::Break,
			"continue" => Token::Continue,
			"match" => Token::Match,
			"defer" => Token::Defer,
			"errdefer" => Token::ErrDefer,
			"phantom" => Token::Phantom,
			"mut" => Token::Mut,
			"pub" => Token::Pub,
			"as" => Token::As,
			"where" => Token::Where,
			"true" => Token::True,
			"false" => Token::False,
			"_" => Token::Wildcard,
			ident => Token::Ident(EcoString::from(ident)),
		};
		Some((Span::new(start, end), tok))
	}
}
