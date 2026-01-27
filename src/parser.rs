use std::borrow::Cow;

use crate::{
	ast::{
		self, Attr, Attrs, Binop, Block, ExternBlock, FnDecl, FnDef, FnParam, FnParamNormal,
		ImplBlockGeneric, ImplBlockGenerics, PREFIX_PREC, SimplePath, SpanName, Statement,
		StructDef, StructDefKind, StructField, StructTupleItem, TypeAst, TypeAstNamed, Unop,
		UntypedBlock, UntypedDefinition, UntypedEnumDef, UntypedEnumDefValue, UntypedExternBlock,
		UntypedFnDecl, UntypedFnDef, UntypedGenericParam, UntypedGenericParams, UntypedImplBlock,
		UntypedModule, UntypedStatement, UntypedStructDef, UntypedUnionDef, infix_table,
		prefix_table,
		untyped::{UntypedExpr, UntypedExprStructField},
	},
	parser::lexer::lex,
	span::Span,
	tokens::{DelimKind, Token},
};

pub mod lexer;

pub fn parse_module(src: &str) -> UntypedModule {
	let tokens = lex(src);
	let mut parser = Parser::new(tokens);

	parser.parse_module()
}

struct Parser {
	tokens: Box<[(Span, Token)]>,
	pos: usize,
}

impl Parser {
	fn new(tokens: Box<[(Span, Token)]>) -> Self {
		Self { tokens, pos: 0 }
	}

	fn parse_module(&mut self) -> UntypedModule {
		let mut definitions = vec![];

		while self.tokens_left() {
			let item = self.parse_item();
			definitions.push(item);
		}

		ast::Module { definitions }
	}

	fn parse_item(&mut self) -> UntypedDefinition {
		let start = self.cur_start();
		let attrs = self.parse_attrs();
		let public = self.parse_publicity();
		let def = match self.peek() {
			// Token::Module => self.parse_module_def(public),
			Token::Struct => ast::TopLevelDef::StructDef(self.parse_struct_def(start, public)),
			Token::Enum => ast::TopLevelDef::EnumDef(self.parse_enum_def(start, public)),
			Token::Union => ast::TopLevelDef::UnionDef(self.parse_union_def(start, public)),
			Token::Fn => ast::TopLevelDef::FnDef(self.parse_fn_def(start, attrs, public)),
			Token::Impl => ast::TopLevelDef::ImplBlock(self.parse_impl_block()),
			Token::Extern => ast::TopLevelDef::ExternBlock(self.parse_extern_block(start, attrs)),
			_ => todo!("not yet handling unexpected items"),
		};

		def
	}

	fn parse_module_def(&mut self, public: bool) -> () {
		todo!("module definition ")
	}

	fn parse_struct_def(&mut self, start: usize, public: bool) -> UntypedStructDef {
		self.expect(Token::Struct);
		let name = self.parse_ident();
		let generics = if self.peek() == &Token::Lt {
			Some(self.parse_generic_params())
		} else {
			None
		};
		let kind = match self.peek() {
			Token::Semicolon => {
				self.shift();
				StructDefKind::Unit
			}
			Token::OParen => {
				let items = self.trailing_comma_separated(
					Token::OParen,
					Token::CParen,
					Self::parse_struct_tuple_item,
				);
				self.expect(Token::Semicolon);
				StructDefKind::Tuple { items }
			}
			Token::OCurlyBracket => {
				let fields = self.trailing_comma_separated(
					Token::OCurlyBracket,
					Token::CCurlyBracket,
					Self::parse_struct_field,
				);
				StructDefKind::Struct { fields }
			}
			_ => todo!(),
		};

		let span = self.span_from(start);
		UntypedStructDef {
			span,
			public,
			name,
			generics,
			kind,
		}
	}

	fn parse_struct_tuple_item(&mut self) -> StructTupleItem<()> {
		let start = self.cur_start();
		let public = if self.peek() == &Token::Pub {
			self.shift();
			true
		} else {
			false
		};
		let annot = self.parse_type();
		let span = self.span_from(start);
		StructTupleItem {
			span,
			public,
			annot,
			type_: (),
		}
	}

	fn parse_struct_field(&mut self) -> StructField<()> {
		let start = self.cur_start();
		let public = self.parse_publicity();
		let name = self.parse_ident();
		self.expect(Token::Colon);
		let annot = self.parse_type();

		let span = self.span_from(start);
		StructField {
			span,
			public,
			name,
			annot,
			type_: (),
		}
	}

	fn parse_enum_def(&mut self, start: usize, public: bool) -> UntypedEnumDef {
		self.expect(Token::Enum);
		let name = self.parse_ident();
		let repr = if self.peek() == &Token::Colon {
			self.shift();
			Some(self.parse_type())
		} else {
			None
		};
		let values = self.trailing_comma_separated(
			Token::OCurlyBracket,
			Token::CCurlyBracket,
			Self::parse_enum_def_value,
		);

		let span = self.span_from(start);
		UntypedEnumDef {
			span,
			name,
			repr,
			type_: (),
			values,
		}
	}

	fn parse_enum_def_value(&mut self) -> UntypedEnumDefValue {
		let name = self.parse_ident();
		let value = if self.peek() == &Token::Eq {
			self.shift();
			Some(self.parse_expr())
		} else {
			None
		};
		UntypedEnumDefValue { name, value }
	}

	fn parse_union_def(&mut self, start: usize, public: bool) -> UntypedUnionDef {
		todo!("union definition")
	}

	fn parse_impl_block(&mut self) -> UntypedImplBlock {
		let start = self.cur_start();
		self.expect(Token::Impl);
		let generics = if self.peek() == &Token::Lt {
			let items =
				self.trailing_comma_separated(Token::Lt, Token::Gt, Self::parse_impl_block_generic);
			Some(ImplBlockGenerics { items })
		} else {
			None
		};

		let impler = self.parse_type();

		let methods = vec![];
		self.expect(Token::OCurlyBracket);
		while self.peek() != &Token::CCurlyBracket {
			let method_start = self.cur_start();
			let attrs = self.parse_attrs();
			let public = self.parse_publicity();
			self.parse_fn_def(method_start, attrs, public);
		}
		self.expect(Token::CCurlyBracket);

		let span = self.span_from(start);
		UntypedImplBlock {
			span,
			generics,
			impler,
			methods,
		}
	}

	fn parse_impl_block_generic(&mut self) -> ImplBlockGeneric {
		let start = self.cur_start();
		let binding = self.parse_ident();
		let span = self.span_from(start);
		ImplBlockGeneric { span, binding }
	}

	fn parse_extern_block(&mut self, start: usize, attrs: Attrs) -> UntypedExternBlock {
		self.expect(Token::Extern);
		self.expect(Token::OCurlyBracket);
		let mut items = vec![];
		while self.peek() != &Token::CCurlyBracket {
			let attrs = self.parse_attrs();
			let publicity = self.parse_publicity();
			items.push(self.parse_fn_decl(start, attrs, publicity));
		}
		self.expect(Token::CCurlyBracket);
		UntypedExternBlock { items }
	}

	fn parse_fn_decl(&mut self, start: usize, attrs: Attrs, publicity: bool) -> UntypedFnDecl {
		self.expect(Token::Fn);
		let name = self.parse_ident();
		let params =
			self.trailing_comma_separated(Token::OParen, Token::CParen, Self::parse_fn_param);
		let ret_annot = if self.peek() == &Token::Arrow {
			self.shift();
			Some(self.parse_type())
		} else {
			None
		};
		self.expect(Token::Semicolon);
		let span = self.span_from(start);
		UntypedFnDecl {
			span,
			attrs,
			publicity,
			name,
			params,
			ret_annot,
			ret_type: (),
			type_: (),
		}
	}

	fn parse_fn_def(&mut self, start: usize, attrs: Attrs, public: bool) -> UntypedFnDef {
		self.expect(Token::Fn);
		let name = self.parse_ident();
		let params =
			self.trailing_comma_separated(Token::OParen, Token::CParen, Self::parse_fn_param);
		let return_annotation = if self.peek() == &Token::Arrow {
			self.shift();
			Some(self.parse_type())
		} else {
			None
		};

		let body = self.parse_block();

		let span = self.span_from(start);
		FnDef {
			span,
			attrs,
			name,
			params,
			return_annotation,
			body,
			return_type: (),
		}
	}

	fn parse_fn_param(&mut self) -> FnParam<(), SpanName> {
		//TODO parse receiver
		let start = self.cur_start();
		let attrs = self.parse_attrs();

		let binding = self.parse_ident();
		self.expect(Token::Colon);
		let annotation = self.parse_type();

		let span = self.span_from(start);
		FnParam::Normal(FnParamNormal {
			span,
			attrs,
			binding,
			annotation,
			type_: (),
		})
	}

	fn parse_generic_params(&mut self) -> UntypedGenericParams {
		let start = self.cur_start();
		let params = self.trailing_comma_separated(Token::Lt, Token::Gt, Self::parse_generic_param);

		let span = self.span_from(start);
		UntypedGenericParams { span, params }
	}

	fn parse_generic_param(&mut self) -> UntypedGenericParam {
		let start = self.cur_start();

		let phantom = if self.peek() == &Token::Phantom {
			self.shift();
			true
		} else {
			false
		};

		let binding = self.parse_ident();

		let default = if self.peek() == &Token::Eq {
			self.shift();
			Some(self.parse_type())
		} else {
			None
		};

		let span = self.span_from(start);
		UntypedGenericParam {
			span,
			phantom,
			binding,
			default,
			type_: (),
		}
	}

	fn parse_type(&mut self) -> TypeAst {
		let start = self.cur_start();
		match self.peek() {
			Token::Ident(_) => {
				let name = self.parse_simple_path();
				let mut type_vars = if self.peek() == &Token::Lt {
					self.trailing_comma_separated(Token::Lt, Token::Gt, Self::parse_type)
				} else {
					vec![]
				};
				let span = self.span_from(start);
				TypeAst::Named(TypeAstNamed {
					span,
					name,
					type_vars,
				})
			}
			Token::Wildcard => {
				self.shift();
				let span = self.span_from(start);
				TypeAst::Wildcard { span }
			}
			Token::Star => {
				self.shift();
				let pointee = Box::new(self.parse_type());
				let span = self.span_from(start);
				TypeAst::Ptr { span, pointee }
			}
			Token::Qmark => {
				self.shift();
				let inner = Box::new(self.parse_type());
				let span = self.span_from(start);
				TypeAst::Optional { span, inner }
			}
			Token::OSquareBracket => {
				self.shift();
				match self.peek() {
					Token::CSquareBracket => {
						self.shift();
						let item = Box::new(self.parse_type());
						let span = self.span_from(start);
						TypeAst::Sliced { span, item }
					}
					Token::LitInt {
						value,
						int_value,
						suffix,
					} => {
						let len = value.parse::<u64>().expect("type_ast array len");
						self.shift();
						self.expect(Token::CSquareBracket);
						let item = Box::new(self.parse_type());
						let span = self.span_from(start);
						TypeAst::Array { span, len, item }
					}
					_ => todo!("parse type_ast brackets"),
				}
			}
			_ => todo!("parse type_ast"),
		}
	}

	fn parse_block(&mut self) -> UntypedBlock {
		let start = self.cur_start();
		self.expect(Token::OCurlyBracket);
		let mut stmts = vec![];
		let mut tail = None;
		while self.peek() != &Token::CCurlyBracket {
			if self.is_nonexpr_start() {
				stmts.push(self.parse_statement());
				continue;
			}
			let start = self.cur_start();
			let expr = self.parse_expr();
			if self.peek() == &Token::Semicolon {
				self.shift();
				let span = self.span_from(start);
				stmts.push(Statement::Expr { span, expr });
				continue;
			} else if expr.is_block_expr() && self.peek2() != &Token::CCurlyBracket {
				let span = self.span_from(start);
				stmts.push(Statement::Expr { span, expr });
				continue;
			}
			tail = Some(expr);
			break;
		}
		self.expect(Token::CCurlyBracket);
		let span = self.span_from(start);

		Block { span, stmts, tail }
	}

	fn is_nonexpr_start(&self) -> bool {
		match self.peek() {
			Token::For | Token::Defer | Token::ErrDefer => true,
			_ => false,
		}
	}

	fn parse_statement(&mut self) -> UntypedStatement {
		let start = self.cur_start();
		match self.peek() {
			Token::Let => return self.parse_let_statement(),
			Token::Defer => {
				self.shift();
				let op = self.parse_expr();
				self.expect(Token::Semicolon);
				let span = self.span_from(start);
				return UntypedStatement::Defer { span, op };
			}
			Token::ErrDefer => {
				self.shift();
				let op = self.parse_expr();
				if op.is_block_expr() && self.peek() == &Token::Semicolon {
					self.shift();
				}
				let span = self.span_from(start);
				return UntypedStatement::ErrDefer { span, op };
			}
			_ => {}
		}

		let expr = self.parse_expr();
		self.expect(Token::Semicolon);
		let span = self.span_from(start);
		Statement::Expr { span, expr }
	}

	fn parse_let_statement(&mut self) -> UntypedStatement {
		let start = self.cur_start();
		self.expect(Token::Let);
		let binding = self.parse_ident();
		let annotation = if self.peek() == &Token::Colon {
			self.shift();
			Some(self.parse_type())
		} else {
			None
		};

		self.expect(Token::Eq);
		let val = self.parse_expr();
		self.expect(Token::Semicolon);

		let span = self.span_from(start);
		UntypedStatement::Let {
			span,
			binding,
			annotation,
			type_: (),
			val,
		}
	}

	fn parse_expr(&mut self) -> UntypedExpr {
		self.parse_expr_pratt(0)
	}

	fn parse_expr_pratt(&mut self, min_prec: usize) -> UntypedExpr {
		let start = self.cur_start();
		let mut left = self.parse_expr_nud();
		loop {
			let op = self.peek();
			if let Some(info) = infix_table(op) {
				if info.prec < min_prec {
					break;
				}
				self.shift(); // consume op token
				let right = self.parse_expr_pratt(info.next_prec());
				let end = self.last_end();
				left = UntypedExpr::Binop {
					span: Span::new(start, end),
					lhs: Box::new(left),
					op: info.op,
					rhs: Box::new(right),
				};
			} else {
				match op {
					Token::OParen => {
						self.shift(); // consume '('
						let mut args = vec![];
						if self.peek() != &Token::CParen {
							args.push(self.parse_expr());
							while self.peek() == &Token::Comma {
								self.shift();
								if self.peek() == &Token::CParen {
									break;
								}
								args.push(self.parse_expr());
							}
						}
						self.expect(Token::CParen);
						let end = self.last_end();
						left = UntypedExpr::CallLike {
							span: Span::new(start, end),
							func: Box::new(left),
							args,
						}
					}
					Token::OSquareBracket => {
						self.shift();
						let idx = self.parse_expr();
						self.expect(Token::CSquareBracket);
						let span = self.span_from(start);
						left = UntypedExpr::Index {
							span,
							val: Box::new(left),
							at: Box::new(idx),
						}
					}
					Token::Dot => {
						self.shift();
						match self.peek() {
							Token::Ident(_) => {
								let field = self.parse_ident();
								if self.peek() == &Token::OParen {
									self.shift();
									let mut args = vec![];
									if self.peek() != &Token::CParen {
										args.push(self.parse_expr());
										while self.peek() == &Token::Comma {
											self.shift();
											if self.peek() == &Token::CParen {
												break;
											}
											args.push(self.parse_expr());
										}
									}
									self.expect(Token::CParen);
									let span = self.span_from(start);
									left = UntypedExpr::MethodCall {
										span,
										val: Box::new(left),
										method: field,
										args,
									}
								} else {
									let span = self.span_from(start);
									left = UntypedExpr::FieldAccess {
										span,
										val: Box::new(left),
										field,
									}
								}
							}
							Token::LitInt {
								value,
								int_value,
								suffix,
							} => {
								// TODO handle better
								let idx = value.parse::<u64>().expect("parse int idx");
								self.shift();
								let span = self.span_from(start);
								left = UntypedExpr::TupleIndex {
									span,
									val: Box::new(left),
									idx,
								}
							}
							Token::Star => {
								self.shift();
								let span = self.span_from(start);
								left = UntypedExpr::Deref {
									span,
									ptr: Box::new(left),
								}
							}
							_ => {
								todo!("Handle invalid syntax")
							}
						}
					}
					_ => break,
				}
			}
		}
		left
	}

	fn parse_expr_nud(&mut self) -> UntypedExpr {
		let start = self.cur_start();
		if let Some(op) = prefix_table(self.peek()) {
			let on = Box::new(self.parse_expr_pratt(PREFIX_PREC));
			let end = self.last_end();
			return UntypedExpr::Unop {
				span: Span::new(start, end),
				op,
				on,
			};
		}
		self.parse_expr_atom()
	}

	fn parse_expr_atom(&mut self) -> UntypedExpr {
		let start = self.cur_start();
		let tok = self.shift().clone();
		let end = self.last_end();
		let span = Span::new(start, end);
		match tok {
			Token::True => UntypedExpr::BoolLiteral { span, value: true },
			Token::False => UntypedExpr::BoolLiteral { span, value: false },
			Token::LitInt {
				value,
				int_value,
				suffix,
			} => UntypedExpr::IntLiteral {
				span,
				value,
				int_value,
				suffix,
			},
			Token::LitStr(str) => UntypedExpr::StrLiteral { span, value: str },
			Token::Ident(_) => {
				self.pos -= 1;
				let binding = self.parse_simple_path();
				match self.peek() {
					Token::OCurlyBracket => {
						// struct like
						let fields = self.trailing_comma_separated(
							Token::OCurlyBracket,
							Token::CCurlyBracket,
							Self::parse_struct_literal_field,
						);

						let span = self.span_from(start);
						UntypedExpr::NamedStructLiteral {
							span,
							name: binding,
							fields,
						}
					}
					_ => {
						let span = self.span_from(start);
						UntypedExpr::Binding { span, binding }
					}
				}
			}
			Token::OParen => {
				self.pos -= 1;
				self.parse_expr_paren()
			}
			Token::If => {
				self.shift();
				let cond = Box::new(self.parse_expr());
				let then = Box::new(self.parse_block());
				let mut else_if = vec![];
				while matches!(self.peekn(2), [(_, Token::Else), (_, Token::If)]) {
					self.shift();
					self.shift();
					let cond = self.parse_expr();
					let then = self.parse_block();
					else_if.push((cond, then));
				}
				let else_ = if self.peek() == &Token::Else {
					self.shift();
					Some(Box::new(self.parse_block()))
				} else {
					None
				};
				let span = self.span_from(start);
				UntypedExpr::If {
					span,
					cond,
					then,
					else_if,
					else_,
				}
			}
			Token::For => {
				self.shift();
				let cond = Box::new(self.parse_expr());
				let body = Box::new(self.parse_block());
				let span = self.span_from(start);
				UntypedExpr::While { span, cond, body }
			}
			Token::While => {
				self.shift();
				let cond = Box::new(self.parse_expr());
				let body = Box::new(self.parse_block());
				let span = self.span_from(start);
				UntypedExpr::While { span, cond, body }
			}
			Token::Loop => {
				self.shift();
				UntypedExpr::Loop(Box::new(self.parse_block()))
			}
			c => todo!("parse expr for {:?}", c),
		}
	}

	fn parse_struct_literal_field(&mut self) -> UntypedExprStructField {
		let start = self.cur_start();
		let field = self.parse_ident();
		if self.peek() == &Token::Colon {
			self.shift();
			let value = self.parse_expr();
			let span = self.span_from(start);
			UntypedExprStructField::Normal { span, field, value }
		} else {
			let span = self.span_from(start);
			UntypedExprStructField::Shorthand { span, field }
		}
	}

	fn parse_expr_paren(&mut self) -> UntypedExpr {
		// could be unit, paren grouping, or tuple
		let start = self.cur_start();
		self.expect(Token::OParen);
		if self.peek() == &Token::CParen {
			self.shift();
			let span = self.span_from(start);
			return UntypedExpr::Unit { span };
		}
		let first = self.parse_expr();

		if self.peek() == &Token::Comma {
			let mut values = vec![first];
			while self.peek() == &Token::Comma {
				self.shift();
				if self.peek() == &Token::CParen {
					break;
				}
				values.push(self.parse_expr());
			}
			self.expect(Token::CParen);
			let span = self.span_from(start);
			UntypedExpr::TupleLiteral { span, values }
		} else {
			self.expect(Token::CParen);
			let span = self.span_from(start);
			UntypedExpr::Group {
				span,
				inner: Box::new(first),
			}
		}
	}

	fn parse_ident(&mut self) -> SpanName {
		let (span, t) = self.shift_spanned();
		match t {
			Token::Ident(name) => SpanName {
				span,
				name: name.clone(),
			},
			_ => todo!("handle errors"),
		}
	}

	fn parse_simple_path(&mut self) -> SimplePath {
		let qual_start = self.cur_start();
		let mut path = vec![self.parse_ident()];
		loop {
			match self.peekn(2) {
				[(_, Token::Scope), (span, Token::Ident(node))] => {
					path.push(SpanName {
						span: *span,
						name: node.clone(),
					});
					self.pos += 2;
				}
				_ => break,
			}
		}
		let span = self.span_from(qual_start);
		SimplePath { span, path }
	}

	pub fn parse_publicity(&mut self) -> bool {
		if self.peek() == &Token::Pub {
			self.shift();
			true
		} else {
			false
		}
	}

	pub fn parse_attrs(&mut self) -> Attrs {
		let mut attrs = vec![];
		while self.peek() == &Token::Hash && self.peek2() == &Token::OSquareBracket {
			self.shift();
			self.shift();
			let key = self.parse_simple_path();
			if self.peek() == &Token::Eq {
				self.shift();
				let value = self.parse_expr();
				attrs.push(Attr::Eq { key, value });
			} else {
				attrs.push(Attr::Simple { value: key });
			}
			self.expect(Token::CSquareBracket);
		}
		Attrs { attrs }
	}
}

impl Parser {
	fn peek(&self) -> &Token {
		self.tokens
			.get(self.pos)
			.map(|x| &x.1)
			.unwrap_or(&Token::EOF)
	}

	fn peek2(&self) -> &Token {
		self.tokens
			.get(self.pos + 1)
			.map(|x| &x.1)
			.unwrap_or(&Token::EOF)
	}

	fn peekn(&self, n: usize) -> &[(Span, Token)] {
		let rest = &self.tokens[self.pos..];
		&rest[..n.min(rest.len())]
	}

	fn shift(&mut self) -> &Token {
		let tok = self
			.tokens
			.get(self.pos)
			.map(|x| &x.1)
			.unwrap_or(&Token::EOF);
		self.pos += 1;
		tok
	}

	fn shift_spanned(&mut self) -> (Span, &Token) {
		let t = &self.tokens[self.pos];
		self.pos += 1;
		(t.0, &t.1)
	}

	fn expect(&mut self, t: Token) {
		if self.peek() != &t {
			panic!("Expected {:?} Found {:?}", t, self.peek());
		}
		self.shift();
	}

	fn cur_start(&self) -> usize {
		self.tokens.get(self.pos).unwrap().0.from
	}

	fn last_end(&self) -> usize {
		self.tokens.get(self.pos - 1).unwrap().0.to
	}

	fn span_from(&self, start: usize) -> Span {
		let end = self.last_end();
		Span::new(start, end)
	}

	fn tokens_left(&self) -> bool {
		self.peek() != &Token::EOF
	}

	fn trailing_comma_separated<Out>(
		&mut self,
		open: Token,
		close: Token,
		mut item: impl FnMut(&mut Self) -> Out,
	) -> Vec<Out> {
		let mut items = vec![];
		self.expect(open.clone());
		if self.peek() != &close {
			items.push(item(self));
			while self.peek() == &Token::Comma {
				self.shift();
				if self.peek() == &close {
					break;
				}
				items.push(item(self));
			}
		}
		self.expect(close);
		items
	}
}
