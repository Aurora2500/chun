use std::collections::HashMap;

use ecow::EcoString;

use crate::{
	ast::{
		FnParam, FnParamNormal, FnParamReceiver, ResolvedBlock, ResolvedDefinition,
		ResolvedEnumDef, ResolvedEnumDefValue, ResolvedFnDecl, ResolvedFnDef, ResolvedModule,
		ResolvedStatement, ResolvedStructDef, SpanName, Statement, UntypedBlock, UntypedEnumDef,
		UntypedEnumDefValue, UntypedFnDecl, UntypedFnDef, UntypedModule, UntypedStatement,
		UntypedStructDef,
		resolved::{ResolvedExpr, ResolvedExprStructField},
		untyped::UntypedExpr,
	},
	span::Span,
};

#[derive(Debug, Clone)]
pub struct Symbol {
	pub id: SymbolId,
	pub kind: SymbolKind,
	pub original: SpanName,
}

#[derive(Debug, Clone, Copy)]
pub enum SymbolKind {
	Local,
	Function,
	Struct,
	Enum,
	EnumValue,
	Union,
	UnionVariant,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(u64);

#[derive(Debug, Default)]
pub struct Gen(u64);

impl Gen {
	pub fn new() -> Self {
		Self(0)
	}

	pub fn get_id(&mut self) -> SymbolId {
		let id = SymbolId(self.0);
		self.0 += 1;
		id
	}
}

#[derive(Default)]
struct SymbolTable<'parent> {
	parent: Option<&'parent Self>,
	table: HashMap<EcoString, Symbol>,
}

impl<'parent> SymbolTable<'parent> {
	fn new() -> Self {
		Self::default()
	}

	fn scope<'child: 'parent>(&'child self) -> SymbolTable<'child> {
		Self {
			parent: Some(self),
			table: HashMap::new(),
		}
	}

	fn get(&self, k: &str) -> Option<Symbol> {
		match self.table.get(k) {
			Some(sym) => Some(sym.clone()),
			None => match &self.parent {
				Some(parent) => parent.get(k),
				None => None,
			},
		}
	}

	fn shadow(&mut self, name: EcoString, sym: Symbol) {
		self.insert_inner(true, name, sym);
	}

	fn insert(&mut self, name: EcoString, sym: Symbol) {
		self.insert_inner(false, name, sym);
	}

	fn insert_inner(&mut self, can_shadow: bool, name: EcoString, sym: Symbol) {
		self.table.insert(name, sym);
	}
}

pub fn symbol_pass(ast: &UntypedModule) -> ResolvedModule {
	let mut resolver = Resolver::default();
	let table = resolver.gen_table(ast);
	let res_ast = resolver.resolve(ast, &table);
	res_ast
}

#[derive(Default)]
struct Resolver {
	id_gen: Gen,
}

impl Resolver {
	fn resolve(&mut self, ast: &UntypedModule, table: &SymbolTable) -> ResolvedModule {
		let mut structs = vec![];
		let mut enums = vec![];
		let mut unions = vec![];
		let mut fn_defs = vec![];
		let mut fn_decls = vec![];
		// let mut structs = vec![];

		for item in ast.definitions.iter() {
			match item {
				crate::ast::TopLevelDef::StructDef(struct_def) => {
					structs.push(self.resolve_struct_def(struct_def, table));
				}
				crate::ast::TopLevelDef::EnumDef(enum_def) => {
					enums.push(self.resolve_enum_def(enum_def, table));
				}
				crate::ast::TopLevelDef::UnionDef(union_def) => todo!(),
				crate::ast::TopLevelDef::ImplBlock(impl_block) => todo!("resolve impls"),
				crate::ast::TopLevelDef::ExternBlock(extern_block) => {
					for decl in extern_block.items.iter() {
						fn_decls.push(self.resolve_fn_decl(decl, table));
					}
				}
				crate::ast::TopLevelDef::FnDef(fn_def) => {
					fn_defs.push(self.resolve_fn_def(fn_def, table));
				}
			}
		}

		let definitions = ResolvedDefinition {
			structs,
			enums,
			unions,
			fn_defs,
			fn_decls,
		};
		ResolvedModule { definitions }
	}

	fn resolve_struct_def(
		&mut self,
		def: &UntypedStructDef,
		table: &SymbolTable,
	) -> ResolvedStructDef {
		let UntypedStructDef {
			span,
			public,
			name,
			generics,
			kind,
		} = def.clone();

		ResolvedStructDef {
			span,
			public,
			name: table
				.get(&name.name)
				.expect("resolve_struct_def resolved")
				.clone(),
			generics,
			kind,
		}
	}

	fn resolve_enum_def(&mut self, def: &UntypedEnumDef, table: &SymbolTable) -> ResolvedEnumDef {
		fn resolve_enum_def_value(val: &UntypedEnumDefValue) -> ResolvedEnumDefValue {
			ResolvedEnumDefValue {
				name: todo!(),
				value: todo!(),
			}
		}

		let UntypedEnumDef {
			span,
			name,
			repr,
			type_,
			values,
		} = def.clone();

		ResolvedEnumDef {
			span,
			name: todo!(),
			repr,
			type_,
			values: todo!(),
		}
	}

	fn resolve_union_def(&mut self) {}

	fn resolve_fn_def(&mut self, def: &UntypedFnDef, table: &SymbolTable) -> ResolvedFnDef {
		let UntypedFnDef {
			span,
			attrs,
			name,
			params,
			return_annotation,
			return_type,
			body,
		} = def.clone();

		let mut fn_scope = table.scope();
		let mut new_params = vec![];
		for param in params.iter() {
			match param {
				FnParam::Receiver(fn_param_receiver) => todo!(),
				FnParam::Normal(p) => {
					let symbol = Symbol {
						id: self.id_gen.get_id(),
						kind: SymbolKind::Local,
						original: p.binding.clone(),
					};
					fn_scope.insert(p.binding.name.clone(), symbol.clone());
					new_params.push(FnParam::Normal(FnParamNormal {
						span: p.span,
						attrs: p.attrs.clone(),
						binding: symbol,
						annotation: p.annotation.clone(),
						type_: (),
					}));
				}
			}
		}

		ResolvedFnDef {
			span,
			attrs,
			name: table
				.get(&name.name)
				.expect("symbol table should have gathered it")
				.clone(),
			params: new_params,
			return_annotation,
			return_type,
			body: self.resolve_block(&body, &mut fn_scope),
		}
	}

	fn resolve_block(&mut self, block: &UntypedBlock, table: &mut SymbolTable) -> ResolvedBlock {
		let UntypedBlock { span, stmts, tail } = block.clone();

		let stmts = stmts
			.into_iter()
			.map(|stmt| self.resolve_stmt(&stmt, table))
			.collect();
		let tail = tail.map(|expr| self.resolve_expr(&expr, table));

		ResolvedBlock { span, stmts, tail }
	}

	fn resolve_stmt(
		&mut self,
		stmt: &UntypedStatement,
		table: &mut SymbolTable,
	) -> ResolvedStatement {
		match stmt {
			Statement::Expr { span, expr } => Statement::Expr {
				span: *span,
				expr: self.resolve_expr(&expr, table),
			},
			Statement::Let {
				span,
				binding,
				annotation,
				type_,
				val,
			} => {
				let val = self.resolve_expr(&val, table);
				let new_binding = Symbol {
					id: self.id_gen.get_id(),
					kind: SymbolKind::Local,
					original: binding.clone(),
				};

				Statement::Let {
					span: *span,
					binding: new_binding,
					annotation: annotation.clone(),
					type_: (),
					val,
				}
			}
			Statement::ErrDefer { span, op } => Statement::ErrDefer {
				span: *span,
				op: self.resolve_expr(op, table),
			},
			Statement::Defer { span, op } => Statement::Defer {
				span: *span,
				op: self.resolve_expr(op, table),
			},
		}
	}

	fn resolve_expr(&mut self, expr: &UntypedExpr, table: &SymbolTable) -> ResolvedExpr {
		match expr.clone() {
			UntypedExpr::Binding { span, binding } => {
				let Some(binding) = table.get(&binding.path.last().unwrap().name) else {
					panic!("not handling this yet")
				};
				ResolvedExpr::Binding { span, binding }
			}
			UntypedExpr::StrLiteral { span, value } => ResolvedExpr::StrLiteral { span, value },
			UntypedExpr::IntLiteral {
				span,
				value,
				int_value,
				suffix,
			} => ResolvedExpr::IntLiteral {
				span,
				value,
				int_value,
				suffix,
			},
			UntypedExpr::FloatLiteral {
				span,
				value,
				suffix,
			} => ResolvedExpr::FloatLiteral {
				span,
				value,
				suffix,
			},
			UntypedExpr::BoolLiteral { span, value } => ResolvedExpr::BoolLiteral { span, value },
			UntypedExpr::Unit { span } => ResolvedExpr::Unit { span },
			UntypedExpr::TupleLiteral { span, values } => ResolvedExpr::TupleLiteral {
				span,
				values: values
					.into_iter()
					.map(|v| self.resolve_expr(&v, table))
					.collect(),
			},
			UntypedExpr::NamedStructLiteral { span, name, fields } => {
				let Some(struct_sym) = table.get(&name.path.last().unwrap().name) else {
					todo!("handle this later")
				};
				let fields = fields
					.into_iter()
					.map(|f| match f {
						crate::ast::untyped::UntypedExprStructField::Shorthand { span, field } => {
							let Some(sym) = table.get(&field.name) else {
								todo!("handle this")
							};
							let value = ResolvedExpr::Binding { span, binding: sym };
							ResolvedExprStructField { span, field, value }
						}
						crate::ast::untyped::UntypedExprStructField::Normal {
							span,
							field,
							value,
						} => ResolvedExprStructField {
							span,
							field,
							value: self.resolve_expr(&value, table),
						},
					})
					.collect();
				ResolvedExpr::NamedStructLiteral {
					span,
					name: struct_sym,
					fields,
				}
			}
			UntypedExpr::Block(block) => {
				let mut scoped = table.scope();
				ResolvedExpr::Block(Box::new(self.resolve_block(&block, &mut scoped)))
			}
			UntypedExpr::If {
				span,
				cond,
				then,
				else_if,
				else_,
			} => {
				let mut then_scope = table.scope();
				ResolvedExpr::If {
					span,
					cond: Box::new(self.resolve_expr(&cond, &then_scope)),
					then: Box::new(self.resolve_block(&then, &mut then_scope)),
					else_if: else_if
						.into_iter()
						.map(|(e, b)| {
							let mut scoped = table.scope();
							(
								self.resolve_expr(&e, &scoped),
								self.resolve_block(&b, &mut scoped),
							)
						})
						.collect(),
					else_: else_.map(|e| {
						Box::new({
							let mut scoped = table.scope();
							self.resolve_block(&e, &mut scoped)
						})
					}),
				}
			}
			UntypedExpr::For {
				span,
				iterator,
				body,
			} => {
				let mut scoped = table.scope();
				ResolvedExpr::For {
					span,
					iterator: Box::new(self.resolve_expr(&iterator, &mut scoped)),
					body: Box::new(self.resolve_block(&body, &mut scoped)),
				}
			}
			UntypedExpr::While { span, cond, body } => {
				let mut scoped = table.scope();
				ResolvedExpr::While {
					span,
					cond: Box::new(self.resolve_expr(&cond, &mut scoped)),
					body: Box::new(self.resolve_block(&body, &mut scoped)),
				}
			}
			UntypedExpr::Loop(block) => todo!(),
			UntypedExpr::Match { span, on, arms } => todo!(),
			UntypedExpr::Binop { span, lhs, op, rhs } => ResolvedExpr::Binop {
				span,
				lhs: Box::new(self.resolve_expr(&lhs, table)),
				op,
				rhs: Box::new(self.resolve_expr(&rhs, table)),
			},
			UntypedExpr::Unop { span, op, on } => ResolvedExpr::Unop {
				span,
				op,
				on: Box::new(self.resolve_expr(&on, table)),
			},
			UntypedExpr::Group { span, inner } => ResolvedExpr::Group {
				span,
				inner: Box::new(self.resolve_expr(&inner, table)),
			},
			UntypedExpr::CallLike { span, func, args } => ResolvedExpr::CallLike {
				span,
				func: Box::new(self.resolve_expr(&func, table)),
				args: args
					.into_iter()
					.map(|arg| self.resolve_expr(&arg, table))
					.collect(),
			},
			UntypedExpr::Index { span, val, at } => todo!(),
			UntypedExpr::TupleIndex { span, val, idx } => todo!(),
			UntypedExpr::FieldAccess { span, val, field } => todo!(),
			UntypedExpr::MethodCall {
				span,
				val,
				method,
				args,
			} => todo!(),
			UntypedExpr::Deref { span, ptr } => ResolvedExpr::Deref {
				span,
				ptr: Box::new(self.resolve_expr(&ptr, table)),
			},
		}
	}

	fn resolve_fn_decl(&mut self, decl: &UntypedFnDecl, table: &SymbolTable) -> ResolvedFnDecl {
		let UntypedFnDecl {
			span,
			attrs,
			publicity,
			name,
			params,
			ret_annot,
			ret_type,
			type_,
		} = decl.clone();
		ResolvedFnDecl {
			span,
			attrs,
			publicity,
			name: table.get(&name.name).unwrap(),
			params: params
				.into_iter()
				.map(|p| match p {
					FnParam::Receiver(FnParamReceiver { span, attrs, type_ }) => {
						FnParam::Receiver(FnParamReceiver {
							span,
							attrs,
							type_: (),
						})
					}
					FnParam::Normal(FnParamNormal {
						span,
						attrs,
						binding,
						annotation,
						type_,
					}) => FnParam::Normal(FnParamNormal {
						span,
						attrs,
						binding: Symbol {
							id: self.id_gen.get_id(),
							kind: SymbolKind::Local,
							original: binding,
						},
						annotation,
						type_: (),
					}),
				})
				.collect(),
			ret_annot,
			ret_type,
			type_,
		}
	}

	fn gen_table(&mut self, ast: &UntypedModule) -> SymbolTable<'static> {
		let mut top_level_symbols = SymbolTable::new();
		let mut symbol_gen = Gen::new();

		let mut impls = vec![];

		for def in ast.definitions.iter() {
			match def {
				crate::ast::TopLevelDef::StructDef(struct_def) => {
					top_level_symbols.insert(
						struct_def.name.name.clone(),
						Symbol {
							id: symbol_gen.get_id(),
							kind: SymbolKind::Struct,
							original: struct_def.name.clone(),
						},
					);
				}
				crate::ast::TopLevelDef::EnumDef(enum_def) => {
					top_level_symbols.insert(
						enum_def.name.name.clone(),
						Symbol {
							id: symbol_gen.get_id(),
							kind: SymbolKind::Enum,
							original: enum_def.name.clone(),
						},
					);
				}
				crate::ast::TopLevelDef::UnionDef(union_def) => {
					top_level_symbols.insert(
						union_def.name.name.clone(),
						Symbol {
							id: symbol_gen.get_id(),
							kind: SymbolKind::Union,
							original: union_def.name.clone(),
						},
					);
				}
				crate::ast::TopLevelDef::ImplBlock(impl_block) => {
					impls.push(impl_block);
				}
				crate::ast::TopLevelDef::ExternBlock(extern_block) => {
					for decl in extern_block.items.iter() {
						top_level_symbols.insert(
							decl.name.name.clone(),
							Symbol {
								id: symbol_gen.get_id(),
								kind: SymbolKind::Function,
								original: decl.name.clone(),
							},
						);
					}
				}
				crate::ast::TopLevelDef::FnDef(fn_def) => {
					top_level_symbols.insert(
						fn_def.name.name.clone(),
						Symbol {
							id: symbol_gen.get_id(),
							kind: SymbolKind::Function,
							original: fn_def.name.clone(),
						},
					);
				}
			}
		}
		top_level_symbols
	}
}
