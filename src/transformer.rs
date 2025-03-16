pub mod error;

use std::collections::{hash_map::Entry, HashMap};

use error::SemanticError;

use crate::parser::{FnDef, FnSignature, Program, TopLevelDef};

#[derive(Default)]
struct Ctx {
	existing_funcs: HashMap<String, FnSignature>,
}

pub fn transform(ast: &Program) -> Result<(), Vec<SemanticError>> {
	let mut ctx = Ctx::default();
	let mut errors = Vec::new();

	for tl in ast.0.iter() {
		let signature = match tl {
			TopLevelDef::Extern(func) => func.clone(),
			TopLevelDef::Fn(FnDef {
				ident,
				params,
				body: _,
			}) => FnSignature {
				ident: ident.clone(),
				params: params.clone(),
			},
		};
		let entry = ctx.existing_funcs.entry(signature.ident.0.clone());
		if matches! { entry,  Entry::Occupied(_) } {
			errors.push(SemanticError::DuplicateFunc {
				name: entry.key().clone(),
			});
			continue;
		} else {
			entry.insert_entry(signature);
		}
	}

	if errors.is_empty() {
		Ok(())
	} else {
		Err(errors)
	}
}
