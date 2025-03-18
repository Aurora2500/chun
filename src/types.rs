use std::ops::Range;

use crate::transformer::error::SemanticError;

pub mod ast;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Primitive {
	I32,
	I64,
	U32,
	U64,
	F32,
	F64,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MonoType {
	Unit,
	Primitive(Primitive),
}

pub fn parse_primitive(ty: &str, span: Range<usize>) -> Result<Primitive, SemanticError> {
	use Primitive::*;
	Ok(match ty {
		"i32" => I32,
		"u32" => U32,
		"i64" => I64,
		"u64" => U64,
		"f32" => F32,
		"f64" => F64,
		_ => return Err(SemanticError::UnrecognizedType { ty, span }),
	})
}
