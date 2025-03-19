use std::ops::Range;

use crate::transformer::error::SemanticError;

pub mod ast;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Scalar {
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
	Scalar(Scalar),
}

pub fn parse_scalar(ty: &str, span: Range<usize>) -> Result<Scalar, SemanticError> {
	use Scalar::*;
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
