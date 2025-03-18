use std::ops::Range;

use super::inference::UniType;

#[derive(Debug, Clone)]
pub enum SemanticError<'a> {
	RefNonExistingFunc { span: Range<usize> },
	DuplicateFunc { name: &'a str },
	UnrecognizedType { ty: &'a str, span: Range<usize> },
	IncompatibleTypes { a: UniType, b: UniType },
}
