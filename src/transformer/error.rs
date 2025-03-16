#[derive(Debug, Clone)]
pub enum SemanticError {
	RefNonExisting,
	BadParams,
	DuplicateFunc { name: String },
}
