use crate::span::Span;

#[derive(Debug, Clone, Copy)]
pub enum DiagnosticsKind {
	ParseError,
}

#[derive(Debug, Clone, Copy)]
pub enum Severity {
	Error,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
	span: Span,
	severity: Severity,
	kind: DiagnosticsKind,
	message: String,
}

pub struct Diagnostics {
	messages: Vec<Diagnostic>,
	flag: bool,
}

impl Diagnostics {
	pub fn new() -> Self {
		Diagnostics {
			messages: vec![],
			flag: false,
		}
	}

	pub fn add(&mut self, diag: Diagnostic) {
		self.flag = true;
		self.messages.push(diag);
	}

	pub fn check(&self) -> bool {
		self.flag
	}

	pub fn reset_flag(&mut self) {
		self.flag = false;
	}
}
