#[derive(Debug, Clone, Copy)]
pub struct Span {
	pub from: usize,
	pub to: usize,
}

impl Span {
	pub fn new(from: usize, to: usize) -> Self {
		Self { from, to }
	}
}

pub trait HasSpan {
	fn span(&self) -> Span;
}
