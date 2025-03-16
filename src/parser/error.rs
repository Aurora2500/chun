use std::{collections::HashSet, hash::Hash, ops::Range};

use chumsky::Error;

type Span = Range<usize>;

#[derive(Debug, Clone)]
pub enum ParserError<I> {
	Specific {
		span: Span,
		expected: HashSet<Option<I>>,
		found: Option<I>,
	},
	General {
		specific: Vec<ParserError<I>>,
	},
}

pub enum ParseErrorLabel {
	None,
	Generic(String),
}

impl<I> Error<I> for ParserError<I>
where
	I: Eq + Hash,
{
	type Span = Span;

	type Label = ParseErrorLabel;

	fn expected_input_found<Iter: IntoIterator<Item = Option<I>>>(
		span: Self::Span,
		expected: Iter,
		found: Option<I>,
	) -> Self {
		Self::Specific {
			span,
			expected: expected.into_iter().collect(),
			found,
		}
	}

	fn with_label(self, label: Self::Label) -> Self {
		todo!()
	}

	fn merge(self, other: Self) -> Self {
		Self::General {
			specific: vec![self, other],
		}
	}
}
