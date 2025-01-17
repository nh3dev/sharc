#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
	pub start: usize,
	pub end:   usize,
}

impl std::fmt::Display for Span {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}-{}", self.start, self.end)
	}
}

impl Span {
	pub fn new(start: usize) -> Self {
		Self { start, end: start }
	}

	pub fn end(mut self, end: usize) -> Self {
		self.end = end; self
	}

	pub fn len(mut self, len: usize) -> Self {
		self.end = self.start + len; self
	}

	pub fn extend(self, other: &Self) -> Self {
		assert!(self.start <= other.start, "other.start behind self.start! {} > {}", other.start, self.start);
		assert!(self.end <= other.end, "other.end behind self.end! {} > {}", other.end, self.end);

		Self {
			start: self.start,
			end:   other.end,
		}
	}
}
