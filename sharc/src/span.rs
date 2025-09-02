use std::fmt::{self, Display};
use colored::Colorize;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
	pub start: usize,
	pub end:   usize,
}

impl Display for Span {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", format!("{}-{}", self.start, self.end).bright_black())
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


#[derive(Debug, Clone)]
pub struct Sp<T> {
	pub span: Span,
	pub elem: T,
}

impl<T> std::ops::Deref for Sp<T> {
	type Target = T;
	fn deref(&self) -> &Self::Target 
		{ &self.elem }
}

impl<T> std::ops::DerefMut for Sp<T> {
	fn deref_mut(&mut self) -> &mut Self::Target 
		{ &mut self.elem }
}

pub trait Spannable {
	fn span(self, span: Span) -> Sp<Self> where Self: Sized
		{ Sp { span, elem: self } }
}

impl<T> Spannable for T {}

impl<T: Display> Display for Sp<T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match std::env::var("NO_SPAN") {
			Ok(_)  => write!(f, "{}", self.elem),
			Err(_) => write!(f, "{} {}", self.span, self.elem)
		}
	}
}
