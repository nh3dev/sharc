use std::fmt::{Display, Formatter};

use crate::span::Span;

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum ReportKind {
	_NOTE_,
	_WARNING_,
	_ERROR_,
	ArgumentParserError,

	// Lexer
	UnexpectedCharacter,
	UnterminatedMultilineComment,
	UnterminatedLiteral,
	EmptyLiteral,

	// Parser
	UnexpectedToken,
	UnexpectedEOF,
	InvalidNumber,

	// typeinf
	TypeError, // instead
	UnexpectedType,
	UnexpectedGenericCount,
	NoSuchTrait,

	// mirgen
	NodeNotAnExpr,	

	// General
	IOError,
	SyntaxError,

	_FATAL_,
}

impl ReportKind {
	pub fn is_err(self) -> bool {
		self >= Self::_ERROR_
	}

	pub fn untitled(self) -> Report<'static> {
		Report {
			filename:  "",
			file:      "",
			kind:      self,
			title:     None,
			span:      None,
			label:     None,
			footers:   None,
			#[cfg(feature = "backtrace")]
			backtrace: Some(Box::new(std::backtrace::Backtrace::force_capture())),
			#[cfg(not(feature = "backtrace"))]
			backtrace: None,
		}
	}

	pub fn title<T: Display>(self, title: T) -> Report<'static> {
		#[cfg(debug_assertions)]
		assert!(!title.to_string().is_empty(), "use ReportKind::untitled() instead.");
		Report {
			filename:  "",
			file:      "",
			kind:      self,
			title:     Some(title.to_string()),
			span:      None,
			label:     None,
			footers:   None,
			#[cfg(feature = "backtrace")]
			backtrace: Some(Box::new(std::backtrace::Backtrace::force_capture())),
			#[cfg(not(feature = "backtrace"))]
			backtrace: None,
		}
	}
}

pub struct Report<'src> {
	pub backtrace: Option<Box<std::backtrace::Backtrace>>,
	pub filename:  &'src str,
	pub file:      &'src str,
	pub kind:      ReportKind,
	pub title:     Option<String>,
	pub span:      Option<Span>,
	pub label:     Option<String>,
	pub footers:   Option<Vec<String>>,
}

pub type Result<'src, T> = std::result::Result<T, Box<Report<'src>>>;

impl<'src> Report<'src> {
	pub fn span(mut self, span: Span) -> Self {
		self.span = Some(span); self
	}

	pub fn label<T: Display>(mut self, label: T) -> Self {
		self.label = Some(label.to_string()); self
	}

	pub fn help<T: Display>(self, help: T) -> Self {
		self.footer(format!("HELP: {help}"))
	}

	pub fn info<T: Display>(self, info: T) -> Self {
		self.footer(format!("INFO: {info}"))
	}

	pub fn note<T: Display>( self, note: T) -> Self {
		self.footer(format!("NOTE: {note}"))
	}

	pub fn as_err<T>(self) -> Result<'src, T> {
		Err(Box::new(self))
	}

	pub fn footer<T: Display>(mut self, text: T) -> Self {
		match self.footers {
			Some(ref mut footers) => footers.push(text.to_string()),
			None => self.footers = Some(vec![text.to_string()]),
		}
		self
	}

	pub fn file(mut self, file: &'src str, filename: &'src str) -> Self {
		self.file = file; 
		self.filename = filename;
		self
	}
}

impl<'a, T> From<Report<'a>> for Result<'a, T> {
	#[inline]
	fn from(report: Report<'a>) -> Self {
		Err(report.into())
	}
}

impl std::fmt::Debug for Report<'_> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{:?}", self.kind)
	}
}


