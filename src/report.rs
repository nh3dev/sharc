use std::fmt::{Display, Formatter};
use std::sync::atomic::{AtomicUsize, Ordering};

use colored::{Color, Colorize};
pub use progress::LogHandler;

use crate::util::CACHE;
use crate::span::Span;

pub static ERR_COUNT: AtomicUsize = AtomicUsize::new(0);

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

	// General
	IOError,
	SyntaxError,

	_FATAL_,
}

impl ReportKind {
	pub fn untitled(self) -> Report {
		Report {
			file:      "",
			kind:      self,
			title:     None,
			span:      None,
			label:     None,
			footers:   None,
		}
	}

	pub fn title<T: Display>(self, title: T) -> Report {
		#[cfg(debug_assertions)]
		assert!(!title.to_string().is_empty(), "use ReportKind::untitled() instead.");
		Report {
			file:      "",
			kind:      self,
			title:     Some(title.to_string()),
			span:      None,
			label:     None,
			footers:   None,
		}
	}
}

#[derive(Clone)]
pub struct Report {
	file:    &'static str,
	kind:    ReportKind,
	title:   Option<String>,
	span:    Option<Span>,
	label:   Option<String>,
	footers: Option<Vec<String>>,
}

impl Report {
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

	pub fn as_err<T>(self) -> Result<T> {
		Err(Box::new(self))
	}

	pub fn footer<T: Display>(mut self, text: T) -> Self {
		match self.footers {
			Some(ref mut footers) => footers.push(text.to_string()),
			None => self.footers = Some(vec![text.to_string()]),
		}
		self
	}

	pub fn file(mut self, file: &'static str) -> Self {
		self.file = file; self
	}
}

impl<T> From<Report> for Result<T> {
	#[inline]
	fn from(report: Report) -> Self {
		Err(report.into())
	}
}

impl Display for Report {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		assert!(self.span.is_some() || self.label.is_none());
		assert!(self.span.is_some() || (self.file == ""));

		if self.kind >= ReportKind::_ERROR_ {
			ERR_COUNT.fetch_add(1, Ordering::Relaxed);
		}

		let (prefix, primary, secondary) = match self.kind {
			k if k > ReportKind::_FATAL_   => ("FATAL", Color::Red,    Color::BrightRed),
			k if k > ReportKind::_ERROR_   => ("ERR",   Color::Red,    Color::BrightRed),
			k if k > ReportKind::_WARNING_ => ("WARN",  Color::Yellow, Color::BrightYellow),
			k if k > ReportKind::_NOTE_    => ("NOTE",  Color::White,  Color::White),
			_ => unreachable!(),
		};

		writeln!(f, "{} {}",
			format!("[{prefix}] {:?}:", self.kind).color(primary).bold(),
			self.title.as_ref().unwrap_or(&String::new()))?;

		let mut padding = String::new();
		if let Some(span) = &self.span {
			let file = CACHE.get(self.file);

			let mut line = 1;
			let mut line_start = 0;
			while let Some(pos) = file[line_start..].find('\n') {
				if line_start + pos >= span.start { break; }
				line_start += pos + 1;
				line += 1;
			}

			let mut line_end = line_start;
			while let Some(pos) = file[line_end..].find('\n') {
				if line_end + pos >= span.end { break; }
				line_end += pos + 1;
			}

			let col = span.start - line_start + 1;

			writeln!(f, " {} {}:{}:{}", 
				"--->".cyan(), 
				self.file,
				if line_start == line_end { line_start.to_string() }
				else { format!("{line_start}-{line_end}") },
				col)?;

			let line_str = line.to_string();

			padding = format!("{} {} ",
				" ".repeat(line_str.len()),
				"|".cyan().dimmed());

			let Some(line) = file.lines().nth(line - 1) else {
				return writeln!(f, "{padding}{}",
					"Could not fetch line.".color(Color::Red).bold());
			};

			writeln!(f, "{padding}{}{}{}",
				&file[line_start..span.start],
				file[span.start..span.end].color(secondary).bold(),
				&file[span.end..line_start + line.len()])?;

			writeln!(f, "{padding}{} {}",
				" ".repeat(file[line_start..span.start].len()),
				"^".repeat(span.end - span.start).color(primary).bold())?;
		}

		if let Some(footers) = &self.footers {
			for footer in footers {
				writeln!(f, "{}{}", padding, footer.bright_black().italic())?;
			}
		}

		Ok(())
	}
}

impl std::fmt::Debug for Report {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{:?}", self.kind)
	}
}

pub type Result<T> = std::result::Result<T, Box<Report>>;
