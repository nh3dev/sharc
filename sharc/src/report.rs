use std::fmt::{self, Display, Formatter};
use colored::{Color, Colorize};

use crate::span::Span;
use crate::weakref::WeakRef;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Level { Note, Warning, Error, Fatal }

pub trait Reportable: Display + Default + Sized {
	fn untitled(self) -> Report<Self> {
		Report {
			filename:  None,
			file:      None,
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
	fn title(self, title: impl Display) -> Report<Self> {
		Report {
			filename:  None,
			file:      None,
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
	fn level(&self) -> Level;
}

#[derive(Default, Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum ReportKind {
	_WARNING_,
	_ERROR_,

	#[default]
	GenericError,
	Unimplemented,

	// Lexer
	UnexpectedCharacter,
	UnterminatedMultilineComment,
	UnterminatedLiteral,
	SyntaxError,
	EmptyLiteral,

	// Parser
	UnexpectedToken,
	UnexpectedEOF,
	InvalidNumber,

	// typeinf
	TypeError,
	UnexpectedGenericCount,
	NoSuchTrait,
	UndefinedIdentifier,
	InvalidFunctionType,
	TypeAnnotationRequired,
	InvalidArity,
	ConstEvalError,

	// mirgen
	NodeNotAnExpr,	

	_FATAL_,
}

impl Display for ReportKind {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "{self:?}")
	}
}

impl Reportable for ReportKind {
	fn level(&self) -> Level {
		match self {
			s if *s < Self::_WARNING_ => Level::Note,
			s if *s < Self::_ERROR_   => Level::Warning,
			s if *s < Self::_FATAL_   => Level::Error,
			_                         => Level::Fatal,
		}
	}
}

pub struct Report<R: Reportable + Sized> {
	pub kind:      R,
	filename:      Option<WeakRef<str>>,
	file:          Option<WeakRef<str>>,
	span:          Option<Span>,
	pub title:     Option<String>,
	pub label:     Option<String>,
	pub footers:   Option<Vec<String>>,
	pub backtrace: Option<Box<std::backtrace::Backtrace>>,
}

impl<R: Reportable> Default for Report<R> {
	fn default() -> Self {
		Report {
			filename:  None,
			file:      None,
			kind:      R::default(),
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
}


pub type Result<T> = std::result::Result<T, Box<Report<ReportKind>>>;

impl<R: Reportable> Report<R> {
	pub fn span(mut self, span: Span) -> Self {
		self.span = Some(span); self
	}

	pub fn label(mut self, label: impl Display) -> Self {
		self.label = Some(label.to_string()); self
	}

	pub fn help(self, help: impl Display) -> Self {
		self.footer(format!("HELP: {help}"))
	}

	pub fn info(self, info: impl Display) -> Self {
		self.footer(format!("INFO: {info}"))
	}

	pub fn note( self, note: impl Display) -> Self {
		self.footer(format!("NOTE: {note}"))
	}

	pub fn footer(mut self, text: impl Display) -> Self {
		match self.footers {
			Some(ref mut footers) => footers.push(text.to_string()),
			None => self.footers = Some(vec![text.to_string()]),
		}
		self
	}

	pub fn filename(mut self, filename: WeakRef<str>) -> Self {
		self.filename = Some(filename); self
	}

	pub fn file(mut self, file: WeakRef<str>, filename: WeakRef<str>) -> Self {
		self.file = Some(file); 
		self.filename = Some(filename);
		self
	}

	pub fn as_err<T>(self) -> std::result::Result<T, Box<Report<R>>> {
		Err(Box::new(self))
	}
}

impl<T> From<Report<ReportKind>> for Result<T> {
	#[inline]
	fn from(report: Report<ReportKind>) -> Self {
		Err(report.into())
	}
}

impl<R: Reportable + fmt::Debug> fmt::Debug for Report<R> {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "{:?}", self.kind)
	}
}

impl<R: Reportable + fmt::Debug> Display for Report<R> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		assert!(self.span.is_some() || self.label.is_none());

		let (prefix, primary, secondary) = match self.kind.level() {
			Level::Fatal   => ("FATAL", Color::Red,    Color::BrightRed),
			Level::Error   => ("ERR",   Color::Red,    Color::BrightRed),
			Level::Warning => ("WARN",  Color::Yellow, Color::BrightYellow),
			Level::Note    => ("NOTE",  Color::White,  Color::White),
		};

		writeln!(f, "{} {}",
			format!("[{prefix}] {}:", self.kind).color(primary).bold(),
			self.title.as_ref().unwrap_or(&String::new()))?;

		let mut padding = String::new();
		match (&self.span, &self.filename, &self.file) {
			(Some(span), Some(filename), Some(file)) => {
				let file = file.as_ref().ok_or(fmt::Error)?;
				let filename = filename.as_ref().ok_or(fmt::Error)?;

				let mut line = 1;
				let mut line_start = 0;
				while let Some(pos) = file[line_start..].find('\n') {
					if line_start + pos >= span.start { break; }
					line_start += pos + 1;
					line += 1;
				}

				let col = span.start - line_start + 1;

				writeln!(f, " {} {}:{line}:{col}", 
					"-->".cyan(), &*filename)?;

				let line_str = line.to_string();

				padding = format!("{} {} ",
					" ".repeat(line_str.len()),
					"|".cyan().dimmed());

				let Some(line) = file.lines().nth(line - 1) else {
					return writeln!(f, "{padding}{}",
						"Could not fetch line.".color(Color::Red).bold());
				};

				let trimmed_start = file[line_start..span.start].trim_start();

				writeln!(f, "{padding}{}{}{}",
					&trimmed_start,
					file[span.start..=span.end].trim_end().color(secondary).bold(),
					&file.get(span.end+1..line_start + line.len()).map_or("", |s| s.trim_end()))?;

				writeln!(f, "{padding}{}{} {}",
					" ".repeat(trimmed_start.len()),
					"^".repeat(span.end+1 - span.start).color(primary).bold(),
					self.label.as_ref().unwrap_or(&String::new()))?;
			},
			(None, Some(filename), None) => {
				let filename = filename.as_ref().ok_or(fmt::Error)?;
				writeln!(f, " {} {}", "-->".cyan(), &*filename)?;
			},
			_ => {},
		}

		if let Some(footers) = &self.footers {
			for footer in footers {
				writeln!(f, "{}{}", padding, footer.bright_black().italic())?;
			}
		}

		if let Some(backtrace) = &self.backtrace {
			writeln!(f, "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ BACKTRACE ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")?;
			let backtrace = backtrace.to_string();
			let lines = backtrace.lines().collect::<Vec<&str>>();
			let len = lines.len().saturating_sub(30);
			writeln!(f, "{}", lines[..len].join("\n"))?;
		}

		Ok(())
	}
}
