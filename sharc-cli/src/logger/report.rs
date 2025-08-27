use colored::{Color, Colorize};
use sharc::ReportKind;

#[repr(transparent)]
pub struct Report<'a>(pub sharc::Report<'a>);

impl std::fmt::Display for Report<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		assert!(self.0.span.is_some() || self.0.label.is_none());
		assert!(self.0.span.is_none() || !self.0.file.is_empty(), "\x1b[31myou forgot to add file before logging :L\x1b[0m");

		let (prefix, primary, secondary) = match self.0.kind {
			k if k > ReportKind::_FATAL_   => ("FATAL", Color::Red,    Color::BrightRed),
			k if k > ReportKind::_ERROR_   => ("ERR",   Color::Red,    Color::BrightRed),
			k if k > ReportKind::_WARNING_ => ("WARN",  Color::Yellow, Color::BrightYellow),
			k if k > ReportKind::_NOTE_    => ("NOTE",  Color::White,  Color::White),
			_ => unreachable!(),
		};

		writeln!(f, "{} {}",
			format!("[{prefix}] {:?}:", self.0.kind).color(primary).bold(),
			self.0.title.as_ref().unwrap_or(&String::new()))?;

		let mut padding = String::new();
		if let Some(span) = &self.0.span {
			let mut line = 1;
			let mut line_start = 0;
			while let Some(pos) = self.0.file[line_start..].find('\n') {
				if line_start + pos >= span.start { break; }
				line_start += pos + 1;
				line += 1;
			}

			let col = span.start - line_start + 1;

			writeln!(f, " {} {}:{line}:{col}", 
				"-->".cyan(), self.0.filename)?;

			let line_str = line.to_string();

			padding = format!("{} {} ",
				" ".repeat(line_str.len()),
				"|".cyan().dimmed());

			let Some(line) = self.0.file.lines().nth(line - 1) else {
				return writeln!(f, "{padding}{}",
					"Could not fetch line.".color(Color::Red).bold());
			};

			let trimmed_start = self.0.file[line_start..span.start].trim_start();

			writeln!(f, "{padding}{}{}{}",
				&trimmed_start,
				self.0.file[span.start..=span.end].trim_end().color(secondary).bold(),
				&self.0.file.get(span.end+1..line_start + line.len()).map_or("", |s| s.trim_end()))?;

			writeln!(f, "{padding}{}{} {}",
				" ".repeat(trimmed_start.len()),
				"^".repeat(span.end+1 - span.start).color(primary).bold(),
				self.0.label.as_ref().unwrap_or(&String::new()))?;
		}

		if let Some(footers) = &self.0.footers {
			for footer in footers {
				writeln!(f, "{}{}", padding, footer.bright_black().italic())?;
			}
		}

		if let Some(backtrace) = &self.0.backtrace {
			writeln!(f, "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ BACKTRACE ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")?;
			let backtrace = backtrace.to_string();
			let lines = backtrace.lines().collect::<Vec<&str>>();
			let len = lines.len().saturating_sub(30);
			writeln!(f, "{}", lines[..len].join("\n"))?;
		}

		Ok(())
	}
}
