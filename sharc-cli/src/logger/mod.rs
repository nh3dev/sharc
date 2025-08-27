// TODO: iteraror type for progress bar then .next() for next step

mod report;
pub use report::Report;

use std::fmt::Display;
use std::io::{self, Write};

pub struct Logger<'lock> {
	progress: Option<ProgressBar>,
	footer:   Option<String>,
	stderr:   io::StderrLock<'lock>,
}

pub struct ProgressBar {
	pub kind: ProgressBarKind,
	pub msg:  String,
	pub pad:  usize,
	pub len:  usize,
}

impl Default for ProgressBar {
	fn default() -> Self {
		Self {
			kind: ProgressBarKind::None(0.0),
			msg:  String::new(),
			pad:  10,
			len:  20,
		}
	}
}

pub enum ProgressBarKind {
	Percent(f64),
	Tasks(f64, f64),
	None(f64),
}

impl Display for ProgressBar {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		const SHARK: &str = "|\\";

		write!(f, "{}", self.msg)?;
		write!(f, "{}", " ".repeat(self.pad - self.msg.len()))?;

		match self.kind {
			ProgressBarKind::Percent(p) => write!(f,
				"{}{SHARK}{} {p}%",
				"_".repeat(((self.len - 2) as f64 * p) as usize), 
				"_".repeat(self.len - 2 - ((self.len - 2) as f64 * p) as usize),
			),
			ProgressBarKind::Tasks(done, tasks) => write!(f,
				"{}{SHARK}{} {done}/{tasks}",
				"_".repeat(((self.len - 2) as f64 / tasks * done) as usize),
				"_".repeat(self.len - 2 - ((self.len - 2) as f64 / tasks * done) as usize),
			),
			ProgressBarKind::None(t) => write!(f,
				"{}{SHARK}{}", 
				"_".repeat(t as usize), 
				"_".repeat(self.len - 2 - t as usize)
			),
		}
	}
}

impl Logger<'_> {
	pub fn new() -> Self {
		Self {
			progress: None,
			footer:   None,
			stderr:   io::stderr().lock(),
		}
	}

	pub fn log(&mut self, msg: impl Display) {
		self.clear();
		write!(self.stderr, "{msg}").unwrap();
		self.draw();
	}

	pub fn bar(&mut self, bar: ProgressBar) {
		self.clear();
		self.progress = Some(bar);
		self.draw();
	}

	pub fn progress(&mut self, val: f64) {
		if let Some(bar) = &mut self.progress {
			match bar.kind {
				ProgressBarKind::Percent(ref mut p)  => *p = val,
				ProgressBarKind::Tasks(ref mut p, _) => *p = val,
				ProgressBarKind::None(ref mut t)     => *t = val,
			}
			self.clear();
			self.draw();
		}
	}

	pub fn clear_bar(&mut self) {
		self.clear();
		self.progress = None;
		self.draw();
	}

	pub fn footer(&mut self, msg: impl Display) {
		self.clear();
		self.footer = Some(msg.to_string());
		self.draw();
	}

	fn draw(&mut self) {
		if let Some(f) = &self.footer   { write!(self.stderr, "{f}").unwrap(); }
		if let Some(p) = &self.progress { write!(self.stderr, "{p}").unwrap(); }
		self.stderr.flush().unwrap();
	}

	fn clear(&mut self) {
		let offset = 
			self.progress.as_ref().map_or(0, |_| 1) 
			+ self.footer.as_ref().map_or(0, |f| f.matches('\n').count());
		write!(self.stderr, "{}", "\x1B[1A\x1B[2K".repeat(offset)).unwrap();
	}
}
