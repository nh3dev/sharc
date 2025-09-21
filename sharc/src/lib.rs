#![deny(clippy::complexity,clippy::suspicious,clippy::correctness,clippy::cargo,
	clippy::perf,clippy::pedantic,clippy::nursery)]
#![allow(clippy::style,clippy::restriction,clippy::match_bool,clippy::too_many_lines,
	clippy::single_match_else,clippy::ignored_unit_patterns,clippy::module_name_repetitions,
	clippy::needless_for_each,clippy::derive_partial_eq_without_eq,clippy::missing_const_for_fn,
	clippy::cognitive_complexity,clippy::option_if_let_else,clippy::option_map_unit_fn,
	clippy::similar_names,clippy::cast_possible_truncation,clippy::cargo_common_metadata,
	clippy::must_use_candidate, clippy::return_self_not_must_use, clippy::missing_errors_doc)]


#![feature(if_let_guard)]
#![feature(bigint_helper_methods)]

use colored::Colorize;

pub use bump;

mod lexer;
mod parser;
mod typeinf;
mod mirgen;

pub mod report;
pub mod weakref;
mod span;
mod bigint;

pub use mirgen::{mir, bytecode};
pub use bigint::IBig;

use report::{Level, Report, ReportKind, Reportable};
use weakref::WeakRef;

const VERSION: (u32, u8) = (1, 0);


pub struct Reporter {
	cb:        Box<dyn FnMut(Report<ReportKind>)>,
	pub count: usize,
	filename:  Option<WeakRef<str>>,
	code:      Option<WeakRef<str>>,
}

impl Reporter {
	pub fn new() -> Self {
		Self { 
			cb:       Box::new(|_| {}),
			count:    0,
			filename: None,
			code:     None,
		}
	}

	pub fn callback(mut self, cb: impl FnMut(Report<ReportKind>) + 'static) -> Self {
		self.cb = Box::new(cb); self
	}

	pub fn source_file(&mut self, filename: WeakRef<str>) {
		self.filename = Some(filename);
	}

	pub fn source(&mut self, code: WeakRef<str>, filename: WeakRef<str>) {
		self.code     = Some(code);
		self.filename = Some(filename);
	}

	pub fn nom(&mut self, r: Report<ReportKind>) {
		if r.kind.level() >= Level::Error { self.count += 1; }

		(self.cb)(match (&self.code, &self.filename) {
			(Some(code), Some(filename)) => r.file(code.clone(), filename.clone()),
			(None,       Some(filename)) => r.filename(filename.clone()),
			_ => r,
		});
	}
}


// pass ref if this gets too big
pub struct CompilerOptions {
	pub include_stdlib:  bool,
	pub debug:           bool,
}

impl Default for CompilerOptions {
	fn default() -> Self {
		Self { 
			include_stdlib: true,
			debug:          false,
		}
	}
}


pub struct Compiler {
	reporter: Reporter,
	opts:     CompilerOptions,
}

impl Compiler {
	pub fn new(opts: CompilerOptions) -> Self {
		Self { opts, reporter: Reporter::new() }
	}

	pub fn report_callback(mut self, callback: impl FnMut(Report<ReportKind>) + 'static) -> Self {
		self.reporter.cb = Box::new(callback); self
	}

	/// DO NOT drop the returned Bump before the mir. its used to allocate the mir and you'll get a 
	/// use after free :L. I dont think rust has a way to enforce this, if there is let me know!
	pub fn compile<'b, 'src>(mut self, code: &'src str, filename: &'src str) -> Result<mirgen::mir::Mir<'b>, usize> {
		let code = WeakRef::new(code);
		let filename = WeakRef::new(filename);

		self.reporter.source(code.clone(), filename.clone());

		let code_ = code.as_ref().unwrap();
		let tokens = lexer::Lexer::tokenize(&code_, &mut self.reporter);

		if self.opts.debug { 
			eprintln!("\n{}", "LEXER".bold());
			tokens.iter().for_each(|token| eprintln!("{token:#}")); 
		}

		if self.reporter.count > 0 { return Err(self.reporter.count); }


		let ast = parser::Parser::parse(tokens, &mut self.reporter);

		if self.opts.debug { 
			eprintln!("\n{}", "PARSER".bold());
			ast.0.iter().for_each(|n| eprintln!("{n:#}")); 
		}
		
		if self.reporter.count > 0 { 
			return Err(self.reporter.count);
		}


		// TODO: modules here before hir :)

		let hir = typeinf::TypeInf::infer(ast, &mut self.reporter);

		if self.opts.debug { 
			eprintln!("\n{}", "TYPEINF".bold());
			hir.0.iter().for_each(|n| eprintln!("{n:#}"));
		}

		if self.reporter.count > 0 { 
			return Err(self.reporter.count); 
		}


		let filename_ = filename.as_ref().unwrap();
		let mir = mirgen::Analyzer::process(Some(&filename_), hir, &mut self.reporter);
		std::mem::drop(filename_);

		if self.opts.debug { 
			eprintln!("\n{}", "MIRGEN".bold());
			eprintln!("{mir}");
		}

		if self.reporter.count > 0 { 
			return Err(self.reporter.count); 
		}

		std::mem::drop(code_);
		code.drop();
		filename.drop();

		Ok(mir)
	}
}
