#![deny(clippy::complexity,clippy::suspicious,clippy::correctness,clippy::cargo,
	clippy::perf,clippy::pedantic,clippy::nursery)]
#![allow(clippy::style,clippy::restriction,clippy::match_bool,clippy::too_many_lines,
	clippy::single_match_else,clippy::ignored_unit_patterns,clippy::module_name_repetitions,
	clippy::needless_for_each,clippy::derive_partial_eq_without_eq,clippy::missing_const_for_fn,
	clippy::cognitive_complexity,clippy::option_if_let_else,clippy::option_map_unit_fn,
	clippy::similar_names,clippy::cast_possible_truncation,clippy::cargo_common_metadata,
	clippy::must_use_candidate, clippy::return_self_not_must_use)]


#![feature(if_let_guard)]
#![feature(bigint_helper_methods)]
#![feature(never_type)]

use colored::Colorize;

pub use bump;

mod lexer;
mod parser;
mod typeinf;
mod mirgen;

mod report;
mod span;
mod bigint;

pub use report::{Report, ReportKind};
pub use mirgen::{mir, bytecode};
pub use bigint::IBig;

const VERSION: (u32, u8) = (1, 0);


struct Reporter<'src> {
	cb:       Box<dyn FnMut(Report)>,
	count:    usize,
	filename: &'src str,
	code:     &'src str,
}

impl Reporter<'_> {
	fn nom(&mut self, r: Report) {
		if r.kind.is_err() { self.count += 1; }
		(self.cb)(r.file(self.code, self.filename));
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


pub struct Compiler<'src> {
	reporter: Reporter<'src>,
	opts:     CompilerOptions,
}

impl<'src> Compiler<'src> {
	pub fn new(opts: CompilerOptions) -> Self {
		Self {
			opts, reporter: Reporter { 
				cb:       Box::new(|_| {}),
				count:    0,
				filename: "",
				code:     "" 
			},
		}
	}

	pub fn report_callback(mut self, callback: impl FnMut(Report) + 'static) -> Self {
		self.reporter.cb = Box::new(callback); self
	}

	/// DO NOT drop the returned Bump before the mir. its used to allocate the mir and you'll get a 
	/// use after free :L. I dont think rust has a way to enforce this, if there is let me know!
	pub fn compile<'b>(mut self, code: &'src str, filename: &'src str) -> Result<mirgen::mir::Mir<'b>, usize> {
		self.reporter.filename = filename;
		self.reporter.code     = code;

		let tokens = lexer::Lexer::tokenize(code, &mut self.reporter);

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


		let mir = mirgen::Analyzer::process(Some(filename), hir, &mut self.reporter);

		if self.opts.debug { 
			eprintln!("\n{}", "MIRGEN".bold());
			eprintln!("{mir}");
		}

		if self.reporter.count > 0 { 
			return Err(self.reporter.count); 
		}

		Ok(mir)
	}
}
