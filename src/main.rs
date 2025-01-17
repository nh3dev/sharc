#![deny(clippy::complexity,clippy::suspicious,clippy::correctness,clippy::cargo,
	clippy::perf,clippy::pedantic,clippy::nursery)]
#![allow(clippy::style,clippy::restriction,clippy::match_bool,clippy::too_many_lines,
	clippy::single_match_else,clippy::ignored_unit_patterns,clippy::module_name_repetitions,
	clippy::needless_for_each,clippy::derive_partial_eq_without_eq,clippy::missing_const_for_fn,
	clippy::cognitive_complexity,clippy::option_if_let_else,clippy::option_map_unit_fn,
	clippy::similar_names)]

use std::sync::atomic::Ordering;

use colored::Colorize;

mod args;
mod lexer;
mod parser;
// mod codegen;
mod report;
mod util;
mod span;

fn main() {
	let args = args::Args::parse(std::env::args().skip(1));

	if args.debug {
		eprintln!("{args:#?}");
	}

	let handler = report::LogHandler::new();


	let tokens = lexer::Lexer::tokenize(util::CACHE.get(args.file), &handler);

	if args.debug {
		eprintln!("\n{}", "LEXER".bold());
		tokens.iter().for_each(|token| eprintln!("{token:#}"));
	}

	if report::ERR_COUNT.load(Ordering::Relaxed) > 0 {
		std::process::exit(1);
	}


	let ast = parser::Parser::parse(tokens, args.file, &handler);

	if args.debug {
		eprintln!("\n{}", "PARSER".bold());
		ast.iter().for_each(|n| eprintln!("{n:#}"));
	}

	if report::ERR_COUNT.load(Ordering::Relaxed) > 0 {
		std::process::exit(1);
	}


	// let code = codegen::Gen::codegen(ast, &handler);
	//
	// if args.debug {
	// 	eprintln!("\n{}", "CODEGEN".bold());
	// 	eprintln!("{code}");
	// }
	//
	// if report::ERR_COUNT.load(Ordering::Relaxed) > 0 {
	// 	std::process::exit(1);
	// }
	//
	// println!("{code}");

	handler.terminate();
}
