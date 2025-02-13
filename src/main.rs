#![deny(clippy::complexity,clippy::suspicious,clippy::correctness,clippy::cargo,
	clippy::perf,clippy::pedantic,clippy::nursery)]
#![allow(clippy::style,clippy::restriction,clippy::match_bool,clippy::too_many_lines,
	clippy::single_match_else,clippy::ignored_unit_patterns,clippy::module_name_repetitions,
	clippy::needless_for_each,clippy::derive_partial_eq_without_eq,clippy::missing_const_for_fn,
	clippy::cognitive_complexity,clippy::option_if_let_else,clippy::option_map_unit_fn,
	clippy::similar_names)]

#![feature(if_let_guard)]

use std::sync::atomic::Ordering;

use colored::Colorize;

mod args;
mod lexer;
mod parser;
mod analyzer;
// mod codegen;
mod report;
mod bigint;
mod fs;
mod span;

fn main() {
	let args = args::Args::parse(std::env::args().skip(1));
	if args.debug { eprintln!("{args:#?}"); }

	let handler = report::LogHandler::new();


	if args.debug { eprintln!("\n{}", "LEXER".bold()); }
	let tokens = lexer::Lexer::tokenize(args.file, fs::CACHE.get(args.file), handler.clone());
	if args.debug { tokens.iter().for_each(|token| eprintln!("{token:#}")); }

	if report::ERR_COUNT.load(Ordering::Relaxed) > 0 {
		std::process::exit(1);
	}


	if args.debug { eprintln!("\n{}", "PARSER".bold()); }
	let ast = parser::Parser::parse(tokens, args.file, handler.clone());
	if args.debug { ast.iter().for_each(|n| eprintln!("{n:#}")); }

	if report::ERR_COUNT.load(Ordering::Relaxed) > 0 {
		std::process::exit(1);
	}


	if args.debug { eprintln!("\n{}", "ANALYSIS".bold()); }
	let (mir, sym) = analyzer::Analyzer::analyze(ast, args.file, &handler);
	if args.debug {
		sym.iter().map(|(k,v)| (k.0, v)).for_each(|(k,v)| eprintln!("{k}: \"{v}\""));
		mir.iter().for_each(|n| eprintln!("{n:#}")); 
	}

	if report::ERR_COUNT.load(Ordering::Relaxed) > 0 {
		std::process::exit(1);
	}


	// if args.debug { eprintln!("\n{}", "CODEGEN".bold()); }
	// let code = codegen::Gen::codegen(args.file, sym, mir, &handler);
	// if args.debug { eprintln!("{code}"); }
	//
	// if report::ERR_COUNT.load(Ordering::Relaxed) > 0 {
	// 	std::process::exit(1);
	// }
	//
	// match args.output.is_empty() {
	// 	true => handler.log(report::ReportKind::IOError
	// 		.title("Output file not specified")),
	// 	false => std::fs::write(args.output, code.to_string()).unwrap(),
	// }

	handler.terminate();
}
