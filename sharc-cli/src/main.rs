#![feature(backtrace_frames)]

use std::cell::RefCell;

mod args;
mod motd;
mod logger;

fn main() {
	let args = args::Args::parse(std::env::args().skip(1));
	if args.debug { eprintln!("{args:#?}"); }
	
	let logger = Box::leak(Box::new(RefCell::new(logger::Logger::new())));

	let code = std::fs::read_to_string(args.file).unwrap();

	let mut rt = sharc::Compiler::new(sharc::CompilerOptions {
		debug: args.debug,
		.. Default::default()
	}).report_callback(|r| logger.borrow_mut().log(logger::Report(r)));

	let mir = match rt.compile(&code, &args.file) {
		Ok(mir) => mir,
		Err(err) => panic!("shardn't :(  {err} errors"),
	};

	println!("{:?}", miri::Runtime::new().run(mir));
}

