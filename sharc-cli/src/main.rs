use std::cell::RefCell;
use colored::Colorize;

mod args;
mod motd;
mod logger;

fn main() {
	let args = args::Args::parse(std::env::args().skip(1));
	if args.debug { eprintln!("{args:#?}"); }
	
	let logger = Box::leak(Box::new(RefCell::new(logger::Logger::new())));
	let mut rt = miri::Runtime::new();

	loop {
		let code = match args.repl {
			false => std::fs::read_to_string(args.output).unwrap(),
			true  => {
				eprint!("> ");
				std::io::Write::flush(&mut std::io::stderr()).unwrap();
				let mut input = String::new();
				std::io::stdin().read_line(&mut input).unwrap();
				if input.chars().all(|c| c.is_whitespace()) { break }
				input
			},
		};

		let conf = sharc::CompilerOptions {
			debug: args.debug,
			.. Default::default()
		};

		let mir = match sharc::Compiler::new(conf)
			.report_callback(|r| logger.borrow_mut().log(logger::Report(r)))
			.compile(&code, args.output) {
			Ok(mir) => mir,
			Err(e) => {
				println!("{}", "shardn't :(  {err} errors".red());
				break;
			}
		};

		if args.debug {
			eprintln!("\n{}", "RUNTIME".bold());
		}

		println!("{}", rt.run(mir));

		if !args.repl { break }
	}
}

