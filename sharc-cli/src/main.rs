use std::sync::{LazyLock, Mutex};
use colored::Colorize;
use sharc::bump::Bump;

mod args;
mod motd;

fn compile_mir<'b>(conf: sharc::CompilerOptions, code: &str, filename: Option<&str>) -> Result<sharc::mir::Mir<'b>, ()> {
	match sharc::Compiler::new(conf)
		.report_callback(|r| LOGGER.lock().unwrap().log(logger::Report(r)))
		.compile(code, filename.unwrap_or("<repl>")) {
		Ok(mir) => Ok(mir),
		Err(e) => {
			println!("\n{}", motd::get_fail_msg().bold().red());
			println!("{}: Could not compile due to {e} error{}", "error".red().bold(), if e == 1 { "" } else { "s" });
			Err(())
		}
	}
}

static LOGGER: LazyLock<Mutex<logger::Logger>> = LazyLock::new(|| Mutex::new(logger::Logger::new()));

fn main() {
	let args = args::Args::parse(std::env::args().skip(1));
	if args.debug { eprintln!("{args:#?}"); }

	match args.action {
		args::Action::Miri { file: None } => {
			let mut rt = miri::Runtime::new();
			loop {
				eprint!("> ");
				std::io::Write::flush(&mut std::io::stderr()).unwrap();
				let mut input = String::new();
				std::io::stdin().read_line(&mut input).unwrap();
				if input.chars().all(|c| c.is_whitespace()) { break }

				let conf = sharc::CompilerOptions {
					debug: args.debug,
					.. Default::default()
				};

				let Ok(mir) = compile_mir(conf, &input, None) else { continue };

				if args.debug {
					eprintln!("\n{}", "MIRI".bold());
				}

				println!("{}", rt.run(mir));
			}
		},
		args::Action::Miri { file: Some(f) } => {
			let mut file = std::fs::File::open(f).unwrap();
			
			let mir = match sharc::bytecode::deserialize(Bump::new(), &mut file) {
				Some(mir) => mir.unwrap(),
				None => {
					let conf = sharc::CompilerOptions {
						debug: args.debug,
						.. Default::default()
					};

					let mut code = String::new();
					std::io::Read::read_to_string(&mut file, &mut code).unwrap();

					match compile_mir(conf, &code, Some(f)) {
						Ok(mir) => mir,
						Err(_)  => return,
					}
				},
			};


			if args.debug {
				eprintln!("{}", "MIRI".bold());
			}

			println!("{}", miri::Runtime::new().run(mir));
		},
		args::Action::Build { file, outfile } => {
			let code = std::fs::read_to_string(file).unwrap();

			let conf = sharc::CompilerOptions {
				debug: args.debug,
				.. Default::default()
			};

			let Ok(mir) = compile_mir(conf, &code, Some(file)) else { return };

			let file = std::fs::OpenOptions::new()
				.create(true).write(true)
				.open(outfile).unwrap();

			let mut writer = std::io::BufWriter::new(file);

			sharc::bytecode::serialize(&mir, &mut writer).unwrap();
		},
		args::Action::Unset => unreachable!(),
	}
}
