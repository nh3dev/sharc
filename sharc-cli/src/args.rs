use std::fmt::Debug;
use std::process::exit;

use sharc::report::{Reportable, Level};

#[derive(Debug, Default)]
struct Error;

impl std::fmt::Display for Error {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "ArgumentParserError")
	}
}

impl Reportable for Error {
	fn level(&self) -> Level { Level::Error }
}

macro_rules! error {
	($($ident:tt)*) => {{
		eprintln!("{}", Error
			.title(format!($($ident)*))
			.note("Run with \x1b[1m--help\x1b[0m for usage information"));
		exit(1);
	}};
}

#[derive(Debug)]
pub struct Args {
	pub debug:  bool,
	pub action: Action,
}

#[derive(Debug)]
pub enum Action {
	Unset,
	Miri {
		file: Option<&'static str>,
	},
	Build {
		file:    &'static str,
		outfile: &'static str,
	},
}

impl Args {
	pub fn default() -> Self {
		Self {
			debug:  false,
			action: Action::Unset,
		}
	}

	pub fn parse(mut args: impl std::iter::Iterator<Item = String>) -> Self {
		let mut out = Self::default();
		out.parse_arg(&mut args);

		if matches!(out.action, Action::Unset) {
			println!("{HELP_MESSAGE}");
			exit(1);
		}

		out
	}

	fn parse_arg(&mut self, args: &mut impl std::iter::Iterator<Item = String>) -> Option<String> {
		let arg = args.next()?;

		match arg.strip_prefix('-') {
			Some("h" | "-help") => {
				println!("{HELP_MESSAGE}");
				exit(0);
			},
			Some("v" | "-version") => {
				println!("sharc {}.{}{}", 
					sharc::VERSION.0, sharc::VERSION.1,
					sharc::GITREV.map_or(String::new(), |r| format!(" rev:{r}")));
				println!("miri {}.{}{}", 
					miri::VERSION.0, miri::VERSION.1,
					miri::GITREV.map_or(String::new(), |r| format!(" rev:{r}")));
				exit(0);
			},
			Some(a) => {
				let flags = match a.strip_prefix('-') {
					Some(a) => vec![a],
					None    => a.char_indices().map(|(i, _)| &a[i..=i]).collect(),
				};

				for flag in flags {
					match flag {
						"d" | "debug" => self.debug = true,
						"o" | "output" if matches!(self.action, Action::Build { .. }) => {
							let file = Box::leak(self.parse_arg(args)
								.unwrap_or_else(|| error!("expected output file"))
								.into_boxed_str());

							let Action::Build { outfile, .. } = &mut self.action else { unreachable!() };

							*outfile = file;
						},
						a => error!("unknown flag: {a}"),
					}
				}
			},
			None    => match arg.as_str() {
				"shark" => {
					eprintln!("\x1b[34m{SHARK_ASCII}\x1b[0m");
					exit(1);
				},
				"miri" => self.action = Action::Miri {
					file: self.parse_arg(args).map(|s| -> &'static str { Box::leak(s.into_boxed_str()) }),
				},
				"build" => self.action = Action::Build {
					file: Box::leak(self.parse_arg(args)
						.unwrap_or_else(|| error!("expected file"))
						.into_boxed_str()),
					outfile: "main.shb",
				},
				_ => return Some(arg),
			},
		}

		self.parse_arg(args)
	}
}

const HELP_MESSAGE: &str = 
"  The compiler and interpreter for the Shard Programming Language. \x1b[34m><_>\x1b[0m
  Documentation can be found at https://nh3.dev/shard/ 

\x1b[1mOPTIONS\x1b[0m 
  -h, --help           Show this message
  -v, --version        Show version
  -d, --debug          Print information not intended for mere mortals.

  \x1b[1mmiri\x1b[0m [FILE]    Run the \x1b[1mmir i\x1b[0mnterpreter
    Runs in repl mode if the file is not specified.

  \x1b[1mbuild\x1b[0m [FILE]   Compile FILE into mir bytecode
    -o, --output FILE  File to write mir output to (default: main.shb)";

// FIXME: placeholder, someone make a good one pls
const SHARK_ASCII: &str = r#"                                 ,-
                               ,'::|
                              /::::|
                            ,'::::o\                                      _..
         ____........-------,..::?88b                                  ,-' /
 _.--"""". . . .      .   .  .  .  ""`-._                           ,-' .;'
<. - :::::o......  ...   . . .. . .  .  .""--._                  ,-'. .;'
 `-._  ` `":`:`:`::||||:::::::::::::::::.:. .  ""--._ ,'|     ,-'.  .;'
     """_=--       //'doo.. ````:`:`::::::::::.:.:.:. .`-`._-'.   .;'
         ""--.__     P(       \               ` ``:`:``:::: .   .;'
                "\""--.:-.     `.                             .:/
                  \. /    `-._   `.""-----.,-..::(--"".\""`.  `:\
                   `P         `-._ \          `-:\          `. `:\
                                   ""            "            `-._)"#;
