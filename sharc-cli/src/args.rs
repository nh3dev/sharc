use std::fmt::Debug;
use std::process::exit;

macro_rules! error {
	($($ident:tt)*) => {{
		eprintln!("{}", logger::Report(sharc::ReportKind::ArgumentParserError
			.title(format!($($ident)*))
			.note("Run with \x1b[1m--help\x1b[0m for usage information")));
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
				"help" => {
					println!("{HELP_MESSAGE}");
					exit(0);
				},
				"version" => {
					println!("sharc {}", env!("CARGO_PKG_VERSION"));
					exit(0);
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

const HELP_MESSAGE: &str = "><_>
    The compiler and interpreter for the Shard Programming Language.
    Documentation can be found at https://nh3.dev/shard/

\x1b[1mOPTIONS\x1b[0m
  -h, --help           `-h` only shows the usage 
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
