use std::fmt::Debug;
use std::process::exit;

macro_rules! error {
	($($ident:tt)*) => {{
		eprintln!("{}", crate::logger::Report(sharc::ReportKind::ArgumentParserError
			.title(format!($($ident)*))
			.note("Run with \x1b[1m--help\x1b[0m for usage information")));
		exit(1);
	}};
}

#[derive(Debug)]
pub struct Args {
	// logging
	pub debug:        bool,
	pub level:        u8,
	
	// io
	pub file:         &'static str,
	pub output:       &'static str,

	// script
	pub verbs:        Vec<&'static str>,
}

impl Args {
	pub fn default() -> Self {
		Self {
			debug:        false,
			level:        2, // warn

			file:         "main.shd",
			output:       "",
			verbs:        Vec::new(),
		}
	}

	pub fn parse<I: std::iter::Iterator<Item = String>>(mut args: I) -> Self {
		let mut out = Self::default();

		while let Some(arg) = args.next() {
			match arg.strip_prefix('-') {
				Some(arg) => out.parse_arg(arg, &mut args),
				None if arg == "shark" => {
					eprintln!("\x1b[34m{SHARK_ASCII}\x1b[0m");
					exit(1);
				},
				None => out.verbs.push(Box::leak(arg.into_boxed_str())),
			}
		}

		out
	}

	fn parse_arg<I: std::iter::Iterator<Item = String>>(&mut self, arg: &str, args: &mut I) {
		let arg: Vec<&str> = match arg.starts_with('-') {
			true  => vec![&arg[1..]],
			false => arg.char_indices()
				.map(|(i, _)| &arg[i..=i])
				.collect(),
		};

		let arg_len = arg.len();

		for (i, arg) in arg.iter().enumerate() {
			macro_rules! err_if_arg_end { 
				() => { 
					if i != arg_len - 1 
					{ error!("{} may only be used at the end of a group", arg) }
				}; 
			}

			match arg.trim_start_matches('-') {
				"h" => {
					println!("{USAGE}");
					exit(0);
				},
				"help" => {
					println!("{USAGE}\n\n{HELP_MESSAGE}");
					exit(0);
				},
				"v" | "version" => {
					println!("sharc {}", env!("CARGO_PKG_VERSION"));
					exit(0);
				},
				"d" | "debug" => self.debug = true,
				"f" | "file" => {
					err_if_arg_end!();
					let file = args.next().unwrap_or_else(|| error!("{arg} expected FILE"));

					self.file = Box::leak(file.into_boxed_str());
				},
				"o" | "output" => {
					err_if_arg_end!();

					self.output = Box::leak(args.next()
						.unwrap_or_else(|| error!("expected file"))
						.into_boxed_str());
				},
				"l" | "level" => {
					err_if_arg_end!();
					let level = args.next().unwrap_or_else(|| error!("expected level"));

					self.level = match level.chars().nth(0).unwrap() {
						's' | '4' => 4,
						'f' | '3' => 3,
						'e' | '2' => 2,
						'w' | '1' => 1,
						'n' | '0' => 0,
						_ => error!("invalid level `{level}`"),
					};
				},
				a => error!("Invalid arg `{a}`")
			}
		}
	}
}

const USAGE: &str = "Usage: sharc [-hVd] [-l LEVEL] [-f FILE] [-o FILE] [VERB...]";
const HELP_MESSAGE: &str = "\x1b[1mDESCRIPTION\x1b[0m
    The compiler for the Shard Programming Language.
    Documentation can be found at https://shardlang.org/doc/

\x1b[1mOPTIONS\x1b[0m
    -h, --help                  `-h` only shows the usage 
    -v, --version               Show version
    -d, --debug                 Print debug information
        Shows a ton of information not intended for mere mortals.
    -l, --level LEVEL           [fatal|error|warn|note|silent] (or 0-4)
        (default: warn)
    -f, --file FILE             File to compile
        (default: main.shd)
    -o, --output FILE           File to write to
        (default: main.asm)";

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
