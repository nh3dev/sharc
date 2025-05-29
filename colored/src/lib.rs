use std::borrow::Cow;
use std::error::Error;
use std::fmt;
use std::ops::{Deref, DerefMut};
use std::sync::atomic::{AtomicBool, Ordering};

mod color;
mod error;
mod style;

pub mod customcolors;
pub use self::customcolors::CustomColor;

pub use color::*;
pub use style::{Style, Styles};

pub static SHOULD_COLORIZE: AtomicBool = AtomicBool::new(true);

#[derive(Clone, Debug, Default, PartialEq, Eq)]
#[non_exhaustive]
pub struct ColoredString {
	pub input: String,
	pub fgcolor: Option<Color>,
	pub bgcolor: Option<Color>,
	pub style: style::Style,
}

#[allow(missing_docs)]
pub trait Colorize {
	fn black(self) -> ColoredString where Self: Sized {
		self.color(Color::Black)
	}
	fn red(self) -> ColoredString where Self: Sized {
		self.color(Color::Red)
	}
	fn green(self) -> ColoredString where Self: Sized {
		self.color(Color::Green)
	}
	fn yellow(self) -> ColoredString where Self: Sized {
		self.color(Color::Yellow)
	}
	fn blue(self) -> ColoredString where Self: Sized {
		self.color(Color::Blue)
	}
	fn magenta(self) -> ColoredString where Self: Sized {
		self.color(Color::Magenta)
	}
	fn purple(self) -> ColoredString where Self: Sized {
		self.color(Color::Magenta)
	}
	fn cyan(self) -> ColoredString where Self: Sized {
		self.color(Color::Cyan)
	}
	fn white(self) -> ColoredString where Self: Sized {
		self.color(Color::White)
	}
	fn bright_black(self) -> ColoredString where Self: Sized {
		self.color(Color::BrightBlack)
	}
	fn bright_red(self) -> ColoredString where Self: Sized {
		self.color(Color::BrightRed)
	}
	fn bright_green(self) -> ColoredString where Self: Sized {
		self.color(Color::BrightGreen)
	}
	fn bright_yellow(self) -> ColoredString where Self: Sized {
		self.color(Color::BrightYellow)
	}
	fn bright_blue(self) -> ColoredString where Self: Sized {
		self.color(Color::BrightBlue)
	}
	fn bright_magenta(self) -> ColoredString where Self: Sized {
		self.color(Color::BrightMagenta)
	}
	fn bright_purple(self) -> ColoredString where Self: Sized {
		self.color(Color::BrightMagenta)
	}
	fn bright_cyan(self) -> ColoredString where Self: Sized {
		self.color(Color::BrightCyan)
	}
	fn bright_white(self) -> ColoredString where Self: Sized {
		self.color(Color::BrightWhite)
	}
	fn truecolor(self, r: u8, g: u8, b: u8) -> ColoredString where Self: Sized {
		self.color(Color::TrueColor { r, g, b })
	}
	fn custom_color<T: Into<CustomColor>>(self, color: T) -> ColoredString where Self: Sized {
		let color = color.into();

		self.color(Color::TrueColor {
			r: color.r,
			g: color.g,
			b: color.b,
		})
	}
	fn color<S: Into<Color>>(self, color: S) -> ColoredString;

	// Background Colors
	fn on_black(self) -> ColoredString where Self: Sized {
		self.on_color(Color::Black)
	}
	fn on_red(self) -> ColoredString where Self: Sized {
		self.on_color(Color::Red)
	}
	fn on_green(self) -> ColoredString where Self: Sized {
		self.on_color(Color::Green)
	}
	fn on_yellow(self) -> ColoredString where Self: Sized {
		self.on_color(Color::Yellow)
	}
	fn on_blue(self) -> ColoredString where Self: Sized {
		self.on_color(Color::Blue)
	}
	fn on_magenta(self) -> ColoredString where Self: Sized {
		self.on_color(Color::Magenta)
	}
	fn on_purple(self) -> ColoredString where Self: Sized {
		self.on_color(Color::Magenta)
	}
	fn on_cyan(self) -> ColoredString where Self: Sized {
		self.on_color(Color::Cyan)
	}
	fn on_white(self) -> ColoredString where Self: Sized {
		self.on_color(Color::White)
	}
	fn on_bright_black(self) -> ColoredString where Self: Sized {
		self.on_color(Color::BrightBlack)
	}
	fn on_bright_red(self) -> ColoredString where Self: Sized {
		self.on_color(Color::BrightRed)
	}
	fn on_bright_green(self) -> ColoredString where Self: Sized {
		self.on_color(Color::BrightGreen)
	}
	fn on_bright_yellow(self) -> ColoredString where Self: Sized {
		self.on_color(Color::BrightYellow)
	}
	fn on_bright_blue(self) -> ColoredString where Self: Sized {
		self.on_color(Color::BrightBlue)
	}
	fn on_bright_magenta(self) -> ColoredString where Self: Sized {
		self.on_color(Color::BrightMagenta)
	}
	fn on_bright_purple(self) -> ColoredString where Self: Sized {
		self.on_color(Color::BrightMagenta)
	}
	fn on_bright_cyan(self) -> ColoredString where Self: Sized {
		self.on_color(Color::BrightCyan)
	}
	fn on_bright_white(self) -> ColoredString where Self: Sized {
		self.on_color(Color::BrightWhite)
	}
	fn on_truecolor(self, r: u8, g: u8, b: u8) -> ColoredString where Self: Sized {
		self.on_color(Color::TrueColor { r, g, b })
	}
	fn on_custom_color<T: Into<CustomColor>>(self, color: T) -> ColoredString where Self: Sized {
		let color = color.into();

		self.on_color(Color::TrueColor {
			r: color.r,
			g: color.g,
			b: color.b,
		})
	}
	fn on_color<S: Into<Color>>(self, color: S) -> ColoredString;

	// Styles
	fn clear(self) -> ColoredString;
	fn normal(self) -> ColoredString;
	fn bold(self) -> ColoredString;
	fn dimmed(self) -> ColoredString;
	fn italic(self) -> ColoredString;
	fn underline(self) -> ColoredString;
	fn blink(self) -> ColoredString;
	fn reversed(self) -> ColoredString;
	fn hidden(self) -> ColoredString;
	fn strikethrough(self) -> ColoredString;
}

impl ColoredString {
	pub fn clear_fgcolor(&mut self) {
		self.fgcolor = None;
	}

	pub fn clear_bgcolor(&mut self) {
		self.bgcolor = None;
	}

	pub fn clear_style(&mut self) {
		self.style = Style::default();
	}

	pub fn is_plain(&self) -> bool {
		self.bgcolor.is_none() && self.fgcolor.is_none() && self.style == style::CLEAR
	}

	fn has_colors() -> bool {
		SHOULD_COLORIZE.load(Ordering::Relaxed)
	}

	fn compute_style(&self) -> String {
		if !ColoredString::has_colors() || self.is_plain() {
			return String::new();
		}

		let mut res = String::from("\x1B[");
		let mut has_wrote = if self.style != style::CLEAR {
			res.push_str(&self.style.to_str());
			true
		} else {
			false
		};

		if let Some(ref bgcolor) = self.bgcolor {
			if has_wrote {
				res.push(';');
			}

			res.push_str(&bgcolor.to_bg_str());
			has_wrote = true;
		}

		if let Some(ref fgcolor) = self.fgcolor {
			if has_wrote {
				res.push(';');
			}

			res.push_str(&fgcolor.to_fg_str());
		}

		res.push('m');
		res
	}

	fn escape_inner_reset_sequences(&self) -> Cow<str> {
		if !ColoredString::has_colors() || self.is_plain() {
			return self.input.as_str().into();
		}

		// TODO: BoyScoutRule
		let reset = "\x1B[0m";
		let style = self.compute_style();
		let matches: Vec<usize> = self
			.input
			.match_indices(reset)
			.map(|(idx, _)| idx)
			.collect();
		if matches.is_empty() {
			return self.input.as_str().into();
		}

		let mut input = self.input.clone();
		input.reserve(matches.len() * style.len());

		for (idx_in_matches, offset) in matches.into_iter().enumerate() {
			// shift the offset to the end of the reset sequence and take in account
			// the number of matches we have escaped (which shift the index to insert)
			let mut offset = offset + reset.len() + idx_in_matches * style.len();

			for cchar in style.chars() {
				input.insert(offset, cchar);
				offset += 1;
			}
		}

		input.into()
	}
}

impl Deref for ColoredString {
	type Target = str;
	fn deref(&self) -> &Self::Target {
		&self.input
	}
}

impl DerefMut for ColoredString {
	fn deref_mut(&mut self) -> &mut <Self as Deref>::Target {
		&mut self.input
	}
}

impl From<String> for ColoredString {
	fn from(s: String) -> Self {
		ColoredString {
			input: s,
			..ColoredString::default()
		}
	}
}

impl<'a> From<&'a str> for ColoredString {
	fn from(s: &'a str) -> Self {
		ColoredString {
			input: String::from(s),
			..ColoredString::default()
		}
	}
}

impl Colorize for ColoredString {
	fn color<S: Into<Color>>(mut self, color: S) -> ColoredString {
		self.fgcolor = Some(color.into());
		self
	}
	fn on_color<S: Into<Color>>(mut self, color: S) -> ColoredString {
		self.bgcolor = Some(color.into());
		self
	}

	fn clear(self) -> ColoredString {
		ColoredString {
			input: self.input,
			..ColoredString::default()
		}
	}
	fn normal(self) -> ColoredString {
		self.clear()
	}
	fn bold(mut self) -> ColoredString {
		self.style.add(style::Styles::Bold);
		self
	}
	fn dimmed(mut self) -> ColoredString {
		self.style.add(style::Styles::Dimmed);
		self
	}
	fn italic(mut self) -> ColoredString {
		self.style.add(style::Styles::Italic);
		self
	}
	fn underline(mut self) -> ColoredString {
		self.style.add(style::Styles::Underline);
		self
	}
	fn blink(mut self) -> ColoredString {
		self.style.add(style::Styles::Blink);
		self
	}
	fn reversed(mut self) -> ColoredString {
		self.style.add(style::Styles::Reversed);
		self
	}
	fn hidden(mut self) -> ColoredString {
		self.style.add(style::Styles::Hidden);
		self
	}
	fn strikethrough(mut self) -> ColoredString {
		self.style.add(style::Styles::Strikethrough);
		self
	}
}

impl Colorize for &str {
	fn color<S: Into<Color>>(self, color: S) -> ColoredString {
		ColoredString {
			fgcolor: Some(color.into()),
			input: String::from(self),
			..ColoredString::default()
		}
	}

	fn on_color<S: Into<Color>>(self, color: S) -> ColoredString {
		ColoredString {
			bgcolor: Some(color.into()),
			input: String::from(self),
			..ColoredString::default()
		}
	}

	fn clear(self) -> ColoredString {
		ColoredString {
			input: String::from(self),
			style: style::CLEAR,
			..ColoredString::default()
		}
	}
	fn normal(self) -> ColoredString {
		self.clear()
	}
	fn bold(self) -> ColoredString {
		ColoredString::from(self).bold()
	}
	fn dimmed(self) -> ColoredString {
		ColoredString::from(self).dimmed()
	}
	fn italic(self) -> ColoredString {
		ColoredString::from(self).italic()
	}
	fn underline(self) -> ColoredString {
		ColoredString::from(self).underline()
	}
	fn blink(self) -> ColoredString {
		ColoredString::from(self).blink()
	}
	fn reversed(self) -> ColoredString {
		ColoredString::from(self).reversed()
	}
	fn hidden(self) -> ColoredString {
		ColoredString::from(self).hidden()
	}
	fn strikethrough(self) -> ColoredString {
		ColoredString::from(self).strikethrough()
	}
}

impl fmt::Display for ColoredString {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if !ColoredString::has_colors() || self.is_plain() {
			return <String as fmt::Display>::fmt(&self.input, f);
		}

		// XXX: see tests. Useful when nesting colored strings
		let escaped_input = self.escape_inner_reset_sequences();

		f.write_str(&self.compute_style())?;
		escaped_input.fmt(f)?;
		f.write_str("\x1B[0m")?;
		Ok(())
	}
}

impl From<ColoredString> for Box<dyn Error> {
	fn from(cs: ColoredString) -> Box<dyn Error> {
		Box::from(error::ColoredStringError(cs))
	}
}
