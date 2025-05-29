use core::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not};

macro_rules! auto_impl_ref_binop_trait {
	(impl $trait_name:ident, $method:ident for $t:ty, $u:ty) => {
		impl $trait_name<&$u> for $t {
			type Output = <$t as $trait_name<$t>>::Output;

			#[inline]
			fn $method(self, rhs: &$u) -> Self::Output {
				$trait_name::$method(self, *rhs)
			}
		}

		impl $trait_name<$u> for &$t {
			type Output = <$t as $trait_name<$t>>::Output;

			#[inline]
			fn $method(self, rhs: $u) -> Self::Output {
				$trait_name::$method(*self, rhs)
			}
		}

		impl $trait_name<&$u> for &$t {
			type Output = <$t as $trait_name<$t>>::Output;

			#[inline]
			fn $method(self, rhs: &$u) -> Self::Output {
				$trait_name::$method(*self, *rhs)
			}
		}
	};
}

macro_rules! impl_assign_op_trait {
	($trait:ident, $method:ident for $t:ty, $u:ty, using $used_trait:ident::$used_method:ident) => {
		impl $trait<$u> for $t {
			#[inline]
			fn $method(&mut self, other: $u) {
				*self = $used_trait::$used_method(&*self, other);
			}
		}

		impl $trait<&$u> for $t {
			#[inline]
			fn $method(&mut self, other: &$u) {
				*self = $used_trait::$used_method(&*self, other);
			}
		}
	};
}

const CLEARV: u8 = 0b0000_0000;
const BOLD: u8 = 0b0000_0001;
const UNDERLINE: u8 = 0b0000_0010;
const REVERSED: u8 = 0b0000_0100;
const ITALIC: u8 = 0b0000_1000;
const BLINK: u8 = 0b0001_0000;
const HIDDEN: u8 = 0b0010_0000;
const DIMMED: u8 = 0b0100_0000;
const STRIKETHROUGH: u8 = 0b1000_0000;

static STYLES: [(u8, Styles); 8] = [
	(BOLD, Styles::Bold),
	(DIMMED, Styles::Dimmed),
	(UNDERLINE, Styles::Underline),
	(REVERSED, Styles::Reversed),
	(ITALIC, Styles::Italic),
	(BLINK, Styles::Blink),
	(HIDDEN, Styles::Hidden),
	(STRIKETHROUGH, Styles::Strikethrough),
];

pub static CLEAR: Style = Style(CLEARV);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Style(u8);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[allow(missing_docs)]
pub enum Styles {
	Clear,
	Bold,
	Dimmed,
	Underline,
	Reversed,
	Italic,
	Blink,
	Hidden,
	Strikethrough,
}

impl Styles {
	fn to_str<'a>(self) -> &'a str {
		match self {
			Styles::Clear => "", // unreachable, but we don't want to panic
			Styles::Bold => "1",
			Styles::Dimmed => "2",
			Styles::Italic => "3",
			Styles::Underline => "4",
			Styles::Blink => "5",
			Styles::Reversed => "7",
			Styles::Hidden => "8",
			Styles::Strikethrough => "9",
		}
	}

	fn to_u8(self) -> u8 {
		match self {
			Styles::Clear => CLEARV,
			Styles::Bold => BOLD,
			Styles::Dimmed => DIMMED,
			Styles::Italic => ITALIC,
			Styles::Underline => UNDERLINE,
			Styles::Blink => BLINK,
			Styles::Reversed => REVERSED,
			Styles::Hidden => HIDDEN,
			Styles::Strikethrough => STRIKETHROUGH,
		}
	}

	fn from_u8(u: u8) -> Option<Vec<Styles>> {
		if u == CLEARV {
			return None;
		}

		let res: Vec<Styles> = STYLES
			.iter()
			.filter(|&(mask, _)| (0 != (u & mask)))
			.map(|&(_, value)| value)
			.collect();
		if res.is_empty() {
			None
		} else {
			Some(res)
		}
	}
}

impl BitAnd<Styles> for Styles {
	type Output = Style;

	fn bitand(self, rhs: Styles) -> Self::Output {
		Style(self.to_u8() & rhs.to_u8())
	}
}

auto_impl_ref_binop_trait!(impl BitAnd, bitand for Styles, Styles);

impl BitAnd<Style> for Styles {
	type Output = Style;

	fn bitand(self, rhs: Style) -> Self::Output {
		Style(self.to_u8() & rhs.0)
	}
}

auto_impl_ref_binop_trait!(impl BitAnd, bitand for Styles, Style);

impl BitOr<Styles> for Styles {
	type Output = Style;

	fn bitor(self, rhs: Styles) -> Self::Output {
		Style(self.to_u8() | rhs.to_u8())
	}
}

auto_impl_ref_binop_trait!(impl BitOr, bitor for Styles, Styles);

impl BitOr<Style> for Styles {
	type Output = Style;

	fn bitor(self, rhs: Style) -> Self::Output {
		Style(self.to_u8() | rhs.0)
	}
}

auto_impl_ref_binop_trait!(impl BitOr, bitor for Styles, Style);

impl BitXor<Styles> for Styles {
	type Output = Style;

	fn bitxor(self, rhs: Styles) -> Self::Output {
		Style(self.to_u8() ^ rhs.to_u8())
	}
}

auto_impl_ref_binop_trait!(impl BitXor, bitxor for Styles, Styles);

impl BitXor<Style> for Styles {
	type Output = Style;

	fn bitxor(self, rhs: Style) -> Self::Output {
		Style(self.to_u8() ^ rhs.0)
	}
}

auto_impl_ref_binop_trait!(impl BitXor, bitxor for Styles, Style);

impl Not for Styles {
	type Output = Style;

	fn not(self) -> Self::Output {
		Style(!self.to_u8())
	}
}

impl Not for &Styles {
	type Output = Style;

	fn not(self) -> Self::Output {
		Style(!self.to_u8())
	}
}

impl Style {
	pub fn contains(self, style: Styles) -> bool {
		let s = style.to_u8();
		self.0 & s == s
	}

	pub(crate) fn to_str(self) -> String {
		let styles = Styles::from_u8(self.0).unwrap_or_default();
		styles
			.iter()
			.map(|s| s.to_str())
			.collect::<Vec<&str>>()
			.join(";")
	}

	pub fn add(&mut self, two: Styles) {
		self.0 |= two.to_u8();
	}

	pub fn remove(&mut self, two: Styles) {
		self.0 &= !two.to_u8();
	}

	pub fn bold(mut self) -> Self {
		self.add(Styles::Bold); self
	}

	pub fn dimmed(mut self) -> Self {
		self.add(Styles::Dimmed); self
	}

	pub fn underline(mut self) -> Self {
		self.add(Styles::Underline); self
	}

	pub fn reversed(mut self) -> Self {
		self.add(Styles::Reversed); self
	}

	pub fn italic(mut self) -> Self {
		self.add(Styles::Italic); self
	}

	pub fn blink(mut self) -> Self {
		self.add(Styles::Blink); self
	}

	pub fn hidden(mut self) -> Self {
		self.add(Styles::Hidden); self
	}

	pub fn strikethrough(mut self) -> Self {
		self.add(Styles::Strikethrough); self
	}
}

impl BitAnd<Style> for Style {
	type Output = Style;

	fn bitand(self, rhs: Style) -> Self::Output {
		Style(self.0 & rhs.0)
	}
}

auto_impl_ref_binop_trait!(impl BitAnd, bitand for Style, Style);

impl BitAnd<Styles> for Style {
	type Output = Style;

	fn bitand(self, rhs: Styles) -> Self::Output {
		Style(self.0 & rhs.to_u8())
	}
}

auto_impl_ref_binop_trait!(impl BitAnd, bitand for Style, Styles);

impl BitOr<Style> for Style {
	type Output = Style;

	fn bitor(self, rhs: Style) -> Self::Output {
		Style(self.0 | rhs.0)
	}
}

auto_impl_ref_binop_trait!(impl BitOr, bitor for Style, Style);

impl BitOr<Styles> for Style {
	type Output = Style;

	fn bitor(self, rhs: Styles) -> Self::Output {
		Style(self.0 | rhs.to_u8())
	}
}

auto_impl_ref_binop_trait!(impl BitOr, bitor for Style, Styles);

impl BitXor<Style> for Style {
	type Output = Style;

	fn bitxor(self, rhs: Style) -> Self::Output {
		Style(self.0 ^ rhs.0)
	}
}

auto_impl_ref_binop_trait!(impl BitXor, bitxor for Style, Style);

impl BitXor<Styles> for Style {
	type Output = Style;

	fn bitxor(self, rhs: Styles) -> Self::Output {
		Style(self.0 ^ rhs.to_u8())
	}
}

auto_impl_ref_binop_trait!(impl BitXor, bitxor for Style, Styles);

impl Not for Style {
	type Output = Style;

	fn not(self) -> Self::Output {
		Style(!self.0)
	}
}

impl Not for &Style {
	type Output = Style;

	fn not(self) -> Self::Output {
		Style(!self.0)
	}
}

impl_assign_op_trait!(BitAndAssign, bitand_assign for Style, Style, using BitAnd::bitand);

impl_assign_op_trait!(BitAndAssign, bitand_assign for Style, Styles, using BitAnd::bitand);

impl_assign_op_trait!(BitOrAssign, bitor_assign for Style, Style, using BitOr::bitor);

impl_assign_op_trait!(BitOrAssign, bitor_assign for Style, Styles, using BitOr::bitor);

impl_assign_op_trait!(BitXorAssign, bitxor_assign for Style, Style, using BitXor::bitxor);

impl_assign_op_trait!(BitXorAssign, bitxor_assign for Style, Styles, using BitXor::bitxor);

impl Default for Style {
	fn default() -> Self {
		CLEAR
	}
}

impl From<Styles> for Style {
	fn from(value: Styles) -> Self {
		Style(value.to_u8())
	}
}

impl From<&Styles> for Style {
	fn from(value: &Styles) -> Self {
		Style(value.to_u8())
	}
}

impl FromIterator<Styles> for Style {
	fn from_iter<T: IntoIterator<Item = Styles>>(iter: T) -> Self {
		let mut style = Style::default();
		for styles in iter.into_iter() {
			style.add(styles);
		}
		style
	}
}
