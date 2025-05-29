#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct CustomColor {
	pub r: u8,
	pub g: u8,
	pub b: u8,
}

impl CustomColor {
	pub fn new(r: u8, g: u8, b: u8) -> Self {
		Self { r, g, b }
	}
}

impl From<(u8, u8, u8)> for CustomColor {
	fn from((r, g, b): (u8, u8, u8)) -> Self {
		Self::new(r, g, b)
	}
}
