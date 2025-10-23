use sharc::report::{Reportable, Level};

#[derive(Debug, Clone, Copy, Default)]
pub enum MiriError {
	#[default]
	RuntimeError,
	InvalidInstruction,
	InvalidConversion,
	IntSizeLimitExceeded,
	InvalidArity,
	DivideByZero,
}

impl std::fmt::Display for MiriError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{self:?}")
	}
}

impl Reportable for MiriError {
	fn level(&self) -> Level { Level::Error }
}

pub type Result<T> = std::result::Result<T, Box<sharc::report::Report<MiriError>>>;
