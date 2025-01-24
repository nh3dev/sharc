#[derive(Debug)]
pub struct iBig(bool, Vec<u64>);

impl From<u64> for iBig {
	fn from(i: u64) -> Self {
		Self(false, vec![i])
	}
}

impl From<i64> for iBig {
	fn from(i: i64) -> Self {
		Self(i < 0, vec![i.unsigned_abs()])
	}
}

impl std::ops::Neg for iBig {
	type Output = Self;
	fn neg(self) -> Self::Output 
	{ Self(!self.0, self.1) }
}

impl std::str::FromStr for iBig {
	type Err = std::num::ParseIntError;

	// NOTE: naive impl assuming input is unsigned, use '-' to negate
	fn from_str(mut s: &str) -> Result<Self, Self::Err> {
		const CHUNK_SIZE: usize = 19; // max u64 is 20 digits, but leave 1 as margin

		let mut res = Vec::new();

		loop {
			if s.len() == 0 { break; }
			res.push(s[s.len().saturating_sub(CHUNK_SIZE)..].parse::<u64>()?);
			s = &s[..s.len().saturating_sub(CHUNK_SIZE)];
		}

		Ok(Self(false, res))
	}
}

impl std::fmt::Display for iBig {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		if self.0 { write!(f, "-")?; }
		self.1.iter().rev().try_for_each(|i| write!(f, "{i}"))
	}
}
