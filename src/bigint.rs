#[derive(Debug)]
pub struct IBig(Vec<u64>);

impl From<u64> for IBig {
	fn from(i: u64) -> Self {
		Self(vec![i])
	}
}

impl TryFrom<&IBig> for u64 {
	type Error = &'static str;
	fn try_from(i: &IBig) -> Result<Self, Self::Error> {
		if i.0.len() > 1 { return Err("IBig is too large"); }
		Ok(i.0[0])
	}
}

impl std::str::FromStr for IBig {
	type Err = std::num::ParseIntError;

	fn from_str(mut s: &str) -> Result<Self, Self::Err> {
		const CHUNK_SIZE: usize = 19; // max u64 is 20 digits, but leave 1 as margin

		let mut res = Vec::new();

		loop {
			if s.len() == 0 { break; }
			res.push(s[s.len().saturating_sub(CHUNK_SIZE)..].parse::<u64>()?);
			s = &s[..s.len().saturating_sub(CHUNK_SIZE)];
		}

		Ok(Self(res))
	}
}

impl std::fmt::Display for IBig {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		self.0.iter().rev().try_for_each(|i| write!(f, "{i}"))
	}
}
