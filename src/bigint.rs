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
		const CHUNK_SIZE: usize = 19;

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
		self.0.iter().rev().enumerate().try_for_each(|(i, n)| match i {
			0 => write!(f, "{n}"),
			_ => write!(f, "{n:0>19}"),
		})
	}
}

impl std::ops::Add for &IBig {
	type Output = IBig;

	fn add(self, rhs: Self) -> Self::Output {
		IBig(self.0.iter().combine(rhs.0.iter())
			.map(|(a, b)| (a.copied().unwrap_or(0), b.copied().unwrap_or(0)))
			.fold((Vec::new(), false), |(mut acc, carry), (a, b)| {
				let (res, carry) = a.carrying_add(b, carry);
				acc.push(res);
				(acc, carry)
			}).0)
	}
}



struct CombinedIter<A: Iterator, B: Iterator>(A, B);

impl<A: Iterator, B: Iterator> Iterator for CombinedIter<A, B> {
	type Item = (Option<A::Item>, Option<B::Item>);

	fn next(&mut self) -> Option<Self::Item> {
		match (self.0.next(), self.1.next()) {
			(None, None) => None,
			(a, b) => Some((a, b)),
		}
	}
}

trait IterExt: Iterator {
	fn combine<B: Iterator>(self, b: B) -> CombinedIter<Self, B> 
		where Self: Sized {
		CombinedIter(self, b)
	}
}

impl<I: Iterator> IterExt for I {}
