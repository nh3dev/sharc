// TODO: there's a LOT of room for optimization here
// I aint the one doing it though :L

use bump::Bump;

#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[derive(Debug, Clone, Copy)]
pub struct IBig<'bump>(&'bump [u64]);

impl IBig<'_> {
	pub fn to_vec(&self) -> Vec<u64> {
		self.0.to_vec()
	}

	pub fn from_u64(bump: &Bump, i: u64) -> Self {
		Self(bump.alloc_sized_slice([i]))
	}

	pub fn try_as_u64(&self) -> Option<u64> {
		if self.0.len() > 1 { return None; }
		Some(self.0[0])
	}

	pub fn from_str(bump: &Bump, mut s: &str) -> Result<Self, std::num::ParseIntError> {
		const CHUNK_SIZE: usize = 19;

		let mut res = Vec::new();

		loop {
			if s.len() == 0 { break; }
			res.push(s[s.len().saturating_sub(CHUNK_SIZE)..].parse::<u64>()?);
			s = &s[..s.len().saturating_sub(CHUNK_SIZE)];
		}

		Ok(Self(bump.alloc_slice(&res)))
	}

	pub fn add(&self, bump: &Bump, rhs: &Self) -> Self {
		let vec = self.0.iter().combine(rhs.0.iter())
			.map(|(a, b)| (a.copied().unwrap_or(0), b.copied().unwrap_or(0)))
			.fold((Vec::new(), false), |(mut acc, carry), (a, b)| {
				let (res, carry) = a.carrying_add(b, carry);
				acc.push(res);
				(acc, carry)
			}).0;
		Self(bump.alloc_slice(&vec))
	}

	pub fn min_bit_size(&self) -> u32 {
		self.0.iter().fold(0, |bits, chunk| match chunk.leading_zeros() {
			64 if self.0.len() > 1 => bits + 64,
			v  => bits + 64 - v,
		})
	}

	pub fn copy<'b>(&self, bump: &Bump) -> IBig<'b> {
		IBig(bump.alloc_slice(self.0))
	}
}

impl std::fmt::Display for IBig<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		self.0.iter().rev().enumerate().try_for_each(|(i, n)| match i {
			0 => write!(f, "{n}"),
			_ => write!(f, "{n:0>19}"),
		})
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
