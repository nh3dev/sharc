use std::ops::Not;
use bump::Bump;

use super::mir::{Expr, Mir, Node, Type, Var, ValId, InstrKind};
use crate::IBig;

const MAGIC: &[u8] = b"SHARDBC";

use std::io::{self, Write, Read, Seek};

pub trait Serialize {
	fn serialize(&self, b: &mut impl Write) -> io::Result<()>;
}

impl Serialize for Mir<'_> {
	fn serialize(&self, b: &mut impl Write) -> io::Result<()> {
		MAGIC.serialize(b)?;
		self.version.0.serialize(b)?;
		self.version.1.unwrap_or("").serialize(b)?;
		self.origin.unwrap_or("").serialize(b)?;
		self.nodes.serialize(b)?;
		b.flush()
	}
}

impl Serialize for Var<'_> {
	fn serialize(&self, b: &mut impl Write) -> io::Result<()> {
		match self {
			Var::None => 0_u8.serialize(b),
			Var::Imm(i) => { 
				1_u8.serialize(b)?;
				i.0.serialize(b)
			},
			Var::Local(i) => {
				2_u8.serialize(b)?;
				i.serialize(b)
			},
		}
	}
}

impl Serialize for Type<'_> {
	fn serialize(&self, b: &mut impl Write) -> io::Result<()> {
		match self {
			Type::U(i)   => {
				0_u8.serialize(b)?;
				i.serialize(b)
			},
			Type::I(i)   => {
				1_u8.serialize(b)?;
				i.serialize(b)
			},
			Type::B(i) => {
				2_u8.serialize(b)?;
				i.serialize(b)
			},
			Type::F(i) => {
				3_u8.serialize(b)?;
				i.serialize(b)
			},
			Type::Usize  => 4_u8.serialize(b),
			Type::Isize  => 5_u8.serialize(b),
			Type::None   => 6_u8.serialize(b),
			Type::Never  => 7_u8.serialize(b),
			Type::Ptr(t) => {
				8_u8.serialize(b)?;
				t.serialize(b)
			},
			Type::Mut(t) => {
				9_u8.serialize(b)?;
				t.serialize(b)
			},
			Type::Fn(a, r) => {
				10_u8.serialize(b)?;
				a.serialize(b)?;
				r.serialize(b)
			},
			Type::Arr(t, s) => {
				11_u8.serialize(b)?;
				t.serialize(b)?;
				s.serialize(b)
			},
		}
	}
}

impl Serialize for Expr<'_> {
	fn serialize(&self, b: &mut impl Write) -> io::Result<()> {
		match self {
			Expr::Call { id, ty, args } => {
				0_u8.serialize(b)?;
				id.serialize(b)?;
				ty.serialize(b)?;
				args.serialize(b)
			},
			Expr::FuncCapture { fid, args } => {
				1_u8.serialize(b)?;
				fid.serialize(b)?;
				args.serialize(b)
			},
			Expr::StrLit(s) => {
				2_u8.serialize(b)?;
				s.serialize(b)
			},
			Expr::ImplCall { path, ident, gener, args } => {
				3_u8.serialize(b)?;
				path.serialize(b)?;
				ident.serialize(b)?;
				gener.serialize(b)?;
				args.serialize(b)
			},
			Expr::Imm(i, ty) => {
				4_u8.serialize(b)?;
				i.0.serialize(b)?;
				ty.serialize(b)
			},
			Expr::DefCFn { sym, args, ret } => {
				5_u8.serialize(b)?;
				sym.serialize(b)?;
				args.serialize(b)?;
				ret.serialize(b)
			},
			Expr::Instr { kind, args } => {
				6_u8.serialize(b)?;
				kind.serialize(b)?;
				args.serialize(b)
			},
		}
	}
}

impl Serialize for Node<'_> {
	fn serialize(&self, b: &mut impl Write) -> io::Result<()> {
		match self {
			Node::Ret(v, t) => {
				0_u8.serialize(b)?;
				v.serialize(b)?;
				t.serialize(b)
			},
			Node::Assign { id, ty, expr } => {
				1_u8.serialize(b)?;
				id.serialize(b)?;
				ty.serialize(b)?;
				expr.serialize(b)
			},
			Node::Store { to, ty, from } => {
				2_u8.serialize(b)?;
				to.serialize(b)?;
				ty.serialize(b)?;
				from.serialize(b)
			},
			Node::Dbg { id, ident } => {
				3_u8.serialize(b)?;
				id.serialize(b)?;
				ident.serialize(b)
			},
			Node::DefFn { id, args, ret, def_proc, body } => {
				4_u8.serialize(b)?;
				id.serialize(b)?;
				args.serialize(b)?;
				ret.serialize(b)?;
				def_proc.serialize(b)?;
				body.serialize(b)
			},
		}
	}
}

impl Serialize for InstrKind {
	fn serialize(&self, b: &mut impl Write) -> io::Result<()> {
		match self {
			Self::Add => 0_u8,
			Self::Sub => 1_u8,
			Self::Mul => 2_u8,
			Self::Div => 3_u8,
			Self::Mod => 4_u8,
		}.serialize(b)
	}
}

macro_rules! serialize_int {
	($($ty:ty),*) => {
		$(impl Serialize for $ty {
			fn serialize(&self, b: &mut impl Write) -> io::Result<()> {
				b.write_all(&self.to_be_bytes())
			}
		})*
	}
}

serialize_int!(u8, u16, u32, u64, i8, i16, i32, i64);

macro_rules! serialize_tuple {
	($a:ident) => {};
	($a:ident $(,$b:ident)*) => {
		impl<$a: Serialize, $($b: Serialize),*> Serialize for ($a, $($b),*) {
			fn serialize(&self, b: &mut impl Write) -> io::Result<()> {
				let ($a, $($b),*) = self;
				$a.serialize(b)?;
				$($b.serialize(b)?;)*
				Ok(())
			}
		}
		serialize_tuple!($($b),*);
	};
}

serialize_tuple!(A, B, C, D, E, F, G);

impl Serialize for ValId {
	fn serialize(&self, b: &mut impl Write) -> io::Result<()> {
		self.0.serialize(b)
	}
}

impl Serialize for bool {
	fn serialize(&self, b: &mut impl Write) -> io::Result<()> {
		b.write_all(&[u8::from(*self)])
	}
}

impl Serialize for str {
	fn serialize(&self, b: &mut impl Write) -> io::Result<()> {
		self.as_bytes().serialize(b)
	}
}

impl<T: Serialize> Serialize for Option<T> {
	fn serialize(&self, b: &mut impl Write) -> io::Result<()> {
		self.is_some().serialize(b)?;
		if let Some(v) = self { return v.serialize(b); }
		Ok(())
	}
}

impl<T: Serialize> Serialize for [T] {
	fn serialize(&self, b: &mut impl Write) -> io::Result<()> {
		(self.len() as u32).serialize(b)?;
		self.iter().try_for_each(|v| v.serialize(b))
	}
}

impl<T: ?Sized + Serialize> Serialize for &T {
	fn serialize(&self, b: &mut impl Write) -> io::Result<()> {
		(*self).serialize(b)
	}
}




trait Deserialize: Sized {
	fn deserialize(bump: &Bump, b: &mut impl Read) -> io::Result<Self>;
}

impl Deserialize for Node<'_> {
	fn deserialize(bump: &Bump, b: &mut impl Read) -> io::Result<Self> {
		Ok(match u8::deserialize(bump, b)? {
			0 => Node::Ret(
				Deserialize::deserialize(bump, b)?, 
				Deserialize::deserialize(bump, b)?),
			1 => Node::Assign { 
				id:   Deserialize::deserialize(bump, b)?,
				ty:   Deserialize::deserialize(bump, b)?,
				expr: Deserialize::deserialize(bump, b)?,
			},
			2 => Node::Store { 
				to:   Deserialize::deserialize(bump, b)?,
				ty:   Deserialize::deserialize(bump, b)?,
				from: Deserialize::deserialize(bump, b)?,
			},
			3 => Node::Dbg { 
				id:    Deserialize::deserialize(bump, b)?,
				ident: Deserialize::deserialize(bump, b)?,
			},
			4 => Node::DefFn { 
				id:       Deserialize::deserialize(bump, b)?,
				args:     Deserialize::deserialize(bump, b)?, 
				ret:      Deserialize::deserialize(bump, b)?,
				def_proc: Deserialize::deserialize(bump, b)?,
				body:     Deserialize::deserialize(bump, b)?,
			},
			_ => return Err(io::Error::new(io::ErrorKind::InvalidData, "Invalid Tag")),
		})
	}
}

impl Deserialize for Expr<'_> {
	fn deserialize(bump: &Bump, b: &mut impl Read) -> io::Result<Self> {
		Ok(match u8::deserialize(bump, b)? {
			0 => Expr::Call { 
				id:   Deserialize::deserialize(bump, b)?,
				ty:   Deserialize::deserialize(bump, b)?,
				args: Deserialize::deserialize(bump, b)?,
			},
			1 => Expr::FuncCapture { 
				fid:  Deserialize::deserialize(bump, b)?,
				args: Deserialize::deserialize(bump, b)?,
			},
			2 => Expr::StrLit(Deserialize::deserialize(bump, b)?),
			3 => Expr::ImplCall { 
				path:  Deserialize::deserialize(bump, b)?,
				ident: Deserialize::deserialize(bump, b)?,
				gener: Deserialize::deserialize(bump, b)?,
				args:  Deserialize::deserialize(bump, b)?,
			},
			4 => Expr::Imm(
				IBig(Deserialize::deserialize(bump, b)?), 
				Deserialize::deserialize(bump, b)?),
			5 => Expr::DefCFn { 
				sym:  Deserialize::deserialize(bump, b)?,
				args: Deserialize::deserialize(bump, b)?,
				ret:  Deserialize::deserialize(bump, b)?,
			},
			6 => Expr::Instr { 
				kind: Deserialize::deserialize(bump, b)?,
				args: Deserialize::deserialize(bump, b)?,
			},
			_ => return Err(io::Error::new(io::ErrorKind::InvalidData, "Invalid Tag")),
		})
		
	}
}

impl Deserialize for Var<'_> {
	fn deserialize(bump: &Bump, b: &mut impl Read) -> io::Result<Self> {
		Ok(match u8::deserialize(bump, b)? {
			0 => Var::None,
			1 => Var::Imm(IBig(Deserialize::deserialize(bump, b)?)),
			2 => Var::Local(Deserialize::deserialize(bump, b)?),
			_ => return Err(io::Error::new(io::ErrorKind::InvalidData, "Invalid Tag")),
		})
	}
}

impl Deserialize for Type<'_> {
	fn deserialize(bump: &Bump, b: &mut impl Read) -> io::Result<Self> {
		Ok(match u8::deserialize(bump, b)? {
			0 => Type::U(u32::deserialize(bump, b)?),
			1 => Type::I(u32::deserialize(bump, b)?),
			2 => Type::B(u32::deserialize(bump, b)?),
			3 => Type::F(u32::deserialize(bump, b)?),
			4 => Type::Usize,
			5 => Type::Isize,
			6 => Type::None,
			7 => Type::Never,
			8 => Type::Ptr(Deserialize::deserialize(bump, b)?),
			9 => Type::Mut(Deserialize::deserialize(bump, b)?),
			10 => Type::Fn(
				Deserialize::deserialize(bump, b)?,
				Deserialize::deserialize(bump, b)?),
			11 => Type::Arr(
				Deserialize::deserialize(bump, b)?,
				Deserialize::deserialize(bump, b)?),
			_ => return Err(io::Error::new(io::ErrorKind::InvalidData, "Invalid Tag")),
		})
	}
}

impl Deserialize for InstrKind {
	fn deserialize(bump: &Bump, b: &mut impl Read) -> io::Result<Self> {
		Ok(match u8::deserialize(bump, b)? {
			0 => Self::Add,
			1 => Self::Sub,
			2 => Self::Mul,
			3 => Self::Div,
			4 => Self::Mod,
			_ => return Err(io::Error::new(io::ErrorKind::InvalidData, "Invalid Tag")),
		})
	}
}

macro_rules! deserialize_int {
	($($ty:ty),*) => {
		$(impl Deserialize for $ty {
			fn deserialize(bump: &Bump, b: &mut impl Read) -> io::Result<Self> {
				Ok(<$ty>::from_be_bytes(Deserialize::deserialize(bump, b)?))
			}
		})*
	}
}

deserialize_int!(u8, u16, u32, u64, i8, i16, i32, i64);

macro_rules! deserialize_tuple {
	($a:ident) => {};
	($a:ident $(,$b:ident)*) => {
		impl<$a: Deserialize, $($b: Deserialize),*> Deserialize for ($a, $($b),*) {
			fn deserialize(bump: &Bump, b: &mut impl Read) -> io::Result<Self> {
				Ok((
					$a::deserialize(bump, b)?,
					$($b::deserialize(bump, b)?),*
				))
			}
		}

		deserialize_tuple!($($b),*);
	};
}

deserialize_tuple!(A, B, C, D, E, F, G);

impl<const N: usize> Deserialize for [u8; N] {
	fn deserialize(_bump: &Bump, b: &mut impl Read) -> io::Result<Self> {
		let mut bytes = [0u8; N];
		b.read_exact(&mut bytes)?;
		Ok(bytes)
	}
}

impl<T: Deserialize> Deserialize for &[T] {
	fn deserialize(bump: &Bump, b: &mut impl Read) -> io::Result<Self> {
		let iter = (0..u32::deserialize(bump, b)?).map(|_| T::deserialize(bump, b));
		bump.try_alloc_from_iter(iter)
	}
}

impl Deserialize for &str {
	fn deserialize(bump: &Bump, b: &mut impl Read) -> io::Result<Self> {
		let len = u32::deserialize(bump, b)?;
		let buf = bump.try_alloc_from_iter((0..len).map(|_| Deserialize::deserialize(bump, b)))?;
		std::str::from_utf8(buf).map_err(
			|_| io::Error::new(io::ErrorKind::InvalidData, "Invalid UTF8"))
	}
}

impl Deserialize for bool {
	fn deserialize(bump: &Bump, b: &mut impl Read) -> io::Result<Self> {
		Ok(u8::deserialize(bump, b)? != 0)
	}
}

impl<T: Deserialize> Deserialize for Option<T> {
	fn deserialize(bump: &Bump, b: &mut impl Read) -> io::Result<Self> {
		bool::deserialize(bump, b)?.then(|| T::deserialize(bump, b)).transpose()
	}
}

impl<T: Deserialize> Deserialize for &T {
	fn deserialize(bump: &Bump, b: &mut impl Read) -> io::Result<Self> {
		Ok(bump.alloc(T::deserialize(bump, b)?))
	}
}

impl Deserialize for ValId {
	fn deserialize(bump: &Bump, b: &mut impl Read) -> io::Result<Self> {
		Ok(Self(Deserialize::deserialize(bump, b)?))
	}
}

impl Mir<'_> {
	pub fn deserialize<'b>(bump: Bump, b: &mut (impl Read + Seek)) -> Option<io::Result<Mir<'b>>> {
		let pos = match b.stream_position() {
			Ok(p) => p,
			Err(e) => return Some(Err(e)),
		};

		match <[u8; MAGIC.len()] as Deserialize>::deserialize(&bump, b) {
			Ok(m) if m != MAGIC => {
				let _ = b.seek(io::SeekFrom::Start(pos));
				return None
			},
			Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => {
				let _ = b.seek(io::SeekFrom::Start(pos));
				return None
			},
			Ok(_) => {},
			Err(e) => return Some(Err(e)),
		}

		Some((|| {
			Ok(Mir {
				version: Deserialize::deserialize(&bump, b)?,
				origin: {
					let s = <&str as Deserialize>::deserialize(&bump, b)?;
					s.is_empty().not().then_some(s)
				},
				nodes: (0..u32::deserialize(&bump, b)?)
					.map(|_| Deserialize::deserialize(&bump, b))
					.collect::<io::Result<Vec<Node>>>()?,
				bump,
			})
		})())
	}
}
