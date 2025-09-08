use colored::Colorize;

use std::fmt::{Display, self};
use sharc::mir::{Var, Type as MirType};

#[derive(Debug)]
pub struct Value {
	pub val: Val,
	pub ty:  Type,
}

impl Value {
	pub fn none() -> Self {
		Self { val: Val::None, ty: Type::None }
	}

	pub fn new(val: Val, ty: Type) -> Self {
		Self { val, ty }
	}

	pub fn add(lhs: Self, rhs: Self, out: Option<Type>) -> Self {
		match (lhs.val, rhs.val) {
			(Val::Int(mut l), Val::Int(r)) => {
				if l.len() < r.len() { l.resize(r.len(), 0); }
				for (i, v) in r.iter().enumerate() {
					let (res, overflow) = l[i].overflowing_add(*v);
					l[i] = res;
					if overflow {
						let mut carry = 1;
						for j in i+1..l.len() {
							let (res, overflow) = l[j].overflowing_add(carry);
							l[j] = res;
							if !overflow { break; }
						}
						if carry > 0 {
							l.push(carry);
						}
					}
				}

				Value::new(Val::Int(l), out.unwrap_or(lhs.ty))
			},
			_ => todo!(),
		}
	}
}

#[derive(Debug)]
pub enum Type {
	U(u32),
	None,
}

impl Type {
	pub fn from_mir(ty: &MirType) -> Self {
		match ty {
			MirType::U(n) => Type::U(*n),
			MirType::None => Type::None,
			_ => todo!(),
		}
	}
}

#[derive(Debug)]
pub enum Val {
	None,
	Int(Vec<u64>),

	Ret(usize),
}

impl Display for Value {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}: {}", self.val, self.ty)
	}
}

impl Display for Val {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Val::None      => write!(f, "none"),
			Val::Int(v)    => v.iter().rev().enumerate().try_for_each(|(i, n)| match i {
				0 => write!(f, "{n}"),
				_ => write!(f, "{n:0>19}"),
			}),
			// warn user if this prints
			Val::Ret(base) => write!(f, "{{ret: {base}}}"),
		}
	}
}

impl Display for Type {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Type::U(n)  => write!(f, "u{n}"),
			Type::None  => write!(f, "none"),
		}
	}
}
