use std::ffi::c_void;
use std::fmt::{self, Display};
use std::num::Wrapping;

use sharc::mir::{Var, Type as MirType};
use sharc::report::Reportable;
use colored::Colorize;
use sharc::IBig;

use crate::error::{Result, MiriError};

#[derive(Debug, Clone)]
pub struct Value {
	pub val: Val,
	pub ty:  Type,
}

impl Value {
	pub const fn none() -> Value {
		Self { val: Val::None, ty: Type::None }
	}

	pub fn new(val: Val, ty: Type) -> Self {
		Self { val, ty }
	}

	fn op(
		lhs: &Self, rhs: &Self, out: Option<Type>, 
		op8:   fn(u8, u8) -> u8,
		op16:  fn(u16, u16) -> u16,
		op32:  fn(u32, u32) -> u32,
		op64:  fn(u64, u64) -> u64,
		op128: fn(u128, u128) -> u128,
	) -> Self {
		let (lhs_i, lhs_size) = lhs.val.as_num().unwrap_or_else(|| todo!());
		let (rhs_i, rhs_size) = rhs.val.as_num().unwrap_or_else(|| todo!());

		let size = lhs_size.max(rhs_size);

		let val = match size {
			8   => Val::Int8(op8(lhs_i as u8, rhs_i as u8)),
			16  => Val::Int16(op16(lhs_i as u16, rhs_i as u16)),
			32  => Val::Int32(op32(lhs_i as u32, rhs_i as u32)),
			64  => Val::Int64(op64(lhs_i as u64, rhs_i as u64)),
			128 => Val::Int128(op128(lhs_i, rhs_i)),
			_   => unreachable!(),
		};

		Value::new(val, out.unwrap_or(lhs.ty.clone()))
	}

	pub fn add(lhs: &Self, rhs: &Self, out: Option<Type>) -> Self {
		Self::op(lhs, rhs, out,
			u8::wrapping_add,
			u16::wrapping_add,
			u32::wrapping_add,
			u64::wrapping_add,
			u128::wrapping_add)
	}

	pub fn sub(lhs: &Self, rhs: &Self, out: Option<Type>) -> Self {
		Self::op(lhs, rhs, out,
			u8::wrapping_sub,
			u16::wrapping_sub,
			u32::wrapping_sub,
			u64::wrapping_sub,
			u128::wrapping_sub)
	}

	pub fn mul(lhs: &Self, rhs: &Self, out: Option<Type>) -> Self {
		Self::op(lhs, rhs, out,
			u8::wrapping_mul,
			u16::wrapping_mul,
			u32::wrapping_mul,
			u64::wrapping_mul,
			u128::wrapping_mul)
	}

	pub fn div(lhs: &Self, rhs: &Self, out: Option<Type>) -> Result<Self> {
		if let Some((0, _)) = rhs.val.as_num() {
			return MiriError::DivideByZero.untitled().as_err();
		}

		Ok(Self::op(lhs, rhs, out,
			u8::wrapping_div,
			u16::wrapping_div,
			u32::wrapping_div,
			u64::wrapping_div,
			u128::wrapping_div))
	}

	pub fn rem(lhs: &Self, rhs: &Self, out: Option<Type>) -> Result<Self> {
		if let Some((0, _)) = rhs.val.as_num() {
			return MiriError::DivideByZero.untitled().as_err();
		}

		Ok(Self::op(lhs, rhs, out,
			u8::wrapping_rem,
			u16::wrapping_rem,
			u32::wrapping_rem,
			u64::wrapping_rem,
			u128::wrapping_rem))
	}
}

#[derive(Debug, Clone)]
pub enum Type {
	Usize, Isize,
	U(u32), I(u32),
	Fn(Vec<Type>, Box<Type>),
	Ptr(Box<Type>),
	Array(Box<Type>, Option<u64>),
	None, Never
}

impl Type {
	pub fn from_mir(ty: &MirType) -> Self {
		match ty {
			MirType::Usize => Self::Usize,
			MirType::Isize => Self::Isize,
			MirType::U(n)  => Self::U(*n),
			MirType::I(n)  => Self::I(*n),
			MirType::None  => Self::None,
			MirType::Never => Self::Never,
			MirType::Fn(args, ret) => Type::Fn(
				args.iter().map(|&t| Self::from_mir(t)).collect(),
				Box::new(Self::from_mir(ret))
			),
			MirType::Ptr(t)    => Type::Ptr(Box::new(Self::from_mir(t))),
			MirType::Arr(t, s) => Type::Array(Box::new(Self::from_mir(t)), *s),
			_ => todo!("{:?}", ty),
		}
	}
}

#[derive(Debug, Clone)]
pub enum Val {
	None,
	Int8(u8),
	Int16(u16),
	Int32(u32),
	Int64(u64),
	Int128(u128),
	CFn(libffi::middle::Cif, libffi::middle::CodePtr), 
	FatPtr((*mut c_void, usize)),
	Ptr(*mut c_void),

	Ret(usize),
}

impl Val {
	pub fn as_num(&self) -> Option<(u128, u32)> {
		Some(match self {
			Val::Int8(v)   => (*v as u128, 8),
			Val::Int16(v)  => (*v as u128, 16),
			Val::Int32(v)  => (*v as u128, 32),
			Val::Int64(v)  => (*v as u128, 64),
			Val::Int128(v) => (*v,         128),
			_ => return None,
		})
	}

	pub fn from_ibig(i: &IBig, ty: &Type) -> Option<Self> {
		Some(match ty {
			Type::Usize | Type::Isize => match std::mem::size_of::<usize>() {
				1 | 2 => Val::Int16(i.try_as_u32()?.try_into().ok()?),
				4     => Val::Int32(i.try_as_u32()?),
				8     => Val::Int64(i.try_as_u64()?),
				16    => Val::Int128(i.try_as_u128()?),
				_     => unreachable!(),
			},
			Type::U(..8)   | Type::I(..8)   => Val::Int8(i.try_as_u32()?.try_into().ok()?),
			Type::U(..16)  | Type::I(..16)  => Val::Int16(i.try_as_u32()?.try_into().ok()?),
			Type::U(..32)  | Type::I(..32)  => Val::Int32(i.try_as_u32()?),
			Type::U(..64)  | Type::I(..64)  => Val::Int64(i.try_as_u64()?),
			Type::U(..128) | Type::I(..128) => Val::Int128(i.try_as_u128()?),
			Type::Ptr(_) => match std::mem::size_of::<*mut c_void>() { 
				1 | 2 => Val::Ptr(i.try_as_u32()? as *mut c_void),
				4     => Val::Ptr(i.try_as_u32()? as *mut c_void),
				8     => Val::Ptr(i.try_as_u64()? as *mut c_void),
				16    => Val::Ptr(i.try_as_u128()? as *mut c_void),
				_     => unreachable!(),
			},
			_ => return None,
		})
	}
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
			Val::Int8(v)   => write!(f, "{v}"),
			Val::Int16(v)  => write!(f, "{v}"),
			Val::Int32(v)  => write!(f, "{v}"),
			Val::Int64(v)  => write!(f, "{v}"),
			Val::Int128(v) => write!(f, "{v}"),
			Val::CFn(_, p) => write!(f, "cfn@{p:p}"),
			Val::Ptr(s)    => write!(f, "{s:p}"),
			Val::FatPtr((p, m)) => write!(f, "({p:p}; {m})"),
			// warn user if this prints
			Val::Ret(base) => write!(f, "{{ret: {base}}}"),
		}
	}
}

impl Display for Type {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Type::U(n)  => write!(f, "u{n}"),
			Type::I(n)  => write!(f, "i{n}"),
			Type::Usize => write!(f, "usize"),
			Type::Isize => write!(f, "isize"),
			Type::Fn(args, ret) => {
				write!(f, "fn(")?;
				args.iter().enumerate().try_for_each(|(i, ty)| {
					if i > 0 { write!(f, ", ")?; }
					write!(f, "{ty}")
				})?;
				write!(f, ") -> {ret}")
			},
			Type::Ptr(t) => write!(f, "&{t}"),
			Type::Array(t, Some(s)) => write!(f, "[{t}; {s}]"),
			Type::Array(t, None)    => write!(f, "[{t}]"),
			Type::None  => write!(f, "none"),
			Type::Never => write!(f, "never"),
		}
	}
}
