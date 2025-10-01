use std::ffi::c_void;
use std::fmt::{self, Display};

use sharc::mir::{Var, Type as MirType};
use sharc::report::Reportable;
use libffi::middle::{Type as FfiType, Arg};
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

	pub fn add(lhs: &Self, rhs: &Self, out: Option<Type>) -> Self {
		match (&lhs.val, &rhs.val) {
			(Val::Int8(a), Val::Int8(b)) =>
				Value::new(Val::Int8(a.overflowing_add(*b).0), out.unwrap_or(lhs.ty.clone())),
			(Val::Int16(a), Val::Int16(b)) =>
				Value::new(Val::Int16(a.overflowing_add(*b).0), out.unwrap_or(lhs.ty.clone())),
			(Val::Int32(a), Val::Int32(b)) =>
				Value::new(Val::Int32(a.overflowing_add(*b).0), out.unwrap_or(lhs.ty.clone())),
			(Val::Int64(a), Val::Int64(b)) =>
				Value::new(Val::Int64(a.overflowing_add(*b).0), out.unwrap_or(lhs.ty.clone())),
			(Val::Int128(a), Val::Int128(b)) =>
				Value::new(Val::Int128(a.overflowing_add(*b).0), out.unwrap_or(lhs.ty.clone())),
			_ => todo!(),
		}
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

	pub fn to_libffi(&self) -> FfiType {
		match self {
			Self::Usize => FfiType::usize(),
			Self::Isize => FfiType::isize(),
			Self::U(n) => match n.next_power_of_two() {
				1..=8 => FfiType::u8(),
				16    => FfiType::u16(),
				32    => FfiType::u32(),
				64    => FfiType::u64(),
				n     => FfiType::structure(std::iter::repeat_n(FfiType::u64(), (n / 64) as usize)),
			}
			Self::I(n) => match n.next_power_of_two() {
				1..=8 => FfiType::i8(),
				16    => FfiType::i16(),
				32    => FfiType::i32(),
				64    => FfiType::i64(),
				n     => FfiType::structure(std::iter::repeat_n(FfiType::i64(), (n / 64) as usize)),
			},
			Self::None | Self::Never => FfiType::void(),
			Self::Fn(_, _) | Self::Ptr(_) => FfiType::pointer(),
			Self::Array(t, Some(s)) => FfiType::structure(std::iter::repeat_n(t.to_libffi(), *s as usize)),
			Self::Array(_, None)    => panic!("cannot convert unsized array to libffi type"),
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
	FatPtr(*mut c_void, usize),
	Ptr(*mut c_void),

	Ret(usize),
}

// needed so we can have libffi func calls return
pub union RawVal {
	none: (),
	int8: u8,
	int16: u16,
	int32: u32,
	int64: u64,
	int128: u128,
	fat_ptr: (*mut c_void, usize),
	ptr:  *mut c_void,
	cfn:  libffi::middle::CodePtr,
}

impl RawVal {
	pub fn to_val(self, ty: &Type) -> Result<Val> {
		unsafe {
			Ok(match ty {
				Type::None  => Val::None,
				Type::U(..8)  | Type::I(..8)   => Val::Int8(self.int8),
				Type::U(..16) | Type::I(..16)  => Val::Int16(self.int16),
				Type::U(..32) | Type::I(..32)  => Val::Int32(self.int32),
				Type::U(..64) | Type::I(..64)  => Val::Int64(self.int64),
				Type::U(..128)| Type::I(..128) => Val::Int128(self.int128),
				Type::Usize   | Type::Isize    => match std::mem::size_of::<usize>() {
					..16  => Val::Int16(self.int16),
					..32  => Val::Int32(self.int32),
					..64  => Val::Int64(self.int64),
					..128 => Val::Int128(self.int128),
					_     => unreachable!(),
				},
				Type::U(_)    | Type::I(_)     => panic!("unsupported integer size for rawval"),

				Type::Fn(args, ret) => Val::CFn(
					libffi::middle::Cif::new(
						args.iter().map(|t| t.to_libffi()),
						ret.to_libffi()
					),
					self.cfn
				),
				Type::Ptr(_) => Val::Ptr(self.ptr),
				Type::Never => 
				return MiriError::InvalidInstruction
					.title("cannot convert a rawval to a value of type never")
					.as_err(),
				Type::U(_) | Type::I(_) => panic!(),

				// TODO: somehow impl sized arrays
				Type::Array(_, _) => todo!(),
			})
		}
	}
}

impl Val {
	pub fn as_arg(&self) -> Arg {
		match self {
			Self::None      => Arg::new(&()),
			Self::Int8(v)   => Arg::new(v),
			Self::Int16(v)  => Arg::new(v),
			Self::Int32(v)  => Arg::new(v),
			Self::Int64(v)  => Arg::new(v),
			Self::Int128(v) => Arg::new(v),
			Self::CFn(_, p) => Arg::new(p),
			Self::Ptr(s)    => Arg::new(s),
			Self::FatPtr(p, m) => Arg::new(&(*p, *m)),
			Self::Ret(_)    => panic!(),
		}
	}

	pub fn from_ibig(i: &IBig, ty: &Type) -> Option<Self> {
		Some(match ty {
			Type::U(..8)   | Type::I(..8)   => Val::Int8(i.try_as_u32()?.try_into().ok()?),
			Type::U(..16)  | Type::I(..16)  => Val::Int16(i.try_as_u32()?.try_into().ok()?),
			Type::U(..32)  | Type::I(..32)  => Val::Int32(i.try_as_u32()?),
			Type::U(..64)  | Type::I(..64)  => Val::Int64(i.try_as_u64()?),
			Type::U(..128) | Type::I(..128) => Val::Int128(i.try_as_u128()?),
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
			// warn user if this prints
			Val::Ret(base) => write!(f, "{{ret: {base}}}"),
			Val::FatPtr(p, m) => write!(f, "({p:p}; {m})"),
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
