use std::ffi::c_void;
use std::fmt::{self, Display};

use sharc::mir::{Var, Type as MirType};
use libffi::middle::{Type as FfiType, Arg};
use colored::Colorize;
use sharc::IBig;

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
			(Val::Int(a), Val::Int(b)) =>
				Value::new(Val::Int(a.overflowing_add(*b).0), out.unwrap_or(lhs.ty.clone())),
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
			MirType::Ptr(t)    => Type::Ptr(Box::new(Self::from_mir(t))),
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
	Int(u64), // TODO: change to u128 if needed
	CFn(libffi::middle::Cif, libffi::middle::CodePtr),
	Ptr(*mut c_void),

	Ret(usize),
}

// needed so we can have libffi func calls return
pub union RawVal {
	none: (),
	int:  u64,
	ptr:  *mut c_void,
	cfn:  libffi::middle::CodePtr,
}

impl RawVal {
	pub fn to_val(self, ty: &Type) -> Val {
		unsafe {
			match ty {
				Type::None  => Val::None,
				Type::U(_)  | Type::I(_) | Type::Usize | Type::Isize => Val::Int(self.int),
				Type::Fn(args, ret) => Val::CFn(
					libffi::middle::Cif::new(
						args.iter().map(|t| t.to_libffi()),
						ret.to_libffi()
					),
					self.cfn
				),
				Type::Ptr(_) | Type::Array(_, _) => Val::Ptr(self.ptr),
				Type::Never => panic!("cannot convert never type to value"),
				Type::U(_) | Type::I(_) => panic!(),
			}
		}
	}
}

impl Val {
	pub fn as_arg(&self) -> Arg {
		match self {
			Self::None   => Arg::new(&()),
			Self::Int(v) => Arg::new(v),
			Self::CFn(_, ptr) => Arg::new(ptr),
			Self::Ptr(s) => Arg::new(s),
			Self::Ret(_) => panic!(),
		}
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
			Val::Int(v)    => write!(f, "{v}"),
			Val::CFn(_, p) => write!(f, "cfn@{p:p}"),
			Val::Ptr(s)    => write!(f, "{s:p}"),
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
