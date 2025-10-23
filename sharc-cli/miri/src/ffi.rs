use std::os::raw::c_void;
use libffi::middle::{Type as FfiType, Arg};
use sharc::report::Reportable;

use crate::val::{Type, Val};
use crate::error::{Result, MiriError};

impl crate::val::Type {
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

// needed so we can have libffi func calls return
pub union RawVal {
	pub none: (),
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
	pub fn into_val(self, ty: &Type) -> Result<Val> {
		unsafe {
			Ok(match ty {
				Type::None  => Val::None,
				Type::U(..8)  | Type::I(..8)   => Val::Int8(self.int8),
				Type::U(..16) | Type::I(..16)  => Val::Int16(self.int16),
				Type::U(..32) | Type::I(..32)  => Val::Int32(self.int32),
				Type::U(..64) | Type::I(..64)  => Val::Int64(self.int64),
				Type::U(..128)| Type::I(..128) => Val::Int128(self.int128),
				Type::Usize   | Type::Isize    => match std::mem::size_of::<usize>() {
					1 | 2 => Val::Int16(self.int16),
					4     => Val::Int32(self.int32),
					8     => Val::Int64(self.int64),
					16    => Val::Int128(self.int128),
					_     => unreachable!(),
				},
				// FIXME: make this a Report
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
			Self::FatPtr(a) => Arg::new(a),
			Self::SharcFnDef(_) => panic!(),
			Self::SharcFnClosure(_, c) => Arg::new(c.code_ptr()),
			Self::Ret(_)    => panic!(),
		}
	}
}
