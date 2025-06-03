use std::fmt::{self, Display};
use colored::Colorize;

use crate::span::Sp;

type Box<T> = bump::Box<'static, T>;

pub struct TypedAST {
	pub bump:  bump::Bump,
	pub nodes: Box<[Ty<Sp<crate::parser::ast::Node>>]>,
}

pub struct Ty<T> {
	pub elem: T,
	pub ty:   Option<Type>, // None for stmts
}

impl<T> TypeInf for T {}
pub trait TypeInf {
	fn typed(self, ty: Type) -> Ty<Self> where Self: Sized {
		Ty { ty: Some(ty), elem: self }
	}

	fn untyped(self) -> Ty<Self> where Self: Sized {
		Ty { ty: None, elem: self }
	}
}

impl<T: Display> Display for Ty<T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self.ty {
			Some(ref ty) => write!(f, "{}{}", self.elem, format!(": {ty}").dimmed()),
			None => write!(f, "{}", self.elem),
		}
	}
}

pub struct Type {
	pub ident: Option<Sp<&'static str>>, // newtypes
	pub kind:  Box<TypeKind>,
}

pub enum TypeKind {
	ArrayLit(Box<[Type]>, Option<u64>),
	UnionLit(Box<[Type]>),
	StructLit(Box<[Type]>),
	U(u32), I(u32), B(u32), F(u32),
	Usize, Isize,
	Any, None, Never,
	Fn(Box<[Type]>, Option<Type>),
}

impl fmt::Display for Type {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if let Some(ident) = &self.ident { write!(f, "{} ", ident.to_string().purple())?; }
		write!(f, "{}", self.kind)
	}
}

impl fmt::Display for TypeKind {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		fn join_tostring(iter: impl IntoIterator<Item = impl ToString>, s: &str) -> String {
			iter.into_iter().map(|e| ToString::to_string(&e)).collect::<std::vec::Vec<_>>().join(s)
		}

		match self {
			TypeKind::ArrayLit(arr, size) => {
				write!(f, "[")?;
				for (i, item) in arr.iter().enumerate() {
					write!(f, "{item}")?;
					if i != arr.len() - 1 { write!(f, ", ")?; }
				}
				if let Some(size) = size { write!(f, "; {size}")?; }
				write!(f, "]")
			},
			TypeKind::UnionLit(variants) => write!(f, "({})", join_tostring(&**variants, " | ")),
			TypeKind::StructLit(fields)  => write!(f, "({})", join_tostring(&**fields, ", ")),
			TypeKind::Fn(args, ret) => {
				write!(f, "{}(", "fn".purple())?;
				for (i, arg) in args.iter().enumerate() {
					write!(f, "{arg}")?;
					if i != args.len() - 1 { write!(f, ", ")?; }
				}
				if let Some(ret) = ret { write!(f, "): {ret}")?; } else { write!(f, ")")?; }
				Ok(())
			},
			k => write!(f, "{}", match k {
				TypeKind::U(u)  => format!("u{u}"),
				TypeKind::I(i)  => format!("i{i}"),
				TypeKind::B(b)  => format!("b{b}"),
				TypeKind::F(f)  => format!("f{f}"),
				TypeKind::Usize => String::from("usize"),
				TypeKind::Isize => String::from("isize"),
				TypeKind::Any   => String::from("any"),
				TypeKind::None  => String::from("none"),
				TypeKind::Never => String::from("never"),
				_ => unreachable!(),
			}.purple())
		}
	}
}
