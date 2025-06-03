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
