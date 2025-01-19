use std::fmt::{self, Display};
use crate::span::Span;

use colored::Colorize;

pub struct Sp<T> {
	pub span: Span,
	pub elem: T,
}

impl<T> std::ops::Deref for Sp<T> {
	type Target = T;
	fn deref(&self) -> &Self::Target 
		{ &self.elem }
}

impl<T> std::ops::DerefMut for Sp<T> {
	fn deref_mut(&mut self) -> &mut Self::Target 
		{ &mut self.elem }
}

pub trait Spannable {
	fn span(self, span: Span) -> Sp<Self> where Self: Sized
		{ Sp { span, elem: self } }
}

impl<T> Spannable for T {}

pub enum Node<'src> {
	DBG,
	Func {
		name:   Sp<&'src str>, 
		export: Sp<bool>, // TODO: move separate struc
		args:   Vec<(Sp<&'src str>, Sp<Type<'src>>)>,
		ret:    Option<Sp<Type<'src>>>,
		body:   Vec<Sp<Node<'src>>>
	},
	Ret(Option<Box<Sp<Node<'src>>>>),
	FuncCall {
		name: Sp<&'src str>,
		args: Vec<Sp<Node<'src>>>,
	},
	StrLit(String),
	UIntLit(u64),
}

pub enum Type<'src> {
	U8, U16, U32, U64,
	I8, I16, I32, I64,
	B8, B16, B32, B64,
	F32, F64,
	Void, Never,
	Opt(Box<Sp<Type<'src>>>),
	Ptr(Box<Sp<Type<'src>>>),
	Arr(Box<Sp<Type<'src>>>),
	Mut(Box<Sp<Type<'src>>>),
	Ident(&'src str),
}

impl<T: Display> Display for Sp<T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{} {}", self.span, self.elem)
	}
}

// TODO: better display for this shit prob
impl Display for Node<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Node::DBG => write!(f, "DBG"),
			Node::Func { name, export, args, ret, body } => {
				write!(f, "Func: {}\n", name.to_string().blue())?;
				writeln!(f, "   Export: {export}")?;
				write!(f, "   Args: [")?;
				for (i, (name, typ)) in args.iter().enumerate() {
					write!(f, "{name}: {typ}")?;
					if i != args.len() - 1 {
						write!(f, ", ")?;
					}
				}
				write!(f, "]\n   Ret: {}\n", ret.as_ref().map_or("void".to_string(), ToString::to_string))?;
				for stmt in body {
					writeln!(f, "      {stmt}")?;
				}
				Ok(())
			}
			Node::Ret(expr) => match expr {
				Some(expr) => write!(f, "Ret: {expr}"),
				None => write!(f, "Ret"),
			},
			Node::FuncCall { name, args } => {
				write!(f, "FuncCall: {}(", name.to_string().blue())?;
				for (i, arg) in args.iter().enumerate() {
					write!(f, "{arg}")?;
					if i != args.len() - 1 {
						write!(f, ", ")?;
					}
				}
				write!(f, ")")
			},
			Node::StrLit(s)  => write!(f, "StrLit: {}", format!("{s:?}").green()),
			Node::UIntLit(i) => write!(f, "UIntLit: {}", i.to_string().green()),
		}
	}
}

impl Display for Type<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", match self {
			Type::U8  => "u8",
			Type::U16 => "u16",
			Type::U32 => "u32",
			Type::U64 => "u64",
			Type::I8  => "i8",
			Type::I16 => "i16",
			Type::I32 => "i32",
			Type::I64 => "i64",
			Type::B8  => "b8",
			Type::B16 => "b16",
			Type::B32 => "b32",
			Type::B64 => "b64",
			Type::F32 => "f32",
			Type::F64 => "f64",
			Type::Void => "void",
			Type::Never => "never",
			Type::Opt(i) => return write!(f, "opt {i}"),
			Type::Ptr(i) => return write!(f, "*{i}"),
			Type::Arr(i) => return write!(f, "[{i}]"),
			Type::Mut(i) => return write!(f, "mut {i}"),
			Type::Ident(name) => name,
		}.red())
	}
}
