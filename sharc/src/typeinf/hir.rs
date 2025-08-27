use std::fmt::{self, Display};
use colored::Colorize;

use crate::span::Sp;

#[derive(Debug)]
pub enum Node<'s> {
	Let {
		ident: Sp<&'s str>,
		ty:    Option<&'s Ty<'s, Sp<Node<'s>>>>,
		gener: &'s [Sp<Node<'s>>],
		expr:  &'s Ty<'s, Sp<Node<'s>>>,
		stat:  bool, // static
	},

	Ident {
		lit:   Sp<&'s str>, 
		gener: &'s [Sp<Node<'s>>],
	},
	StrLit(&'s str),
	IntLit(crate::bigint::IBig<'s>),
	ArrayLit(&'s [Sp<Node<'s>>], Option<u64>), // [u8], [20, 3, 8], [u16; 10]
	UnionLit(&'s [Sp<Node<'s>>]),  // ('foo | u8 | foo: u8)
	StructLit(&'s [Sp<Node<'s>>]), // (u8, 20, foo: 20, foo: u8)
	Primitive(&'s Type<'s>),
	Quote(&'s str), // 'foo

	FuncCall {
		lhs:  &'s Ty<'s, Sp<Node<'s>>>,
		args: &'s [Sp<Node<'s>>],
	},
	Lambda {
		args:   &'s [LambdaArg<'s>],
		ret:    Option<&'s Ty<'s, Sp<Node<'s>>>>,
		body:   Option<&'s Ty<'s, Sp<Node<'s>>>>,
		ext:    Option<Sp<&'s str>>,
		export: Option<Sp<&'s str>>,
	},

	Block(&'s [Sp<Node<'s>>]),
	Builtin {
		kind:  BuiltinKind,
		gener: &'s [&'s Type<'s>],
		vals:  &'s [Ty<'s, Sp<Node<'s>>>],
	},

	// UNARY EXPR
	Ret(Option<&'s Ty<'s, Sp<Node<'s>>>>),
	Move(&'s Ty<'s, Sp<Node<'s>>>),

	// BINARY EXPR
	Store(&'s Ty<'s, Sp<Node<'s>>>, &'s Ty<'s, Sp<Node<'s>>>), // foo = 20
	Field(&'s Ty<'s, Sp<Node<'s>>>, &'s Ty<'s, Sp<Node<'s>>>), // foo: bar
}

#[derive(Debug)]
pub struct LambdaArg<'s> {
	pub ident:   Sp<&'s str>,
	pub ty:      Option<Ty<'s, Sp<Node<'s>>>>,
	pub default: Option<Ty<'s, Sp<Node<'s>>>>,
}

#[derive(Debug)]
pub enum BuiltinKind {
	As, Add,
}


#[derive(Debug)]
pub struct Ty<'s, T> {
	pub elem: T,
	pub ty:   &'s Type<'s>,
}

impl<T> TypeInf for T {}
pub trait TypeInf {
	fn typed<'a>(self, ty: &'a Type<'a>) -> Ty<'a, Self> where Self: Sized {
		Ty { ty, elem: self }
	}
}

#[derive(Debug)]
pub struct Type<'s> {
	pub ident: Option<Sp<&'s str>>, // newtypes
	pub kind:  TypeKind<'s>,
}

impl<'s> Type<'s> {
	pub fn new(kind: TypeKind<'s>) -> Self {
		Type { ident: None, kind }
	}

	pub fn with_ident(ident: Sp<&'s str>, kind: TypeKind<'s>) -> Self {
		Type { ident: Some(ident), kind }
	}
}

#[derive(Debug)]
pub enum TypeKind<'s> {
	Generic(usize),

	ArrayLit(&'s [&'s Type<'s>], Option<u64>),
	UnionLit(&'s [&'s Type<'s>]),
	StructLit(&'s [&'s Type<'s>]),
	U(u32), I(u32), B(u32), F(u32),
	Ref(&'s Type<'s>),
	Mut(&'s Type<'s>),
	Usize, Isize,
	Any, None, Never, Type,
	// Type(Rc<'s, Type<'s>>),
	Fn(&'s [&'s Type<'s>], Option<&'s Type<'s>>),
}




impl Display for Node<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		fn join_tostring(iter: impl IntoIterator<Item = impl ToString>, s: &str) -> String {
			iter.into_iter().map(|e| ToString::to_string(&e)).collect::<std::vec::Vec<_>>().join(s)
		}

		match self {
			Self::Let { ident, ty, gener, expr, stat } =>
				write!(f, "{} {ident}{}{} = {expr}",
					if *stat { "static" } else { "let" }.yellow().dimmed(),
					if gener.is_empty() { String::new() } else { format!("<{}>", join_tostring(&**gener, ", ")) },
					if let Some(ty) = ty { format!(": {ty}") } else { String::new() } ),

			Self::Ident { lit, gener } => 
				write!(f, "{}{}", lit.normal(), 
					if gener.is_empty() { String::new() } else { format!("<{}>", join_tostring(&**gener, ", ")) }),
			Self::StrLit(s) => write!(f, "{}", format!("{s:?}").green()),
			Self::IntLit(i) => write!(f, "{}", i.to_string().cyan()),
			Self::ArrayLit(arr, size) => {
				write!(f, "[")?;
				for (i, item) in arr.iter().enumerate() {
					write!(f, "{item}")?;
					if i != arr.len() - 1 { write!(f, ", ")?; }
				}
				if let Some(size) = size { write!(f, "; {size}")?; }
				write!(f, "]")
			},
			Self::UnionLit(variants) => write!(f, "({})", join_tostring(&**variants, " | ")),
			Self::StructLit(fields)  => write!(f, "({})", join_tostring(&**fields, ", ")),
			Self::Primitive(p) => write!(f, "{p}"),
			Self::Quote(ident) => write!(f, "{}", format!("'{ident}").bright_red()),

			Self::Block(body) => match &body[..] {
				[]  => Ok(()),
				[n] => write!(f, "{{ {n} }}"),
				ref n => {
					writeln!(f, "{{")?;
					n.iter().enumerate().try_for_each(
						|(i, stmt)| writeln!(f, "  {stmt}{}",
							if i == n.len() - 1 { "" } else { ";" }))?;
					write!(f, "}}")
				},
			},

			Self::Lambda { args, ret, body, ext, export } => {
				use crate::span::Spannable;

				if let Some(e) = ext { write!(f, "{} {} ", 
					"extern".yellow().dimmed(),
					Self::StrLit(e.elem).span(e.span).to_string().green())?; }
				if let Some(e) = export { write!(f, "{} {} ", 
					"export".yellow().dimmed(),
					Self::StrLit(e.elem).span(e.span).to_string().green())?; }

				write!(f, "|")?;
				for (i, arg) in args.iter().enumerate() {
					write!(f, "{arg}")?;
					if i != args.len() - 1 { write!(f, ", ")?; }
				}
				write!(f, "|")?;

				if let Some(ret) = ret { write!(f, ": {ret}")?; }
				if let Some(body) = body { write!(f, " {body}")?; }
				write!(f, ";")?;

				Ok(())
			},

			Self::FuncCall { lhs, args } => {
				write!(f, "{}(", lhs.to_string().red())?;
				for (i, arg) in args.iter().enumerate() {
					write!(f, "{arg}")?;
					if i != args.len() - 1 { write!(f, ", ")?; }
				}
				write!(f, ")")
			},
			
			Self::Builtin { kind, gener, vals } => {
				write!(f, "core::{}", format!("{kind:?}").red())?;
				if !gener.is_empty() { write!(f, "<{}>", join_tostring(&**gener, ", "))?; }
				write!(f, "(")?;
				if !vals.is_empty() { write!(f, "{}", join_tostring(&**vals, ", "))?; }
				write!(f, ")")
			},

			Self::Ret(expr) => {
				write!(f, "{}", "return".yellow().dimmed())?;
				if let Some(expr) = expr { write!(f, " {expr}")?; }
				Ok(())
			},

			Self::Move(expr)  => write!(f, "{} {expr}", "move".yellow().dimmed()),
			Self::Store(a, b) => write!(f, "{a} = {b}"),
			Self::Field(a, b) => write!(f, "{a}: {b}"),
		}
	}
}

impl Display for LambdaArg<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.ident.normal())?;
		if let Some(ty) = &self.ty { write!(f, ": {ty}")?; }
		if let Some(def) = &self.default { write!(f, " = {def}")?; }
		Ok(())
	}
}

impl<T: Display> Display for Ty<'_, T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}{}", self.elem, format!(": {}", self.ty).dimmed())
	}
}

impl fmt::Display for Type<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if let Some(ident) = &self.ident { write!(f, "{} ", ident.to_string().purple())?; }
		write!(f, "{}", self.kind)
	}
}

impl fmt::Display for TypeKind<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		fn join_tostring(iter: impl IntoIterator<Item = impl ToString>, s: &str) -> String {
			iter.into_iter().map(|e| ToString::to_string(&e)).collect::<std::vec::Vec<_>>().join(s)
		}

		match self {
			Self::ArrayLit(arr, size) => {
				write!(f, "[")?;
				for (i, item) in arr.iter().enumerate() {
					write!(f, "{item}")?;
					if i != arr.len() - 1 { write!(f, ", ")?; }
				}
				if let Some(size) = size { write!(f, "; {size}")?; }
				write!(f, "]")
			},
			Self::UnionLit(variants) => write!(f, "({})", join_tostring(&**variants, " | ")),
			Self::StructLit(fields)  => write!(f, "({})", join_tostring(&**fields, ", ")),
			Self::Fn(args, ret) => {
				write!(f, "{}(", "fn".purple())?;
				for (i, arg) in args.iter().enumerate() {
					write!(f, "{arg}")?;
					if i != args.len() - 1 { write!(f, ", ")?; }
				}
				if let Some(ret) = ret { write!(f, "): {ret}")?; } else { write!(f, ")")?; }
				Ok(())
			},
			k => write!(f, "{}", match k {
				Self::Generic(v) => format!("${v}"),
				Self::U(u)  => format!("u{u}"),
				Self::I(i)  => format!("i{i}"),
				Self::B(b)  => format!("b{b}"),
				Self::F(f)  => format!("f{f}"),
				Self::Usize => String::from("usize"),
				Self::Isize => String::from("isize"),
				Self::Any   => String::from("any"),
				Self::None  => String::from("none"),
				Self::Never => String::from("never"),
				Self::Type  => String::from("type"),
				Self::Ref(t) => format!("&{t}"),
				_ => unreachable!(),
			}.purple())
		}
	}
}
