use std::fmt::{self, Display};
use colored::Colorize;

use crate::span::Sp;

#[derive(Debug)]
pub enum Node<'src, 'b> {
	Let {
		ident: Sp<&'src str>,
		ty:    Option<&'b Type<'src, 'b>>,
		gener: &'b [&'b Type<'src, 'b>], // TODO bounds, default vals
		expr:  &'b Ty<'src, 'b, Sp<Node<'src, 'b>>>,
		stat:  bool, // static // maybe 
	},

	Ident {
		lit:   Sp<&'src str>, 
		gener: &'b [&'b Type<'src, 'b>], // TODO bounds, default vals
	},
	StrLit(&'b str),
	IntLit(crate::bigint::IBig<'b>),
	ArrayLit(&'b [Ty<'src, 'b, Sp<Node<'src, 'b>>>], Option<u64>), // [u8], [20, 3, 8], [u16; 10]
	UnionLit(&'b [Ty<'src, 'b, Sp<Node<'src, 'b>>>]),  // ('foo | u8 | foo: u8)
	StructLit(&'b [Ty<'src, 'b, Sp<Node<'src, 'b>>>]), // (u8, 20, foo: 20, foo: u8)
	Primitive(&'b Type<'src, 'b>),
	Quote(&'src str), // 'foo

	FuncCall {
		lhs:  &'b Ty<'src, 'b, Sp<Node<'src, 'b>>>,
		args: &'b [Ty<'src, 'b, Sp<Node<'src, 'b>>>],
	},
	Lambda {
		args:   &'b [LambdaArg<'src, 'b>],
		ret:    Option<&'b Type<'src, 'b>>,
		body:   Option<&'b Ty<'src, 'b, Sp<Node<'src, 'b>>>>,
		ext:    Option<Sp<&'b str>>,
		export: Option<Sp<&'b str>>,
	},

	Block(&'b [Sp<Node<'src, 'b>>]),
	ImplCall {
		path:  &'b [Sp<&'src str>],
		ident: Sp<&'src str>,
		gener: &'b [&'b Type<'src, 'b>],
		vals:  &'b [Ty<'src, 'b, Sp<Node<'src, 'b>>>],
	},

	// UNARY EXPR
	Ret(Option<&'b Ty<'src, 'b, Sp<Node<'src, 'b>>>>),
	Move(&'b Ty<'src, 'b, Sp<Node<'src, 'b>>>),

	// BINARY EXPR
	Store(&'b Ty<'src, 'b, Sp<Node<'src, 'b>>>, &'b Ty<'src, 'b, Sp<Node<'src, 'b>>>), // foo = 20
	Field(&'b Ty<'src, 'b, Sp<Node<'src, 'b>>>, &'b Ty<'src, 'b, Sp<Node<'src, 'b>>>), // foo: bar
}

#[derive(Debug)]
pub struct LambdaArg<'src, 'b> {
	pub ident:   Sp<&'src str>,
	pub ty:      Option<&'b Type<'src, 'b>>,
	pub default: Option<Ty<'src, 'b, Sp<Node<'src, 'b>>>>,
}

#[derive(Debug)]
pub struct Ty<'src, 'b, T> {
	pub elem: T,
	pub ty:   &'b Type<'src, 'b>,
}

impl<T> TypeInf for T {}
pub trait TypeInf {
	fn typed<'src, 'b>(self, ty: &'b Type<'src, 'b>) -> Ty<'src, 'b, Self> where Self: Sized {
		Ty { ty, elem: self }
	}
}

#[derive(Debug)]
pub struct Type<'src, 'b> {
	pub ident: Option<Sp<&'src str>>, // newtypes
	pub kind:  TypeKind<'src, 'b>,
}

impl PartialEq for Type<'_, '_> {
	fn eq(&self, other: &Self) -> bool {
		self.kind == other.kind
	}
}

impl<'src, 'b> Type<'src, 'b> {
	pub const fn new(kind: TypeKind<'src, 'b>) -> Self {
		Type { ident: None, kind }
	}

	pub fn with_ident(ident: Sp<&'src str>, kind: TypeKind<'src, 'b>) -> Self {
		Type { ident: Some(ident), kind }
	}
}

#[derive(Debug, PartialEq)]
pub enum TypeKind<'src, 'b> {
	Generic(usize),

	Array(&'b Type<'src, 'b>, Option<u64>),
	Union(&'b [&'b Type<'src, 'b>]),
	Struct(&'b [&'b Type<'src, 'b>]),
	U(u32), I(u32), B(u32), F(u32),
	Ref(&'b Type<'src, 'b>),
	Mut(&'b Type<'src, 'b>),
	Usize, Isize,
	None, Never, Type,
	// Type(Rc<'b, Type<'src, 'b>>),
	Fn(&'b [&'b Type<'src, 'b>], Option<&'b Type<'src, 'b>>),
}




impl Display for Node<'_, '_> {
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

			Self::ImplCall { path, ident, gener, vals } => {
				write!(f, "{}::{}", join_tostring(&**path, "::"), ident.red())?;
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

impl Display for LambdaArg<'_, '_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.ident.normal())?;
		if let Some(ty) = &self.ty { write!(f, ": {ty}")?; }
		if let Some(def) = &self.default { write!(f, " = {def}")?; }
		Ok(())
	}
}

impl<T: Display> Display for Ty<'_, '_, T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}{}", self.elem, format!(": {}", self.ty).dimmed())
	}
}

impl fmt::Display for Type<'_, '_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if let Some(ident) = &self.ident { write!(f, "{} ", ident.to_string().purple())?; }
		write!(f, "{}", self.kind)
	}
}

impl fmt::Display for TypeKind<'_, '_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		fn join_tostring(iter: impl IntoIterator<Item = impl ToString>, s: &str) -> String {
			iter.into_iter().map(|e| ToString::to_string(&e)).collect::<std::vec::Vec<_>>().join(s)
		}

		match self {
			Self::Array(arr, size) => {
				write!(f, "[{arr}")?;
				if let Some(size) = size { write!(f, "; {size}")?; }
				write!(f, "]")
			},
			Self::Union(variants) => write!(f, "({})", join_tostring(&**variants, " | ")),
			Self::Struct(fields)  => write!(f, "({})", join_tostring(&**fields, ", ")),
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
				Self::None  => String::from("none"),
				Self::Never => String::from("never"),
				Self::Type  => String::from("type"),
				Self::Ref(t) => format!("&{t}"),
				_ => unreachable!(),
			}.purple())
		}
	}
}
