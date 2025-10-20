use std::fmt::{self, Display};
use std::cell::RefCell;

use colored::Colorize;

use crate::span::Sp;

#[derive(Debug)]
pub enum Node<'src, 'b> {
	None,

	Let {
		ident: Sp<&'src str>,
		gener: &'b [&'b RefCell<Type<'src, 'b>>], // TODO bounds, default vals
		expr:  &'b Ty<'src, 'b, Sp<Node<'src, 'b>>>,
		stat:  bool, // static // maybe 
	},

	Loop {
		initlet: Option<&'b Ty<'src, 'b, Sp<Node<'src, 'b>>>>,
		iterty:  &'b RefCell<Type<'src, 'b>>,
		expr:    &'b Ty<'src, 'b, Sp<Node<'src, 'b>>>,
	},

	Ident(&'src str),
	StrLit(&'b str),
	IntLit(crate::bigint::IBig<'b>),
	ArrayLit(&'b [Ty<'src, 'b, Sp<Node<'src, 'b>>>], Option<u64>), // [u8], [20, 3, 8], [u16; 10]
	UnionLit(&'b [Ty<'src, 'b, Sp<Node<'src, 'b>>>]),  // ('foo | u8 | foo: u8)
	StructLit(&'b [Ty<'src, 'b, Sp<Node<'src, 'b>>>]), // (u8, 20, foo: 20, foo: u8)
	Primitive(&'b RefCell<Type<'src, 'b>>),
	Quote(&'src str), // 'foo

	FuncCall {
		lhs:  &'b Ty<'src, 'b, Sp<Node<'src, 'b>>>,
		args: &'b [Ty<'src, 'b, Sp<Node<'src, 'b>>>],
	},
	Lambda {
		args:   &'b [LambdaArg<'src, 'b>],
		ret:    &'b RefCell<Type<'src, 'b>>,
		body:   Option<&'b Ty<'src, 'b, Sp<Node<'src, 'b>>>>,
		ext:    Option<Sp<&'b str>>,
		export: Option<Sp<&'b str>>,
	},

	Block(&'b [Ty<'src, 'b, Sp<Node<'src, 'b>>>]),
	ImplCall {
		path:  &'b [Sp<&'src str>],
		ident: Sp<&'src str>,
		gener: &'b [&'b RefCell<Type<'src, 'b>>],
		vals:  &'b [Ty<'src, 'b, Sp<Node<'src, 'b>>>],
	},

	// UNARY EXPR
	Ret(Option<&'b Ty<'src, 'b, Sp<Node<'src, 'b>>>>),
	Move(&'b Ty<'src, 'b, Sp<Node<'src, 'b>>>),

	// BINARY EXPR
	Store(&'b Ty<'src, 'b, Sp<Node<'src, 'b>>>, &'b Ty<'src, 'b, Sp<Node<'src, 'b>>>), // foo = 20
	Field(&'b Ty<'src, 'b, Sp<Node<'src, 'b>>>, &'b Ty<'src, 'b, Sp<Node<'src, 'b>>>), // foo: bar
	Add(&'b Ty<'src, 'b, Sp<Node<'src, 'b>>>, &'b Ty<'src, 'b, Sp<Node<'src, 'b>>>),
	Sub(&'b Ty<'src, 'b, Sp<Node<'src, 'b>>>, &'b Ty<'src, 'b, Sp<Node<'src, 'b>>>),
	Mul(&'b Ty<'src, 'b, Sp<Node<'src, 'b>>>, &'b Ty<'src, 'b, Sp<Node<'src, 'b>>>),
	Div(&'b Ty<'src, 'b, Sp<Node<'src, 'b>>>, &'b Ty<'src, 'b, Sp<Node<'src, 'b>>>),
	Mod(&'b Ty<'src, 'b, Sp<Node<'src, 'b>>>, &'b Ty<'src, 'b, Sp<Node<'src, 'b>>>),
}

#[derive(Debug)]
pub struct LambdaArg<'src, 'b> {
	pub ident:   Sp<&'src str>,
	pub ty:      &'b RefCell<Type<'src, 'b>>,
	pub default: Option<Ty<'src, 'b, Sp<Node<'src, 'b>>>>,
}

#[derive(Debug)]
pub struct Ty<'src, 'b, T> {
	pub elem: T,
	pub ty:   &'b RefCell<Type<'src, 'b>>,
}

impl<T> TypeInf for T {}
pub trait TypeInf {
	fn typed<'src, 'b>(self, ty: &'b RefCell<Type<'src, 'b>>) -> Ty<'src, 'b, Self> where Self: Sized {
		Ty { ty, elem: self }
	}
}

#[derive(Clone, Debug)]
pub struct Type<'src, 'b> {
	pub ident: Option<Sp<&'b Node<'src, 'b>>>, // newtypes
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

	pub fn with_ident(ident: Sp<&'b Node<'src, 'b>>, kind: TypeKind<'src, 'b>) -> Self {
		Type { ident: Some(ident), kind }
	}
}

#[derive(Debug, PartialEq)]
pub enum TypeKind<'src, 'b> {
	// should always get dropped manually so we never leak
	Generic(usize, Vec<Sp<&'b RefCell<Type<'src, 'b>>>>),

	Array(&'b RefCell<Type<'src, 'b>>, Option<u64>),
	Union(&'b [&'b RefCell<Type<'src, 'b>>]),
	Struct(&'b [&'b RefCell<Type<'src, 'b>>]),
	U(u32), I(u32), B(u32), F(u32),
	Ref(&'b RefCell<Type<'src, 'b>>),
	Mut(&'b RefCell<Type<'src, 'b>>),
	Usize, Isize,
	None, Never, Type,
	// Type(Rc<'b, RefCell<Type<'src, 'b>>>),
	Fn(&'b [&'b RefCell<Type<'src, 'b>>], &'b RefCell<Type<'src, 'b>>),
}

impl Clone for TypeKind<'_, '_> {
	fn clone(&self) -> Self {
		match self {
			Self::Generic(_, _) => panic!("Generic types should not be cloned"),
			Self::Array(t, s) => Self::Array(t, *s),
			Self::Union(v)    => Self::Union(v),
			Self::Struct(f)   => Self::Struct(f),
			Self::U(n)   => Self::U(*n),
			Self::I(n)   => Self::I(*n),
			Self::B(n)   => Self::B(*n),
			Self::F(n)   => Self::F(*n),
			Self::Ref(t) => Self::Ref(t),
			Self::Mut(t) => Self::Mut(t),
			Self::Usize  => Self::Usize,
			Self::Isize  => Self::Isize,
			Self::None   => Self::None,
			Self::Never  => Self::Never,
			Self::Type   => Self::Type,
			Self::Fn(a, r) => Self::Fn(a, r),
		}
	}
}

fn join_tostring(iter: impl IntoIterator<Item = impl ToString>, s: &str) -> String {
	iter.into_iter().map(|e| e.to_string())
		.reduce(|a, b| a + s + &b)
		.unwrap_or_default()
}

fn join_cell_tostring<'a>(iter: impl IntoIterator<Item = &'a &'a RefCell<impl ToString + 'a>>, s: &str) -> String {
	iter.into_iter().map(|e| e.borrow().to_string())
		.reduce(|a, b| a + s + &b)
		.unwrap_or_default()
}

impl Display for Node<'_, '_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::None => write!(f, "{}", "none".dimmed()),

			Self::Let { ident, gener, expr, stat } =>
				write!(f, "{} {ident}{}: {} = {}",
					if *stat { "static" } else { "let" }.yellow().dimmed(),
					if gener.is_empty() { String::new() } else { format!("<{}>", join_cell_tostring(*gener, ", ")) },
					expr.ty.borrow(), expr.elem),

			Self::Loop { initlet, expr, .. } => {
				write!(f, "{} ", "loop".yellow().dimmed())?;

				if let Some(initlet) = initlet {
					write!(f, "{initlet} ")?;
				} 

				write!(f, "{expr}")
			},

			Self::Ident(sym) => write!(f, "{}", sym.normal()),
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
			Self::Primitive(p) => write!(f, "{}", p.borrow()),
			Self::Quote(ident) => write!(f, "{}", format!("'{ident}").bright_red()),

			Self::Block(body) => match &body[..] {
				[]  => Ok(()),
				[n] => write!(f, "{{ {n} }}"),
				n => {
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
				write!(f, "|: {}", ret.borrow())?;

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
				if !gener.is_empty() { write!(f, "({})", join_cell_tostring(*gener, ", "))?; }
				write!(f, "(")?;
				if !vals.is_empty() { write!(f, "{}", join_tostring(*vals, ", "))?; }
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

			Self::Add(a, b) => write!(f, "({a} + {b})"),
			Self::Sub(a, b) => write!(f, "({a} - {b})"),
			Self::Mul(a, b) => write!(f, "({a} * {b})"),
			Self::Div(a, b) => write!(f, "({a} / {b})"),
			Self::Mod(a, b) => write!(f, "({a} % {b})"),
		}
	}
}

impl Display for LambdaArg<'_, '_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}: {}", self.ident.normal(), self.ty.borrow())?;
		if let Some(def) = &self.default { write!(f, " = {def}")?; }
		Ok(())
	}
}

impl<T: Display> Display for Ty<'_, '_, T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}{}", self.elem, format!(": {}", self.ty.borrow()).dimmed())
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
		match self {
			Self::Array(arr, size) => {
				write!(f, "[{}", arr.borrow())?;
				if let Some(size) = size { write!(f, "; {size}")?; }
				write!(f, "]")
			},
			Self::Union(variants) => write!(f, "({})", join_cell_tostring(*variants, " | ")),
			Self::Struct(fields)  => write!(f, "({})", join_cell_tostring(*fields, ", ")),
			Self::Fn(args, ret) => {
				write!(f, "{}(", "fn".purple())?;
				for (i, arg) in args.iter().enumerate() {
					write!(f, "{}", arg.borrow())?;
					if i != args.len() - 1 { write!(f, ", ")?; }
				}
				write!(f, "): {}", ret.borrow())?;
				Ok(())
			},
			k => write!(f, "{}", match k {
				Self::Generic(v, c) => format!("${v}[{}]", join_tostring(c.iter().map(|c| c.borrow()), ", ")),
				Self::U(u)  => format!("u{u}"),
				Self::I(i)  => format!("i{i}"),
				Self::B(b)  => format!("b{b}"),
				Self::F(f)  => format!("f{f}"),
				Self::Usize => String::from("usize"),
				Self::Isize => String::from("isize"),
				Self::None  => String::from("none"),
				Self::Never => String::from("never"),
				Self::Type  => String::from("type"),
				Self::Ref(t) => format!("&{}", t.borrow()),
				_ => unreachable!(),
			}.purple())
		}
	}
}
