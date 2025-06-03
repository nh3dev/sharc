use std::fmt::{self, Display};
use crate::span::Sp;
use crate::bigint::IBig;

use colored::Colorize;

type Box<T> = bump::Box<'static, T>;

pub struct AST {
	pub nodes: Box<[Sp<Node>]>,
	pub bump:  bump::Bump,
}

#[derive(Debug)]
pub enum Node {
	Let {
		ident: Sp<&'static str>,
		ty:    Option<Box<Sp<Node>>>,
		gener: Box<[Sp<Node>]>,
		expr:  Box<Sp<Node>>,
		stat:  bool, // static
	},

	Ident {
		lit:   Sp<&'static str>, 
		gener: Box<[Sp<Node>]>,
	},
	StrLit(String),
	IntLit(IBig),
	ArrayLit(Box<[Sp<Node>]>, Option<u64>), // [u8], [20, 3, 8], [u16; 10]
	UnionLit(Box<[Sp<Node>]>),  // ('foo | u8 | foo: u8)
	StructLit(Box<[Sp<Node>]>), // (u8, 20, foo: 20, foo: u8)
	Primitive(Primitive),
	Quote(&'static str), // 'foo

	FuncCall {
		lhs:  Box<Sp<Node>>,
		args: Box<[Sp<Node>]>,
	},
	Lambda {
		args:   Box<[LambdaArg]>,
		ret:    Option<Box<Sp<Node>>>,
		body:   Option<Box<Sp<Node>>>,
		ext:    Option<Sp<String>>, // string since these can contain escape codes
		export: Option<Sp<String>>,
	},

	Block(Box<[Sp<Node>]>),

	// UNARY EXPR
	Star(Box<Sp<Node>>),
	Amp(Box<Sp<Node>>),
	Neg(Box<Sp<Node>>),
	Ret(Option<Box<Sp<Node>>>),
	Move(Box<Sp<Node>>),
	Mut(Box<Sp<Node>>),

	// BINARY EXPR
	Sub(Box<Sp<Node>>,   Box<Sp<Node>>),
	Add(Box<Sp<Node>>,   Box<Sp<Node>>),
	Mul(Box<Sp<Node>>,   Box<Sp<Node>>),
	Div(Box<Sp<Node>>,   Box<Sp<Node>>),
	Mod(Box<Sp<Node>>,   Box<Sp<Node>>),
	Store(Box<Sp<Node>>, Box<Sp<Node>>), // foo = 20
	Field(Box<Sp<Node>>, Box<Sp<Node>>), // foo: bar
}

#[derive(Debug)]
pub struct LambdaArg {
	pub ident:   Sp<&'static str>,
	pub ty:      Option<Sp<Node>>,
	pub default: Option<Sp<Node>>,
}

#[derive(Debug)]
pub enum Primitive {
	U(u32), I(u32), B(u32), F(u32),
	Usize, Isize,
	Any, None, Never,
	Type,
	Fn(Box<[Sp<Node>]>, Option<Box<Sp<Node>>>),
}

impl Display for Node {
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
					Self::StrLit(e.elem.clone()).span(e.span).to_string().green())?; }
				if let Some(e) = export { write!(f, "{} {} ", 
					"export".yellow().dimmed(),
					Self::StrLit(e.elem.clone()).span(e.span).to_string().green())?; }

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
				write!(f, "{}(", format!("{lhs}").red())?;
				for (i, arg) in args.iter().enumerate() {
					write!(f, "{arg}")?;
					if i != args.len() - 1 { write!(f, ", ")?; }
				}
				write!(f, ")")
			},

			Self::Ret(expr) => {
				write!(f, "{}", "ret".yellow().dimmed())?;
				if let Some(expr) = expr { write!(f, " {expr}")?; }
				Ok(())
			},

			Self::Star(expr)  => write!(f, "*{expr}"),
			Self::Amp(expr)   => write!(f, "&{expr}"),
			Self::Neg(expr)   => write!(f, "-{expr}"),
			Self::Move(expr)  => write!(f, "{} {expr}", "move".yellow().dimmed()),
			Self::Mut(expr)   => write!(f, "{} {expr}", "mut".yellow().dimmed()),

			Self::Sub(a, b)   => write!(f, "({a} - {b})"),
			Self::Add(a, b)   => write!(f, "({a} + {b})"),
			Self::Mul(a, b)   => write!(f, "({a} * {b})"),
			Self::Div(a, b)   => write!(f, "({a} / {b})"),
			Self::Mod(a, b)   => write!(f, "({a} % {b})"),
			Self::Store(a, b) => write!(f, "{a} = {b}"),
			Self::Field(a, b) => write!(f, "{a}: {b}"),
		}
	}
}

impl Display for LambdaArg {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.ident.normal())?;
		if let Some(ty) = &self.ty { write!(f, ": {ty}")?; }
		if let Some(def) = &self.default { write!(f, " = {def}")?; }
		Ok(())
	}
}

impl Display for Primitive {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", match self {
			Self::U(i)   => format!("u{i}"),
			Self::I(i)   => format!("i{i}"),
			Self::B(i)   => format!("b{i}"),
			Self::F(i)   => format!("f{i}"),
			Self::Usize  => String::from("usize"),
			Self::Isize  => String::from("isize"),
			Self::Any    => String::from("any"),
			Self::None   => String::from("none"),
			Self::Never  => String::from("never"),
			Self::Type   => String::from("type"),
			Self::Fn(args, ret) => {
				write!(f, "{}(", "fn".yellow().dimmed())?;
				for (i, arg) in args.iter().enumerate() {
					write!(f, "{arg}")?;
					if i != args.len() - 1 { write!(f, ", ")?; }
				}
				write!(f, ")")?;
				if let Some(ret) = ret { write!(f, " {ret}")?; }
				return Ok(());
			},
		}.purple())
	}
}
