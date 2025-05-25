use std::fmt::{self, Display};
use crate::span::Sp;
use crate::bigint::IBig;

use colored::Colorize;

#[derive(Debug)]
pub enum Node<'src> {
	Let {
		name:  Sp<&'src str>,
		ty:    Option<Sp<Type<'src>>>,
		expr:  Box<Sp<Node<'src>>>,
		stat:  bool,
	},

	Ident(&'src str),
	StrLit(String),
	IntLit(IBig),
	ArrayLit(Vec<Sp<Node<'src>>>),

	FuncCall {
		lhs:  Box<Sp<Node<'src>>>,
		args: Vec<Sp<Node<'src>>>,
	},
	Lambda {
		args:   Vec<(Sp<&'src str>, Option<Sp<Type<'src>>>)>,
		ret:    Option<Sp<Type<'src>>>,
		body:   Vec<Sp<Node<'src>>>,
		ext:    Option<String>,
		export: Option<String>,
	},

	// UNARY EXPR
	Neg(Box<Sp<Node<'src>>>),
	Ret(Option<Box<Sp<Node<'src>>>>),
	ImplRet(Box<Sp<Node<'src>>>),

	// BINARY EXPR
	Sub(Box<Sp<Node<'src>>>,   Box<Sp<Node<'src>>>),
	Add(Box<Sp<Node<'src>>>,   Box<Sp<Node<'src>>>),
	Mul(Box<Sp<Node<'src>>>,   Box<Sp<Node<'src>>>),
	Div(Box<Sp<Node<'src>>>,   Box<Sp<Node<'src>>>),
	Mod(Box<Sp<Node<'src>>>,   Box<Sp<Node<'src>>>),
	Store(Box<Sp<Node<'src>>>, Box<Sp<Node<'src>>>),
}

#[derive(Debug, Clone)]
pub enum Type<'src> {
	U(u32), I(u32), B(u32), F(u32),
	Usize, Isize,
	Any, None, Never,
	Opt(Box<Sp<Type<'src>>>),
	Ptr(Box<Sp<Type<'src>>>),
	Arr(Box<Sp<Type<'src>>>, Option<u64>),
	Mut(Box<Sp<Type<'src>>>),
	Fn(Vec<Sp<Type<'src>>>, Option<Box<Sp<Type<'src>>>>),
	Ident(&'src str),
}

impl Display for Node<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Let { name, expr, ty, stat } =>
				write!(f, "{} {name}{} = {expr};",
					if *stat { "static" } else { "let" }.yellow().dimmed(),
					if let Some(ty) = ty { format!(": {ty}").blue() } else { String::new().normal() } ),
			Self::Lambda { args, ret, body, ext, export } => {
				if let Some(e) = ext { write!(f, "{} {} ", 
					"extern".yellow().dimmed(),
					format!("{e:?}").green())?; }
				if let Some(e) = export { write!(f, "{} {} ", 
					"export".yellow().dimmed(),
					format!("{e:?}").green())?; }

				write!(f, "|")?;
				for (i, (name, ty)) in args.iter().enumerate() {
					write!(f, "{name}")?;
					if let Some(ty) = ty { write!(f, ": {ty}")?; }
					if i != args.len() - 1 { write!(f, ", ")?; }
				}
				write!(f, "|")?;
				if let Some(ret) = ret { write!(f, " {ret}")?; }

				if body.is_empty() { return Ok(()); }

				if body.len() == 1 {
					write!(f, ": {};", body[0])?;
					return Ok(());
				}

				writeln!(f, " {{")?;
				body.iter().try_for_each(|s| writeln!(f, "   {s}"))?;
				write!(f, "}};")
			},
			Self::Ret(expr) => match expr {
				Some(expr) => write!(f, "{} {expr}", "ret".yellow().dimmed()),
				None => write!(f, "{}", "ret".yellow().dimmed()),
			},
			Self::ImplRet(expr) => write!(f, "{} {expr}", "iret".yellow().dimmed()),
			Self::FuncCall { lhs, args } => {
				write!(f, "{}(", format!("{lhs}").red())?;
				for (i, arg) in args.iter().enumerate() {
					write!(f, "{arg}")?;
					if i != args.len() - 1 { write!(f, ", ")?; }
				}
				write!(f, ")")
			},
			Self::StrLit(s)  => write!(f, "{}", format!("{s:?}").green()),
			Self::IntLit(i) => write!(f, "{}", i.to_string().cyan()),
			Self::Ident(name) => write!(f, "{name}"),
			Self::ArrayLit(arr) => {
				write!(f, "[")?;
				for (i, item) in arr.iter().enumerate() {
					write!(f, "{item}")?;
					if i != arr.len() - 1 { write!(f, ", ")?; }
				}
				write!(f, "]")
			},

			Self::Neg(expr) => write!(f, "-{expr}"),

			Self::Sub(a, b) => write!(f, "({a} - {b})"),
			Self::Add(a, b) => write!(f, "({a} + {b})"),
			Self::Mul(a, b) => write!(f, "({a} * {b})"),
			Self::Div(a, b) => write!(f, "({a} / {b})"),
			Self::Mod(a, b) => write!(f, "({a} % {b})"),
			Self::Store(a, b) => write!(f, "{a} = {b}"),
		}
	}
}

impl Display for Type<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", match self {
			Self::U(i)   => format!("u{i}"),
			Self::I(i)   => format!("i{i}"),
			Self::B(i)   => format!("b{i}"),
			Self::F(i)   => format!("f{i}"),
			Self::Any    => String::from("any"),
			Self::None   => String::from("none"),
			Self::Never  => String::from("never"),
			Self::Isize  => String::from("isize"),
			Self::Usize  => String::from("usize"),
			Self::Opt(i) => format!("opt {i}"),
			Self::Ptr(i) => format!("*{i}"),
			Self::Arr(i, Some(s)) => format!("[{i} {s}]"),
			Self::Arr(i, None)    => format!("[{i}]"),
			Self::Mut(i) => format!("mut {i}"),
			Self::Fn(args, ret) => {
				write!(f, "{}(", "fn".yellow().dimmed())?;
				for (i, arg) in args.iter().enumerate() {
					write!(f, "{arg}")?;
					if i != args.len() - 1 {
						write!(f, ", ")?;
					}
				}
				write!(f, ")")?;
				if let Some(ret) = ret { write!(f, " {ret}")?; }
				return Ok(());
			},
			Self::Ident(name) => String::from(*name),
		}.purple())
	}
}
