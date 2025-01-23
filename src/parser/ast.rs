use std::fmt::{self, Display};
use crate::span::{Span, Sp, Spannable};

use colored::Colorize;

pub enum Node<'src> {
	Func {
		name:   Sp<&'src str>,
		attrs:  Vec<Sp<Attrs>>,
		args:   Vec<(Sp<&'src str>, Sp<Type<'src>>)>,
		ret:    Option<Sp<Type<'src>>>,
		body:   Vec<Sp<Node<'src>>>
	},
	Assign {
		name: Sp<&'src str>,
		ty: Sp<Type<'src>>,
		value: Box<Sp<Node<'src>>>
	},
	Ret(Option<Box<Sp<Node<'src>>>>),
	FuncCall {
		name: Sp<&'src str>,
		args: Vec<Sp<Node<'src>>>,
	},
	StrLit(String),
	UIntLit(u64),
}

pub enum Attrs {
	Export,
	Extern,
	Pub,
}

#[derive(Clone)]
pub enum Type<'src> {
	U(u32), I(u32), B(u32), F(u32),
	Usize, Isize,
	Void, Never,
	Opt(Box<Sp<Type<'src>>>),
	Ptr(Box<Sp<Type<'src>>>),
	Arr(Box<Sp<Type<'src>>>, Option<u64>),
	Mut(Box<Sp<Type<'src>>>),
	Fn(Vec<Sp<Type<'src>>>, Option<Box<Sp<Type<'src>>>>),
	Ident(&'src str),
}

// TODO: better display for this shit prob
impl Display for Node<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Func { name, attrs, args, ret, body } => {
				attrs.iter().try_for_each(|a| write!(f, "{a} "))?;
				write!(f, "{} {}(", "fn".yellow().dimmed(), name.red())?;
				for (i, (name, typ)) in args.iter().enumerate() {
					write!(f, "{name}: {typ}")?;
					if i != args.len() - 1 { write!(f, ", ")?; }
				}
				write!(f, ")")?;

				if let Some(ret) = ret { write!(f, " {ret}")?; }

				if body.is_empty() {
					write!(f, ";")?;
					return Ok(());
				}

				writeln!(f, " {{")?;
				body.iter().try_for_each(|s| writeln!(f, "   {s}"));
				write!(f, "}}")
			},
			Self::Assign { name, value, ty } => 
				write!(f, "{} {name}: {} = {value};",
					"let".yellow().dimmed(),
					ty.to_string().blue()),
			Self::Ret(expr) => match expr {
				Some(expr) => write!(f, "ret {expr};"),
				None => write!(f, "ret;"),
			},
			Self::FuncCall { name, args } => {
				write!(f, "{}(", format!("${name}").red())?;
				for (i, arg) in args.iter().enumerate() {
					write!(f, "{arg}")?;
					if i != args.len() - 1 { write!(f, ", ")?; }
				}
				write!(f, ")")
			},
			Self::StrLit(s)  => write!(f, "{}", format!("{s:?}").green()),
			Self::UIntLit(i) => write!(f, "{}", i.to_string().cyan()),
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
			Self::Usize  => String::from("usize"),
			Self::Isize  => String::from("isize"),
			Self::Void   => String::from("void"),
			Self::Never  => String::from("never"),
			Self::Isize  => String::from("isize"),
			Self::Usize  => String::from("usize"),
			Self::Opt(i) => format!("opt {i}"),
			Self::Ptr(i) => format!("*{i}"),
			Self::Arr(i, Some(s)) => format!("[{i}:{s}]"),
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

impl Display for Attrs {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", match self {
			Self::Export => "export",
			Self::Extern => "extern",
			Self::Pub    => "pub",
		}.yellow().dimmed())
	}
}
