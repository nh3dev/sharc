use std::fmt;
use colored::Colorize;
use crate::bigint::IBig;

#[repr(transparent)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[derive(Clone, Copy, Default, Debug, Eq, Hash, PartialEq)]
pub struct ValId(pub u64);
impl std::ops::Deref for ValId { // FIXME: prob not needed
	type Target = u64;
	fn deref(&self) -> &Self::Target 
	{ &self.0 }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[derive(Debug)]
pub enum Node<'b> {
	Assign {
		id:   ValId,
		ty:   &'b Type<'b>,
		expr: Expr<'b>, // TODO: maybe Box, saves heaps (hehe) of mem
	},
	Ret(Var<'b>, &'b Type<'b>),
}

#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[derive(Debug)]
pub enum Expr<'b> {
	Call {
		id:   Var<'b>,
		args: &'b [(Var<'b>, &'b Type<'b>)],
	},
	ImplCall {
		path:  &'b [&'b str],
		ident: &'b str,
		gener: &'b [&'b Type<'b>],
		args:  &'b [(Var<'b>, &'b Type<'b>)],
	},
	StrLit(&'b str),
}

#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[derive(Debug, Clone, Copy)]
pub enum Var<'b> {
	Imm(IBig<'b>),
	Local(ValId),
	None,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[derive(Debug, PartialEq)]
pub enum Type<'b> {
	U(u32), I(u32), B(u32), F(u32),
	Usize, Isize,
	None, Never,
	Ptr(&'b Type<'b>),
	Mut(&'b Type<'b>),
}

impl fmt::Display for Node<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Assign { id, ty, expr } => write!(f, "%{id}: {ty} = {expr};"),
			Self::Ret(v, ty) => write!(f, "{} {v}: {ty};", "return".yellow().dimmed()),
		}
	}
}

impl fmt::Display for Expr<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::StrLit(s) => write!(f, "{}", format!("{s:?}").green()),
			Self::Call { id, args } => {
				write!(f, "{id}(")?;
				for (i, (arg, ty)) in args.iter().enumerate() {
					if i != 0 { write!(f, ", ")?; }
					write!(f, "{arg}: {ty}")?;
				}
				write!(f, ")")
			},
			Self::ImplCall { path, ident, gener, args } => {
				if !path.is_empty() {
					for p in *path { write!(f, "{p}::")?; }
				}
				write!(f, "{}", ident.blue())?;
				if !gener.is_empty() {
					write!(f, "<")?;
					for (i, g) in gener.iter().enumerate() {
						if i != 0 { write!(f, ", ")?; }
						write!(f, "{g}")?;
					}
					write!(f, ">")?;
				}
				write!(f, "(")?;
				for (i, (arg, ty)) in args.iter().enumerate() {
					if i != 0 { write!(f, ", ")?; }
					write!(f, "{arg}: {ty}")?;
				}
				write!(f, ")")
			},
		}
	}
}

impl fmt::Display for Type<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", match self {
			Self::U(n)    => format!("u{n}"),
			Self::I(n)    => format!("i{n}"),
			Self::B(n)    => format!("b{n}"),
			Self::F(n)    => format!("f{n}"),
			Self::Usize   => String::from("usize"),
			Self::Isize   => String::from("isize"),
			Self::None    => String::from("none"),
			Self::Never   => String::from("never"),
			Self::Ptr(ty) => format!("*{ty}"),
			Self::Mut(ty) => format!("mut {ty}"),
		}.purple())
	}
}

impl fmt::Display for Var<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Imm(v)    => write!(f, "{}", v.to_string().cyan()),
			Self::Local(id) => write!(f, "%{}", **id),
			Self::None      => write!(f, "{}", "none".red().dimmed()),
		}
	}
}

impl fmt::Display for ValId {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result 
	{ write!(f, "{}", self.0) }
}
