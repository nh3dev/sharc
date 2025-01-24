use std::fmt;
use colored::Colorize;
use crate::bigint::iBig;

#[derive(Clone, Copy, Default, Debug, Eq, Hash, PartialEq)]
pub struct ValId(pub u64);
impl std::ops::Deref for ValId {
	type Target = u64;
	fn deref(&self) -> &Self::Target 
	{ &self.0 }
}

pub enum Node {
	Func {
		id:      ValId,
		export:  bool, // TODO: perhaps remove, the id can be checked in the sym table
		args:    Vec<(ValId, Type)>,
		ret:     Type,
		body:    Vec<Node>,
	},
	FuncDecl {
		id:   ValId,
		args: Vec<Type>,
		ret:  Type,
	},
	Assign {
		id:  ValId,
		ty:  Type,
		val: Box<Node>, // FuncCall | Var
	},
	Global {
		id:  ValId,
		ty:  Type,
		val: Box<Node>, // StrLit | Var::Imm | Var::Glob
	},
	Ret(Option<Var>, Type),
	FuncCall {
		id: Var, // Var::Local | Var::Glob
		args: Vec<(Var, Type)>,
	},
	StrLit(String),
	Var(Var),
}

pub enum Var {
	Imm(iBig),
	Local(ValId),
	Glob(ValId),
}

#[derive(Clone, PartialEq)]
pub enum Type {
	U(u32), I(u32), B(u32), F(u32),
	Usize, Isize,
	Puint, Pint, Pbool, Pfloat,
	Void, Never,
	Ptr(Box<Type>),
	Arr(Box<Type>, Option<u64>),
	Mut(Box<Type>),
	Opt(Box<Type>),
	Fn(Vec<Type>, Box<Type>),
	// Ident(,
}

impl fmt::Display for Node {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Func { id, export, args, ret, body } => {
				if *export { write!(f, "{} ", "export".yellow().dimmed())?; }

				write!(f, "{} {}(", "fn".yellow().dimmed(), **id)?;

				for (i, (id, ty)) in args.iter().enumerate() {
					if i != 0 { write!(f, ", ")?; }
					write!(f, "%{}: {ty}", **id)?;
				}

				write!(f, ") {ret}")?;

				if body.is_empty() {
					writeln!(f, ";")?;
					return Ok(());
				}

				write!(f, " {{\n")?;
				for node in body {
					writeln!(f, "   {node};")?;
				}
				write!(f, "}}")
			},
			Self::FuncDecl { id, args, ret } => {
				write!(f, "{} {}(", "fn".yellow().dimmed(), **id)?;

				for (i, ty) in args.iter().enumerate() {
					if i != 0 { write!(f, ", ")?; }
					write!(f, "{ty}")?;
				}

				write!(f, ") {ret}")
			},
			Self::Assign { id, ty, val } => write!(f, "%{}: {ty} = {val}", **id),
			Self::Global { id, ty, val } => write!(f, "@{}: {ty} = {val}", **id),
			Self::Ret(Some(v), ty) => write!(f, "ret {v}: {ty}"),
			Self::Ret(None, ty) => write!(f, "ret {ty}"),
			Self::FuncCall { id, args } => {
				write!(f, "{id}(")?;
				for (i, (v, ty)) in args.iter().enumerate() {
					if i != 0 { write!(f, ", ")?; }
					write!(f, "{v}: {ty}")?;
				}
				write!(f, ")")
			},
			Self::StrLit(s) => write!(f, "{}", format!("{s:?}").green()),
			Self::Var(v)    => write!(f, "{}", v.to_string().cyan()),
		}
	}
}

impl fmt::Display for Type {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", match self {
			Self::U(n)    => format!("u{n}"),
			Self::I(n)    => format!("i{n}"),
			Self::B(n)    => format!("b{n}"),
			Self::F(n)    => format!("f{n}"),
			Self::Puint   => String::from("{uint}"),
			Self::Pint    => String::from("{int}"),
			Self::Pbool   => String::from("{bool}"),
			Self::Pfloat  => String::from("{float}"),
			Self::Usize   => String::from("usize"),
			Self::Isize   => String::from("isize"),
			Self::Void    => String::from("void"),
			Self::Never   => String::from("never"),
			Self::Ptr(ty) => format!("*{ty}"),
			Self::Arr(ty, None) => format!("[{ty}]"),
			Self::Arr(ty, Some(n)) => format!("[{ty}; {n}]"),
			Self::Mut(ty) => format!("mut {ty}"),
			Self::Opt(ty) => format!("opt {ty}"),
			Self::Fn(args, ret) => {
				write!(f, "{}(", "fn".yellow().dimmed())?;
				for (i, ty) in args.iter().enumerate() {
					if i != 0 { write!(f, ", ")?; }
					write!(f, "{ty}")?;
				}
				write!(f, ") {ret}")?;
				return Ok(());
			},
		}.purple())
	}
}

impl fmt::Display for Var {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Imm(v)    => write!(f, "{}", v.to_string().cyan()),
			Self::Local(id) => write!(f, "%{}", **id),
			Self::Glob(id)  => write!(f, "@{}", **id),
		}
	}
}
