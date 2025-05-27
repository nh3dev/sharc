use std::fmt;
use colored::Colorize;
use crate::bigint::IBig;

#[derive(Clone, Copy, Default, Debug, Eq, Hash, PartialEq)]
pub struct ValId(pub u64);
impl std::ops::Deref for ValId { // FIXME: prob not needed
	type Target = u64;
	fn deref(&self) -> &Self::Target 
	{ &self.0 }
}

pub enum Node {
	Func {
		id:     ValId,
		export: bool,
		args:   Vec<(ValId, Type)>, // !Void | !Never
		ret:    Type,
		body:   Vec<Node>, // Assign | Global | Ret | FuncCall
	},
	FuncDecl {
		id: ValId,
		ty: Type, // Fn
	},
	Assign {
		id:  ValId,
		ty:  Type, // !Void | !Never
		val: Box<Node>, // FuncCall | Var
	},
	Global {
		id:  ValId,
		ty:  Type, // !Void | !Never
		val: Box<Node>, // StrLit | Var::Imm | Var::Glob
	},
	Store { // TODO: maybe make work with other types than ptr
		to:   Var, // Local | Glob
		from: (Var, Type), // Local | Glob | Imm && !Void | !Never
	},
	Ret(Option<Var>, Type),
	FuncCall {
		id:   Var, // Var::Local | Var::Glob
		args: Vec<(Var, Type)>,
	},
	StrLit(String), // ?!
	Var(Var), // ?!
}

pub enum Var {
	Imm(Box<IBig>),
	Local(ValId),
	Glob(ValId),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
	U(u32), I(u32), B(u32), F(u32),
	Usize, Isize,
	Puint, Pint, Pbool, Pfloat,
	Any, None, Never,
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

				write!(f, "@{id} {}(", "fn".yellow().dimmed())?;

				for (i, (id, ty)) in args.iter().enumerate() {
					if i != 0 { write!(f, ", ")?; }
					write!(f, "%{id}: {ty}")?;
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
			Self::FuncDecl { id, ty } => write!(f, "{} @{id} {ty}", "decl".yellow().dimmed()),
			Self::Assign { id, ty, val } => write!(f, "%{id}: {ty} = {val}"),
			Self::Store { to, from: (from, ty) } => write!(f, "{to} <- {from}: {ty}"),
			Self::Global { id, ty, val } => write!(f, "@{id}: {ty} = {val}"),
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
			Self::Any     => String::from("any"),
			Self::None    => String::from("none"),
			Self::Never   => String::from("never"),
			Self::Ptr(ty) => format!("*{ty}"),
			Self::Arr(ty, None) => format!("[{ty}]"),
			Self::Arr(ty, Some(n)) => format!("[{ty} {n}]"),
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

impl fmt::Display for ValId {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result 
	{ write!(f, "{}", self.0) }
}
