use std::fmt;
use colored::Colorize;
use crate::bigint::IBig;

#[repr(transparent)]
#[derive(Clone, Copy, Default, Debug, Eq, Hash, PartialEq)]
pub struct ValId(pub u64);
impl std::ops::Deref for ValId { // FIXME: prob not needed
	type Target = u64;
	fn deref(&self) -> &Self::Target 
	{ &self.0 }
}

pub struct Mir<'b> {
	pub origin:  Option<&'b str>, // filename
	pub version: ((u16, u16), Option<&'static str>), // ((version, patch), git rev)
	pub nodes:   Vec<Node<'b>>,
	pub bump:    bump::Bump,
}

impl fmt::Debug for Mir<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.debug_struct("Mir")
			.field("origin", &self.origin)
			.field("version", &self.version)
			.field("nodes", &self.nodes)
			.finish()
	}
}

impl fmt::Display for Mir<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		self.nodes.iter().try_for_each(|n| writeln!(f, "{n}"))
	}
}

#[derive(Debug)]
pub enum Node<'b> {
	Assign {
		id:     ValId,
		ty:     &'b Type<'b>,
		expr:   Expr<'b>,
	},
	Store {
		to:   ValId,
		ty:   &'b Type<'b>,
		from: Var<'b>,
	},
	Dbg { 
		id:    ValId,
		ident: &'b str,
	},
	DefFn {
		id:   ValId,
		args: &'b [(ValId, &'b Type<'b>)],
		ret:  &'b Type<'b>,
		def_proc: &'b [(ValId, &'b [Node<'b>])],
		body: &'b [Node<'b>],
	},
	Ret(Var<'b>, &'b Type<'b>),
}

#[derive(Debug)]
pub enum Expr<'b> {
	DefCFn {
		sym:     &'b str,
		args:    &'b [&'b Type<'b>],
		ret:     &'b Type<'b>,
	},
	FuncCapture {
		fid:  ValId,
		args: &'b [ValId],
	},
	Call {
		id:   ValId,
		ty:   &'b Type<'b>,
		args: &'b [(Var<'b>, &'b Type<'b>)],
	},
	ImplCall {
		path:  &'b [&'b str],
		ident: &'b str,
		gener: &'b [&'b Type<'b>],
		args:  &'b [(Var<'b>, &'b Type<'b>)],
	},
	Instr {
		kind: InstrKind,
		args: &'b [(Var<'b>, &'b Type<'b>)],
	},
	StrLit(&'b str),
	Imm(IBig<'b>, &'b Type<'b>),
}

#[derive(Debug, Clone, Copy)]
pub enum InstrKind {
	Add, Sub, Mul, Div, Mod,
}

#[derive(Debug, Clone, Copy)]
pub enum Var<'b> {
	Imm(IBig<'b>),
	Local(ValId),
	None,
}

#[derive(Debug, PartialEq)]
pub enum Type<'b> {
	U(u32), I(u32), B(u32), F(u32),
	Usize, Isize,
	None, Never,
	Fn(&'b [&'b Type<'b>], &'b Type<'b>),
	Arr(&'b Type<'b>, Option<u64>),
	Ptr(&'b Type<'b>),
	Mut(&'b Type<'b>),
}

fn join_tostring(iter: impl IntoIterator<Item = impl ToString>, s: &str) -> String {
	iter.into_iter().map(|e| ToString::to_string(&e)).collect::<std::vec::Vec<_>>().join(s)
}

impl fmt::Display for Node<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Assign { id, ty, expr } => write!(f, "%{id}: {ty} = {expr};"),
			Self::Dbg { id, ident } => write!(f, "{} %{id} = {ident:?};", "dbg".yellow().dimmed()),
			Self::Store { to, ty, from } => write!(f, "{to}: {ty} <- {from};"),
			Self::Ret(v, ty) => write!(f, "{} {v}: {ty};", "return".yellow().dimmed()),
			Self::DefFn { id, args, ret, def_proc, body } => {
				write!(f, "{} fn%{id}({}) -> {ret} {{", 
					"def".yellow().dimmed(),
					join_tostring(args.iter().map(|(i, t)| format!("%{i}: {t}")), ", "),
				)?;

				for (proc_id, proc_body) in *def_proc {
					writeln!(f, "  {proc_id}: {{")?;
					for n in *proc_body {
						writeln!(f, "    {n}")?;
					}
					writeln!(f, "  }}")?;
				}

				if !def_proc.is_empty() { writeln!(f)?; }

				for n in *body {
					writeln!(f, "  {n}")?;
				}
				write!(f, "}}")
			},
		}
	}
}

impl fmt::Display for Expr<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::DefCFn { sym, args, ret } => 
				write!(f, "{} {sym:?} fn({}) -> {ret}", 
					"cdef".yellow().dimmed(),
					join_tostring(args.iter().map(|a| format!("{a}")), ", ")
				),
			Self::FuncCapture { fid, args } => write!(f, "fn#{fid}{{{}}}", 
				join_tostring(args.iter().map(|a| format!("%{a}")), ", ")),
			Self::StrLit(s) => write!(f, "{}", format!("{s:?}").green()),
			Self::Call { id, ty, args } => write!(f, "%{id}: {ty}({})", 
				join_tostring(args.iter().map(|(a, t)| format!("{a}: {t}")), ", ")),
			Self::ImplCall { path, ident, gener, args } => {
				if !path.is_empty() {
					path.iter().try_for_each(|p| write!(f, "{}::", p))?;
				}
				write!(f, "{}", ident.blue())?;
				if !gener.is_empty() {
					write!(f, "<{}>", join_tostring(*gener, ", "))?;
				}
				write!(f, "({})", join_tostring(args.iter().map(|(a, t)| format!("{a}: {t}")), ", "))
			},
			Self::Instr { kind, args } => write!(f, "{} {}", kind, 
				join_tostring(args.iter().map(|(a, t)| format!("{a}: {t}")), ", ")),
			Self::Imm(v, t) => write!(f, "{}: {t}", v.to_string().cyan()),
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
			Self::Ptr(ty) => format!("&{ty}"),
			Self::Mut(ty) => format!("mut {ty}"),
			Self::Arr(ty, Some(size)) => format!("[{ty}; {size}]"),
			Self::Arr(ty, None) => format!("[{ty}]"),
			Self::Fn(args, ret) => format!("fn({}) -> {ret}", join_tostring(*args, ", ")),
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

impl fmt::Display for InstrKind {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		format!("{:?}", self).to_lowercase().yellow().dimmed().fmt(f)
	}
}
