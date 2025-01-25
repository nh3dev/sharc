use std::fmt::{self, Formatter, Display};

pub type Name = String;

#[derive(Default)]
pub struct Module {
	pub name:  &'static str,

	pub data:  Vec<DataDef>,
	pub decls: Vec<FuncDecl>,
	pub funcs: Vec<Function>,
}


pub enum DataAttr {
	Internal,
	Global,
	Private,
	UnnamedAddr,
	Constant,
}

pub struct DataDef {
	pub name:  Name,
	pub attr:  Vec<DataAttr>,
	pub value: TypedVal,
	// TODO: align
}


pub enum FuncAttr {
	Nounwind,
	NoReturn,
	NoInline,
	AlwaysInline,
	Cold,
}

pub struct Function {
	pub name: Name,
	pub attr: Vec<FuncAttr>,
	pub args: Vec<(Type, Name)>,
	pub ret:  Type,
	pub body: Vec<Instr>,
}

pub struct FuncDecl {
	pub name: Name,
	pub attr: Vec<FuncAttr>,
	pub args: Vec<Type>,
	pub ret:  Type,
}


pub enum Instr {
	Assign(Val, Box<Instr>),
	Val(Val), // NOTE: This is an invalid instr by itself
	Ret(Option<Val>, Type),
	Call {
		func: TypedVal,
		args: Vec<TypedVal>,
	},
}

pub enum ValKind { Local, Global, Str, Const, }
pub struct Val(pub ValKind, pub Name);
pub struct TypedVal(pub Type, pub ValKind, pub Name);

pub enum Type {
	Int(u32),
	F16, F32, F64, F128,
	Ptr, Void,
	Array(usize, Box<Type>),
	// TODO: Vector, Struct, Function
}


impl Display for DataAttr {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			Self::Internal    => write!(f, "internal "),
			Self::Private     => write!(f, "private "),
			Self::UnnamedAddr => write!(f, "unnamed_addr "),
			Self::Constant    => write!(f, "constant "),
			Self::Global      => write!(f, "global "),
		}
	}
}

impl Display for FuncAttr {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			Self::Nounwind => write!(f, "nounwind "),
			Self::NoReturn => write!(f, "noreturn "),
			Self::NoInline => write!(f, "noinline "),
			Self::AlwaysInline => write!(f, "alwaysinline "),
			Self::Cold     => write!(f, "cold "),
		}
	}
}

impl Display for Module {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		self.data .iter().try_for_each(|d| writeln!(f, "{d}"))?;
		self.decls.iter().try_for_each(|c| writeln!(f, "{c}"))?;
		self.funcs.iter().try_for_each(|c| writeln!(f, "{c}"))?;
		writeln!(f, "!llvm.ident = !{{!0}}")?;
		writeln!(f, "!0 = !{{!\"sharc {}\"}}", env!("CARGO_PKG_VERSION"))
	}
}

impl Display for DataDef {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "@{} = ", self.name)?;
		self.attr.iter().try_for_each(|a| write!(f, "{a}"))?;
		write!(f, "{}", self.value)
	}
}

impl Display for Function {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "define {} @{}(", self.ret, self.name)?;

		for (i, arg) in self.args.iter().enumerate() {
			write!(f, "{} %{}", arg.0, arg.1)?;
			if i != self.args.len() - 1 { write!(f, ", ")?; }
		}

		write!(f, ")")?;
		self.attr.iter().try_for_each(|a| write!(f, " {a}"))?;
		writeln!(f, "{{")?;

		self.body.iter().try_for_each(|i| writeln!(f, "\t{i}"))?;
		writeln!(f, "}}")
	}
}

impl Display for FuncDecl {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "declare {} @{}(", self.ret, self.name)?;

		for (i, arg) in self.args.iter().enumerate() {
			write!(f, "{arg}")?;
			if i != self.args.len() - 1 { write!(f, ", ")?; }
		}

		write!(f, ")")?;
		self.attr.iter().try_for_each(|a| write!(f, " {a}"))?;
		write!(f, "\n")
	}
}

impl Display for Instr {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			Self::Assign(v, i) => write!(f, "{v} = {i}"),
			Self::Val(i) => write!(f, "{i}"),
			Self::Ret(v, t) => write!(f, "ret {t} {}", v.as_ref()
				.map_or(String::new(), ToString::to_string)),
			Self::Call { func, args } => {
				write!(f, "call {func}(")?;
				for (i, arg) in args.iter().enumerate() {
					write!(f, "{arg}")?;
					if i != args.len() - 1 { write!(f, ", ")?; }
				}
				write!(f, ")")
			},
		}
	}
}

impl Display for Val {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "{}{}", self.0, self.1)
	}
}

impl Display for TypedVal {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self.1 {
			ValKind::Local | ValKind::Global | ValKind::Const => write!(f, "{} {}{}", self.0, self.1, self.2),
			ValKind::Str    => {
				write!(f, "{} c\"", self.0)?;
				self.2.chars().try_for_each(|c| match c {
					'\x00'..='\x1f' => write!(f, "\\{:02x}", c as u8),
					_ => write!(f, "{c}"),
				})?;
				write!(f, "\"")
			},
		}
	}
}

impl Display for ValKind {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			Self::Local   => write!(f, "%"),
			Self::Global  => write!(f, "@"),
			Self::Str     => unreachable!(),
			Self::Const   => Ok(()),
		}
	}
}

impl Display for Type {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			Self::Int(w)      => write!(f, "i{w}"),
			Self::F16         => write!(f, "half"),
			Self::F32         => write!(f, "float"),
			Self::F64         => write!(f, "double"),
			Self::F128        => write!(f, "fp128"),
			Self::Ptr         => write!(f, "ptr"),
			Self::Void        => write!(f, "void"),
			Self::Array(n, t) => write!(f, "[{n} x {t}]"),
		}
	}
}
