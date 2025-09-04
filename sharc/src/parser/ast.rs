use std::fmt::{self, Display};
use crate::span::Sp;
use crate::bigint::IBig;
use bump::Box;

use colored::Colorize;

#[derive(Debug)]
pub enum Node<'src, 'b> {
	Let {
		ident: Sp<&'src str>,
		ty:    Option<Box<'b, Sp<Node<'src, 'b>>>>,
		gener: &'b [Sp<Node<'src, 'b>>],
		expr:  &'b Sp<Node<'src, 'b>>,
		stat:  bool, // static
	},

	Ident {
		lit:   Sp<&'src str>, 
		gener: &'b [Sp<Node<'src, 'b>>],
	},
	StrLit(&'b str),
	IntLit(IBig<'b>),
	ArrayLit(&'b [Sp<Node<'src, 'b>>], Option<u64>), // [u8], [20, 3, 8], [u16; 10]
	UnionLit(&'b [Sp<Node<'src, 'b>>]),  // ('foo | u8 | foo: u8)
	StructLit(&'b [Sp<Node<'src, 'b>>]), // (u8, 20, foo: 20, foo: u8)
	Primitive(Primitive<'src, 'b>),
	Quote(&'src str), // 'foo

	FuncCall {
		lhs:  &'b Sp<Node<'src, 'b>>,
		args: &'b [Sp<Node<'src, 'b>>],
	},
	Lambda {
		args:   &'b [LambdaArg<'src, 'b>],
		ret:    Option<&'b Sp<Node<'src, 'b>>>,
		body:   Option<&'b Sp<Node<'src, 'b>>>,
		ext:    Option<Sp<&'src str>>,
		export: Option<Sp<&'src str>>,
	},

	Block(&'b [Sp<Node<'src, 'b>>]),

	// UNARY EXPR
	Star(&'b Sp<Node<'src, 'b>>),
	Amp(&'b Sp<Node<'src, 'b>>),
	Neg(&'b Sp<Node<'src, 'b>>),
	Ret(Option<&'b Sp<Node<'src, 'b>>>),
	Move(&'b Sp<Node<'src, 'b>>),
	Mut(&'b Sp<Node<'src, 'b>>),

	// BINARY EXPR
	Sub(&'b Sp<Node<'src, 'b>>,   &'b Sp<Node<'src, 'b>>),
	Add(&'b Sp<Node<'src, 'b>>,   &'b Sp<Node<'src, 'b>>),
	Mul(&'b Sp<Node<'src, 'b>>,   &'b Sp<Node<'src, 'b>>),
	Div(&'b Sp<Node<'src, 'b>>,   &'b Sp<Node<'src, 'b>>),
	Mod(&'b Sp<Node<'src, 'b>>,   &'b Sp<Node<'src, 'b>>),
	// NOTE: needs to be box for now so we can take out the Node
	// if there is a better way to do this chicanery in the parser feel free to fix :)
	Store(Box<'b, Sp<Node<'src, 'b>>>, Box<'b, Sp<Node<'src, 'b>>>), // foo = 20
	Field(&'b Sp<Node<'src, 'b>>, &'b Sp<Node<'src, 'b>>), // foo: bar
	As(&'b Sp<Node<'src, 'b>>,    &'b Sp<Node<'src, 'b>>),
}

#[derive(Debug)]
pub struct LambdaArg<'src, 'b> {
	pub ident:   Sp<&'src str>,
	pub ty:      Option<Sp<Node<'src, 'b>>>,
	pub default: Option<Sp<Node<'src, 'b>>>,
}

#[derive(Debug)]
pub enum Primitive<'src, 'b> {
	U(u32), I(u32), B(u32), F(u32),
	Usize, Isize,
	Any, None, Never,
	Type,
	Fn(&'b [Sp<Node<'src, 'b>>], Option<&'b Sp<Node<'src, 'b>>>),
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
			Self::As(a, b) => write!(f, "({a} {} {b})", "as".yellow().dimmed()),
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

impl Display for Primitive<'_, '_> {
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
