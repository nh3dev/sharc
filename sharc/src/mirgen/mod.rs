use std::cell::RefCell;

use crate::report::{Result, ReportKind, Reportable};
use crate::span::Sp;
use crate::typeinf::hir;

pub mod mir;
pub mod bytecode;
use mir::{Node, ValId, Var, Type, Expr};

use colored::Colorize;

use bump::Bump;

pub struct Analyzer<'r, 'src, 'bo, 'b> {
	reporter: &'r mut crate::Reporter,
	bump:     Bump,
	bump_lt:  std::marker::PhantomData<&'b Bump>,
	bumpo_lt: std::marker::PhantomData<&'bo Bump>,

	scope:    Vec<Scope<'src, 'b>>,
	// impls:    
}

macro_rules! its_fine {
	($bump:expr) => { unsafe { #[allow(clippy::deref_addrof)] &*&raw const $bump } };
}

static TY_NONE: Type = Type::None;


#[derive(Default, Debug)]
struct Scope<'src, 'b> {
	idacc:  ValId,
	locals: Vec<(&'src str, ValId, &'b Type<'b>)>, // TODO: might not need type here
}

impl Scope<'_, '_> {
	fn id(&mut self) -> ValId {
		self.idacc.0 += 1;
		self.idacc
	}
}

impl<'r, 'src, 'bo, 'b> Analyzer<'r, 'src, 'bo, 'b> {
	fn scope(&mut self) -> &mut Scope<'src, 'b> {
		self.scope.last_mut().unwrap()
	}

	fn find_symbol(&self, ident: &str) -> Option<(ValId, &'b Type<'b>)> {
		for scope in self.scope.iter().rev() {
			if let Some((_, id, ty)) = scope.locals.iter().rev().find(|(i, _, _)| *i == ident) {
				return Some((*id, *ty));
			}
		}
		None
	}

	pub fn process(
		origin: Option<&str>,
		(hir, hir_bump): (hir::Ty<'src, 'bo, hir::Node<'src, 'bo>>, Bump),
		reporter: &'r mut crate::Reporter
	) -> mir::Mir<'b> {
		let mut analyzer = Self { 
			reporter, 
			bump: Bump::new(), 
			bump_lt:  std::marker::PhantomData,
			bumpo_lt: std::marker::PhantomData,
			scope: vec![Scope::default()], 
			// symbols: HashMap::new(),
		};

		let hir::Node::Block(hir) = hir.elem else {
			unreachable!("top level must be a block");
		};

		let hir_len = hir.len();
		let mir = hir.iter().enumerate().flat_map(
			|(i, node)| analyzer.process_node(i == hir_len - 1, node).map_or_else(
				|l| { analyzer.reporter.nom(*l); Vec::new().into_iter() }, 
				|(_, _, n)| n.into_iter()))
			.collect::<Vec<_>>();

		std::mem::drop(hir_bump);
		mir::Mir {
			nodes:   mir,
			origin:  origin.map(|s| its_fine!(analyzer.bump).alloc_str(s)),
			version: (crate::VERSION, crate::GITREV),
			bump:    analyzer.bump,
		}
	}

	pub fn process_node(&mut self, ret: bool, node: &hir::Ty<'src, 'bo, Sp<hir::Node<'src, 'bo>>>) 
		-> Result<(Var<'b>, &'b Type<'b>, Vec<Node<'b>>)> {
		let mut nodes = Vec::new();

		let ty = self.process_type(node.ty);

		let var = match &node.elem.elem {
			hir::Node::None => Var::None,
			hir::Node::ImplCall { path, ident, gener, vals } => {
				let gener = self.bump.alloc_from_iter(gener.iter().map(|g| self.process_type(g)));

				let args = vals.iter().map(|val| {
					let (var, ty, n_nodes) = self.process_node(false, val)?;
					nodes.extend(n_nodes);
					Ok((var, ty))
				}).collect::<Result<Vec<_>>>()?;

				let id = self.scope().id();

				nodes.push(Node::Assign { id, ty, expr: Expr::ImplCall { 
					path:  self.bump.alloc_from_iter(path.iter().map(|s| self.bump.alloc_str(s))),
					ident: self.bump.alloc_str(ident),
					args:  self.bump.alloc_from_vec(args),
					gener,
				}});

				Var::Local(id)
			},
			hir::Node::IntLit(v) => Var::Imm(v.copy(&self.bump)),

			hir::Node::Add(lhs, rhs) | hir::Node::Sub(lhs, rhs) | hir::Node::Mul(lhs, rhs) |
				hir::Node::Div(lhs, rhs) | hir::Node::Mod(lhs, rhs) => {
				let (lvar, lty, lnodes) = self.process_node(false, lhs)?;
				let (rvar, rty, rnodes) = self.process_node(false, rhs)?;

				nodes.extend(lnodes);
				nodes.extend(rnodes);

				let id = self.scope().id();

				let kind = match node.elem.elem {
					hir::Node::Add(_, _) => mir::InstrKind::Add,
					hir::Node::Sub(_, _) => mir::InstrKind::Sub,
					hir::Node::Mul(_, _) => mir::InstrKind::Mul,
					hir::Node::Div(_, _) => mir::InstrKind::Div,
					hir::Node::Mod(_, _) => mir::InstrKind::Mod,
					_ => unreachable!(),
				};

				nodes.push(Node::Assign { id, ty: lty, expr: Expr::Instr { 
					kind, args: self.bump.alloc_array([(lvar, lty), (rvar, rty)]),
				}});

				Var::Local(id)
			},

			hir::Node::Let { ident, gener, expr, stat } => {
				if !gener.is_empty() {
					return ReportKind::Unimplemented
						.title("generics on let exprs are not supported yet")
						.note("nick do this holy shit aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
						.span(node.elem.span)
						.as_err();
				}

				let ty = expr.ty;

				// TODO: propagate generics to node parse step
				let (var, _, mut n_nodes) = self.process_node(false, expr)?;

				match var {
					Var::Imm(i) => {
						let ident = self.bump.alloc_str(ident.elem);
						let id = self.scope().id();
						let ty = self.process_type(ty);

						self.scope().locals.push((ident, id, ty));
						nodes.reserve(n_nodes.len() + 2);
						nodes.push(Node::Dbg { id, ident });
						nodes.push(Node::Assign { id, ty, expr: Expr::Imm(i, ty) });
						nodes.extend(n_nodes);

						Var::Local(id)
					},
					Var::Local(id) => {
						let ident = self.bump.alloc_str(ident.elem);
						let ty = self.process_type(ty);

						if let Some(Node::Assign { ty: t, .. }) = n_nodes.last_mut() {
							*t = ty;
						}

						self.scope().locals.push((ident, id, ty));
						nodes.reserve(n_nodes.len() + 1);
						nodes.push(Node::Dbg { id, ident });
						nodes.extend(n_nodes);

						Var::Local(id)
					},
					Var::None => unreachable!("let expr cannot be none"),
				}
			},
			hir::Node::Store(lhs, rhs) => {
				let (lvar, lty, lnodes) = self.process_node(false, lhs)?;
				let (rvar, _, rnodes) = self.process_node(false, rhs)?;

				let id = match lvar {
					Var::None => return ReportKind::SyntaxError
						.title("cannot store to an expr that does not yield a value")
						.span(lhs.elem.span)
						.as_err(),
					Var::Imm(_) => return ReportKind::SyntaxError
						.title("cannot store to an immediate")
						.span(lhs.elem.span)
						.as_err(),
					Var::Local(id) => id,
				};

				nodes.extend(lnodes);
				nodes.extend(rnodes);

				nodes.push(Node::Store { 
					to:   id, 
					ty:   lty, 
					from: rvar, 
				});

				Var::None
			},
			hir::Node::Ident(sym) => {
				let (valid, _) = self.find_symbol(sym).ok_or_else(|| 
					ReportKind::UndefinedIdentifier
						.title(format!("unknown symbol: {}", sym.red()))
						.help("learn to spell :)")
						.span(node.elem.span))?;

				Var::Local(valid)
			},
			hir::Node::Lambda { args, ret, ext: Some(sym), .. } => {
				let has_default = args.iter().any(|a| a.default.is_some());
				if has_default { todo!() }

				let args = self.bump.alloc_from_iter(args.iter().map(|a| self.process_type(a.ty.unwrap())));
				let ret = self.process_type(ret);

				let id = self.scope().id();

				nodes.push(Node::Assign { 
					id, ty, expr: Expr::DefCFn { 
					args, ret, 
					sym: self.bump.alloc_str(sym) 
				} });

				Var::Local(id)
			},
			hir::Node::FuncCall { lhs, args } => {
				let (var, ty, n_nodes) = self.process_node(false, lhs)?;

				let cid = match var {
					// TODO: Imm is technically legal, but this can never possibly get reached
					Var::None | Var::Imm(_) => unreachable!(),
					Var::Local(id) => id,
				};

				let id = self.scope().id();

				let args = its_fine!(self.bump).try_alloc_from_iter(args.iter().map(|arg| {
					let (var, ty, n_nodes) = self.process_node(false, arg)?;
					nodes.extend(n_nodes);
					Result::Ok((var, ty))
				}))?;

				let Type::Fn(_, ret) = &ty else { unreachable!() };

				nodes.push(Node::Assign { id, ty: ret, expr: Expr::Call { id: cid, ty, args } });

				Var::Local(id)
			},
			hir::Node::StrLit(s) => {
				let id = self.scope().id();

				nodes.push(Node::Assign { 
					id, ty,
					expr: Expr::StrLit(self.bump.alloc_str(s)) 
				});

				Var::Local(id)
			},
			_ => todo!("{}", node),
		};

		if ret {
			nodes.push(Node::Ret(var, self.process_type(node.ty)));
		}

		Ok((var, ty, nodes))
	}

	pub fn process_type(&self, ty: &'bo RefCell<hir::Type<'src, 'bo>>) -> &'b Type<'b> {
		static NONE : Type = Type::None;
		static NEVER: Type = Type::Never;
		static USIZE: Type = Type::Usize;
		static ISIZE: Type = Type::Isize;

		let ty = ty.borrow();

		match ty.kind {
			hir::TypeKind::U(n)  => self.bump.alloc(Type::U(n)),
			hir::TypeKind::I(n)  => self.bump.alloc(Type::I(n)),
			hir::TypeKind::None  => &NONE,
			hir::TypeKind::Never => &NEVER,
			hir::TypeKind::Usize => &USIZE,
			hir::TypeKind::Isize => &ISIZE,
			hir::TypeKind::Fn(a, r) => {
				self.bump.alloc(Type::Fn(
					self.bump.alloc_from_iter(a.iter().map(|a| self.process_type(a))),
					self.process_type(r)))
			},
			hir::TypeKind::Ref(t) => self.bump.alloc(Type::Ptr(self.process_type(t))),
			hir::TypeKind::Array(t, s) => self.bump.alloc(Type::Arr(self.process_type(t), s)),
			_ => todo!("{:?}", ty),
		}
	}
}
