// FIXME: at some point consider changing RefCell to Cell

use std::cell::RefCell;

use crate::parser::ast::{self, Node, LambdaArg};
use crate::report::{Report, ReportKind, Result, Reportable};
use crate::span::{Span, Sp, Spannable};

use bump::{Bump, CollectWith};

pub mod hir;
use hir::{TypeInf as _, Ty, Type, TypeKind, Node as TyNode, LambdaArg as TyLambdaArg};

mod statics;

pub struct TypeInf<'src, 'bo, 'b, 'r> {
	reporter: &'r mut crate::Reporter,
	bump:     Bump,
	bump_lt:  std::marker::PhantomData<&'b Bump>,
	bumpo_lt: std::marker::PhantomData<&'bo Bump>,

	types:    statics::StaticTypes<'src, 'b>,

	scope_stack: Vec<Scope<'src, 'b>>,
	gener_id:    usize,
}

struct Scope<'src, 'b> {
	kind: ScopeKind,
	sym:  Vec<(&'src str, &'b RefCell<Type<'src, 'b>>)>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ScopeKind {
	Root, Func, Loop, Block, Let
}

impl<'src, 'b> Scope<'src, 'b> {
	fn new(kind: ScopeKind) -> Self {
		Self { kind, sym: Vec::new() }
	}

	fn push_sym(&mut self, ident: &'src str, ty: &'b RefCell<Type<'src, 'b>>) {
		self.sym.push((ident, ty));
	}

	fn find_sym(&self, ident: &str) -> Option<&'b RefCell<Type<'src, 'b>>> {
		self.sym.iter().rev().find_map(|(i, t)| (*i == ident).then_some(*t))
	}
}

impl<'src, 'bo, 'b, 'r> TypeInf<'src, 'bo, 'b, 'r> {
	// SAFETY: we only ever get immutable refs to the bump. 
	// SAFETY: this is technically forbidden but it's *mostly* fine..
	fn bump(&self) -> &'static Bump {
		unsafe { std::mem::transmute(&self.bump) }
	}

	fn newid(&mut self) -> usize {
		let id = self.gener_id;
		self.gener_id += 1;
		id
	}

	#[inline]
	fn log(&mut self, report: Report<ReportKind>) {
		self.reporter.nom(report);
	}

	fn push_scope(&mut self, kind: ScopeKind) {
		self.scope_stack.push(Scope::new(kind));
	}

	fn pop_scope(&mut self) -> Scope<'src, 'b> {
		self.scope_stack.pop().expect("if this panics you're in big trouble")
	}
	
	fn scope(&mut self) -> &mut Scope<'src, 'b> {
		self.scope_stack.last_mut().unwrap()
	}

	pub fn infer(
		(ast, ast_bump): (Node<'src, 'bo>, Bump),
		reporter: &'r mut crate::Reporter
	) -> (Ty<'src, 'b, TyNode<'src, 'b>>, Bump) {
		let bump = Bump::new();
		let mut inf = Self { 
			reporter, 
			bump_lt:  std::marker::PhantomData,
			bumpo_lt: std::marker::PhantomData,
			types:    statics::StaticTypes::new(&bump),
			scope_stack: vec![Scope::new(ScopeKind::Root)],
			gener_id: 0,
			bump,
		};

		let Some(ast) = inf.infer_node(&ast.span(Span::new(0))) else {
			return (TyNode::None.typed(inf.types.none), ast_bump);
		};

		// println!("\n{ast}");

		inf.resolve_node_constraints(&ast);

		let Ty { elem, ty } = ast;

		std::mem::drop(ast_bump);
		(elem.elem.typed(ty), inf.bump)
	}

	// type const eval
	fn eval_node_as_type(&mut self, node: &Ty<'src, 'b, Sp<TyNode<'src, 'b>>>)
		-> Option<&'b RefCell<Type<'src, 'b>>> {
		// FIXME: hacky mess
		match &node.elem.elem {
			TyNode::Primitive(ty) => Some(ty),
			TyNode::ArrayLit([t], s) => { 
				let ty = self.eval_node_as_type(t)?;
				Some(self.bump.alloc(RefCell::new(Type::new(TypeKind::Array(ty, *s)))))
			},
			TyNode::ArrayLit(t, s) => {
				self.log(ReportKind::TypeError
					.title(format!("array type expects 1 argument, found {}", t.len()))
					.span(node.elem.span));
				return None;
			},
			TyNode::ImplCall { path, ident, vals, .. }
				if *path == statics::CORE_PATH && ident.elem == "Ref" => {
				vals.get(0).and_then(|v| self.eval_node_as_type(v))
					.map(|t| self.bump.alloc(RefCell::new(Type::new(TypeKind::Ref(t)))))
			},
			_ => {
				self.log(ReportKind::TypeError
					.title(format!("expected `{}`, found `{}`", TypeKind::Type, node.ty.borrow().kind))
					.span(node.elem.span));
				None
			},
		}
	}

	// pass 1
	fn infer_node(&mut self, node: &Sp<Node<'src, 'bo>>) 
		-> Option<Ty<'src, 'b, Sp<TyNode<'src, 'b>>>> {
		Some(match &node.elem {
			Node::Primitive(p) => {
				let kind = match p {
					ast::Primitive::Usize   => TypeKind::Usize,
					ast::Primitive::Isize   => TypeKind::Isize,
					ast::Primitive::U(size) => TypeKind::U(*size),
					ast::Primitive::I(size) => TypeKind::I(*size),
					ast::Primitive::None    => TypeKind::None,
					ast::Primitive::Fn(args, ret) => {
						let bump = self.bump();
						let args = args.iter()
							.filter_map(|a| self.infer_node(a).and_then(|n| self.eval_node_as_type(&n)))
							.collect_with(bump.alloc_vec(args.len()));

						let ret = match ret {
							Some(r) => self.infer_node(r).and_then(|n| self.eval_node_as_type(&n))?,
							None    => self.types.none,
						};

						TypeKind::Fn(args.into_slice(), ret)
					},
					_ => todo!("{:?}", p),
				};

				let ty = self.bump.alloc(RefCell::new(Type::new(kind)));
				TyNode::Primitive(ty).span(node.span).typed(self.types.type_)
			},
			Node::Ident(sym) => {
				let Some(ty) = self.scope_stack.iter().rev().find_map(|s| s.find_sym(sym)) else {
					self.log(ReportKind::UndefinedIdentifier
						.title(format!("`{sym}`"))
						.span(node.span));
					return None;
				};

				TyNode::Ident(sym).span(node.span).typed(ty)
			},
			Node::Let { ident, gener, ty, expr, stat } => {
				// TODO: generics
				// let mut gener_ty = self.bump.alloc_vec(gener.len());
				// self.push_scope(ScopeKind::Let);
				// for g in gener {
				// }

				let expr = self.infer_node(expr)?;
				let ty_expr = match ty {
					Some(t) => Some(self.infer_node(t).and_then(|t| self.eval_node_as_type(&t))?),
					None    => None,
				};

				match &mut expr.ty.borrow_mut().kind {
					TypeKind::Generic(_, cons) if let Some(ty) = ty_expr 
						=> cons.push(ty.span(expr.elem.span)),
					t if let Some(ty) = ty_expr && *t != ty.borrow().kind => {
						self.log(ReportKind::TypeError
							.title(format!("expected `{}`, found `{}`", ty.borrow().kind, t))
							.span(expr.elem.span));
					},
					_ => {},
				}

				self.scope().push_sym(ident.elem, expr.ty);

				TyNode::Let { 
					ident: *ident,
					gener: &[], 
					expr: self.bump.alloc(expr),
					stat: false, // prob a better way to do statics language side.. StaticCell?
				}.span(node.span).typed(self.types.none)
			},
			Node::As(lhs, rhs) => { // TODO: trait resolve
				let lhs = self.infer_node(lhs)?;
				let rhs = self.infer_node(rhs)?;

				let ty = self.eval_node_as_type(&rhs)?;

				TyNode::ImplCall { 
					path:  statics::CORE_PATH,
					ident: "As".span(node.span),
					gener: self.bump.alloc_array([ty]),
					vals:  self.bump.alloc_array([lhs]),
				}.span(node.span).typed(ty)
			},
			Node::Amp(expr) => { // TODO: trait resolve
				let expr = self.infer_node(expr)?;

				let id = self.newid();
				let ty = vec![self.bump.alloc(RefCell::new(Type::new(TypeKind::Ref(expr.ty)))).span(expr.elem.span)];
				let ty = self.bump.alloc(RefCell::new(Type::new(TypeKind::Generic(id, ty))));

				TyNode::ImplCall { 
					path:  statics::CORE_PATH,
					ident: "Ref".span(node.span),
					gener: &[], 
					vals:  self.bump.alloc_array([expr]),
				}.span(node.span).typed(ty)
			},
			Node::None => {
				let ty = self.bump.alloc(RefCell::new(Type::new(TypeKind::None)));
				TyNode::None.span(node.span).typed(ty)
			},
			Node::Add(lhs, rhs) | Node::Sub(lhs, rhs) | Node::Mul(lhs, rhs) 
				| Node::Div(lhs, rhs) | Node::Mod(lhs, rhs) => {

				let lhs = self.infer_node(lhs)?;
				let rhs = self.infer_node(rhs)?;

				let id   = self.newid();
				let cons = vec![lhs.ty.span(lhs.elem.span), rhs.ty.span(rhs.elem.span)];

				let ty = self.bump.alloc(RefCell::new(Type::new(TypeKind::Generic(id, cons))));

				(match node.elem {
					Node::Add(_, _) => TyNode::Add,
					Node::Sub(_, _) => TyNode::Sub,
					Node::Mul(_, _) => TyNode::Mul,
					Node::Div(_, _) => TyNode::Div,
					Node::Mod(_, _) => TyNode::Mod,
					_ => unreachable!(),
				})(self.bump.alloc(lhs), self.bump.alloc(rhs))
					.span(node.span).typed(ty)
			},
			Node::Lambda { ext: Some(_), export: Some(_), .. } => {
				self.log(ReportKind::InvalidFunctionType
					.title("extern functions cannot be exported")
					.span(node.span));
				return None;
			},
			Node::Lambda { body: Some(_), ext: Some(_), .. } => {
				self.log(ReportKind::InvalidFunctionType
					.title("extern functions cannot have a body")
					.span(node.span));
				return None;
			},
			Node::Lambda { args, ret, ext: Some(ext), .. } => {
				let mut args_out = self.bump.alloc_vec(args.len());
				let mut args_ty  = self.bump.alloc_vec(args.len());

				for LambdaArg { ident, ty, default } in args.iter() {
					let ty = match ty {
						Some(t) => self.infer_node(t).and_then(|n| self.eval_node_as_type(&n))?,
						None    => {
							self.log(ReportKind::TypeAnnotationRequired
								.title("extern functions must have all arguments annotated")
								.span(ident.span));
							continue;
						},
					};

					args_ty.push(ty);

					args_out.push(TyLambdaArg {
						ty, ident: *ident,
						default: match default {
							Some(d) => Some(self.infer_node(d)?),
							None    => None,
						},
					});
				}

				let ret = match ret {
					Some(r) => self.infer_node(r).and_then(|n| self.eval_node_as_type(&n))?,
					None    => self.types.none,
				};

				let ty = self.bump.alloc(RefCell::new(Type::new(TypeKind::Fn(args_ty.into_slice(), ret))));

				TyNode::Lambda { 
					ret,
					args:   args_out.into_slice(), 
					body:   None, 
					export: None, 
					ext:    Some(self.bump.alloc_str(ext.elem).span(ext.span)), 
				}.span(node.span).typed(ty)
			},
			Node::StrLit(s) => {
				let ty = RefCell::new(Type::new(TypeKind::Array(self.types.u8, Some(s.len() as u64))));
				TyNode::StrLit(self.bump.alloc_str(s)).span(node.span).typed(self.bump.alloc(ty))
			},
			// TODO: export lambda without body
			Node::Lambda { args, ret, export, body: Some(body), .. } => {
				self.push_scope(ScopeKind::Func);

				let mut args_out = self.bump.alloc_vec(args.len());
				let mut args_ty  = self.bump.alloc_vec(args.len());

				for LambdaArg { ident, ty, default } in args.iter() {
					let ty = match ty {
						Some(t) => self.infer_node(t).and_then(|n| self.eval_node_as_type(&n))?,
						None    => {
							let id = self.newid();
							let ty = self.bump.alloc(RefCell::new(Type::new(TypeKind::Generic(id, Vec::new()))));
							ty
						},
					};

					args_ty.push(ty);

					args_out.push(TyLambdaArg {
						ty, ident: *ident,
						default: match default {
							Some(d) => Some(self.infer_node(d)?),
							None    => None,
						},
					});

					self.scope().push_sym(ident.elem, ty);
				}

				let body = self.infer_node(body)?;

				let ret = match ret {
					Some(r) => {
						let ret = self.infer_node(r).and_then(|n| self.eval_node_as_type(&n))?;
						
						if body.ty.borrow().kind != ret.borrow().kind && !Self::constraint_compatible(&body.ty, &ret) {
							self.log(ReportKind::TypeError
								.title(format!("expected `{}`, found `{}`", ret.borrow().kind, body.ty.borrow().kind))
								.span(body.elem.span));
						}

						ret
					},
					None    => body.ty,
				};

				let ty = self.bump.alloc(RefCell::new(Type::new(TypeKind::Fn(args_ty.into_slice(), ret))));

				TyNode::Lambda { 
					ret,
					args: args_out.into_slice(),
					body: Some(self.bump.alloc(body)),
					ext:  None,
					export: export.map(|e| self.bump.alloc_str(e.elem).span(e.span)),
				}.span(node.span).typed(ty)
			},
			Node::FuncCall { lhs, args } => {
				let lhs = self.infer_node(lhs)?;

				let bump = self.bump();
				let args_out = args.iter()
					.filter_map(|a| self.infer_node(a))
					.collect_with(bump.alloc_vec(args.len()));

				let ty = match &mut lhs.ty.borrow_mut().kind {
					TypeKind::Fn(args, ret) => {
						if args.len() != args_out.len() {
							self.log(ReportKind::InvalidArity
								.title(format!("expected {} arguments, found {}", args.len(), args_out.len()))
								.span(node.span));
							return None;
						}

						args.iter().zip(args_out.iter()).for_each(|(exp, act)| {
							if let TypeKind::Generic(_, cons) = &mut act.ty.borrow_mut().kind {
								cons.insert(0, (*exp).span(act.elem.span));
								return;
							}

							if exp.borrow().kind != act.ty.borrow().kind && !Self::constraint_compatible(exp, &act.ty) {
								self.log(ReportKind::TypeError
									.title(format!("expected `{}`, found `{}`", exp.borrow(), act.ty.borrow()))
									.span(act.elem.span));
							}
						});

						ret
					},
					TypeKind::Generic(_, cons) => {
						let id = self.newid();
						let ret = self.bump.alloc(RefCell::new(Type::new(TypeKind::Generic(id, Vec::new()))));

						let args = args_out.iter().map(|a| a.ty)
							.collect_with(self.bump.alloc_vec(args_out.len()));

						cons.push(self.bump.alloc(RefCell::new(Type::new(TypeKind::Fn(args.into_slice(), ret))))
							.span(lhs.elem.span));

						ret
					},
					_ => {
						self.log(ReportKind::TypeError
							.title(format!("expected fn, found `{}`", lhs.ty.borrow().kind))
							.span(lhs.elem.span));
						return None;
					},
				};

				TyNode::FuncCall {
					lhs:  self.bump.alloc(lhs),
					args: args_out.into_slice(),
				}.span(node.span).typed(ty)
			},
			Node::ArrayLit(exprs, len) => {
				let bump = self.bump();
				let exprs = exprs.iter()
					.filter_map(|e| self.infer_node(e))
					.collect_with(bump.alloc_vec(exprs.len()));

				let exprs_ty = exprs.iter().fold(
					Vec::with_capacity(exprs.len()), 
					|mut acc, e| { 
						if !acc.contains(&e.ty.span(e.elem.span)) {
							acc.push(e.ty.span(e.elem.span)) 
						}; acc 
					});
				
				let id = self.newid();
				let ty = self.bump.alloc(RefCell::new(Type::new(TypeKind::Array(
					self.bump.alloc(RefCell::new(Type::new(TypeKind::Generic(id, exprs_ty)))),
					*len))));

				TyNode::ArrayLit(exprs.into_slice(), *len).span(node.span).typed(ty)
			},
			Node::Block(exprs) => {
				let bump = self.bump();
				let exprs = exprs.iter()
					.filter_map(|e| self.infer_node(e))
					.collect_with(bump.alloc_vec(exprs.len()));

				let ty = match exprs.last() {
					Some(e) => e.ty,
					None    => self.bump.alloc(RefCell::new(Type::new(TypeKind::None))),
				};

				TyNode::Block(exprs.into_slice()).span(node.span).typed(ty)
			},
			Node::IntLit(i) => {
				let id   = self.newid();
				let cons = vec![self.bump.alloc(RefCell::new(Type::new(TypeKind::Usize))).span(node.span)];

				TyNode::IntLit(i.copy(&self.bump)).span(node.span)
					.typed(self.bump.alloc(RefCell::new(Type::new(TypeKind::Generic(id, cons)))))
			},
			_ => todo!("{:?}", node.elem),
		})
	}

	// pass 2
	fn resolve_node_constraints(&mut self, node: &Ty<'src, 'b, Sp<TyNode<'src, 'b>>>) {
		match &node.elem.elem {
			TyNode::Block(exprs) => {
				exprs.iter().for_each(|e| self.resolve_node_constraints(e))
			},
			TyNode::ImplCall { vals, .. } => {
				// TODO: trait impl resolution
				vals.iter().for_each(|e| self.resolve_node_constraints(e))
			},
			TyNode::Add(lhs, rhs) | TyNode::Sub(lhs, rhs) | TyNode::Mul(lhs, rhs) 
				| TyNode::Div(lhs, rhs) | TyNode::Mod(lhs, rhs) => {
				self.resolve_node_constraints(lhs);
				self.resolve_node_constraints(rhs);
			},
			TyNode::Let { expr, .. } => {
				self.resolve_node_constraints(expr);
			},
			TyNode::FuncCall { lhs, args } => { // TODO supply correct cons :)
				self.resolve_node_constraints(lhs);
				args.iter().for_each(|a| self.resolve_node_constraints(a));
			},
			TyNode::ArrayLit(exprs, _) => {
				exprs.iter().for_each(|e| self.resolve_node_constraints(e));
			},
			TyNode::Lambda { body: Some(b), args, .. } => {
				self.push_scope(ScopeKind::Func);
				args.iter().for_each(|TyLambdaArg { ident, ty, .. }| {
					self.resolve_type_constraints(ty, None);
					self.scope().push_sym(ident.elem, ty);
				});
				self.resolve_node_constraints(b);
				self.pop_scope();
			},
			TyNode::Lambda { body: None, args, .. } => {
				args.iter().for_each(|a| self.resolve_type_constraints(a.ty, None));
			},
			_ => {},
		}

		self.resolve_type_constraints(&node.ty, None);
	}

	fn resolve_type_constraints(&mut self, ty: &RefCell<Type<'src, 'b>>, cons: Option<&RefCell<Type<'src, 'b>>>) {
		let mut ty = match ty.try_borrow_mut() {
			Ok(ty) => match ty.kind {
				TypeKind::Generic(_, _) => ty,
				TypeKind::Ref(t) => return self.resolve_type_constraints(t, None),
				TypeKind::Array(t, _) => return self.resolve_type_constraints(t, None),
				_ => return,
			},
			Err(e) => {
				self.log(ReportKind::InternalError
					.title(format!("constraint resolution: {e}")));
				return;
			},
		};

		if let Some(cons) = cons {
			self.resolve_type_constraints(cons, None);
			*ty = cons.borrow().clone();
			return;
		}

		let TypeKind::Generic(_, cons) = &ty.kind else { unreachable!() };

		match cons.as_slice() {
			[] => {}, //FIXME: maybe warn or err here?
			[n] => {
				self.resolve_type_constraints(n, None);
				*ty = (*n.elem).clone().into_inner()
			},
			_ => {
				// FIXME: wtf is this logic
				let mut cons_ty = None;

				for c in cons {
					self.resolve_type_constraints(c, cons_ty);

					match cons_ty {
						Some(t) if !Self::constraint_compatible(t, c) => {
							self.log(ReportKind::TypeError
								.title(format!("cannot unify into `{}`, got `{}`", t.borrow().kind, c.borrow().kind))
								.span(c.span));
							continue;
						},
						None => cons_ty = Some(c),
						_ => {},
					}
				}
				
				if let Some(t) = cons_ty {
					*ty = (*t).clone().into_inner()
				}
			}, 
		}
	}

	#[inline]
	fn constraint_compatible(
		a: &RefCell<Type<'src, 'b>>, 
		b: &RefCell<Type<'src, 'b>>
	) -> bool {
		if a == b { return true; }

		match (&a.borrow().kind, &b.borrow().kind) {
			(TypeKind::Ref(f), TypeKind::Ref(t)) => Self::constraint_compatible(f, t),
			(TypeKind::Ref(f), TypeKind::Type) => Self::constraint_compatible(f, &Type::new(TypeKind::Type).into()),
			(TypeKind::Usize, TypeKind::U(_))
				| (TypeKind::Isize, TypeKind::I(_))
				| (TypeKind::U(_), TypeKind::U(_) | TypeKind::Usize)
				| (TypeKind::I(_), TypeKind::I(_) | TypeKind::Isize) => true,

			(TypeKind::Ref(a), TypeKind::Array(t, Some(_))) if let TypeKind::Array(f, None) = a.borrow().kind => Self::constraint_compatible(f, t),

			(TypeKind::Array(f, Some(fs)), TypeKind::Array(t, Some(ts)) ) => {
				if fs != ts { return false; }
				Self::constraint_compatible(f, t)
			},

			_ => false,
		}
	}
}


	// pub fn resolve_type(&mut self, node: &Ty<'src, 'b, Sp<TyNode<'src, 'b>>>) -> Result<&'b Type<'src, 'b>> {
	//
	// 	Ok(match &node.elem.elem {
	// 		TyNode::Primitive(ty) => ty,
	// 		TyNode::Ident { lit, gener } => todo!(),
	// 		TyNode::ArrayLit(ty, len) => {
	// 			if ty.len() != 1 {
	// 				return ReportKind::InvalidArity
	// 					.title(format!("array type expects 1 argument, found {}", ty.len()))
	// 					.span(node.elem.span)
	// 					.as_err();
	// 			}
	//
	// 			let ty = self.resolve_type(&ty[0])?;
	//
	// 			self.bump.alloc(Type::new(TypeKind::Array(ty, *len)))
	// 		},
	// 		TyNode::ImplCall { path, ident, gener, vals } => {
	// 			if path.get(0).is_none_or(|p| **p != "core") {
	// 				panic!(); // FIXME: idfk come up with an error
	// 			}
	//
	// 			match **ident {
	// 				"Add" | "As" => return ReportKind::ConstEvalError
	// 					.title(format!("cannot resolve `core::{}` on types", *ident))
	// 					.span(ident.span)
	// 					.as_err(),
	// 				"Ref" => {
	// 					if vals.len() != 1 {
	// 						return ReportKind::InvalidArity
	// 							.title(format!("`core::Ref` expects 1 argument, found {}", vals.len()))
	// 							.span(ident.span)
	// 							.as_err();
	// 					}
	//
	// 					let ty = self.resolve_type(&vals[0])?;	
	// 					self.bump.alloc(Type::new(TypeKind::Ref(ty)))
	// 				},
	// 				_ => return ReportKind::NoSuchTrait
	// 					.title(format!("`core::{ident}`"))
	// 					.span(ident.span)
	// 					.as_err(),
	// 			}
	// 		},
	// 		_ => todo!(),
	// 	})
	// }
	//
	// pub fn try_coerce_type(
	// 	from: &'b Type<'src, 'b>,
	// 	to:   &'b Type<'src, 'b>,
	// ) -> bool {
	// 	if from == to { return true; }
	//
	// 	match (&from.kind, &to.kind) {
	// 		(TypeKind::Usize, TypeKind::U(_))
	// 			| (TypeKind::Isize, TypeKind::I(_))
	// 			| (TypeKind::U(_), TypeKind::U(_) | TypeKind::Usize)
	// 			| (TypeKind::I(_), TypeKind::I(_) | TypeKind::Isize)
	// 			| (TypeKind::B(_), TypeKind::B(_)) => true,
	//
	// 		(TypeKind::Array(f, Some(fs)), TypeKind::Array(t, Some(ts))) => {
	// 			if fs != ts { return false; }
	// 			Self::try_coerce_type(f, t)
	// 		},
	// 		(TypeKind::Array(f, Some(_)), TypeKind::Array(t, None))
	// 			| (TypeKind::Ref(f), TypeKind::Ref(t)) => Self::try_coerce_type(f, t),
	//
	// 		(TypeKind::Array(f, Some(_)), TypeKind::Ref(t)) 
	// 			if let TypeKind::Array(t, None) = t.kind =>
	// 			Self::try_coerce_type(f, t),
	//
	// 		_ => false,
	// 	}
	// }
	//
	// pub fn resolve_trait(
	// 	&self,
	// 	ty:    &'b Type<'src, 'b>, 
	// 	gener: &'b [&'b Type<'src, 'b>], 
	// 	path:  &'b [Sp<&'src str>], // aaaaaaa
	// 	ident: &Sp<&'src str>,
	// ) -> Result<Option<&'b [&'b Type<'src, 'b>]>> {
	// 	if path.get(0).is_some_and(|p| **p == "core") {
	// 		return self.resolve_core_trait(ty, gener, ident);
	// 	}
	//
	// 	todo!()
	// }
	//
	// pub fn resolve_core_trait(
	// 	&self,
	// 	ty:    &'b Type<'src, 'b>, 
	// 	gener: &'b [&'b Type<'src, 'b>], 
	// 	ident: &Sp<&'src str>,
	// ) -> Result<Option<&'b [&'b Type<'src, 'b>]>> {
	// 	Ok(Some(match ident.elem {
	// 		"Add" => {
	// 			if !(1..=2).contains(&gener.len()) {
	// 				return ReportKind::UnexpectedGenericCount
	// 					.title(format!("`core::{}` expects 1..=2 generic arguments, found {}", *ident, gener.len()))
	// 					.span(ident.span)
	// 					.as_err();
	// 			}
	//
	// 			// FIXME: naive impl :)
	// 			match gener.get(1) {
	// 				Some(t) => self.bump.alloc_sized_slice([ty, gener[0], t]),
	// 				None    => self.bump.alloc_sized_slice([ty, gener[0], ty]),
	// 			}
	// 		},
	// 		"Ref" => {
	// 			let ty = match &ty.kind {
	// 				TypeKind::Type => &TY_TYPE,
	// 				_ => self.bump.alloc(Type::new(TypeKind::Ref(ty))),
	// 			};
	// 			self.bump.alloc_sized_slice([ty])
	// 		},
	// 		_ => return ReportKind::NoSuchTrait
	// 			.title(format!("`core::{ident}`"))
	// 			.span(ident.span)
	// 			.as_err(),
	// 	}))
	// }
	//
	// 		Node::Store(lhs, rhs) => {
	// 			let lhs = self.infer_node(lhs)?;
	// 			let rhs = self.infer_node(rhs)?;
	//
	// 			if lhs.ty.kind != rhs.ty.kind && !Self::try_coerce_type(rhs.ty, lhs.ty) {
	// 				return ReportKind::TypeError
	// 					.title(format!("expected `{}`, found `{}`", lhs.ty.kind, rhs.ty.kind))
	// 					.span(rhs.elem.span)
	// 					.as_err();
	// 			}
	//
	// 			TyNode::Store(self.bump.alloc(lhs), self.bump.alloc(rhs))
	// 				.span(node.span).typed(&TY_NONE)
	// 		},
	//
	//
	// 		Node::Amp(expr) => {
	// 			let expr = self.infer_node(expr)?;
	//
	// 			let trait_ = self.resolve_trait(&expr.ty, &[], &CORE, &"Ref".span(node.span))?
	// 				.ok_or_else(|| ReportKind::NoSuchTrait
	// 					.title(format!("`core::Ref` not implemented for {}", expr.ty.kind))
	// 					.span(node.span))?;
	//
	// 			TyNode::ImplCall { 
	// 				path:  CORE,
	// 				ident: "Ref".span(node.span),
	// 				gener: self.bump.alloc_sized_slice([expr.ty]),
	// 				vals:  self.bump.alloc_sized_slice([expr]), 
	// 			}.span(node.span).typed(trait_[0])
	// 		},
	// 		Node::FuncCall { lhs, args } => {
	// 			let lhs = self.infer_node(lhs)?;
	//
	// 			let TypeKind::Fn(ex_args, ex_ret) = &lhs.ty.kind else {
	// 				return ReportKind::TypeError
	// 					.title(format!("expected function, found `{}`", lhs.ty.kind))
	// 					.span(lhs.elem.span)
	// 					.as_err();
	// 			};
	//
	// 			if ex_args.len() != args.len() {
	// 				return ReportKind::InvalidArity
	// 					.title(format!("expected {} arguments, found {}", ex_args.len(), args.len()))
	// 					.span(node.span)
	// 					.as_err();
	// 			}
	//
	// 			let args = its_fine!(self.bump).try_alloc_from_iter(
	// 				args.iter().zip(ex_args.iter()).map(|(a, ety)| {
	// 					let expr = self.infer_node(a)?;
	// 					if expr.ty != *ety && !Self::try_coerce_type(expr.ty, ety) {
	// 						return ReportKind::TypeError
	// 							.title(format!("expected `{}`, found `{}`", ety.kind, expr.ty.kind))
	// 							.span(expr.elem.span)
	// 							.as_err();
	// 					}
	// 					Ok(expr)
	// 				}
	// 			))?;
	//
	// 			TyNode::FuncCall { lhs: self.bump.alloc(lhs), args }
	// 				.span(node.span).typed(ex_ret.unwrap_or(&TY_NONE))
	// 		},
	// 		Node::ArrayLit(exprs, len) => {
	// 			let exprs = its_fine!(self.bump).try_alloc_from_iter(exprs.iter()
	// 				.map(|e| self.infer_node(e)))?;
	//
	// 			if exprs.len() > 1 && exprs.iter().any(|e| e.ty != exprs[0].ty) {
	// 				return ReportKind::TypeError
	// 					.title("array elements must share the same type")
	// 					.span(node.span)
	// 					.as_err();
	// 			}
	//
	// 			let ty = exprs.get(0).map_or_else(
	// 				|| self.bump.alloc(Type::new(TypeKind::Generic(0))), 
	// 				|e| e.ty);
	//
	// 			let ty = match &ty.kind {
	// 				TypeKind::Type => &TY_TYPE,
	// 				_ => self.bump.alloc(Type::new(TypeKind::Array(ty, *len)))
	// 			};
	//
	// 			TyNode::ArrayLit(exprs, *len).span(node.span).typed(ty)
	// 		},
