use std::cell::RefCell;

use crate::parser::ast::{self, Node};
use crate::report::{Report, ReportKind, Result, Reportable};
use crate::span::{Span, Sp, Spannable};

use bump::Bump;

pub mod hir;
use hir::{TypeInf as _, Ty, Type, TypeKind, Node as TyNode, LambdaArg as TyLambdaArg};

pub struct TypeInf<'src, 'bo, 'b, 'r> {
	reporter: &'r mut crate::Reporter,
	bump:     Bump,
	bump_lt:  std::marker::PhantomData<&'b Bump>,
	bumpo_lt: std::marker::PhantomData<&'bo Bump>,

	scope_stack: Vec<Scope<'src, 'b>>,
	gener_id:    usize,
}

struct Scope<'src, 'b> {
	kind: ScopeKind,
	sym:  Vec<(&'src str, &'b RefCell<Type<'src, 'b>>)>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ScopeKind {
	Root, Func, Loop, Block
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
	// yea yea okay mai
	fn bump(&self) -> &'static Bump {
		unsafe { std::mem::transmute(&self.bump) }
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
		let mut inf = Self { 
			reporter, 
			bump: Bump::new(),
			bump_lt:  std::marker::PhantomData,
			bumpo_lt: std::marker::PhantomData,
			scope_stack: vec![Scope::new(ScopeKind::Root)],
			gener_id: 0,
		};

		let hir = match inf.infer_node(ast.span(Span::new(0))) {
			Ok(n) => {
				let Ty { elem, ty } = n;
				elem.elem.typed(ty)
			},
			Err(l) => {
				// should realistically pretty much never happen
				inf.log(*l); 
				TyNode::Block(&[]).typed(inf.bump.alloc(Type::new(TypeKind::None).into()))
			},
		};

		std::mem::drop(ast_bump);
		(hir, inf.bump)
	}

	fn infer_node(&mut self, node: Sp<Node<'src, 'bo>>) 
		-> Result<Ty<'src, 'b, Sp<TyNode<'src, 'b>>>> {

		match node.elem {
			_ => todo!("{:?}", node.elem),
		}
	}

	// pub fn resolve_type(&mut self, node: &Ty<'src, 'b, Sp<TyNode<'src, 'b>>>) -> Result<&'b Type<'src, 'b>> {
	// 	if !matches!(node.ty.kind, TypeKind::Type) {
	// 		return ReportKind::TypeError
	// 			.title(format!("expected `{}`, found `{}`", TypeKind::Type, node.ty.kind))
	// 			.span(node.elem.span)
	// 			.as_err();
	// 	}
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
	// pub fn try_const_eval(node: Sp<Node<'src, 'bo>>) -> Option<Ty<'src, 'b, Sp<TyNode<'src, 'b>>>> {
	// 	todo!()
	// }
	//
	// pub fn resolve_type_ident(ident: &str) -> Option<&'b Type<'src, 'b>> {
	// 	todo!()
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
	// pub fn infer_node(&mut self, node: &Sp<Node<'src, 'bo>>) -> Result<Ty<'src, 'b, Sp<TyNode<'src, 'b>>>> {
	// 	Ok(match &node.elem {
	// 		Node::Primitive(t) => {
	// 			let ty = match t {
	// 				ast::Primitive::U(size) => TypeKind::U(*size),
	// 				ast::Primitive::I(size) => TypeKind::I(*size),
	// 				ast::Primitive::B(size) => TypeKind::B(*size),
	// 				ast::Primitive::None    => TypeKind::None,
	// 				ast::Primitive::Fn(args, ret) => {
	// 					let args = its_fine!(self.bump).try_alloc_from_iter(args.iter()
	// 						.map(|a| self.infer_node(a).and_then(|n| self.resolve_type(&n))))?;
	//
	// 					let ret = ret.map(|r| self.infer_node(r)
	// 						.and_then(|n| self.resolve_type(&n)))
	// 						.transpose()?;
	//
	// 					TypeKind::Fn(args, ret)
	// 				},
	// 				_ => todo!(),
	// 			};
	// 			TyNode::Primitive(self.bump.alloc(Type::new(ty)))
	// 				.span(node.span).typed(&TY_TYPE)
	// 		},
	// 		Node::Ident { lit, gener } => {
	// 			let Some((ty, _)) = self.get_scope_val(lit) else {
	// 				return ReportKind::UndefinedIdentifier
	// 					.title(format!("`{}`", *lit))
	// 					.span(node.span)
	// 					.as_err();
	// 			};
	//
	// 			let gener = its_fine!(self.bump).try_alloc_from_iter(gener.iter()
	// 				.map(|g| self.infer_node(g).and_then(|n| self.resolve_type(&n))))?;
	//
	// 			TyNode::Ident { lit: *lit, gener }.span(node.span).typed(ty)
	// 		},
	//
	// 		Node::Let { ident, ty, gener, expr, stat } => {
	// 			let gener = its_fine!(self.bump).try_alloc_from_iter(gener.iter()
	// 				.map(|g| self.infer_node(g).and_then(|n| self.resolve_type(&n))))?;
	//
	// 			let expr = self.infer_node(expr)?;
	//
	// 			let ty = match ty {
	// 				Some(ty) => {
	// 					let ty = self.infer_node(ty)?;
	// 					self.resolve_type(&ty)?
	// 				},
	// 				None     => expr.ty,
	// 			};
	//
	// 			self.scope().push_sym(ident, ty);
	//
	// 			if expr.ty.kind != ty.kind && !Self::try_coerce_type(expr.ty, ty) {
	// 				return ReportKind::TypeError
	// 					.title(format!("expected `{}`, found `{}`", ty.kind, expr.ty.kind))
	// 					.span(expr.elem.span)
	// 					.as_err();
	// 			}
	//
	// 			TyNode::Let { 
	// 				gener,
	// 				ident: *ident, 
	// 				ty:    Some(ty), 
	// 				expr:  self.bump.alloc(expr),
	// 				stat:  false, // TODO do something here
	// 			}.span(node.span).typed(&TY_NONE)
	// 		},
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
	// 		Node::Add(lhs, rhs) => {
	// 			let lhs = self.infer_node(lhs)?;
	// 			let rhs = self.infer_node(rhs)?;
	//
	// 			let trait_ = self.resolve_trait(
	// 				&lhs.ty, 
	// 				&self.bump.alloc_sized_slice([rhs.ty]), 
	// 				&CORE, &"Add".span(node.span))?
	// 				.ok_or_else(|| ReportKind::NoSuchTrait
	// 					.title(format!("`core::Add<{}>` not implemented for {}", rhs.ty.kind, lhs.ty.kind))
	// 					.span(node.span))?;
	//
	// 			TyNode::ImplCall {
	// 				path:  CORE,
	// 				ident: "Add".span(node.span),
	// 				gener: self.bump.alloc_sized_slice([lhs.ty, rhs.ty]), 
	// 				vals:  self.bump.alloc_sized_slice([lhs, rhs]),
	// 			}.span(node.span).typed(trait_.last().unwrap())
	// 		},
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
	// 		Node::As(lhs, rhs) => {
	// 			let lhs = self.infer_node(lhs)?;
	// 			let rhs = self.infer_node(rhs)?; // lossy
	// 			let ty  = self.resolve_type(&rhs)?;
	//
	// 			TyNode::ImplCall { 
	// 				path:  CORE,
	// 				ident: "As".span(node.span),
	// 				gener: self.bump.alloc_sized_slice([ty]),
	// 				vals:  self.bump.alloc_sized_slice([lhs]),
	// 			}.span(node.span).typed(ty)
	// 		},
	//
	//
	//
	// 		Node::IntLit(i) => {
	// 			// let ty = Type::new(TypeKind::U(i.min_bit_size()));
	// 			TyNode::IntLit(i.copy(&self.bump)).span(node.span).typed(&TY_USIZE)
	// 		},
	// 		Node::StrLit(s) => {
	// 			let ty = Type::new(TypeKind::Array(&TY_U8, Some(s.len() as u64)));
	// 			TyNode::StrLit(self.bump.alloc_str(s)).span(node.span).typed(self.bump.alloc(ty))
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
	//
	//
	//
	// 		Node::Lambda { ext: Some(_), export: Some(_), .. } => 
	// 			return ReportKind::InvalidFunctionType
	// 				.title("extern functions cannot be exported")
	// 				.span(node.span).as_err(),
	// 		Node::Lambda { body: Some(_), ext: Some(_), export: None, .. } =>
	// 			return ReportKind::InvalidFunctionType
	// 				.title("extern functions cannot have a body")
	// 				.span(node.span).as_err(),
	// 		Node::Lambda { args, ret, body: None, ext: Some(ext), export: None } => {
	// 			let args = its_fine!(self.bump).try_alloc_from_iter(args.iter().map(|a| {
	// 				let Some(ty) = &a.ty else {
	// 					return ReportKind::TypeAnnotationRequired
	// 						.title("extern functions must have all types known")
	// 						.span(node.span)
	// 						.as_err();
	// 				};
	//
	// 				Result::Ok(TyLambdaArg {
	// 					ty:      Some(self.infer_node(&ty).and_then(|n| self.resolve_type(&n))?),
	// 					default: a.default.as_ref().map(|d| self.infer_node(d)).transpose()?,
	// 					ident: a.ident,
	// 				})
	// 			}))?;
	//
	// 			let ret = ret.map(|r| self.infer_node(r)
	// 				.and_then(|n| self.resolve_type(&n)))
	// 				.transpose()?;
	//
	// 			let ty = Type::new(TypeKind::Fn(
	// 				self.bump.try_alloc_from_iter(args.iter()
	// 					.map(|a| a.ty.ok_or_else(|| ReportKind::Unimplemented
	// 						.title("type inference not implemented")
	// 						.note("bug nick about this")
	// 						.span(node.span))))?,
	// 				ret,
	// 			));
	//
	// 			let ext = self.bump.alloc_str(ext.elem).span(ext.span);
	//
	// 			TyNode::Lambda { args, ret, body: None, ext: Some(ext), export: None }
	// 				.span(node.span).typed(self.bump.alloc(ty))
	// 		},
	// 		n => todo!("{n:?}"),
	// 	})
	// }
}
