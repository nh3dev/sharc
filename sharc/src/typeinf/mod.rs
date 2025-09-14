use crate::parser::ast::{self, Node};
use crate::report::{Report, ReportKind, Result};
use crate::span::{Sp, Spannable};

use bump::Bump;

pub mod hir;
use hir::{TypeInf as _, Ty, Type, TypeKind, Node as TyNode, LambdaArg as TyLambdaArg};

pub struct TypeInf<'src, 'bo, 'b, 'r> {
	reporter: &'r mut crate::Reporter<'src>,
	bump:     Bump,
	bump_lt:  std::marker::PhantomData<&'b Bump>,
	bumpo_lt: std::marker::PhantomData<&'bo Bump>,

	scope_stack: Vec<Vec<(&'src str, &'b Type<'src, 'b>)>>,
}

static TY_TYPE:  Type = Type::new(TypeKind::Type);
static TY_NONE:  Type = Type::new(TypeKind::None);
static TY_USIZE: Type = Type::new(TypeKind::Usize);
static TY_U8:    Type = Type::new(TypeKind::U(8));

static CORE: &[Sp<&str>] = &[Sp { elem: "core", span: crate::span::Span::new(0) }];

macro_rules! its_fine {
	($bump:expr) => { unsafe { &*&raw const $bump } };
}

impl<'src, 'bo, 'b, 'r> TypeInf<'src, 'bo, 'b, 'r> {
	#[inline]
	fn log(&mut self, report: Report<'src>) {
		self.reporter.nom(report);
	}

	fn push_scope(&mut self) {
		self.scope_stack.push(Vec::new());
	}

	fn pop_scope(&mut self) {
		self.scope_stack.pop();
	}
	
	fn push_scope_val(&mut self, ident: &'src str, ty: &'b Type<'src, 'b>) {
		self.scope_stack.last_mut().unwrap().push((ident, ty));
	}

	fn get_scope_val(&self, ident: &str) -> Option<(&'b Type<'src, 'b>, bool)> {
		for (i, scope) in self.scope_stack.iter().rev().enumerate() {
			if let Some((_, ty)) = scope.iter().rev().find(|(i, _)| *i == ident) {
				return Some((ty, i == 0));
			}
		}
		None
	}

	pub fn infer((ast, ast_bump): (Vec<Sp<Node<'src, 'bo>>>, Bump), reporter: &'r mut crate::Reporter<'src>) -> (Vec<Ty<'src, 'b, Sp<TyNode<'src, 'b>>>>, Bump) {
		let bump = Bump::new();
		let mut inf = Self { 
			reporter, bump, 
			bump_lt:  std::marker::PhantomData,
			bumpo_lt: std::marker::PhantomData,
			scope_stack: vec![Vec::new()] };

		let hir = ast.into_iter().filter_map(
			|node| inf.infer_node(&node).map_or_else(
				|l| { inf.log(*l); None }, 
				|n| Some(n)))
			.collect::<Vec<_>>();

		std::mem::drop(ast_bump);
		(hir, inf.bump)
	}

	pub fn resolve_type(&mut self, node: &Ty<'src, 'b, Sp<TyNode<'src, 'b>>>) -> Result<'src, &'b Type<'src, 'b>> {
		if !matches!(node.ty.kind, TypeKind::Type) {
			return ReportKind::TypeError
				.title(format!("expected `{}`, found `{}`", TypeKind::Type, node.ty.kind))
				.span(node.elem.span)
				.as_err();
		}

		Ok(match &node.elem.elem {
			TyNode::Primitive(ty) => ty,
			TyNode::Ident { lit, gener } => todo!(),
			TyNode::ArrayLit(ty, len) => {
				if ty.len() != 1 {
					return ReportKind::InvalidArity
						.title(format!("array type expects 1 argument, found {}", ty.len()))
						.span(node.elem.span)
						.as_err();
				}

				let ty = self.resolve_type(&ty[0])?;

				self.bump.alloc(Type::new(TypeKind::Array(ty, *len)))
			},
			TyNode::ImplCall { path, ident, gener, vals } => {
				if !path.get(0).is_some_and(|p| **p == "core") {
					panic!(); // FIXME: idfk come up with an error
				}

				match **ident {
					"Add" | "As" => return ReportKind::ConstEvalError
						.title(format!("cannot resolve `core::{}` on types", *ident))
						.span(ident.span)
						.as_err(),
					"Ref" => {
						if vals.len() != 1 {
							return ReportKind::InvalidArity
								.title(format!("`core::Ref` expects 1 argument, found {}", vals.len()))
								.span(ident.span)
								.as_err();
						}

						let ty = self.resolve_type(&vals[0])?;	
						self.bump.alloc(Type::new(TypeKind::Ref(ty)))
					},
					_ => return ReportKind::NoSuchTrait
						.title(format!("`core::{ident}`"))
						.span(ident.span)
						.as_err(),
				}
			},
			_ => todo!(),
		})
	}

	pub fn try_coerce_type(
		&mut self, 
		from: &'b Type<'src, 'b>,
		to:   &'b Type<'src, 'b>,
	) -> bool {
		if from == to { return true; }

		match (&from.kind, &to.kind) {
			(TypeKind::Usize, TypeKind::U(_))
				| (TypeKind::U(_), TypeKind::Usize)
				| (TypeKind::Isize, TypeKind::I(_))
				| (TypeKind::I(_), TypeKind::Isize)
				| (TypeKind::U(_), TypeKind::U(_))
				| (TypeKind::I(_), TypeKind::I(_))
				| (TypeKind::B(_), TypeKind::B(_)) => true,

			(TypeKind::Array(f, Some(fs)), TypeKind::Array(t, Some(ts))) => {
				if fs != ts { return false; }
				self.try_coerce_type(f, t)
			},
			(TypeKind::Array(f, Some(_)), TypeKind::Array(t, None)) =>
				self.try_coerce_type(f, t),

			(TypeKind::Array(f, Some(_)), TypeKind::Ref(t)) 
				if let TypeKind::Array(t, None) = t.kind =>
				self.try_coerce_type(f, t),

			(TypeKind::Ref(f), TypeKind::Ref(t)) => self.try_coerce_type(f, t),
			_ => false,
		}
	}

	pub fn try_const_eval(node: Sp<Node<'src, 'bo>>) -> Option<Ty<'src, 'b, Sp<TyNode<'src, 'b>>>> {
		todo!()
	}

	pub fn resolve_type_ident(ident: &str) -> Option<&'b Type<'src, 'b>> {
		todo!()
	}

	pub fn resolve_trait(
		&self,
		ty:    &'b Type<'src, 'b>, 
		gener: &'b [&'b Type<'src, 'b>], 
		path:  &'b [Sp<&'src str>], // aaaaaaa
		ident: &Sp<&'src str>,
	) -> Result<'src, Option<&'b [&'b Type<'src, 'b>]>> {
		if path.get(0).is_some_and(|p| **p == "core") {
			return self.resolve_core_trait(ty, gener, ident);
		}

		todo!()
	}

	pub fn resolve_core_trait(
		&self,
		ty:    &'b Type<'src, 'b>, 
		gener: &'b [&'b Type<'src, 'b>], 
		ident: &Sp<&'src str>,
	) -> Result<'src, Option<&'b [&'b Type<'src, 'b>]>> {
		Ok(Some(match ident.elem {
			"Add" => {
				if !(1..=2).contains(&gener.len()) {
					return ReportKind::UnexpectedGenericCount
						.title(format!("`core::{}` expects 1..=2 generic arguments, found {}", *ident, gener.len()))
						.span(ident.span)
						.as_err();
				}

				// FIXME: naive impl :)
				match gener.get(1) {
					Some(t) => self.bump.alloc_sized_slice([ty, gener[0], t]),
					None    => self.bump.alloc_sized_slice([ty, gener[0], ty]),
				}
			},
			"Ref" => {
				let ty = match &ty.kind {
					TypeKind::Type => &TY_TYPE,
					_ => self.bump.alloc(Type::new(TypeKind::Ref(ty))),
				};
				self.bump.alloc_sized_slice([ty])
			},
			_ => return ReportKind::NoSuchTrait
				.title(format!("`core::{ident}`"))
				.span(ident.span)
				.as_err(),
		}))
	}

	pub fn infer_node(&mut self, node: &Sp<Node<'src, 'bo>>) -> Result<'src, Ty<'src, 'b, Sp<TyNode<'src, 'b>>>> {
		Ok(match &node.elem {
			Node::Primitive(t) => {
				let ty = match t {
					ast::Primitive::U(size) => TypeKind::U(*size),
					ast::Primitive::I(size) => TypeKind::I(*size),
					ast::Primitive::B(size) => TypeKind::B(*size),
					ast::Primitive::None    => TypeKind::None,
					ast::Primitive::Fn(args, ret) => {
						let args = its_fine!(self.bump).try_alloc_from_iter(args.iter()
							.map(|a| self.infer_node(a).and_then(|n| self.resolve_type(&n))))?;

						let ret = ret.map(|r| self.infer_node(r)
							.and_then(|n| self.resolve_type(&n)))
							.transpose()?;

						TypeKind::Fn(args, ret)
					},
					_ => todo!(),
				};
				TyNode::Primitive(self.bump.alloc(Type::new(ty)))
					.span(node.span).typed(&TY_TYPE)
			},
			Node::Ident { lit, gener } => {
				let Some((ty, _)) = self.get_scope_val(lit) else {
					return ReportKind::UndefinedIdentifier
						.title(format!("`{}`", *lit))
						.span(node.span)
						.as_err();
				};

				let gener = its_fine!(self.bump).try_alloc_from_iter(gener.iter()
					.map(|g| self.infer_node(g).and_then(|n| self.resolve_type(&n))))?;

				TyNode::Ident { lit: *lit, gener }.span(node.span).typed(ty)
			},
			Node::Let { ident, ty, gener, expr, stat } => {
				let gener = its_fine!(self.bump).try_alloc_from_iter(gener.iter()
					.map(|g| self.infer_node(g).and_then(|n| self.resolve_type(&n))))?;

				let expr = self.infer_node(expr)?;

				let ty = match ty {
					Some(ty) => {
						let ty = self.infer_node(ty)?;
						self.resolve_type(&ty)?
					},
					None     => expr.ty,
				};

				self.push_scope_val(ident, ty);

				if expr.ty.kind != ty.kind && !self.try_coerce_type(expr.ty, ty) {
					return ReportKind::TypeError
						.title(format!("expected `{}`, found `{}`", ty.kind, expr.ty.kind))
						.span(expr.elem.span)
						.as_err();
				}

				TyNode::Let { 
					gener,
					ident: *ident, 
					ty:    Some(ty), 
					expr:  self.bump.alloc(expr),
					stat:  false, // TODO do something here
				}.span(node.span).typed(&TY_NONE)
			},
			Node::Add(lhs, rhs) => {
				let lhs = self.infer_node(lhs)?;
				let rhs = self.infer_node(rhs)?;

				let trait_ = self.resolve_trait(
					&lhs.ty, 
					&self.bump.alloc_sized_slice([rhs.ty]), 
					&CORE, &"Add".span(node.span))?
					.ok_or_else(|| ReportKind::NoSuchTrait
						.title(format!("`core::Add<{}>` not implemented for {}", rhs.ty.kind, lhs.ty.kind))
						.span(node.span))?;

				TyNode::ImplCall {
					path:  CORE,
					ident: "Add".span(node.span),
					gener: self.bump.alloc_sized_slice([lhs.ty, rhs.ty]), 
					vals:  self.bump.alloc_sized_slice([lhs, rhs]),
				}.span(node.span).typed(trait_.last().unwrap())
			},
			Node::Amp(expr) => {
				let expr = self.infer_node(expr)?;

				let trait_ = self.resolve_trait(&expr.ty, &[], &CORE, &"Ref".span(node.span))?
					.ok_or_else(|| ReportKind::NoSuchTrait
						.title(format!("`core::Ref` not implemented for {}", expr.ty.kind))
						.span(node.span))?;

				TyNode::ImplCall { 
					path:  CORE,
					ident: "Ref".span(node.span),
					gener: self.bump.alloc_sized_slice([expr.ty]),
					vals:  self.bump.alloc_sized_slice([expr]), 
				}.span(node.span).typed(trait_[0])
			},
			Node::FuncCall { lhs, args } => {
				let lhs = self.infer_node(lhs)?;

				let TypeKind::Fn(ex_args, ex_ret) = &lhs.ty.kind else {
					return ReportKind::TypeError
						.title(format!("expected function, found `{}`", lhs.ty.kind))
						.span(lhs.elem.span)
						.as_err();
				};

				if ex_args.len() != args.len() {
					return ReportKind::InvalidArity
						.title(format!("expected {} arguments, found {}", ex_args.len(), args.len()))
						.span(node.span)
						.as_err();
				}

				let args = its_fine!(self.bump).try_alloc_from_iter(
					args.iter().zip(ex_args.iter()).map(|(a, ety)| {
						let expr = self.infer_node(a)?;
						if expr.ty != *ety && !self.try_coerce_type(expr.ty, ety) {
							return ReportKind::TypeError
								.title(format!("expected `{}`, found `{}`", ety.kind, expr.ty.kind))
								.span(expr.elem.span)
								.as_err();
						}
						Ok(expr)
					}
				))?;

				TyNode::FuncCall { lhs: self.bump.alloc(lhs), args }
					.span(node.span).typed(ex_ret.unwrap_or(&TY_NONE))
			},
			Node::As(lhs, rhs) => {
				let lhs = self.infer_node(lhs)?;
				let rhs = self.infer_node(rhs)?; // lossy
				let ty  = self.resolve_type(&rhs)?;

				TyNode::ImplCall { 
					path:  CORE,
					ident: "As".span(node.span),
					gener: self.bump.alloc_sized_slice([ty]),
					vals:  self.bump.alloc_sized_slice([lhs]),
				}.span(node.span).typed(ty)
			},



			Node::IntLit(i) => {
				// let ty = Type::new(TypeKind::U(i.min_bit_size()));
				TyNode::IntLit(i.copy(&self.bump)).span(node.span).typed(&TY_USIZE)
			},
			Node::StrLit(s) => {
				let ty = Type::new(TypeKind::Array(&TY_U8, Some(s.len() as u64)));
				TyNode::StrLit(self.bump.alloc_str(s)).span(node.span).typed(self.bump.alloc(ty))
			},
			Node::ArrayLit(exprs, len) => {
				let exprs = its_fine!(self.bump).try_alloc_from_iter(exprs.iter()
					.map(|e| self.infer_node(e)))?;

				if exprs.len() > 1 && exprs.iter().any(|e| e.ty != exprs[0].ty) {
					return ReportKind::TypeError
						.title("array elements must share the same type")
						.span(node.span)
						.as_err();
				}

				let ty = exprs.get(0).map(|e| e.ty).unwrap_or_else(
					|| self.bump.alloc(Type::new(TypeKind::Generic(0))));

				let ty = match &ty.kind {
					TypeKind::Type => &TY_TYPE,
					_ => self.bump.alloc(Type::new(TypeKind::Array(ty, *len)))
				};

				TyNode::ArrayLit(exprs, *len).span(node.span).typed(ty)
			},



			Node::Lambda { ext: Some(_), export: Some(_), .. } => 
				return ReportKind::InvalidFunctionType
					.title("extern functions cannot be exported")
					.span(node.span).as_err(),
			Node::Lambda { body: Some(_), ext: Some(_), export: None, .. } =>
				return ReportKind::InvalidFunctionType
					.title("extern functions cannot have a body")
					.span(node.span).as_err(),
			Node::Lambda { args, ret, body: None, ext: Some(ext), export: None } => {
				let args = its_fine!(self.bump).try_alloc_from_iter(args.iter().map(|a| {
					let Some(ty) = &a.ty else {
						return ReportKind::TypeAnnotationRequired
							.title("extern functions must have all types known")
							.span(node.span)
							.as_err();
					};

					Result::Ok(TyLambdaArg {
						ty:      Some(self.infer_node(&ty).and_then(|n| self.resolve_type(&n))?),
						default: a.default.as_ref().map(|d| self.infer_node(d)).transpose()?,
						ident: a.ident,
					})
				}))?;

				let ret = ret.map(|r| self.infer_node(r)
					.and_then(|n| self.resolve_type(&n)))
					.transpose()?;

				let ty = Type::new(TypeKind::Fn(
					self.bump.try_alloc_from_iter(args.iter()
						.map(|a| a.ty.ok_or_else(|| ReportKind::Unimplemented
							.title("type inference not implemented")
							.note("bug nick about this")
							.span(node.span))))?,
					ret,
				));

				let ext = self.bump.alloc_str(ext.elem).span(ext.span);

				TyNode::Lambda { args, ret, body: None, ext: Some(ext), export: None }
					.span(node.span).typed(self.bump.alloc(ty))
			},
			n => todo!("{n:?}"),
		})
	}
}
