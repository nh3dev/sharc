use std::collections::HashMap;

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

	scope_stack: Vec<HashMap<&'src str, &'b Type<'src, 'b>>>,
	ty_type:     &'src Type<'src, 'b>,
}

impl<'src, 'bo, 'b, 'r> TypeInf<'src, 'bo, 'b, 'r> {
	#[inline]
	fn log(&mut self, report: Report<'src>) {
		self.reporter.nom(report);
	}

	fn push_scope(&mut self) {
		self.scope_stack.push(HashMap::new());
	}

	fn pop_scope(&mut self) {
		self.scope_stack.pop();
	}

	pub fn infer((ast, ast_bump): (Vec<Sp<Node<'src, 'bo>>>, Bump), reporter: &'r mut crate::Reporter<'src>) -> (Vec<Ty<'src, 'b, Sp<TyNode<'src, 'b>>>>, Bump) {
		let bump = Bump::new();
		let mut inf = Self { 
			ty_type: bump.alloc(Type::new(TypeKind::Type)),
			reporter, bump, 
			bump_lt:  std::marker::PhantomData,
			bumpo_lt: std::marker::PhantomData,
			scope_stack: vec![HashMap::new()] };

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
			return ReportKind::UnexpectedType
				.title(format!("expected value of type `{}`, found `{}`", TypeKind::Type, node.ty.kind))
				.span(node.elem.span)
				.as_err();
		}

		match &node.elem.elem {
			TyNode::Primitive(ty) => Ok(ty),
			TyNode::Ident { lit, gener } => todo!(),
			_ => todo!(),
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
					_ => todo!(),
				};
				TyNode::Primitive(self.bump.alloc(Type::new(ty)))
					.span(node.span).typed(self.ty_type)
			},

			Node::Let { ident, ty, gener, expr, .. } => {
				todo!()
			},
			Node::Add(lhs, rhs) => {
				let lhs = self.infer_node(lhs)?;
				let rhs = self.infer_node(rhs)?;

				let trait_ = self.resolve_trait(
					&lhs.ty, 
					&self.bump.alloc_sized_slice([rhs.ty]), 
					&self.bump.alloc_sized_slice(["core".span(node.span)]),
					&"Add".span(node.span))?
					.ok_or_else(|| ReportKind::NoSuchTrait
						.title(format!("`core::Add<{}>` not implemented for {}", rhs.ty.kind, lhs.ty.kind))
						.span(node.span))?;

				TyNode::ImplCall {
					path:  self.bump.alloc_sized_slice(["core".span(node.span)]),
					ident: "Add".span(node.span),
					gener: self.bump.alloc_sized_slice([lhs.ty, rhs.ty]), 
					vals:  self.bump.alloc_sized_slice([lhs, rhs]),
				}.span(node.span).typed(trait_.last().unwrap())
			},
			Node::Neg(rhs) => {
				todo!()
			},
			Node::As(lhs, rhs) => {
				let lhs = self.infer_node(lhs)?;
				let rhs = self.infer_node(rhs)?; // lossy
				let ty  = self.resolve_type(&rhs)?;

				TyNode::ImplCall { 
					path:  self.bump.alloc_sized_slice(["core".span(node.span)]),
					ident: "As".span(node.span),
					gener: self.bump.alloc_sized_slice([ty]),
					vals:  self.bump.alloc_sized_slice([lhs]),
				}.span(node.span).typed(ty)
			},
			Node::IntLit(i) => {
				let ty = Type::new(TypeKind::U(i.min_bit_size()));
				TyNode::IntLit(i.copy(&self.bump)).span(node.span).typed(self.bump.alloc(ty))
			},
			_ => todo!(),
		})
	}
}
