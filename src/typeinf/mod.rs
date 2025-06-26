use std::collections::HashMap;

use crate::parser::ast::{Node, AST};
use crate::span::{Sp, Spannable};
use crate::report::LogHandler;
use crate::bump::{Box, Rc};

mod hir;
use hir::{Ty, Type, TypeKind};

pub struct TypeInf {
	handler:  LogHandler,
	filename: &'static str,
	scopes:   Vec<HashMap<&'static str, &'static Type>>, // prob do stuff with bump::Rc instead of ref
}

impl TypeInf {
	pub fn push_scope(&mut self) {
		self.scopes.push(HashMap::new());
	}

	pub fn pop_scope(&mut self) {
		self.scopes.pop();
	}

	pub fn alloc<T>(&self, elem: T) -> Box<T> {
		crate::bump::THREAD_BUMP.with(|a| a.alloc(elem).into_static_unsafe())
	}

	pub fn alloc_vec<T>(&self, elems: Vec<T>) -> Box<[T]> {
		crate::bump::THREAD_BUMP.with(|a| a.alloc_from_vec(elems).into_static_unsafe())
	}

	pub fn alloc_rc<T>(&self, elem: T) -> Rc<T> {
		crate::bump::THREAD_BUMP.with(|a| a.alloc_rc(elem).into_static_unsafe())
	}

	pub fn find_sym(&self, ident: &str) -> Option<&'static Type> {
		for scope in self.scopes.iter().rev() {
			if let Some(ty) = scope.get(ident) {
				return Some(*ty);
			}
		}
		None
	}

	pub fn process(ast: AST, filename: &'static str, handler: LogHandler) -> hir::TypedAST {
		let mut inf = Self { handler, filename, scopes: vec![HashMap::new()] };

		crate::bump::THREAD_BUMP.with(|a|
			a.alloc_from_iter(ast.into_iter().map(|node| inf.process_node(node))))
	}

	pub fn process_node(&mut self, node: Sp<Node>) -> Ty<Sp<Node>> {
		match node.elem {
			Node::Let { ident, ty, gener, expr, .. } => {
				todo!();
			},
			_ => todo!(),
		}
	}
}
