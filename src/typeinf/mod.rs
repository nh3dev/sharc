use std::collections::HashMap;

use crate::parser::ast::{AST, Node};
use crate::span::{Sp, Spannable};

mod hir;
use hir::{Ty, Type, TypeKind};

pub struct TypeInf {
	scopes: Vec<HashMap<&'static str, &'static Type>>,
	bump:   bump::Bump,
}

impl TypeInf {
	pub fn push_scope(&mut self) {
		self.scopes.push(HashMap::new());
	}

	pub fn pop_scope(&mut self) {
		self.scopes.pop();
	}

	pub fn alloc<T>(&self, elem: T) -> bump::Box<'static, T> {
		self.bump.alloc(elem).into_static_unsafe()
	}

	pub fn alloc_vec<T>(&self, elems: Vec<T>) -> bump::Box<'static, [T]> {
		self.bump.alloc_from_vec(elems).into_static_unsafe()
	}

	pub fn find_sym(&self, ident: &str) -> Option<&'static Type> {
		for scope in self.scopes.iter().rev() {
			if let Some(ty) = scope.get(ident) {
				return Some(*ty);
			}
		}
		None
	}

	pub fn process(ast: AST) -> hir::TypedAST {
		let AST { bump, nodes } = ast;

		let mut inf = Self { bump, scopes: vec![HashMap::new()] };

		hir::TypedAST {
			#[allow(clippy::deref_addrof)]
			nodes: unsafe { &*(&raw const inf.bump) } // lol
				.alloc_from_iter(nodes.into_iter().map(|node| inf.process_node(node))),
			bump:  inf.bump,
		}
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
