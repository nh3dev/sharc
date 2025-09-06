use std::collections::HashMap;

use crate::report::{Result, ReportKind};
use crate::span::Sp;
use crate::typeinf::hir;

pub mod mir;
use mir::{Node, ValId, Var, Type, Expr};

use colored::Colorize;

use bump::Bump;

pub struct Analyzer<'r, 'src, 'bo, 'b> {
	reporter: &'r mut crate::Reporter<'src>,
	bump:     Bump,
	bump_lt:  std::marker::PhantomData<&'b Bump>,
	bumpo_lt: std::marker::PhantomData<&'bo Bump>,

	scope:    Vec<Scope<'b>>,
	// impls:    

	// symbols:  HashMap<ValId, String>,
}

#[derive(Default, Debug)]
struct Scope<'b> {
	idacc:  ValId,
	locals: Vec<(ValId, String, Type<'b>)>, 
}

impl Scope<'_> {
	fn id(&mut self) -> ValId {
		self.idacc.0 += 1;
		self.idacc
	}
}

impl<'r, 'src, 'bo, 'b> Analyzer<'r, 'src, 'bo, 'b> {
	fn scope(&mut self) -> &mut Scope<'b> {
		self.scope.last_mut().unwrap()
	}

	pub fn process((hir, hir_bump): (Vec<hir::Ty<'src, 'bo, Sp<hir::Node<'src, 'bo>>>>, Bump), reporter: &'r mut crate::Reporter<'src>) -> (Vec<Node<'b>>, Bump) {
		let mut analyzer = Self { 
			reporter, 
			bump: Bump::new(), 
			bump_lt:  std::marker::PhantomData,
			bumpo_lt: std::marker::PhantomData,
			scope: vec![Scope::default()], 
			// symbols: HashMap::new(),
		};

		let hir_len = hir.len();
		let mir = hir.into_iter().enumerate().flat_map(
			|(i, node)| analyzer.process_node(i == hir_len - 1, &node).map_or_else(
				|l| { analyzer.reporter.nom(*l); Vec::new().into_iter() }, 
				|(_, _, n)| n.into_iter()))
			.collect::<Vec<_>>();

		std::mem::drop(hir_bump);
		(mir, analyzer.bump)
	}

	pub fn process_node(&mut self, ret: bool, node: &hir::Ty<'src, 'bo, Sp<hir::Node<'src, 'bo>>>) 
		-> Result<'src, (Var<'b>, &'b Type<'b>, Vec<Node<'b>>)> {
		let mut nodes = Vec::new();

		let ty = self.process_type(node.ty);

		let var = match &node.elem.elem {
			hir::Node::ImplCall { path, ident, gener, vals } => {
				let gener = self.bump.alloc_from_iter(gener.iter().map(|g| self.process_type(g)));

				let args = vals.into_iter().map(|val| {
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
			_ => todo!("{}", node),
		};

		if ret {
			nodes.push(Node::Ret(var, self.process_type(node.ty)));
		}

		Ok((var, ty, nodes))
	}

	pub fn process_type(&self, ty: &'bo hir::Type<'src, 'bo>) -> &'b Type<'b> {
		match ty.kind {
			hir::TypeKind::U(n)  => self.bump.alloc(Type::U(n)),
			hir::TypeKind::I(n)  => self.bump.alloc(Type::I(n)),
			_ => todo!("{:?}", ty),
		}
	}
}

// impl Analyzer {
// 	#[inline]
// 	fn push_new_scope(&mut self) {
// 		self.scope.push(Scope::default());
// 	}
//
// 	#[inline]
// 	fn pop_scope(&mut self) {
// 		self.scope.pop();
// 	}
//
// 	#[inline]
// 	fn peek_scope_mut(&mut self) -> &mut Scope {
// 		unsafe { self.scope.last_mut().unwrap_unchecked() }
// 	}
//
// 	#[inline]
// 	fn peek_scope(&self) -> &Scope {
// 		unsafe { self.scope.last().unwrap_unchecked() }
// 	}
//
// 	#[inline]
// 	fn get_global_mut(&mut self) -> &mut Scope {
// 		unsafe { self.scope.first_mut().unwrap_unchecked() }
// 	}
//
// 	#[inline]
// 	fn get_global(&self) -> &Scope {
// 		unsafe { self.scope.first().unwrap_unchecked() }
// 	}
//
// 	// TODO: mayhaps dont clone
// 	fn find_matching_descending<F: Fn((ValId, &str, &Type)) -> bool>(&self, f: F) 
// 		-> Option<(usize, (ValId, String, Type))> {
// 		self.scope.iter().enumerate().rev().find_map(|(d, scope)|
// 			scope.locals.iter().rev().find(|(i,n,t)| f((*i,n,t))).cloned().map(|v| (d, v)))
// 	}

	// pub fn analyze(ast: Vec<Sp<ast::Node>>, file: &'static str, handler: &LogHandler) -> (Vec<Node>, HashMap<ValId, String>) {
	// 	let mut analyzer = Self {
	// 		scope: vec![Scope::default()],
	// 		symbols: HashMap::new(),
	// 	};
	//
	// 	(ast.into_iter().fold(Vec::new(), |mut acc, node| {
	// 		match analyzer.analyze_stmt(node) {
	// 			Ok(n)  => {
	// 				acc.extend(n);
	// 			},
	// 			Err(e) => {
	// 				handler.log(e.file(file));
	// 				analyzer.scope.truncate(1); // TODO: ??!?!?!?!?!?!??!
	// 			},
	// 		} acc
	// 	}), analyzer.symbols)
	// }

// 	fn analyze_stmt(&mut self, node: Sp<ast::Node>) -> Result<Vec<Node>> {
// 		Ok(match node.elem {
// 			ast::Node::Let { name, ty, expr, stat: false } => {
// 				if ty.as_ref().is_some_and(|ty| matches!(**ty, ast::Type::Any | ast::Type::None)) {
// 					return ReportKind::TypeError
// 						.title("Type 'any' and 'none' are not allowed as a variable type")
// 						.help("Change the type to a concrete type")
// 						.span(ty.unwrap().span)
// 						.as_err();
// 				}
//
// 				let (expr_ty, mut expr, var) = self.analyze_expr(*expr)?;
// 				let ty = ty.map_or_else(|| expr_ty.clone(), |t| convert_ast_ty(&t));
//
// 				if !cmp_ty(&expr_ty, &ty) {
// 					return ReportKind::TypeError
// 						.title("Type mismatch in variable assignment")
// 						.label(format!("expected '{ty}', found '{expr_ty}'"))
// 						.span(node.span)
// 						.as_err();
// 				}
//
// 				match expr.last() {
// 					Some(Node::FuncDecl { id, .. } | Node::Func { id, .. }) => {
// 						self.peek_scope_mut().locals.push((*id, name.to_string(), ty));
// 						expr
// 					},
// 					_ => {
// 						let id = self.peek_scope_mut().new_id();
// 						self.peek_scope_mut().locals.push((id, name.to_string(), ty.clone()));
//
// 						expr.push(Node::Assign { id, ty, val: Node::Var(var).into() });
// 						expr
// 					},
// 				}
// 			},
// 			ast::Node::ImplRet(node) => {
// 				let (ty, mut nodes, val) = self.analyze_expr(*node)?;
//
// 				nodes.push(Node::Ret(Some(val), ty));
// 				nodes
// 			},
// 			// expr as stmt
// 			// TODO: idk if this is actually correct
// 			_ => self.analyze_expr(node)?.1,
// 		})
// 	}
//
// 	// TODO: return a vec so we can have string and whateverthefucks
// 	fn analyze_expr(&mut self, node: Sp<ast::Node>) -> Result<(Type, Vec<Node>, Var)> {
// 		Ok(match node.elem {
// 			ast::Node::Lambda { ext: Some(_), export: Some(_), .. } =>
// 				return ReportKind::SyntaxError
// 					.title(format!("Lambda cannot be both '{}' and '{}'", 
// 						"extern".yellow().dimmed(), "export".yellow().dimmed()))
// 					.span(node.span)
// 					.as_err(),
//
// 			ast::Node::Lambda { body, ext: Some(_), .. } if !body.is_empty() =>
// 				return ReportKind::SyntaxError // TODO: maybe not SyntaxError
// 					.title(format!("{} functions cannot have a body", "extern".yellow().dimmed()))
// 					.span(node.span)
// 					.as_err(),
//
// 			ast::Node::Lambda { args, ret, ext: Some(sym), .. } => {
// 				let ret = ret.map_or(Type::None, |t| convert_ast_ty(&t));
//
// 				let mut fargs = Vec::with_capacity(args.len());
// 				for (_, ty) in args {
// 					let ty = ty.ok_or_else(|| ReportKind::SyntaxError
// 						// TODO: better message :p
// 						.title(format!("All arg types must be specified in {} functions", "extern".yellow().dimmed()))
// 						.span(node.span))?;
//
// 					if matches!(*ty, ast::Type::Any | ast::Type::None) {
// 						return ReportKind::TypeError
// 							.title("Type 'any' and 'none' are not allowed as a function argument")
// 							.help("Remove the arg, or change the type to '*any'")
// 							.span(ty.span)
// 							.as_err();
// 					}
//
// 					fargs.push(convert_ast_ty(&ty.elem));
// 				}
//
// 				let id = self.peek_scope_mut().new_id();
// 				self.symbols.insert(id, sym);
//
// 				let ty = Type::Fn(fargs, ret.into());
// 				(ty.clone(), vec![Node::FuncDecl { id, ty }], Var::Glob(id))
// 			},
// 			ast::Node::Lambda { args, ret, body, export, .. } => {
// 				// TODO: TYPE INFERENCE
// 				let ret = ret.map_or_else(|| todo!("type inference"), |t| convert_ast_ty(&t));
//
// 				self.push_new_scope();
//
// 				let mut fargs = Vec::with_capacity(args.len());
// 				for (n, ty) in args {
// 					// TODO: TYPE INFERENCE
// 					let ty = ty.unwrap_or_else(|| todo!("type inference"));
//
// 					if matches!(*ty, ast::Type::Any | ast::Type::None) {
// 						return ReportKind::TypeError
// 							.title("Type 'any' and 'none' are not allowed as a function argument")
// 							.help(format!("Remove the arg, or change the type to '{}'", Type::Ptr(Type::Any.into())))
// 							.span(ty.span)
// 							.as_err();
// 					}
//
// 					let ty = convert_ast_ty(&ty.elem);
//
// 					let id = self.peek_scope_mut().new_id();
// 					self.peek_scope_mut().locals.push((id, n.elem.to_string(), ty.clone()));
// 					fargs.push((id, ty));
// 				}
//
// 				self.pop_scope();
//
// 				let id = self.peek_scope_mut().new_id();
//
// 				let export = export.map(|sym| self.symbols.insert(id, sym)).is_some();
//
// 				let blen = body.len();
// 				let mut fbody = Vec::with_capacity(blen);
// 				for (i, node) in body.into_iter().enumerate() {
// 					let span = node.span;
// 					let mut nodes = self.analyze_stmt(node)?;
//
// 					if let Some(Node::Ret(_, ty)) = nodes.last_mut() {
// 						if !cmp_ty(ty, &ret) {
// 							return Err(ReportKind::TypeError
// 								.title("Return type mismatch")
// 								.label(format!("expected '{ret}', found '{ty}'"))
// 								.span(span).into());
// 						}
//
// 						*ty = ret.clone();
// 					}
//
// 					if !matches!(ret, Type::None) && i == blen - 1 
// 						&& !matches!(nodes.last(), Some(Node::Ret(_, _)))  {
// 						return Err(ReportKind::TypeError
// 							.title("Return type mismatch")
// 							.label(format!("expected {ret}, found {}", Type::None))
// 							.span(span).into());
// 					}
//
// 					fbody.extend(nodes);
// 				}
//
// 				(Type::Fn(fargs.iter().map(|(_, t)| t).cloned().collect(), ret.clone().into()),
// 					vec![Node::Func { id, export, args: fargs, ret, body: fbody }],
// 					Var::Glob(id))
// 			},
// 			ast::Node::IntLit(v) => (Type::Puint, Vec::new(), Var::Imm(v.into())),
// 			ast::Node::StrLit(s) => {
// 				let id = self.get_global_mut().new_id();
//
// 				let ty = Type::Arr(Type::U(8).into(), Some(s.len() as u64));
// 				
// 				self.get_global_mut().locals.push((id, format!("__const{id:?}"), ty.clone()));
// 				(ty.clone(), 
// 					vec![Node::Global { ty, id, val: Node::StrLit(s).into(), }], 
// 					Var::Glob(id))
// 			},
// 			ast::Node::Ident(name) => {
// 				let (depth, (id, _, ty)) = self.find_matching_descending(|(_, n, _)| n == name)
// 					.ok_or_else(|| ReportKind::UndefinedSym
// 						.title(format!("'{name}' is not defined"))
// 						.span(node.span))?;
//
// 				(ty, Vec::new(), match depth {
// 					0 => Var::Glob(id),
// 					_ => Var::Local(id),
// 				})
// 			},
// 			ast::Node::FuncCall { lhs, args } => {
// 				let span = lhs.span;
// 				let (lhs_ty, lhs, lhs_val) = self.analyze_expr(*lhs)?;
//
// 				let mut nodes = Vec::with_capacity(args.len() + 2); // best case guess
// 				nodes.extend(lhs);
//
// 				// TODO: traits whatever stuff
// 				let Type::Fn(fn_args, fn_ret) = lhs_ty else {
// 					return Err(ReportKind::TypeError
// 						.title(format!("'{lhs_ty}' is not callable"))
// 						.span(span).into());
// 				};
//
// 				let mut fargs = Vec::with_capacity(args.len());
// 				for (inx, arg) in args.into_iter().enumerate() {
// 					let span = arg.span;
// 					let (arg_ty, arg_node, arg_val) = self.analyze_expr(arg)?;
//
// 					// TODO: default args and shit
// 					if inx >= fn_args.len() {
// 						return Err(ReportKind::InvalidArgCount
// 							.title(format!("Expected {} arguments, got {}", fn_args.len(), inx + 1))
// 							.span(span).into());
// 					}
//
// 					if !cmp_ty(&arg_ty, &fn_args[inx]) {
// 						return Err(ReportKind::TypeError
// 							.title("Type mismatch in function call")
// 							.label(format!("expected {}, found {}", fn_args[inx], arg_ty))
// 							.span(span).into());
// 					}
//
// 					nodes.extend(arg_node);
// 					fargs.push((arg_val, fn_args[inx].clone()));
// 				}
//
// 				nodes.push(Node::FuncCall { id: lhs_val, args: fargs });
//
// 				(*fn_ret, nodes, Var::Local(self.peek_scope_mut().new_id()))
// 			},
// 			// TODO: at some point we get rid of this. also, maybe get better printing for dbg?
// 			_ => todo!("{:?}", node.elem),
// 		})
// 	}
//
// 	// fn analyze_root(&mut self, node: Sp<ast::Node>) -> Result<Node> {
// 	// 	Ok(match node.elem {
// 	// 		ast::Node::Func { name, args, ret, attrs, body } => {
// 	// 			let id = self.peek_scope_mut().new_id();
// 	//
// 	// 			if attrs.iter().any(|a| matches!(**a, ast::Attrs::Export)) {
// 	// 				self.symbols.insert(id, name.elem.to_string());
// 	// 			}
// 	//
// 	//
// 	// 			let mut nargs = Vec::new();
// 	// 			let mut fargs = Vec::new();
// 	// 			for (n, ty) in args {
// 	// 				if matches!(*ty, ast::Type::Void) {
// 	// 					return ReportKind::TypeError
// 	// 						.title("Type 'void' is not allowed as a function argument")
// 	// 						.help("Remove the arg, or change the type to '*void'")
// 	// 						.span(ty.span)
// 	// 						.as_err();
// 	// 				}
// 	//
// 	// 				let ty = convert_ast_ty(&ty.elem);
// 	//
// 	// 				nargs.push(ty);
// 	// 			}
// 	//
// 	// 			let ret = ret.map_or(Type::Void, |t| convert_ast_ty(&t));
// 	// 			let ty = Type::Fn(nargs, Box::new(ret.clone()));
// 	//
// 	// 			let scope_len = self.scope.len();
// 	// 			self.scope.get_mut(scope_len - 2).unwrap()
// 	// 				.locals.push((id, name.elem.to_string(), ty));
// 	//
// 	// 			// TODO: try fold? 🥺👉👈
// 	// 			let mut nodes = Vec::new();
// 	// 			for node in body {
// 	// 				nodes.extend(self.analyze_stmt(node, &ret)?);
// 	// 			}
// 	//
// 	// 			for node in nodes.iter_mut().filter(|n| matches!(n, Node::Ret(_, _))) {
// 	// 				let &mut Node::Ret(_, ref mut t) = node 
// 	// 					else { unreachable!() };
// 	//
// 	// 				if !cmp_ty(&t, &ret) {
// 	// 					return ReportKind::TypeError
// 	// 						.title("Return type mismatch")
// 	// 						// .label(format!("expected '{ret}', found '{t}'"))
// 	// 						// .span(node.span) // TODO: span
// 	// 						.as_err();
// 	// 				}
// 	//
// 	// 				*t = ret.clone();
// 	// 			}
// 	//
// 	// 			self.pop_scope();
// 	// 			Node::Func {
// 	// 				id, ret, 
// 	// 				body:   nodes,
// 	// 				args:   fargs,
// 	// 				export: attrs.iter().any(|a| matches!(**a, ast::Attrs::Export)),
// 	// 			}
// 	// 		},
// 	// 		_ => todo!(),
// 	// 	})
// 	// }
//
// 	// fn analyze_stmt(&mut self, node: Sp<ast::Node>, ret: &Type) -> Result<Vec<Node>> {
// 	// 	Ok(match node.elem {
// 	// 		ast::Node::Ret(None) => vec![Node::Ret(None, Type::Void)],
// 	// 		ast::Node::Ret(Some(node)) => {
// 	// 			let (ty, n, v) = self.analyze_expr(*node)?;
// 	//
// 	// 			let mut nodes = n.map_or(Vec::new(), |n| vec![n]);
// 	//
// 	// 			nodes.push(Node::Ret(Some(v), ty));
// 	// 			nodes
// 	// 		},
// 	// 		ast::Node::FuncCall { name, args } => {
// 	// 			// TODO: do the whole overloading match thing
// 	// 			let (depth, (id, _, ty)) = self.find_matching_descending(|(_, n, _)| n == name.elem)
// 	// 				.ok_or_else(|| ReportKind::UndefinedSym
// 	// 					.title(format!("Function '{}' is not defined", *name))
// 	// 					.span(name.span))?;
// 	//
// 	// 			let Type::Fn(fn_args, fn_ret) = ty else {
// 	// 				return ReportKind::TypeError
// 	// 					.title(format!("'{}' is not callable", *name))
// 	// 					.help("consider changing the type to 'fn(...) ...'")
// 	// 					.span(name.span)
// 	// 					.as_err();
// 	// 			};
// 	//
// 	// 			if fn_args.len() != args.len() {
// 	// 				return ReportKind::InvalidArgCount
// 	// 					.title(format!("Expected {} arguments, got {}", fn_args.len(), args.len()))
// 	// 					.span(node.span)
// 	// 					.as_err();
// 	// 			}
// 	//
// 	// 			let id = match depth {
// 	// 				0 => Var::Glob(id),
// 	// 				_ => Var::Local(id),
// 	// 			};
// 	//
// 	// 			let mut nargs = Vec::new();
// 	// 			let mut nodes = Vec::new();
// 	// 			for (inx, arg) in args.into_iter().enumerate() {
// 	// 				let span = arg.span;
// 	// 				let (t, n, v) = self.analyze_expr(arg)?;
// 	//
// 	// 				if !cmp_ty(&t, &fn_args[inx]) {
// 	// 					return ReportKind::TypeError
// 	// 						.title("Type mismatch in function call")
// 	// 						.label(format!("expected '{}', found '{t}'", fn_args[inx]))
// 	// 						.span(span)
// 	// 						.as_err();
// 	// 				}
// 	//
// 	// 				n.map(|n| nodes.push(n));
// 	// 				nargs.push((v, fn_args[inx].clone()));
// 	// 			}
// 	//
// 	// 			let call = Node::FuncCall { id, args: nargs };
// 	//
// 	// 			nodes.push(match *fn_ret {
// 	// 				Type::Void => call,
// 	// 				_ => {
// 	// 					let retid = self.peek_scope_mut().new_id();
// 	// 					self.peek_scope_mut().locals.push((retid, "__ret".to_string(), (*fn_ret).clone()));
// 	// 					Node::Assign {
// 	// 						id:  retid,
// 	// 						ty:  (*fn_ret).clone(),
// 	// 						val: Box::new(call),
// 	// 					}
// 	// 				},
// 	// 			});
// 	//
// 	// 			nodes
// 	// 		},
// 	// 		ast::Node::Assign { name, ty, value } => {
// 	// 			let (t, n, v) = self.analyze_expr(*value)?;
// 	//
// 	// 			if matches!(*ty, ast::Type::Void) {
// 	// 				return ReportKind::TypeError
// 	// 					.title("Cannot assign as type 'void'")
// 	// 					.span(ty.span)
// 	// 					.as_err();
// 	// 			}
// 	//
// 	// 			let ty = convert_ast_ty(&ty.elem);
// 	//
// 	// 			if !cmp_ty(&t, &ty) {
// 	// 				return ReportKind::TypeError
// 	// 					.title("Type mismatch in assignment")
// 	// 					.label(format!("expected '{ty}', found '{t}'"))
// 	// 					.span(node.span)
// 	// 					.as_err();
// 	// 			}
// 	//
// 	// 			let id = self.peek_scope_mut().new_id();
// 	// 			self.peek_scope_mut().locals.push((id, name.elem.to_string(), ty.clone()));
// 	//
// 	// 			vec![Node::Assign {
// 	// 				id, ty, val: Box::new(n.unwrap_or(Node::Var(v))),
// 	// 			}]
// 	// 		},
// 	// 		ast::Node::Store { name, value } => {
// 	// 			let (t, n, v) = self.analyze_expr(*value)?;
// 	//
// 	// 			let (depth, (id, _, ty)) = self.find_matching_descending(|(_, n, _)| n == name.elem)
// 	// 				.ok_or_else(|| ReportKind::UndefinedSym
// 	// 					.title(format!("'{}' is not defined", *name))
// 	// 					.span(name.span))?;
// 	//
// 	// 			if !cmp_ty(&t, &ty) {
// 	// 				return ReportKind::TypeError
// 	// 					.title("Type mismatch in store")
// 	// 					.label(format!("expected '{ty}', found '{t}'"))
// 	// 					.span(node.span)
// 	// 					.as_err();
// 	// 			}
// 	//
// 	// 			if !matches!(ty, Type::Mut(_)) {
// 	// 				return ReportKind::TypeError
// 	// 					.title("Cannot store to immutable variable")
// 	// 					.help(format!("change the type to 'mut {ty}'"))
// 	// 					.span(node.span)
// 	// 					.as_err();
// 	// 			}
// 	//
// 	// 			let mut nodes = Vec::new();
// 	//
// 	// 			if !matches!(n, Some(Node::Var(_)) | None) {
// 	// 				let id = self.peek_scope_mut().new_id();
// 	// 				self.peek_scope_mut().locals.push((id, name.elem.to_string(), t.clone()));
// 	//
// 	// 				nodes.push(Node::Assign {
// 	// 					id, ty: t, val: Box::new(n.unwrap()),
// 	// 				});
// 	// 			}
// 	//
// 	// 			let id = match depth {
// 	// 				0 => Var::Glob(id),
// 	// 				_ => Var::Local(id),
// 	// 			};
// 	//
// 	// 			nodes.push(Node::Store { to: id, from: (v, ty) });
// 	// 			nodes
// 	// 		},
// 	// 		_ => todo!(),
// 	// 	})
// 	// }
// }
//
// fn convert_ast_ty(ty: &ast::Type) -> Type {
// 	match ty {
// 		ast::Type::U(n)  => Type::U(*n),
// 		ast::Type::I(n)  => Type::I(*n),
// 		ast::Type::B(n)  => Type::B(*n),
// 		ast::Type::F(n)  => Type::F(*n),
// 		ast::Type::Usize => Type::Usize,
// 		ast::Type::Isize => Type::Isize,
// 		ast::Type::None  => Type::None,
// 		ast::Type::Any   => Type::Any,
// 		ast::Type::Never => Type::Never,
// 		ast::Type::Ptr(ty)    => Type::Ptr(convert_ast_ty(ty).into()),
// 		ast::Type::Arr(ty, n) => Type::Arr(convert_ast_ty(ty).into(), *n),
// 		ast::Type::Mut(ty)    => Type::Mut(convert_ast_ty(ty).into()),
// 		ast::Type::Opt(ty)    => Type::Opt(convert_ast_ty(&ty).into()),
// 		ast::Type::Fn(args, ret) => Type::Fn(
// 			args.iter().map(|t| convert_ast_ty(&t)).collect(),
// 			Box::new(ret.as_ref().map_or(Type::None, |t| convert_ast_ty(t)))),
// 		ast::Type::Ident(_) => todo!("ident type"),
// 	}
// }
//
// // allow cause we check mut before placeholder types
// #[allow(clippy::match_same_arms)]
// fn cmp_ty(ty1: &Type, ty2: &Type) -> bool {
// 	match (ty1, ty2) {
// 		(Type::Any, _) | (_, Type::Any) => true,
// 		(_, Type::Mut(ty2) | Type::Opt(ty2)) => cmp_ty(ty1, ty2),
// 		(Type::Mut(ty1) | Type::Opt(ty1), _) => cmp_ty(ty1, ty2),
// 		(Type::Puint, _)  => matches!(ty2, Type::Puint  | Type::U(_) | Type::Usize | Type::I(_) | Type::F(_)),
// 		(Type::Pint, _)   => matches!(ty2, Type::Pint   | Type::I(_) | Type::Isize | Type::F(_)),
// 		(Type::Pbool, _)  => matches!(ty2, Type::Pbool  | Type::B(_)),
// 		(Type::Pfloat, _) => matches!(ty2, Type::Pfloat | Type::F(_)),
// 		(Type::Arr(_, _), Type::Ptr(ty2)) => cmp_ty(ty1, ty2),
// 		(Type::Arr(ty1, _), Type::Arr(ty2, None)) => cmp_ty(ty1, ty2),
// 		_ => ty1 == ty2,
// 	}
// }
