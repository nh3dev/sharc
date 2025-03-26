use std::collections::HashMap;

use crate::report::{Result, ReportKind, LogHandler};
use crate::span::Sp;
use crate::parser::ast;

pub mod mir;
use mir::{Node, ValId, Var, Type};

#[derive(Default)]
pub struct Analyzer {
	scope:   Vec<Scope>,
	symbols: HashMap<ValId, String>,
}

#[derive(Default, Debug)]
struct Scope {
	idacc:  ValId,
	locals: Vec<(ValId, String, Type)>, 
}

impl Scope {
	fn new_id(&mut self) -> ValId {
		self.idacc.0 += 1;
		self.idacc
	}
}

impl Analyzer {
	#[inline]
	fn push_new_scope(&mut self) {
		self.scope.push(Scope::default());
	}

	#[inline]
	fn pop_scope(&mut self) {
		self.scope.pop();
	}

	#[inline]
	fn peek_scope_mut(&mut self) -> &mut Scope {
		self.scope.last_mut().unwrap()
	}

	#[inline]
	fn peek_scope(&self) -> &Scope {
		self.scope.last().unwrap()
	}

	#[inline]
	fn get_global_mut(&mut self) -> &mut Scope {
		self.scope.first_mut().unwrap()
	}

	#[inline]
	fn get_global(&self) -> &Scope {
		self.scope.first().unwrap()
	}

	// TODO: mayhaps dont clone
	fn find_matching_descending<F: Fn((ValId, &str, &Type)) -> bool>(&self, f: F) 
		-> Option<(usize, (ValId, String, Type))> {
		self.scope.iter().enumerate().rev().find_map(|(d, scope)|
			scope.locals.iter().rev().find(|(i,n,t)| f((*i,n,t))).cloned().map(|v| (d, v)))
	}

	pub fn analyze(ast: Vec<Sp<ast::Node>>, file: &'static str, handler: &LogHandler) -> (Vec<Node>, HashMap<ValId, String>) {
		let mut analyzer = Self {
			scope: vec![Scope::default()],
			symbols: HashMap::new(),
		};

		(ast.into_iter().fold(Vec::new(), |mut acc, node| {
			match analyzer.analyze_root(node) {
				Ok(n)  => acc.push(n),
				Err(e) => {
					handler.log(e.file(file));
					analyzer.scope.truncate(1);
				},
			}; acc
		}), analyzer.symbols)
	}

	fn analyze_root(&mut self, node: Sp<ast::Node>) -> Result<Node> {
		Ok(match node.elem {
			ast::Node::Func { name, args, ret, attrs, body } 
				if attrs.iter().any(|a| matches!(**a, ast::Attrs::Extern)) => {
				// Extern functions cannot have a body, so we return an error if one exists.
				if !body.is_empty() {
					return ReportKind::SyntaxError
						.title("Extern functions cannot have a body")
						.span(node.span)
						.as_err();
				}
	
				// Extern functions cannot be exported, so we return an error if exported.
				if attrs.iter().any(|a| matches!(**a, ast::Attrs::Export)) {
					return ReportKind::SyntaxError
						.title("Extern functions cannot be exported")
						.span(node.span)
						.as_err();
				}
	
				// Generate a new identifier for the function and add it to the symbols table.
				let id = self.peek_scope_mut().new_id();
				self.symbols.insert(id, name.elem.to_string());
	
				// Process function arguments and ensure they are valid.
				let mut nargs = Vec::new();
				let mut fargs = Vec::new();
				for (_, ty) in args {
					if matches!(*ty, ast::Type::Void) {
						return ReportKind::TypeError
							.title("Type 'void' is not allowed as a function argument")
							.help("Remove the arg, or change the type to '*void'")
							.span(ty.span)
							.as_err();
					}
	
					let ty = convert_ast_ty(&ty.elem);
					fargs.push(ty.clone());
					nargs.push(ty);
				}
	
				// Determine the return type of the function.
				let ret = ret.map_or(Type::Void, |t| convert_ast_ty(&t));
	
				// Add the function type to the local scope.
				let ty = Type::Fn(nargs, Box::new(ret.clone()));
				self.peek_scope_mut().locals.push((id, name.elem.to_string(), ty));
	
				// Return a function declaration node.
				Node::FuncDecl {
					id,
					ret,
					args: fargs,
				}
			},
			ast::Node::Func { name, args, ret, attrs, body } => {
				// Generate a new identifier for the function.
				let id = self.peek_scope_mut().new_id();
	
				// If the function is exported, add it to the symbols table.
				if attrs.iter().any(|a| matches!(**a, ast::Attrs::Export)) {
					self.symbols.insert(id, name.elem.to_string());
				}
	
				// Push a new scope for analysing the function body.
				self.push_new_scope();
	
				// Process function arguments and ensure they are valid.
				let mut nargs = Vec::new();
				let mut fargs = Vec::new();
				for (n, ty) in args {
					if matches!(*ty, ast::Type::Void) {
						return ReportKind::TypeError
							.title("Type 'void' is not allowed as a function argument")
							.help("Remove the arg, or change the type to '*void'")
							.span(ty.span)
							.as_err();
					}
	
					let ty = convert_ast_ty(&ty.elem);
	
					// Add each argument to the local scope with its identifier and type.
					let id = self.peek_scope_mut().new_id();
					self.peek_scope_mut().locals.push((id, n.elem.to_string(), ty.clone()));
					fargs.push((id, ty.clone()));
					nargs.push(ty);
				}
	
				// Determine the return type of the function.
				let ret = ret.map_or(Type::Void, |t| convert_ast_ty(&t));
				let ty = Type::Fn(nargs, Box::new(ret.clone()));
	
				// Add the function type to its parent scope (one level above).
				let scope_len = self.scope.len();
				self.scope.get_mut(scope_len - 2).unwrap()
					.locals.push((id, name.elem.to_string(), ty));
	
				// Analyse each statement in the body of the function.
				let mut nodes = Vec::new();
				for node in body {
					nodes.extend(self.analyze_stmt(node, &ret)?);
				}
	
				// Ensure that all return statements match the declared return type.
				for node in nodes.iter_mut().filter(|n| matches!(n, Node::Ret(_, _))) {
					let Node::Ret(_, ref mut t) = node 
						else { unreachable!() };
	
					if !cmp_ty(&t, &ret) {
						return ReportKind::TypeError
							.title("Return type mismatch")
							.as_err();
					}
	
					*t = ret.clone();
				}
	
				// Pop the current scope after analysing the function body.
				self.pop_scope();
	
				// Return a function node without an 'export' field (removed earlier).
				Node::Func {
					id,
					ret,
					body: nodes,
					args: fargs,
				}
			},
			_ => todo!(),
		})
	}		

	fn analyze_stmt(&mut self, node: Sp<ast::Node>, ret: &Type) -> Result<Vec<Node>> {
		Ok(match node.elem {
			ast::Node::Ret(None) => vec![Node::Ret(None, Type::Void)],
			ast::Node::Ret(Some(node)) => {
				let (ty, n, v) = self.analyze_expr(*node)?;

				let mut nodes = n.map_or(Vec::new(), |n| vec![n]);

				nodes.push(Node::Ret(Some(v), ty));
				nodes
			},
			ast::Node::FuncCall { name, args } => {
				// TODO: do the whole overloading match thing
				let (depth, (id, _, ty)) = self.find_matching_descending(|(_, n, _)| n == name.elem)
					.ok_or_else(|| ReportKind::UndefinedSym
						.title(format!("Function '{}' is not defined", *name))
						.span(name.span))?;

				let Type::Fn(fn_args, fn_ret) = ty else {
					return ReportKind::TypeError
						.title(format!("'{}' is not callable", *name))
						.help("consider changing the type to 'fn(...) ...'")
						.span(name.span)
						.as_err();
				};

				if fn_args.len() != args.len() {
					return ReportKind::InvalidArgCount
						.title(format!("Expected {} arguments, got {}", fn_args.len(), args.len()))
						.span(node.span)
						.as_err();
				}

				let id = match depth {
					0 => Var::Glob(id),
					_ => Var::Local(id),
				};

				let mut nargs = Vec::new();
				let mut nodes = Vec::new();
				for (inx, arg) in args.into_iter().enumerate() {
					let span = arg.span;
					let (t, n, v) = self.analyze_expr(arg)?;

					if !cmp_ty(&t, &fn_args[inx]) {
						return ReportKind::TypeError
							.title("Type mismatch in function call")
							.label(format!("expected '{}', found '{t}'", fn_args[inx]))
							.span(span)
							.as_err();
					}

					n.map(|n| nodes.push(n));
					nargs.push((v, fn_args[inx].clone()));
				}

				let call = Node::FuncCall { id, args: nargs };

				nodes.push(match *fn_ret {
					Type::Void => call,
					_ => {
						let retid = self.peek_scope_mut().new_id();
						self.peek_scope_mut().locals.push((retid, "__ret".to_string(), (*fn_ret).clone()));
						Node::Assign {
							id:  retid,
							ty:  (*fn_ret).clone(),
							val: Box::new(call),
						}
					},
				});

				nodes
			},
			ast::Node::Assign { name, ty, value } => {
				let (t, n, v) = self.analyze_expr(*value)?;

				if matches!(*ty, ast::Type::Void) {
					return ReportKind::TypeError
						.title("Cannot assign as type 'void'")
						.span(ty.span)
						.as_err();
				}

				let ty = convert_ast_ty(&ty.elem);

				if !cmp_ty(&t, &ty) {
					return ReportKind::TypeError
						.title("Type mismatch in assignment")
						.label(format!("expected '{ty}', found '{t}'"))
						.span(node.span)
						.as_err();
				}

				let id = self.peek_scope_mut().new_id();
				self.peek_scope_mut().locals.push((id, name.elem.to_string(), ty.clone()));

				vec![Node::Assign {
					id, ty, val: Box::new(n.unwrap_or(Node::Var(v))),
				}]
			},
			_ => todo!(),
		})
	}

	fn analyze_expr(&mut self, node: Sp<ast::Node>) -> Result<(Type, Option<Node>, Var)> {
		Ok(match node.elem {
			ast::Node::StrLit(s) => {
				let id = self.get_global_mut().new_id();

				let ty = Type::Arr(Type::U(8).into(), Some(s.len() as u64));
				
				self.get_global_mut().locals.push((id, format!("__const{id:?}"), ty.clone()));
				(ty.clone(), Some(Node::Global { 
					ty, id, 
					val: Node::StrLit(s).into(),
				}), Var::Glob(id))
			},
			ast::Node::UIntLit(v) => (Type::Puint, None, Var::Imm(v)),
			ast::Node::SIntLit(v) => (Type::Pint,  None, Var::Imm(v)),
			ast::Node::Ident(name) => {
				let (depth, (id, _, ty)) = self.find_matching_descending(|(_, n, _)| n == name)
					.ok_or_else(|| ReportKind::UndefinedSym
						.title(format!("'{name}' is not defined"))
						.span(node.span))?;

				(ty, None, match depth {
					0 => Var::Glob(id),
					_ => Var::Local(id),
				})
			},
			_ => todo!(),
		})
	}
}

fn convert_ast_ty(ty: &ast::Type) -> Type {
	match ty {
		ast::Type::U(n)  => Type::U(*n),
		ast::Type::I(n)  => Type::I(*n),
		ast::Type::B(n)  => Type::B(*n),
		ast::Type::F(n)  => Type::F(*n),
		ast::Type::Usize => Type::Usize,
		ast::Type::Isize => Type::Isize,
		ast::Type::Void  => Type::Void,
		ast::Type::Never => Type::Never,
		ast::Type::Ptr(ty)    => Type::Ptr(convert_ast_ty(ty).into()),
		ast::Type::Arr(ty, n) => Type::Arr(convert_ast_ty(ty).into(), *n),
		ast::Type::Mut(ty)    => Type::Mut(convert_ast_ty(ty).into()),
		ast::Type::Opt(ty)    => Type::Opt(convert_ast_ty(&ty).into()),
		ast::Type::Fn(args, ret) => Type::Fn(
			args.iter().map(|t| convert_ast_ty(&t)).collect(),
			Box::new(ret.as_ref().map_or(Type::Void, |t| convert_ast_ty(t)))),
		ast::Type::Ident(_) => unimplemented!("ident"),
	}
}

fn cmp_ty(ty1: &Type, ty2: &Type) -> bool {
	match (ty1, ty2) {
		(Type::Puint, _)  => matches!(ty2, Type::Puint  | Type::U(_) | Type::Usize | Type::I(_) | Type::F(_)),
		(Type::Pint, _)   => matches!(ty2, Type::Pint   | Type::I(_) | Type::Isize | Type::F(_)),
		(Type::Pbool, _)  => matches!(ty2, Type::Pbool  | Type::B(_)),
		(Type::Pfloat, _) => matches!(ty2, Type::Pfloat | Type::F(_)),
		(Type::Arr(_, _), Type::Ptr(ty2)) => cmp_ty(ty1, ty2),
		(Type::Arr(ty1, _), Type::Arr(ty2, None)) => cmp_ty(ty1, ty2),
		_ => ty1 == ty2,
	}
}
