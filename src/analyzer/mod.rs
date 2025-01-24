use std::collections::HashMap;

use crate::report::{Result, ReportKind, LogHandler};
use crate::span::Sp;
use crate::parser::ast;

pub mod mir;
use mir::{Node, ValId, Var, Type};

pub type Name = String;

pub type AnalyzedOut = (Vec<Node>, HashMap<ValId, Name>);

#[derive(Default)]
pub struct Analyzer {
	scope:   Vec<Scope>,
	symbols: HashMap<ValId, String>,
}

#[derive(Default)]
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

	pub fn analyze(ast: Vec<Sp<ast::Node>>, file: &'static str, handler: &LogHandler) -> AnalyzedOut {
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
				if !body.is_empty() {
					return ReportKind::SyntaxError // TODO: maybe not SyntaxError
						.title("Extern functions cannot have a body")
						.span(node.span)
						.as_err();
				}

				if attrs.iter().any(|a| matches!(**a, ast::Attrs::Export)) {
					return ReportKind::SyntaxError
						.title("Extern functions cannot be exported")
						.span(node.span)
						.as_err();
				}

				let id = self.peek_scope_mut().new_id();
				self.symbols.insert(id, name.elem.to_string());

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

				let ret = ret.map_or(Type::Void, |t| convert_ast_ty(&t));

				let ty = Type::Fn(nargs, Box::new(ret.clone()));
				self.peek_scope_mut().locals.push((id, name.elem.to_string(), ty));

				Node::FuncDecl {
					id, ret,
					args: fargs,
				}
			},
			ast::Node::Func { name, args, ret, attrs, body } => {
				let id = self.peek_scope_mut().new_id();

				if attrs.iter().any(|a| matches!(**a, ast::Attrs::Export)) {
					self.symbols.insert(id, name.elem.to_string());
				}

				self.push_new_scope();

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

					let id = self.peek_scope_mut().new_id();
					self.peek_scope_mut().locals.push((id, n.elem.to_string(), ty.clone()));
					fargs.push((id, ty.clone()));
					nargs.push(ty);
				}

				let ret = ret.map_or(Type::Void, |t| convert_ast_ty(&t));
				let ty = Type::Fn(nargs, Box::new(ret.clone()));

				let scope_len = self.scope.len();
				self.scope.get_mut(scope_len - 2).unwrap()
					.locals.push((id, name.elem.to_string(), ty));

				// TODO: try fold? 🥺👉👈
				let mut nodes = Vec::new();
				for node in body {
					nodes.extend(self.analyze_stmt(node, &ret)?);
				}

				for node in nodes.iter_mut().filter(|n| matches!(n, Node::Ret(_, _))) {
					let Node::Ret(_, ref mut t) = node 
						else { unreachable!() };

					if !cmp_ty(&t, &ret) {
						return ReportKind::TypeError
							.title("Type mismatch in function call")
							//.span(span)
							.as_err();
					}

					*t = ret.clone();
				}

				Node::Func {
					id, ret, 
					body:   nodes,
					args:   fargs,
					export: attrs.iter().any(|a| matches!(**a, ast::Attrs::Export)),
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
					.ok_or_else(|| ReportKind::Undefined
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

				self.pop_scope();
				nodes
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

// linter doesnt detect ty2 being used cause its always in a matches!
#[allow(unused_variables)]
fn cmp_ty(ty1: &Type, ty2: &Type) -> bool {
	match ty1 {
		Type::Puint  => matches!(ty2, Type::Puint  | Type::U(_) | Type::Usize | Type::I(_) | Type::F(_)),
		Type::Pint   => matches!(ty2, Type::Pint   | Type::I(_) | Type::Isize | Type::F(_)),
		Type::Pbool  => matches!(ty2, Type::Pbool  | Type::B(_)),
		Type::Pfloat => matches!(ty2, Type::Pfloat | Type::F(_)),
		_ => matches!(ty1, ty2),
	}
}
