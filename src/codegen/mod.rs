use std::collections::HashMap;

use crate::span::Sp;
use crate::parser::ast::{self, Attrs, Node, Type as aType};
use crate::report::{Result, LogHandler, ReportKind};
use crate::analyzer::hir;

mod llvm;
use llvm::{FuncAttr, Module, TypedVal};

struct Gen {
	module: Module,
}

fn map_valid_to_name(id: &hir::ValId) -> String {
	//TODO:

	id.to_string()
}

impl<'src> Gen {
	pub fn codegen(file: &'static str, hirs: Vec<hir::Node<'src>>, handler: &LogHandler) -> Module {
		let mut cgen = Gen {
			module: Module::default(),
		};

		hirs.iter().for_each(|global|
			if let Err(e) = cgen.gen_global(global) {
				handler.log(e.file(file));
		});

		cgen.module
	}

	fn gen_global(&mut self, x_hir: &hir::Node<'src>) -> Result<()> {
		match x_hir {
			hir::Node::Func { id, args, ret, export, .. } => {
				let decl = llvm::FuncDecl {
					name: map_valid_to_name(id),
					attr: if *export {vec![llvm::FuncAttr::Export]} else {vec![]},
					args: args.into_iter().map(|(_, ty)| Self::gen_type(&ty).unwrap()).collect(),
					ret:  Self::gen_type(ret),
				};

				self.module.decls.push(decl);
			},
			hir::Node::Func { id, export, args, ret, body } => {
				let ret = Self::gen_type(ret);

				let out_args = args
					.iter()
					.map(|(a_id, ty)| (
						Self::gen_type(&ty).unwrap(),
						map_valid_to_name(a_id)
					)).collect();

				// let mut attr = FuncAttr::empty();
				// if attrs.iter().any(|a| matches!(&**a, Attrs::Export)) 
				// 	{ attr |= FuncAttr::EXPORT; }

				let func = llvm::Function {
					ret,
					attr: if *export {vec![FuncAttr::Export]} else {vec![]},
					name: map_valid_to_name(id),
					args: out_args,
					body: body.iter().map(|stmt| Self::gen_stmt(stmt)).collect(),
				};

				self.module.funcs.push(func);
			},
			_ => todo!(),
		}
		Ok(())
	}

	fn gen_stmt(&mut self, x_hir: hir::Node<'src>) -> llvm::Instr {
		Ok(match x_hir {
			hir::Node::Assign { name, ty, value } => {
				let val = match value {
						hir::Var::Local(id) => ()),
						hir::Var::Glob(id) => (),
						hir::Var::Imm(_) => (),
				};
				llvm::Instr::Assign(llvm::Val(llvm::ValKind::Temp, map_valid_to_name(&name)), Box::new())
				//Instr::Assign(Value::Temp(name.elem.to_string()), self.gen_type(&ty)?.expect("todo: implicit type"), Box::new(self.gen_stmt(*value)?))
			},
			hir::Node::Ret(None)       => Instr::Ret(None),
			hir::Node::Ret(Some(expr)) => {
				let mut val = self.gen_expr(&expr)?;
				val.typ = self.peek_scope().ret.clone();
				Instr::Ret(Some(val))
			},

			// FIXME: move this mess to semantic analysis, that should tell us which func to call
			hir::Node::FuncCall { name, args } => Instr::Call {
				func: match self.peek_scope().locals.get(&ValKind::Temp(name.elem.to_string())) {
					Some((i, t)) => Val::new(ValKind::Temp(i.to_string()), self.gen_type(t)?),
					None => match self.get_global().locals.get(&ValKind::Global(name.elem.to_string())) {
						Some((i, t)) => Val::new(ValKind::Global(i.to_string()), self.gen_type(t)?),
						None => match self.module.decls.iter().find(|decl| decl.name == name.elem) {
							Some(FuncDecl { name, ret, .. }) => Val::new(ValKind::Global(String::from(*name)), ret.clone()),
							None => return ReportKind::Undefined
								.title("Call to an undefined function")
								.span(name.span).as_err(),
						},
					},
				},
				args: args.into_iter().map(|arg| self.gen_expr(&arg)).collect::<Result<_>>()?,
			},
			_ => panic!("GOT: {ast}"),
		})
	}

	fn gen_atom() -> llvm::Val {
		todo!()
	}

	fn gen_expr(ast: &hir::Node<'src>) -> llvm::TypedVal {
		match &ast {
			hir::Node::UIntLit(n) => llvm::Val::new(ValKind::Const(*n), Type::Ptr),
			hir::Node::StrLit(s)  => {
				// TODO: prevent user from naming shit like this
				let val = ValKind::Global(format!("__tmp{}", self.gen_id()));

				self.module.data.push(DataDef {
					name:   val.clone(),
					attr:   DataAttr::INTERNAL | DataAttr::CONSTANT,
					value:  Val::new(ValKind::Str(s.clone()), Type::Array(s.len(), Box::new(Type::Int(8)))),
				});

				Val::new(val, Type::Ptr)
			},
			_ => todo!(),
		}
	}

	fn gen_type(ty: &aType) -> Option<llvm::Type> {
		match &ty {
			aType::U(i) | aType::B(i) | aType::I(i) => Some(llvm::Type::Int(*i)),
			aType::F(i) => Some(match i {
				16 => llvm::Type::F16,
				32 => llvm::Type::F32,
				64 => llvm::Type::F64,
				128 => llvm::Type::F128,
				_ => unreachable!()
			}),

			aType::Void | aType::Never => None,

			aType::Opt(ty) | aType::Mut(ty) => Self::gen_type(&ty.elem),
			aType::Ptr(_) => Some(llvm::Type::Ptr),
			aType::Arr(_, _) => panic!("Stack arrays are not yet supported. Heap allocate instead."),
			_ => panic!("Unexpected ast::Type when generating an llvm::Type.")
		}
	}
}
