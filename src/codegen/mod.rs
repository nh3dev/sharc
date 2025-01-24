use std::collections::HashMap;

use crate::analyzer::{self, AnalyzedOut};
use crate::span::Sp;
use crate::report::{Result, LogHandler, ReportKind};
use crate::analyzer::mir::{self, Type as mType};

mod llvm;
use llvm::{Module, Type as lType};

struct Gen {
	module: Module,
	names: HashMap<mir::ValId, analyzer::Name>
}

impl<'src> Gen {
	pub fn codegen(file: &'static str, (mirs, names): AnalyzedOut, handler: &LogHandler) -> Module {
		let mut cgen = Gen {
			module: Module::default(),
			names
		};

		mirs.iter().for_each(|global|
			if let Err(e) = cgen.gen_global(global) {
				handler.log(e.file(file));
		});

		cgen.module
	}

	fn gen_global(&mut self, x_mir: &mir::Node) -> Result<()> {
		match x_mir {
			// NOTE:
			// This branch can be removed. Exists just as a reference for writing codegen
			// mir::Node::Func { id, args, ret, export, .. } => {
			// 	let decl = llvm::FuncDecl {
			// 		name: map_valid_to_name(id),
			// 		attr: if *export {vec![llvm::FuncAttr::Export]} else {vec![]},
			// 		args: args.into_iter().map(|(_, ty)| Self::gen_type(&ty).unwrap()).collect(),
			// 		ret:  Self::gen_type(ret).unwrap(),
			// 	};
			//
			// 	self.module.decls.push(decl);
			// },
			mir::Node::Func { id, export, args, ret, body } => {
				let ret = Self::gen_type(ret);

				let out_args = args
					.iter()
					.map(|(a_id, ty)| (
						Self::gen_type(&ty).unwrap(),
						// FIXME: Maybe don't `clone`
						self.get_id_name(a_id).clone()
					)).collect();

				// let mut attr = FuncAttr::empty();
				// if attrs.iter().any(|a| matches!(&**a, Attrs::Export)) 
				// 	{ attr |= FuncAttr::EXPORT; }

				let func = llvm::Function {
					ret: ret.expect("Return type should be `Some`."),
					attr: if *export {vec![llvm::FuncAttr::Export]} else {vec![]},
					name: self.get_id_name(id).clone(),
					args: out_args,
					body: body.iter().map(|stmt| self.gen_stmt(stmt)).collect(),
				};

				self.module.funcs.push(func);
			},
			_ => todo!(),
		}
		Ok(())
	}

	fn gen_stmt(&mut self, x_mir: &mir::Node) -> llvm::Instr {
		match x_mir {
			// FIXME: Very questionable cabbaging.
			mir::Node::Assign { id, ty, val } => {
				let llvm_instr = match &**val {
					mir::Node::FuncCall { id, args } => 
					llvm::Instr::Call {
						func: self.f0(id).to_typed(Self::gen_type(ty).unwrap()),
						args: args.iter().map(|(v_id, v_ty)| self.f0(v_id).to_typed(Self::gen_type(v_ty).unwrap())).collect()
					},
					mir::Node::Var(v) => llvm::Instr::Val(self.f0(v).to_typed(Self::gen_type(ty).unwrap())),
					_ => panic!("Invalid assignment for `{id:?} : {ty}`")
				};
				llvm::Instr::Assign(llvm::Val(llvm::ValKind::Local, self.get_id_name(id).clone()), Box::new(llvm_instr))
				//Instr::Assign(Value::Temp(name.elem.to_string()), self.gen_type(&ty)?.expect("todo: implicit type"), Box::new(self.gen_stmt(*value)?))
			},
			mir::Node::Ret(None, _)       => llvm::Instr::Ret(None),
			mir::Node::Ret(Some(v), ty) => {
				llvm::Instr::Ret(Some(self.f0(v).to_typed(Self::gen_type(ty).unwrap())))
			},

			// FIXME: move this mess to semantic analysis, that should tell us which func to call
			mir::Node::FuncCall { name, args } => Instr::Call {
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
		}
	}

	fn gen_atom() -> llvm::Val {
		todo!()
	}

	fn gen_expr(ast: &mir::Node) -> lTypedVal {
		match &ast {
			mir::Node::UIntLit(n) => llvm::Val::new(ValKind::Const(*n), Type::Ptr),
			mir::Node::StrLit(s)  => {
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

	fn gen_type(ty: &mir::Type) -> Option<lType> {
		match &ty {
			mType::U(i) | mType::B(i) | mType::I(i) => Some(lType::Int(*i)),
			mType::F(i) => Some(match i {
				16 => lType::F16,
				32 => lType::F32,
				64 => lType::F64,
				128 => lType::F128,
				_ => unreachable!()
			}),

			mType::Void | mType::Never => None,

			mType::Opt(ty) | mType::Mut(ty) => Self::gen_type(&*ty),
			mType::Ptr(_) => Some(lType::Ptr),
			mType::Arr(_, _) => panic!("Stack arrays are not yet supported. Heap allocate instead."),
			_ => panic!("Unexpected ast::Type when generating an lType.")
		}
	}

	fn f0(&self, v: &mir::Var) -> llvm::Val {
		match v {
    mir::Var::Imm(_) => todo!(),
    mir::Var::Local(id) => llvm::Val(llvm::ValKind::Local, self.get_id_name(id).clone()),
    mir::Var::Glob(id) => llvm::Val(llvm::ValKind::Global, self.get_id_name(id).clone()),
}
	}

	fn get_id_name(&self, id: &mir::ValId) -> &String {
		self.names.get(id).expect("ValId `{id}` was expected to have a name.")
	}
}
