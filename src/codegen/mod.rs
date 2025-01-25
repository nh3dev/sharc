use std::collections::HashMap;

use crate::report::{Result, LogHandler, ReportKind};
use crate::analyzer::mir::{self, ValId, Var, Node, Type as mType};

mod llvm;
use llvm::{Instr, Val, TypedVal, ValKind, Module, DataDef, DataAttr, FuncAttr, Type};

pub struct Gen {
	module: Module,
	sym:    HashMap<ValId, String>,
}

impl Gen {
	pub fn codegen(
		file: &'static str, sym: HashMap<ValId, String>, 
		mir: Vec<Node>, handler: &LogHandler) 
	-> Module {
		let mut cgen = Self {
			sym,
			module: Module::default(),
		};

		mir.into_iter().for_each(|node|
			if let Err(e) = cgen.gen_node(node) {
				handler.log(e.file(file));
			});

		cgen.module
	}

	fn get_id_name(&self, id: mir::ValId) -> &str {
		self.sym.get(&id).expect("ValId not found in sym table")
	}

	fn gen_node(&mut self, node: Node) -> Result<()> {
		match node {
			Node::Func { id, export, args, ret, body } => {
				let func = llvm::Function {
					attr: Vec::new(),
					name: match export {
						true  => self.get_id_name(id).to_string(),
						false => format!("__glb{}", *id),
					},
					args: {
						let mut nargs = Vec::new();
						for (i, t) in args {
							nargs.push((gen_type(&t)?, self.get_id_name(i).to_string()));
						}
						nargs
					},
					ret:  gen_type(&ret)?,
					body: body.into_iter().filter_map(|stmt| self.gen_stmt(stmt).transpose())
						.collect::<Result<Vec<_>>>()?,
				};

				self.module.funcs.push(func);
			},
			Node::FuncDecl { id, args, ret } => {
				let func = llvm::FuncDecl {
					attr: Vec::new(),
					name: self.get_id_name(id).to_string(),
					args: args.into_iter().map(|t| gen_type(&t)).collect::<Result<Vec<_>>>()?,
					ret:  gen_type(&ret)?,
				};

				self.module.decls.push(func);
			},
			_ => todo!(),
		}
		Ok(())
	}

	fn gen_stmt(&mut self, node: Node) -> Result<Option<Instr>> {
		Ok(Some(match node {
			// FIXME: Very questionable cabbaging.
			Node::Assign { id, ty, val } => Instr::Assign(Val(ValKind::Local, format!("__tmp{}", *id)), match *val {
				Node::FuncCall { id, args } => self.gen_fncall(&id, args, gen_type(&ty)?)?.into(),
				_ => self.gen_stmt(*val)?.expect("pretty sure SA guarantees this").into(),
			}),
			Node::Global { id, ty, val } => {
				let Some(Instr::Val(Val(kind, name))) = self.gen_stmt(*val)?
					else { unreachable!() };

				let data = llvm::DataDef {
					value: TypedVal(gen_type(&ty)?, kind, name),
					name: format!("__glb{}", *id),
					attr: vec![DataAttr::Internal, DataAttr::Global], // TODO: global var attrs
				};

				self.module.data.push(data);
				return Ok(None);
			},
			Node::Ret(None, ty)    => Instr::Ret(None, gen_type(&ty)?), // realistically this is only ever void
			Node::Ret(Some(v), ty) => Instr::Ret(Some(self.gen_val(&v)), gen_type(&ty)?),
			Node::FuncCall { id, args } => self.gen_fncall(&id, args, Type::Void)?,
			Node::Var(v)    => Instr::Val(self.gen_val(&v)),
			Node::StrLit(l) => Instr::Val(Val(ValKind::Str, l)),
			_ => todo!(),
		}))
	}

	fn gen_fncall(&self, var: &Var, args: Vec<(Var, mType)>, ret: Type) -> Result<Instr> {
		Ok(Instr::Call {
			func: {
				let Val(kind, name) = self.gen_val(var);
				TypedVal(ret, kind, name)
			},
			args: {
				let mut nargs = Vec::new();
				for (var, ty) in args {
					let Val(kind, name) = self.gen_val(&var);
					nargs.push(TypedVal(gen_type(&ty)?, kind, name));
				}
				nargs
			},
		})
	}

	fn gen_val(&self, v: &mir::Var) -> Val {
		match v {
			Var::Imm(v)    => Val(ValKind::Const, v.to_string()),
			Var::Local(id) => Val(ValKind::Local, format!("__tmp{id}")),
			Var::Glob(id) if self.sym.contains_key(id) 
				=> Val(ValKind::Global, self.get_id_name(*id).to_string()),
			Var::Glob(id)  => Val(ValKind::Global, format!("__glb{}", *id)),
		}
	}
}

fn gen_type(ty: &mType) -> Result<Type> {
	Ok(match &ty {
		mType::U(i) | mType::B(i) | mType::I(i) => Type::Int(*i),

		mType::F(16)  => Type::F16,
		mType::F(32)  => Type::F32,
		mType::F(64)  => Type::F64,
		mType::F(128) => Type::F128,
		mType::F(_) => return ReportKind::TypeError
			.title("Unsuported bit width for float")
			// TODO: span on type mir
			.as_err(),

		mType::Void | mType::Never => Type::Void,

		mType::Opt(ty) | mType::Mut(ty) => return gen_type(ty),
		mType::Ptr(_)    => Type::Ptr,
		mType::Arr(t, Some(n)) => Type::Array(usize::try_from(*n).unwrap(), Box::new(gen_type(t)?)),
		mType::Arr(_, _) => panic!("Stack arrays are not yet supported. Heap allocate instead."),
		_ => unreachable!()
	})
}
