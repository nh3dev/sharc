use std::collections::HashMap;

use crate::report::{Result, LogHandler, ReportKind};
use crate::analyzer::mir::{self, ValId, Var, Node, Type as mType};

mod llvm;
use llvm::{Instr, Val, TypedVal, ValKind, Module, DataAttr, Type};

pub struct Gen {
	module: Module,
	sym:    HashMap<ValId, String>,
	ucnt:   u64,
}

impl Gen {
	pub fn codegen(
		file: &'static str, sym: HashMap<ValId, String>, 
		mir: Vec<Node>, handler: &LogHandler) 
	-> Module {
		let mut cgen = Self {
			sym, ucnt: 0,
			module: Module::default(),
		};

		mir.into_iter().for_each(|node|
			if let Err(e) = cgen.gen_node(node) {
				handler.log(e.file(file));
			});

		cgen.module
	}

	fn inc_ucnt(&mut self) -> u64 {
		let ucnt = self.ucnt;
		self.ucnt += 1;
		ucnt
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
						false => format!("g{}", *id),
					},
					args: {
						let mut nargs = Vec::new();
						for (i, t) in args {
							nargs.push((gen_type(&t, false)?, self.get_id_name(i).to_string()));
						}
						nargs
					},
					ret: gen_type(&ret, false)?,
					body: {
						let mut nbody = Vec::new();
						for stmt in body {
							nbody.extend(self.gen_stmt(stmt)?);
						}
						nbody
					},
				};

				self.module.funcs.push(func);
			},
			Node::FuncDecl { id, ty } => {
				let mType::Fn(args, ret) = ty
					else { return ReportKind::TypeError
						.title("Expected function type for function declaration")
						.as_err() };

				let func = llvm::FuncDecl {
					attr: Vec::new(),
					name: self.get_id_name(id).to_string(),
					args: args.into_iter().map(|t| gen_type(&t, false)).collect::<Result<Vec<_>>>()?,
					ret:  gen_type(&ret, false)?,
				};

				self.module.decls.push(func);
			},
			_ => todo!(),
		}
		Ok(())
	}

	fn gen_stmt(&mut self, node: Node) -> Result<Vec<Instr>> {
		Ok(vec![match node {
			// FIXME: Very questionable cabbaging.
			Node::Assign { id, ty, val } => return match *val {
				Node::FuncCall { id, args } =>
					self.gen_fncall(&id, args, gen_type(&ty, false)?),
				_ => {
					let Instr::Val(Val(kind, name)) = self.gen_stmt(*val)?.remove(0)
						else { unreachable!() };

					Ok(vec![
						Instr::Assign(Val(ValKind::Local, format!("t{}", *id)), Instr::Alloca(gen_type(&ty, false)?).into()),
						Instr::Store(TypedVal(gen_type(&ty, false)?, kind, name), 
							TypedVal(Type::Ptr, ValKind::Local, format!("t{}", *id))),
					])
				},
			},
			Node::Global { id, ty, val } => {
				let Instr::Val(val) = self.gen_stmt(*val)?.remove(0)
					else { unreachable!() };

				let data = llvm::DataDef {
					value: val.typed(gen_type(&ty, false)?),
					name:  format!("g{}", *id),
					attr:  vec![DataAttr::Internal, DataAttr::Global], // TODO: global var attrs
				};

				self.module.data.push(data);
				return Ok(Vec::new());
			},
			Node::Ret(None, ty)    => Instr::Ret(None, gen_type(&ty, false)?), // realistically this is only ever void
			Node::Ret(Some(v), ty) => {
				let (instr, tyval) = self.use_val(self.gen_val(&v).typed(gen_type(&ty, false)?));
				let mut instrs = instr.map_or(Vec::new(), |i| vec![i]);

				let (ty, val) = tyval.val();
				instrs.push(Instr::Ret(Some(val), ty));

				return Ok(instrs);
			},
			Node::FuncCall { id, args } => return self.gen_fncall(&id, args, Type::Void),
			Node::Var(v)    => Instr::Val(self.gen_val(&v)),
			Node::StrLit(l) => Instr::Val(Val(ValKind::Str, l)),
			Node::Store { to, from: (val, ty) } => {
				let Val(tkind, tname) = self.gen_val(&to);

				let ty = gen_type(&ty, matches!(val, Var::Glob(_)))?;
				let tty = match tkind {
					ValKind::Local => Type::Ptr,
					_ => ty.clone(),
				};

				Instr::Store(self.gen_val(&val).typed(ty), TypedVal(tty, tkind, tname))
			},
			_ => todo!(),
		}])
	}

	fn gen_fncall(&mut self, var: &Var, args: Vec<(Var, mType)>, ret: Type) -> Result<Vec<Instr>> {
		let mut instrs = Vec::new();

		let instr = Instr::Call {
			func: {
				let (instr, tyval) = self.use_val(self.gen_val(var).typed(ret));
				instr.map(|i| instrs.push(i));
				tyval
			},
			args: {
				let mut nargs = Vec::new();
				for (var, ty) in args {
					let (instr, tyval) = self.use_val(self.gen_val(&var)
						.typed(gen_type(&ty, matches!(var, Var::Glob(_)))?));
					instr.map(|i| instrs.push(i));
					nargs.push(tyval);
				}
				nargs
			},
		};
		instrs.push(instr);
		Ok(instrs)
	}

	// TODO: maybe not always a ptr
	fn use_val(&mut self, val: TypedVal) -> (Option<Instr>, TypedVal) {
		match val {
			TypedVal(ty, ValKind::Local, name) => {
				let nname = format!("{}_{}", name, self.inc_ucnt());

				(Some(Instr::Assign(Val(ValKind::Local, nname.clone()), 
					Instr::Load(ty.clone(), TypedVal(Type::Ptr, ValKind::Local, name)).into())),
					TypedVal(ty, ValKind::Local, nname))
			},
			_ => (None, val),
		}
	}

	fn gen_val(&self, v: &mir::Var) -> Val {
		match v {
			Var::Imm(v)    => Val(ValKind::Const, v.to_string()),
			Var::Local(id) => Val(ValKind::Local, format!("t{id}")),
			Var::Glob(id) if self.sym.contains_key(id) 
				=> Val(ValKind::Global, self.get_id_name(*id).to_string()),
			Var::Glob(id)  => Val(ValKind::Global, format!("g{}", *id)),
		}
	}
}

fn gen_type(ty: &mType, global: bool) -> Result<Type> {
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

		mType::None | mType::Never => Type::Void,

		mType::Opt(ty) | mType::Mut(ty) => return gen_type(ty, global),
		mType::Ptr(_) => Type::Ptr,
		mType::Arr(_, Some(_)) if global => Type::Ptr,
		mType::Arr(t, Some(n)) => Type::Array(*n as usize, Box::new(gen_type(t, global)?)),
		mType::Arr(_, _) => panic!("Stack arrays are not yet supported. Heap allocate instead."),
		t => unreachable!("{t:?}"),
	})
}
