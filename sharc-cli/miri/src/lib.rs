use std::sync::LazyLock;
use std::ffi::c_void;

use libffi::middle::{Cif, CodePtr};
use sharc::mir::{Expr, Node, Type as MirType, Var};
use sharc::report::Reportable;

mod val;
mod sheep;
mod error;
mod sys;

use val::{RawVal, Type, Val, Value};
use sheep::Sheep;
use error::{MiriError, Result};

pub const VERSION: (u16, u16) = (1, 0);
pub const GITREV: Option<&'static str> = option_env!("GITREV");

pub struct Runtime {
	stack:  Vec<Value>,
	base:   usize,
	bump:   sharc::bump::Bump,
}

impl<'b> Runtime {
	pub fn new() -> Self {
		Self { 
			stack: Vec::with_capacity(64), 
			base:  0,
			bump:  sharc::bump::Bump::new(),
		}
	}

	fn resolve_var(&mut self, val: &Var, ty: &MirType) -> Result<Sheep<Value>> {
		let ty = Type::from_mir(ty);
		Ok(match val {
			Var::None      => Sheep::Owned(Value::none()),
			Var::Imm(i)    => {
				let int = Val::from_ibig(i, &ty).ok_or_else(||
					Box::new(MiriError::IntSizeLimitExceeded
						.title("miri does not support ints larger than u64 natively")))?;

				Sheep::Owned(Value::new(int, ty))
			},
			Var::Local(id) => Sheep::Ptr(self.stack.get_mut(self.base + id.0 as usize - 1).unwrap()),
		})
	}

	fn assign(&mut self, i: usize, val: Value) {
		match self.stack.get_mut(self.base + i - 1) {
			Some(v) => *v = val,
			None    => {
				if i != self.stack.len() - self.base + 1 {
					self.stack.resize(self.base + i - 1, Value::none());
				}

				self.stack.insert(self.base + i - 1, val)
			}
		}
	}

	fn store(&mut self, i: usize, val: Val) {
		match self.stack.get_mut(self.base + i - 1) {
			Some(v) => v.val = val,
			None    => panic!("Local not found: %{i}"),
		}
	}

	pub fn run(&mut self, mir: sharc::mir::Mir<'b>) -> Result<Value> {
		self.bump = mir.bump;
		Ok(self.eval_mir_block(&mir.nodes)?.into_owned())
	}

	fn eval_mir_block(&mut self, block: &[Node<'b>]) -> Result<Sheep<Value>> {
		self.stack.push(Value::new(Val::Ret(self.base), Type::None));
		self.base = self.stack.len();

		let ret = self.eval_mir_block_inner(block)?;
		self.stack.truncate(self.base);

		let Value { val: Val::Ret(base), .. } = self.stack.pop().unwrap()
			else { unreachable!() };

		self.base = base;

		Ok(ret)
	}

	fn eval_mir_block_inner(&mut self, block: &[Node<'b>]) -> Result<Sheep<Value>> {
		for node in block {
			match node {
				Node::Assign { id, ty, expr } => {
					let mut val = self.eval_mir_expr(expr)?;
					val.ty = Type::from_mir(ty);
					self.assign(id.0 as usize, val.into_owned());
				},
				Node::Ret(val, ty) => return self.resolve_var(&val, ty),
				Node::Store { to, ty, from } => { // assumes the type remains the same
					let val = self.resolve_var(from, ty)?;
					self.store(to.0 as usize, val.val.clone());
				},
				Node::Dbg { id, ident } => { },

				Node::DefFn { id, args, ret, def_proc, body } => todo!(),
			}
		}

		Ok(Sheep::Owned(Value::none()))
	}

	fn eval_mir_expr(&mut self, expr: &Expr<'b>) -> Result<Sheep<Value>> {
		Ok(match expr {
			Expr::ImplCall { path: ["core"], ident, gener, args } => match *ident {
				"Add" => {
					let lhs = self.resolve_var(&args[0].0, &args[0].1)?;
					let rhs = self.resolve_var(&args[1].0, &args[1].1)?;

					Sheep::Owned(Value::add(&lhs, &rhs, gener.get(2).map(|t| Type::from_mir(t))))
				},
				"As" => {
					self.resolve_var(&args[0].0, &gener[0])?
				},
				_ => Sheep::Owned(Value::none()),
			},
			Expr::Imm(i, ty) => {
				let ty = Type::from_mir(ty);
				let int = Val::from_ibig(i, &ty).ok_or_else(||
					Box::new(MiriError::IntSizeLimitExceeded
						.title("miri does not support ints larger than u64 natively")))?;

				Sheep::Owned(Value::new(int, ty))
			},
			Expr::StrLit(s) => Sheep::Owned(Value::new(
				Val::Ptr(s.as_ptr() as *mut c_void),
				Type::Ptr(Box::new(Type::U(8))))),
			Expr::DefCFn { sym, args, ret } => {
				use libffi::middle::Type as FfiType;

				let args = args.iter().map(|&t| Type::from_mir(t)).collect::<Vec<_>>();
				let cif_args = args.iter().map(|t| t.to_libffi()).collect::<Vec<_>>();

				let ret = Type::from_mir(ret);
				let cif_ret = ret.to_libffi();

				let sym = format!("{sym}\0");
				let ptr = unsafe { sys::ldsym(&sym) };

				Sheep::Owned(Value::new(
					Val::CFn(Cif::new(cif_args, cif_ret), CodePtr::from_ptr(ptr)), 
					Type::Fn(args, Box::new(ret))))
			},
			Expr::Call { id, ty, args } => {
				let func = self.resolve_var(&Var::Local(*id), ty)?;
				let args = args.iter().map(|(v, t)| Ok(self.resolve_var(v, t)?.val.as_arg())).collect::<Result<Vec<_>>>()?;

				let Val::CFn(ref cif, fptr) = func.val else { 
					return MiriError::InvalidInstruction
						.title("Attempted to call a non-function value")
						.as_err();
				};

				let Type::Fn(_, ref ret) = func.ty else {
					return MiriError::InvalidInstruction
						.title("Function value has non-function type")
						.as_err();
				};

				let val = RawVal::to_val(unsafe { cif.call(fptr, &args) }, ret);

				Sheep::Owned(Value::new(val?, (**ret).clone()))
			},

			Expr::ImplCall { path, ident, gener, args } => todo!(),
			Expr::FuncCapture { fid, args } => todo!(),
		})
	}
}
