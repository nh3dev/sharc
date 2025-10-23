#![allow(static_mut_refs)]

use std::sync::LazyLock;
use std::ffi::c_void;

use libffi::middle::{Cif, CodePtr};
use sharc::mir::{Expr, InstrKind, Node, Type as MirType, Var};
use sharc::report::{Reportable, Report};

mod val;
mod sheep;
mod maybedrop;
mod error;
mod sys;
mod ffi;

use val::{Type, Val, Value};
use ffi::RawVal;
use sheep::Sheep;
use maybedrop::MaybeDrop;
use error::{MiriError, Result};

pub const VERSION: (u16, u16) = (1, 0);
pub const GITREV: Option<&'static str> = option_env!("GITREV");

static mut STUPID_FFI_ERROR_WORKAROUND: Option<Box<Report<MiriError>>> = None;

pub struct Runtime {
	stack:  Vec<MaybeDrop<Value>>,
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

	fn resolve_var(&mut self, val: &Var, ty: &MirType) -> Result<Sheep<MaybeDrop<Value>>> {
		let ty = Type::from_mir(ty);
		Ok(match val {
			Var::None      => Sheep::Owned(MaybeDrop::new(Value::none())),
			Var::Imm(i)    => {
				let int = Val::from_ibig(i, &ty).ok_or_else(||
					Box::new(MiriError::InvalidConversion
						.title(format!("cannot represent `{i}` as `{ty}`"))))?;

				Sheep::Owned(MaybeDrop::new(Value::new(int, ty)))
			},
			Var::Local(id) => Sheep::Ptr(self.stack.get_mut(self.base + id.0 as usize - 1).ok_or_else(||
				MiriError::RuntimeError.title(format!("Local variable %{id} not found on stack")))?),
		})
	}

	fn assign(&mut self, i: usize, val: MaybeDrop<Value>) {
		match self.stack.get_mut(self.base + i - 1) {
			Some(v) => *v = val,
			None    => {
				if i != self.stack.len() - self.base + 1 {
					self.stack.resize(self.base + i - 1, MaybeDrop::new(Value::none()));
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

	pub fn run(mut self, mir: sharc::mir::Mir<'b>) -> Result<Value> {
		self.bump = mir.bump;
		Ok(self.eval_mir_block(&mir.nodes, &[])?)
	}

	fn eval_mir_block(&mut self, block: &[Node<'b>], init: &[MaybeDrop<Value>]) -> Result<Value> {
		self.stack.push(MaybeDrop::new(Value::new(Val::Ret(self.base), Type::None)));
		self.base = self.stack.len();

		self.stack.extend_from_slice(init);

		let ret = self.eval_mir_block_inner(block)?;
		ret.set_drop(false);
		let ret = unsafe { Sheep::make_owned(ret) }.into_inner();

		self.stack.truncate(self.base);

		let Value { val: Val::Ret(base), .. } = *self.stack.pop().unwrap()
			else { unreachable!() };

		self.base = base;

		Ok(ret)
	}

	fn eval_mir_block_inner(&mut self, block: &[Node<'b>]) -> Result<Sheep<MaybeDrop<Value>>> {
		for node in block {
			match node {
				Node::Assign { id, ty, expr } => {
					let mut val = self.eval_mir_expr(expr)?;
					val.ty = Type::from_mir(ty);
					self.assign(id.0 as usize, val.into_owned());
				},
				Node::Ret(val, ty) => return self.resolve_var(val, ty),
				Node::Store { to, ty, from } => { // assumes the type remains the same
					let val = self.resolve_var(from, ty)?;
					self.store(to.0 as usize, val.val.clone());
				},
				Node::Dbg { id, ident } => { },

				Node::DefFn { id, args, ret, body } => {
					// TODO! default args :)
					let args = args.iter().map(|(_, t, _)| Type::from_mir(*t)).collect::<Vec<_>>();
					let ret = Type::from_mir(ret);

					self.assign(id.0 as usize, MaybeDrop::new(Value::new(
						// dont wanna pollute Value with the lifetime :p
						// FIXME: do this at some point I guess
						Val::SharcFnDef(unsafe { std::mem::transmute(*body) }),
						Type::Fn(args, Box::new(ret))
					)));
				},
			}
		}

		Ok(Sheep::Owned(MaybeDrop::new(Value::none())))
	}

	fn eval_mir_expr(&mut self, expr: &Expr<'b>) -> Result<Sheep<MaybeDrop<Value>>> {
		Ok(match expr {
			Expr::ImplCall { path: ["core"], ident, gener, args } => match *ident {
				"As" => {
					self.resolve_var(&args[0].0, gener[0])?
				},
				_ => Sheep::Owned(MaybeDrop::new(Value::none())),
			},
			Expr::Instr { kind, args } => match kind {
				InstrKind::Add | InstrKind::Sub | InstrKind::Mul | InstrKind::Div | InstrKind::Mod => {
					let [(lhs_var, lhs_ty), (rhs_var, rhs_ty)] = args else {
						return MiriError::InvalidArity
							.title(format!("{kind:?} instruction expected 2, got {}", args.len()))
							.as_err();
					};

					let lhs = self.resolve_var(lhs_var, lhs_ty)?;
					let rhs = self.resolve_var(rhs_var, rhs_ty)?;

					Sheep::Owned(MaybeDrop::new(match kind {
						InstrKind::Add => Value::add(&lhs, &rhs, Some(lhs.ty.clone())),
						InstrKind::Sub => Value::sub(&lhs, &rhs, Some(lhs.ty.clone())),
						InstrKind::Mul => Value::mul(&lhs, &rhs, Some(lhs.ty.clone())),
						InstrKind::Div => Value::div(&lhs, &rhs, Some(lhs.ty.clone()))?,
						InstrKind::Mod => Value::rem(&lhs, &rhs, Some(lhs.ty.clone()))?,
						_ => unreachable!(),
					}))
				},
				_ => todo!(),
			},
			Expr::Imm(i, ty) => {
				let ty = Type::from_mir(ty);
				let int = Val::from_ibig(i, &ty).ok_or_else(||
					Box::new(MiriError::IntSizeLimitExceeded
						.title("miri does not support ints larger than u64 natively")))?;

				Sheep::Owned(MaybeDrop::new(Value::new(int, ty)))
			},
			Expr::StrLit(s) => Sheep::Owned(MaybeDrop::new(Value::new(
				Val::Ptr(s.as_ptr() as *mut c_void),
				Type::Ptr(Box::new(Type::U(8)))))),
			Expr::DefCFn { sym, args, ret } => {
				use libffi::middle::Type as FfiType;

				let args = args.iter().map(|&t| Type::from_mir(t)).collect::<Vec<_>>();
				let cif_args = args.iter().map(|t| t.to_libffi()).collect::<Vec<_>>();

				let ret = Type::from_mir(ret);
				let cif_ret = ret.to_libffi();

				let sym = format!("{sym}\0");
				let ptr = unsafe { sys::ldsym(&sym) };

				Sheep::Owned(MaybeDrop::new(Value::new(
					Val::CFn(Cif::new(cif_args, cif_ret), CodePtr::from_ptr(ptr)), 
					Type::Fn(args, Box::new(ret)))))
			},
			Expr::Call { id, ty, args } => {
				let func = self.resolve_var(&Var::Local(*id), ty)?;
				let args = args.iter().map(|(v, t)| self.resolve_var(v, t)).collect::<Result<Vec<_>>>()?;
				let args = args.iter().map(|v| v.val.as_arg()).collect::<Vec<_>>();

				match (&func.val, &func.ty) {
					(Val::CFn(cif, fptr), Type::Fn(_, ret)) => {
						let val = RawVal::into_val(unsafe { cif.call(*fptr, &args) }, ret);
						Sheep::Owned(MaybeDrop::new(Value::new(val?, (**ret).clone())))
					},
					(Val::SharcFnClosure(_, closure), Type::Fn(arg_types, ret)) => {
						let cif = Cif::new(arg_types.iter().map(|t| t.to_libffi()), ret.to_libffi());
						let ptr = CodePtr::from_fun(*closure.code_ptr());

						let val = RawVal::into_val(unsafe { cif.call(ptr, &args) }, ret)?;

						if let Some(e) = unsafe { STUPID_FFI_ERROR_WORKAROUND.take() } {
							return Err(e);
						}

						Sheep::Owned(MaybeDrop::new(Value::new(val, (**ret).clone())))
					},
					_ => return MiriError::InvalidInstruction
						.title("Attempted to call a non-function value")
						.as_err(),
				}
			},

			Expr::ImplCall { path, ident, gener, args } => todo!(),
			Expr::FuncCapture { fid, args } => {
				let val = self.resolve_var(&Var::Local(*fid), &MirType::None)?;

				let Val::SharcFnDef(body) = &val.val else {
					return MiriError::InvalidInstruction
						.title("Attempted to capture a non-function value")
						.as_err();
				};

				let Type::Fn(arg_types, ret) = &val.ty else {
					return MiriError::InvalidInstruction
						.title("Function definition does not have function type")
						.as_err();
				};

				unsafe extern "C" fn fncallback(
					cif:      &libffi::low::ffi_cif,
					result:   &mut c_void,
					args:     *const *const c_void,
					userdata: &(*mut Runtime, Vec<Type>, &[Node<'static>], Vec<MaybeDrop<Value>>),
				) {
					let (rt, arg_types, body, captures) = userdata;

					let mut new_args = Vec::with_capacity(captures.len() + cif.nargs as usize);
					let args = args as *const libffi::middle::Arg;

					for i in 0..cif.nargs as usize {
						let size = unsafe { **cif.arg_types.offset(i as isize) }.size;
						let mut val = ffi::RawVal { none: () };

						unsafe {
							std::ptr::copy_nonoverlapping(
								std::mem::transmute(args.offset(i as isize)),
								(&raw mut val) as *mut u8,
								size
							);
						}

						let arg = RawVal::into_val(val, &arg_types[i]).unwrap();
						new_args.push(MaybeDrop::new(Value::new(arg, arg_types[i].clone())));
					}

					new_args.extend_from_slice(captures);

					let val = match unsafe { &mut **rt }.eval_mir_block(body, &new_args) {
						Ok(v) => v,
						Err(e) => {
							unsafe { STUPID_FFI_ERROR_WORKAROUND = Some(e); }
							return;
						}
					};

					unsafe {
						std::ptr::copy_nonoverlapping::<u8>(
							std::mem::transmute(val.val.as_arg()),
							result as *mut c_void as *mut u8,
							(*cif.rtype).size
						);
					}
				}
				
				let captures = args.iter()
					.map(|v| Ok(self.resolve_var(&Var::Local(*v), &MirType::None)?.into_owned()))
					.collect::<Result<Vec<_>>>()?;

				let args = Box::new((self as *mut Self, arg_types.clone(), *body, captures));

				let closure = libffi::middle::Closure::new(
					Cif::new(arg_types.iter().map(|t| t.to_libffi()), ret.to_libffi()),
					fncallback, unsafe { &*(&*args as *const _) }
				);

				Sheep::Owned(MaybeDrop::new(Value::new(
					Val::SharcFnClosure(args, closure),
					val.ty.clone()
				)))
			}
		})
	}
}
