use std::sync::LazyLock;
use std::ffi::c_void;

use libffi::middle::{Cif, CodePtr};
use sharc::mir::{Expr, Node, Type as MirType, Var};
use std::borrow::Cow;

mod val;
mod sheep;

use val::{RawVal, Type, Val, Value};
use sheep::Sheep;

macro_rules! error {
	($($ident:tt)*) => {{
		eprintln!("{}", logger::Report(sharc::ReportKind::RuntimeError.title(format!($($ident)*))));
		std::process::exit(1);
	}};
}

#[repr(transparent)]
#[derive(Clone, Copy)]
struct DlHandle(*mut u8);

unsafe impl Send for DlHandle {}
unsafe impl Sync for DlHandle {}

unsafe extern "C-unwind" {
	fn dlopen(path: *const u8, flags: i32) -> DlHandle;
	fn dlsym(handle: DlHandle, symbol: *const u8) -> *const c_void;
}

static CURRENT_PROCESS: LazyLock<DlHandle> = LazyLock::new(|| 
	unsafe { dlopen(std::ptr::null(), 0x0) });

pub struct Runtime {
	stack: Vec<Value>,
	base:  usize,
	bump:  sharc::bump::Bump,
}

impl<'b> Runtime {
	pub fn new() -> Self {
		Self { 
			stack: Vec::with_capacity(64), 
			base:  0,
			bump:  sharc::bump::Bump::new(),
		}
	}

	fn resolve_var(&mut self, val: &Var, ty: &MirType) -> Sheep<Value> {
		let ty = Type::from_mir(ty);
		match val {
			Var::None      => Value::none().into(),
			Var::Imm(i)    => Value::new(
				Val::Int(i.try_as_u64().unwrap_or_else(
					|| error!("miri does not support ints larger than u64 natively"))), 
				ty).into(),
			Var::Local(id) => Sheep::Ptr(self.stack.get_mut(self.base + id.0 as usize - 1).unwrap()),
		}
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

	pub fn run(&mut self, mir: sharc::mir::Mir<'b>) -> Value {
		self.bump = mir.bump;
		self.eval_mir_block(&mir.nodes).into_owned()
	}

	fn eval_mir_block(&mut self, block: &[Node<'b>]) -> Sheep<Value> {
		self.stack.push(Value::new(Val::Ret(self.base), Type::None));
		self.base = self.stack.len();
		let ret = self.eval_mir_block_inner(block);
		self.stack.truncate(self.base);
		let Value { val: Val::Ret(base), .. } = self.stack.pop().unwrap()
			else { unreachable!() };
		self.base = base;
		ret
	}

	fn eval_mir_block_inner(&mut self, block: &[Node<'b>]) -> Sheep<Value> {
		for node in block {
			match node {
				Node::Assign { id, ty, expr } => {
					let mut val = self.eval_mir_expr(expr);
					val.ty = Type::from_mir(ty);
					self.assign(id.0 as usize, val.into_owned());
				},
				Node::Ret(val, ty) => return self.resolve_var(&val, ty),
				Node::Store { to, ty, from } => { // assumes the type remains the same
					let val = self.eval_mir_expr(from);
					self.store(to.0 as usize, val.val.clone());
				},
				Node::Dbg { id, ident } => { },

				Node::DefFn { id, args, ret, def_proc, body } => todo!(),
			}
		}
		Value::none().into()
	}

	fn eval_mir_expr(&mut self, expr: &Expr<'b>) -> Sheep<Value> {
		match expr {
			Expr::ImplCall { path: ["core"], ident, gener, args } => match *ident {
				"Add" => {
					let lhs = self.resolve_var(&args[0].0, &args[0].1);
					let rhs = self.resolve_var(&args[1].0, &args[1].1);

					Value::add(&lhs, &rhs, gener.get(2).map(|t| Type::from_mir(t))).into()
				},
				"As" => {
					self.resolve_var(&args[0].0, &gener[0])
				},
				_ => Value::none().into(),
			},
			Expr::Imm(i, ty) => Value::new(
				Val::Int(i.try_as_u64().unwrap_or_else(
					|| error!("miri does not support ints larger than u64 natively"))), 
				Type::from_mir(ty)).into(),
			Expr::StrLit(s) => Value::new(
				Val::Ptr(s.as_ptr() as *mut c_void),
				Type::Ptr(Box::new(Type::U(8)))).into(),
			Expr::DefCFn { sym, args, ret } => {
				use libffi::middle::Type as FfiType;

				let args = args.iter().map(|&t| Type::from_mir(t)).collect::<Vec<_>>();
				let cif_args = args.iter().map(|t| t.to_libffi()).collect::<Vec<_>>();

				let ret = Type::from_mir(ret);
				let cif_ret = ret.to_libffi();

				let sym = format!("{sym}\0").into_bytes();
				let ptr = unsafe { dlsym(*CURRENT_PROCESS, sym.as_ptr()) };

				Value::new(
					Val::CFn(Cif::new(cif_args, cif_ret), CodePtr::from_ptr(ptr)), 
					Type::Fn(args, Box::new(ret))).into()
			},
			Expr::Call { id, ty, args } => {
				let func = self.resolve_var(&Var::Local(*id), ty);
				let args = args.iter().map(|(v, t)| self.resolve_var(v, t).val.as_arg()).collect::<Vec<_>>();

				let Val::CFn(ref cif, fptr) = func.val else { 
					panic!("Attempted to call a non-function value: {func:?}")
				};

				let Type::Fn(_, ref ret) = func.ty else {
					panic!("Function value has non-function type: {func:?}")
				};

				let val = RawVal::to_val(unsafe { cif.call(fptr, &args) }, ret);

				Value::new(val, (**ret).clone()).into()
			},

			Expr::ImplCall { path, ident, gener, args } => todo!(),
			Expr::FuncCapture { fid, args } => todo!(),
		}
	}
}
