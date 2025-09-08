use sharc::mir::{Expr, Node, Var, Type as MirType};

mod val;
use val::{Type, Val, Value};

pub struct Runtime {
	stack: Vec<Option<Value>>,
	base:  usize,
}

impl<'b> Runtime {
	pub fn new() -> Self {
		Self { 
			stack: Vec::with_capacity(64), 
			base:  0
		}
	}

	fn resolve_var(&mut self, val: &Var, ty: &MirType) -> Value {
		let ty = Type::from_mir(ty);
		match val {
			Var::None      => Value::none(),
			Var::Imm(i)    => Value::new(Val::Int(i.to_vec()), ty),
			Var::Local(id) => std::mem::take(self.stack.get_mut(self.base + id.0 as usize - 1).unwrap()).unwrap(),
		}
	}

	fn assign(&mut self, i: usize, val: Value) {
		match self.stack.get_mut(self.base + i - 1) {
			Some(v) => panic!("Local already assigned: %{i} = {}", v.as_ref()
				.map_or_else(|| String::from("dropped"), |v| v.to_string())),
			None    => self.stack.push(Some(val)),
		}
	}

	pub fn run(&mut self, mir: (Vec<sharc::mir::Node<'b>>, sharc::bump::Bump)) -> Value {
		self.eval_mir_block(&mir.0)
	}

	fn eval_mir_block(&mut self, block: &[Node<'b>]) -> Value {
		self.stack.push(Some(Value::new(Val::Ret(self.base), Type::None)));
		self.base = self.stack.len();
		let ret = self.eval_mir_block_inner(block);
		self.stack.truncate(self.base);
		let Some(Value { val: Val::Ret(base), .. }) = self.stack.pop().unwrap() 
			else { unreachable!() };
		self.base = base;
		ret
	}

	fn eval_mir_block_inner(&mut self, block: &[Node<'b>]) -> Value {
		for node in block {
			match node {
				Node::Assign { id, ty, expr } => {
					let val = self.eval_mir_expr(expr);
					self.assign(id.0 as usize, val);
				},
				Node::Ret(val, ty) => return self.resolve_var(&val, ty),
			}
		}
		Value::none()
	}

	fn eval_mir_expr(&mut self, expr: &Expr<'b>) -> Value {
		match expr {
			Expr::ImplCall { path: ["core"], ident, gener, args } => match *ident {
				"Add" => {
					let lhs = self.resolve_var(&args[0].0, &args[0].1);
					let rhs = self.resolve_var(&args[1].0, &args[1].1);

					Value::add(lhs, rhs, gener.get(2).map(|t| Type::from_mir(t)))
				},
				"As" => {
					self.resolve_var(&args[0].0, &gener[0])
				},
				_ => Value::none(),
			},
			Expr::ImplCall { path, ident, gener, args } => todo!(),
			Expr::StrLit(s) => todo!(),
			Expr::Call { id, args } => todo!(),
		}
	}
}
