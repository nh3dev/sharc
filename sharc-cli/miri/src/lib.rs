use sharc::mir::{Expr, Node, Var, Type};

pub struct Value {
	val: Val,
	ty:  Type,
}

#[derive(Debug, Clone)]
pub enum Val {
	None,
	Int(Vec<u64>),

	Ret(usize),
}

pub struct Runtime {
	stack: Vec<Value>,
	base:  usize,
}

impl<'b> Runtime {
	pub fn new() -> Self {
		Self { 
			stack: Vec::with_capacity(64), 
			base:  0
		}
	}

	fn resolve_var(&self, val: &sharc::mir::Var<'b>) -> Value {
		match val {
			Var::None      => Value::None,
			Var::Imm(i)    => Value::Int(i.to_vec()),
			Var::Local(id) => self.stack.get(self.base + id.0 as usize - 1).unwrap().clone(),
		}
	}

	fn assign(&mut self, i: usize, val: Value) {
		match self.stack.get_mut(self.base + i - 1) {
			Some(v) => *v = val,
			None    => self.stack.insert(self.base + i - 1, val),
		}
	}

	pub fn run(&mut self, mir: (Vec<sharc::mir::Node<'b>>, sharc::bump::Bump)) -> Value {
		self.eval_mir_block(&mir.0)
	}

	fn eval_mir_block(&mut self, block: &[Node<'b>]) -> Value {
		self.stack.push(Value::Ret(self.base));
		self.base = self.stack.len();
		let ret = self.eval_mir_block_inner(block);
		self.stack.truncate(self.base);
		let Value::Ret(base) = self.stack.pop().unwrap() 
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
				Node::Ret(val, ty) => return self.resolve_var(&val),
			}
		}
		Value::None
	}

	fn eval_mir_expr(&mut self, expr: &Expr<'b>) -> Value {
		match expr {
			Expr::ImplCall { path: ["core"], ident, gener, args } => match *ident {
				"Add" => {
					let lhs = self.resolve_var(&args[0].0);
					let rhs = self.resolve_var(&args[1].0);
					let (Value::Int(mut l), Value::Int(r)) = (lhs, rhs) 
						else { return Value::None; };

					if l.len() < r.len() { l.resize(r.len(), 0); }
					for (i, v) in r.iter().enumerate() {
						let (res, overflow) = l[i].overflowing_add(*v);
						l[i] = res;
						if overflow {
							let mut carry = 1;
							for j in i+1..l.len() {
								let (res, overflow) = l[j].overflowing_add(carry);
								l[j] = res;
								if !overflow { break; }
							}
							if carry > 0 {
								l.push(carry);
							}
						}
					}

					Value::Int(l)
				},
				"As" => {
					self.resolve_var(&args[0].0)
				},
				_ => Value::None,
			},
			Expr::ImplCall { path, ident, gener, args } => todo!(),
			Expr::StrLit(s) => todo!(),
			Expr::Call { id, args } => todo!(),
		}
	}
}
