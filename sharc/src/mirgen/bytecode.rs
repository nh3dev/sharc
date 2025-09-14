use super::mir::*;

pub use self::{ser::serialize, de::deserialize};

const MAGIC: &[u8] = b"SHARDBC";

mod ser {
	use std::io::{self, Write};
	use super::*;

	pub fn serialize(mir: &Mir, buf: &mut impl Write) -> io::Result<()> {
		buf.write_all(MAGIC)?;
		buf.write_all(&mir.version.0.0.to_be_bytes())?;
		buf.write_all(&[mir.version.0.1])?;
		serialize_str(mir.version.1, buf)?;
		serialize_str(mir.origin.unwrap_or(""), buf)?;
		serialize_block(&mir.nodes, buf)
	}

	fn serialize_block(mir: &[Node], buf: &mut impl Write) -> io::Result<()> {
		mir.into_iter().try_for_each(|node| match node {
			Node::Ret(v, t) => {
				buf.write_all(&[0])?;
				serialize_var(v, buf)?;
				serialize_type(t, buf)
			},
			Node::Assign { id, ty, expr } => {
				buf.write_all(&[1])?;
				serialize_valid(*id, buf)?;
				serialize_type(ty, buf)?;
				serialize_expr(expr, buf)
			},
			Node::Store { to, ty, from } => {
				buf.write_all(&[2])?;
				serialize_valid(*to, buf)?;
				serialize_type(ty, buf)?;
				serialize_expr(from, buf)
			},
			Node::Dbg { id, ident } => {
				buf.write_all(&[3])?;
				serialize_valid(*id, buf)?;
				serialize_str(ident, buf)
			},
			Node::DefFn { id, args, ret, def_proc, body } => {
				buf.write_all(&[4])?;
				serialize_valid(*id, buf)?;

				buf.write_all(&(args.len() as u32).to_be_bytes())?;
				args.iter().try_for_each(|(arg_id, arg_ty)| {
					serialize_valid(*arg_id, buf)?;
					serialize_type(arg_ty, buf)
				})?;

				serialize_type(ret, buf)?;

				buf.write_all(&(def_proc.len() as u32).to_be_bytes())?;
				def_proc.iter().try_for_each(|(proc_id, proc_body)| {
					serialize_valid(*proc_id, buf)?;
					buf.write_all(&(proc_body.len() as u32).to_be_bytes())?;
					serialize_block(proc_body, buf)
				})?;

				buf.write_all(&(body.len() as u32).to_be_bytes())?;
				serialize_block(body, buf)
			},
		})?;

		buf.flush()
	}

	fn serialize_expr(expr: &Expr, buf: &mut impl Write) -> io::Result<()> {
		match expr {
			Expr::Call { id, ty, args } => {
				buf.write_all(&[0])?;
				serialize_valid(*id, buf)?;
				serialize_type(ty, buf)?;
				buf.write_all(&(args.len() as u32).to_be_bytes())?;
				args.iter().try_for_each(|(arg, ty)| {
					serialize_var(arg, buf)?;
					serialize_type(ty, buf)
				})
			},
			Expr::FuncCapture { fid, args } => {
				buf.write_all(&[1])?;
				serialize_valid(*fid, buf)?;
				buf.write_all(&(args.len() as u32).to_be_bytes())?;
				args.iter().try_for_each(|v| serialize_valid(*v, buf))
			},
			Expr::StrLit(s) => {
				buf.write_all(&[2])?;
				serialize_str(s, buf)
			},
			Expr::ImplCall { path, ident, gener, args } => {
				buf.write_all(&[3])?;
				buf.write_all(&(path.len() as u32).to_be_bytes())?;
				path.iter().try_for_each(|p| serialize_str(p, buf))?;

				serialize_str(ident, buf)?;

				buf.write_all(&(gener.len() as u32).to_be_bytes())?;
				gener.iter().try_for_each(|g| serialize_type(g, buf))?;

				buf.write_all(&(args.len() as u32).to_be_bytes())?;
				args.iter().try_for_each(|(arg, ty)| {
					serialize_var(arg, buf)?;
					serialize_type(ty, buf)
				})
			},
			Expr::Imm(i, ty) => {
				buf.write_all(&[4])?;
				buf.write_all(&(i.0.len() as u32).to_be_bytes())?;
				i.0.iter().try_for_each(|b| buf.write_all(&b.to_be_bytes()))?;
				serialize_type(ty, buf)
			},
			Expr::DefCFn { sym, args, ret } => {
				buf.write_all(&[5])?;
				serialize_str(sym, buf)?;
				buf.write_all(&(args.len() as u32).to_be_bytes())?;
				args.iter().try_for_each(|t| serialize_type(t, buf))?;
				serialize_type(ret, buf)
			},
		}
	}

	fn serialize_type(ty: &Type, buf: &mut impl Write) -> io::Result<()> {
		match ty {
			Type::U(i)   => {
				buf.write_all(&[0])?;
				buf.write_all(&i.to_be_bytes())
			},
			Type::I(i)   => {
				buf.write_all(&[1])?;
				buf.write_all(&i.to_be_bytes())
			},
			Type::B(i) => {
				buf.write_all(&[2])?;
				buf.write_all(&i.to_be_bytes())
			},
			Type::F(i) => {
				buf.write_all(&[3])?;
				buf.write_all(&i.to_be_bytes())
			},
			Type::Usize  => buf.write_all(&[4]),
			Type::Isize  => buf.write_all(&[5]),
			Type::None   => buf.write_all(&[6]),
			Type::Never  => buf.write_all(&[7]),
			Type::Ptr(t) => {
				buf.write_all(&[8])?;
				serialize_type(t, buf)
			},
			Type::Mut(t) => {
				buf.write_all(&[9])?;
				serialize_type(t, buf)
			},
			Type::Fn(a, r) => {
				buf.write_all(&[10])?;
				buf.write_all(&(a.len() as u32).to_be_bytes())?;
				a.iter().try_for_each(|t| serialize_type(t, buf))?;
				serialize_type(r, buf)
			},
			Type::Arr(t, s) => {
				buf.write_all(&[11])?;
				serialize_type(t, buf)?;
				buf.write_all(&[s.is_some() as u8])?;
				if let Some(s) = s {
					buf.write_all(&s.to_be_bytes())?;
				}
				Ok(())
			},
		}
	}

	fn serialize_var(var: &Var, buf: &mut impl Write) -> io::Result<()> {
		match var {
			Var::None => buf.write_all(&[0]),
			Var::Imm(i) => { 
				buf.write_all(&[1])?;
				buf.write_all(&(i.0.len() as u32).to_be_bytes())?;
				i.0.iter().try_for_each(|b| buf.write_all(&b.to_be_bytes()))
			},
			Var::Local(i) => {
				buf.write_all(&[2])?;
				serialize_valid(*i, buf)
			},
		}
	}

	fn serialize_valid(id: ValId, buf: &mut impl Write) -> io::Result<()> {
		buf.write_all(&id.0.to_be_bytes())
	}

	fn serialize_str(s: &str, buf: &mut impl Write) -> io::Result<()> {
		buf.write_all(&(s.len() as u32).to_be_bytes())?;
		buf.write_all(s.as_bytes())
	}
}


mod de {
	use std::io::{self, Read, Seek};
	use std::ops::Not;

	use super::*;
	use crate::IBig;
	use bump::Bump;


	pub fn deserialize<'b>(bump: Bump, buf: &mut (impl Read + Seek)) -> Option<io::Result<Mir<'b>>> {
		let pos = match buf.stream_position() {
			Ok(p) => p,
			Err(e) => return Some(Err(e)),
		};

		match deserialize_bytes::<{ MAGIC.len() }>(buf) {
			Ok(m) if m != MAGIC => {
				let _ = buf.seek(io::SeekFrom::Start(pos));
				return None
			},
			Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => {
				let _ = buf.seek(io::SeekFrom::Start(pos));
				return None
			},
			Ok(_) => {},
			Err(e) => return Some(Err(e)),
		}

		let deserialize = || -> io::Result<Mir<'b>> {
			Ok(Mir {
				version: (
					(u32::from_be_bytes(deserialize_bytes::<4>(buf)?),
					deserialize_bytes::<1>(buf)?[0]),
					deserialize_str(&bump, buf)?,
				),
				origin: {
					let s = deserialize_str(&bump, buf)?;
					s.is_empty().not().then_some(s)
				},
				nodes:   std::iter::from_fn(|| deserialize_node(&bump, buf).transpose()).collect::<io::Result<_>>()?,
				bump,
			})
		};

		Some(deserialize())
	}


	fn deserialize_bytes<const N: usize>(buf: &mut impl Read) -> io::Result<[u8; N]> {
		let mut bytes = [0u8; N];
		buf.read_exact(&mut bytes)?;
		Ok(bytes)
	}

	fn deserialize_node<'b>(bump: &Bump, buf: &mut impl Read) -> io::Result<Option<Node<'b>>> {
		let flag = match deserialize_bytes::<1>(buf) {
			Ok(f) => f[0],
			Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => return Ok(None),
			Err(e) => return Err(e),
		};

		Ok(Some(match flag {
			0 => Node::Ret(
				deserialize_var(bump, buf)?, 
				deserialize_type(bump, buf)?),
			1 => Node::Assign { 
				id:     deserialize_valid(buf)?,
				ty:     deserialize_type(bump, buf)?,
				expr:   deserialize_expr(bump, buf)?,
			},
			2 => Node::Store { 
				to:   deserialize_valid(buf)?,
				ty:   deserialize_type(bump, buf)?,
				from: deserialize_expr(bump, buf)?,
			},
			3 => Node::Dbg { 
				id:    deserialize_valid(buf)?,
				ident: deserialize_str(bump, buf)?,
			},
			4 => {
				let id = deserialize_valid(buf)?;

				let args_len = u32::from_be_bytes(deserialize_bytes::<4>(buf)?) as usize;
				let args = bump.try_alloc_from_iter((0..args_len).map(|_| {
					let var = deserialize_valid(buf)?;
					let ty  = deserialize_type(bump, buf)?;
					io::Result::Ok((var, ty))
				}))?;

				let ret = deserialize_type(bump, buf)?;

				let def_proc_len = u32::from_be_bytes(deserialize_bytes::<4>(buf)?) as usize;
				let def_proc = bump.try_alloc_from_iter((0..def_proc_len).map(|_| {
					let proc_id = deserialize_valid(buf)?;
					let body_len = u32::from_be_bytes(deserialize_bytes::<4>(buf)?) as usize;
					let body = bump.try_alloc_from_iter((0..body_len).map(|_| deserialize_node(bump, buf).and_then(
						|n| n.ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "Unexpected EOF")))))?;
					io::Result::Ok((proc_id, body))
				}))?;

				let body_len = u32::from_be_bytes(deserialize_bytes::<4>(buf)?) as usize;
				let body = bump.try_alloc_from_iter((0..body_len).map(|_| deserialize_node(bump, buf).and_then(
					|n| n.ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "Unexpected EOF")))))?;

				Node::DefFn { id, args, ret, def_proc, body }
			},
			_ => return Err(io::Error::new(io::ErrorKind::InvalidData, "Invalid Tag")),
		}))
	}

	fn deserialize_expr<'b>(bump: &Bump, buf: &mut impl Read) -> io::Result<Expr<'b>> {
		Ok(match deserialize_bytes::<1>(buf)?[0] {
			0 => {
				let id = deserialize_valid(buf)?;
				let ty = deserialize_type(bump, buf)?;
				let len = u32::from_be_bytes(deserialize_bytes::<4>(buf)?) as usize;
				let args = bump.try_alloc_from_iter((0..len).map(|_| io::Result::Ok(
					(deserialize_var(bump, buf)?,
					deserialize_type(bump, buf)?))))?;

				Expr::Call { id, ty, args }
			},
			1 => {
				let fid = deserialize_valid(buf)?;
				let len = u32::from_be_bytes(deserialize_bytes::<4>(buf)?) as usize;
				let args = bump.try_alloc_from_iter((0..len).map(|_| deserialize_valid(buf)))?;
				Expr::FuncCapture { fid, args }
			},
			2 => Expr::StrLit(deserialize_str(bump, buf)?),
			3 => {
				let path_len = u32::from_be_bytes(deserialize_bytes::<4>(buf)?) as usize;
				let path = bump.try_alloc_from_iter((0..path_len).map(|_| deserialize_str(bump, buf)))?;

				let ident = deserialize_str(bump, buf)?;

				let gener_len = u32::from_be_bytes(deserialize_bytes::<4>(buf)?) as usize;
				let gener = bump.try_alloc_from_iter((0..gener_len).map(|_| deserialize_type(bump, buf)))?;

				let args_len = u32::from_be_bytes(deserialize_bytes::<4>(buf)?) as usize;
				let args = bump.try_alloc_from_iter((0..args_len).map(|_| {
					let var = deserialize_var(bump, buf)?;
					let ty  = deserialize_type(bump, buf)?;
					io::Result::Ok((var, ty))
				}))?;

				Expr::ImplCall { path, ident, gener, args }
			},
			4 => {
				let len = u32::from_be_bytes(deserialize_bytes::<4>(buf)?) as usize;
				let iter = bump.try_alloc_from_iter((0..len).map(|_| 
					io::Result::Ok(u64::from_be_bytes(deserialize_bytes::<8>(buf)?))))?;
				let ty = deserialize_type(bump, buf)?;
				Expr::Imm(IBig(iter), ty)
			},
			5 => {
				let sym = deserialize_str(bump, buf)?;

				let len = u32::from_be_bytes(deserialize_bytes::<4>(buf)?) as usize;
				let args = bump.try_alloc_from_iter((0..len).map(|_| deserialize_type(bump, buf)))?;

				let ret = deserialize_type(bump, buf)?;

				Expr::DefCFn { sym, args, ret }
			},
			_ => return Err(io::Error::new(io::ErrorKind::InvalidData, "Invalid Tag")),
		})
	}

	fn deserialize_var<'b>(bump: &Bump, buf: &mut impl Read) -> io::Result<Var<'b>> {
		Ok(match deserialize_bytes::<1>(buf)?[0] {
			0 => Var::None,
			1 => {
				let len = u32::from_be_bytes(deserialize_bytes::<4>(buf)?) as usize;
				let iter = (0..len).map(|_| io::Result::Ok(u64::from_be_bytes(deserialize_bytes::<8>(buf)?)));
				Var::Imm(IBig(bump.try_alloc_from_iter(iter)?))
			},
			2 => Var::Local(deserialize_valid(buf)?),
			_ => return Err(io::Error::new(io::ErrorKind::InvalidData, "Invalid Tag")),
		})
	}

	fn deserialize_type<'b>(bump: &Bump, buf: &mut impl Read) -> io::Result<&'b Type<'b>> {
		let ty = match deserialize_bytes::<1>(buf)?[0] {
			0 => Type::U(u32::from_be_bytes(deserialize_bytes::<4>(buf)?)),
			1 => Type::I(u32::from_be_bytes(deserialize_bytes::<4>(buf)?)),
			2 => Type::B(u32::from_be_bytes(deserialize_bytes::<4>(buf)?)),
			3 => Type::F(u32::from_be_bytes(deserialize_bytes::<4>(buf)?)),
			4 => Type::Usize,
			5 => Type::Isize,
			6 => Type::None,
			7 => Type::Never,
			8 => Type::Ptr(deserialize_type(bump, buf)?),
			9 => Type::Mut(deserialize_type(bump, buf)?),
			10 => {
				let len = u32::from_be_bytes(deserialize_bytes::<4>(buf)?) as usize;
				let args = bump.try_alloc_from_iter((0..len).map(|_| deserialize_type(bump, buf)))?;
				let ret = deserialize_type(bump, buf)?;
				Type::Fn(args, ret)
			},
			11 => {
				let t = deserialize_type(bump, buf)?;
				let size = match deserialize_bytes::<1>(buf)?[0] != 0 {
					true => Some(u64::from_be_bytes(deserialize_bytes::<8>(buf)?)),
					_    => None,
				};
				Type::Arr(t, size)
			},
			_ => return Err(io::Error::new(io::ErrorKind::InvalidData, "Invalid Tag")),
		};

		Ok(bump.alloc(ty))
	}

	fn deserialize_valid(buf: &mut impl Read) -> io::Result<ValId> {
		Ok(ValId(u64::from_be_bytes(deserialize_bytes::<8>(buf)?)))
	}

	fn deserialize_str<'b>(bump: &Bump, buf: &mut impl Read) -> io::Result<&'b str> {
		let len = u32::from_be_bytes(deserialize_bytes::<4>(buf)?) as usize;
		let buf = bump.try_alloc_from_iter((0..len).map(|_| deserialize_bytes::<1>(buf).map(|b| b[0])))?;

		Ok(bump.alloc_str(std::str::from_utf8(buf).map_err(
			|e| io::Error::new(io::ErrorKind::InvalidData, "Invalid UTF8"))?))
	}
}
