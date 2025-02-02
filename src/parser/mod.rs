use crate::lexer::{Token, TokenKind};
use crate::report::{LogHandler, ReportKind, Result};
use crate::span::{Spannable, Sp};
use crate::bigint::IBig;

pub mod ast;
use ast::{Node, Type, Attrs};

pub struct Parser<'src> {
	tokens:   Vec<Token<'src>>,
	index:    usize,
	handler:  LogHandler,
	filename: &'static str,
}

impl<'src> Parser<'src> {
	#[inline]
	fn current(&self) -> Token<'src> {
		self.tokens[self.index]
	}

	#[inline]
	fn advance_if<F: FnOnce(TokenKind) -> bool>(&mut self, f: F) -> bool {
		if f(self.current().kind) { self.advance(); true } else { false }
	}

	#[inline]
	fn advance(&mut self) {
		self.index += 1;
	}

	#[inline]
	fn log(&self, report: crate::report::Report) {
		self.handler.log(report.file(self.filename));
	}

	pub fn parse(tokens: Vec<Token<'src>>, filename: &'static str, handler: LogHandler) -> Vec<Sp<Node<'src>>> {
		if tokens.is_empty() { return Vec::new(); }

		let mut parser = Self {
			tokens, handler, filename,
			index: 0,
		};

		parser.parse_block(true)
	}

	fn parse_block(&mut self, global: bool) -> Vec<Sp<Node<'src>>> {
		let mut exprs = Vec::new();
		let until = if global { TokenKind::EOF } else { TokenKind::RBrace };

		while self.current().kind != until {
			match self.parse_stmt() {
				Ok(expr) => exprs.push(expr),
				Err(report) => {
					self.log(*report);

					// TODO: check if this is correct
					if matches!(self.current().kind, TokenKind::EOF) { break; }
					while !matches!(self.current().kind, 
						TokenKind::Semicolon | TokenKind::RBrace) 
					{ self.advance(); }
					self.advance();
				},
			}
		}

		self.advance();
		exprs
	}

	fn parse_stmt(&mut self) -> Result<Sp<Node<'src>>> {
		let token = self.current();
		let stmt = match token.kind {
			TokenKind::KWLet    => self.parse_assign(false)?,
			TokenKind::KWStatic => self.parse_assign(true)?,
			_ => {
				let expr = self.parse_expr(0)?;

				return match self.advance_if(|t| matches!(t, TokenKind::Semicolon)) {
					true  => Ok(expr),
					false => {
						let span = expr.span;
						Ok(Node::ImplRet(expr.into()).span(span))
					},
				}
			},
		};

		self.advance_if(|t| matches!(t, TokenKind::Semicolon)).then_some(())
			.ok_or_else(|| ReportKind::UnexpectedToken
				.title("Expected ';'")
				.span(self.current().span))?;

		Ok(stmt)
	}

	fn parse_expr(&mut self, mbp: u8) -> Result<Sp<Node<'src>>> {
		let token = self.current();

		let mut lhs = match token.kind {
			// TODO: for<T: Display> |a: T| T
			TokenKind::Pipe | TokenKind::PipePipe => self.parse_lambda()?,
			t if let Some(pow) = t.pbind_power() => {
				self.advance();

				let rhs = self.parse_expr(pow.get())?;
				let span = rhs.span;

				match token.kind {
					TokenKind::Minus => Node::Neg(rhs.into()),
					_ => unreachable!(),
				}.span(token.span.extend(&span))
			},
			_ => self.parse_atom()?,
		};

		loop {
			let token = self.current();
			match token.kind {
				TokenKind::EOF    => break,
				t if t.is_delim() => break,
				t if let Some(pow) = t.sbind_power() => {
					if pow.get() < mbp { break; }

					let token = self.current();

					self.advance();
					lhs = match token.kind {
						TokenKind::LParen => {
							let mut args = Vec::new();

							loop {
								let token = self.current();
								match token.kind {
									TokenKind::RParen => break,
									TokenKind::Comma => self.advance(),
									_ => args.push(self.parse_expr(0)?),
								}
							}

							self.advance_if(|t| matches!(t, TokenKind::RParen)).then_some(())
								.ok_or_else(|| ReportKind::UnexpectedToken
									.title("Expected ')' or expression")
									.span(self.current().span))?;

							let span = lhs.span.extend(&token.span);

							Node::FuncCall {
								lhs: lhs.into(),
								args,
							}.span(span)
						},
						_ => todo!(),
					};
				},
				t if let Some(pow) = t.ibind_power() => {
					if pow.get() < mbp { break; }

					self.advance();
					let rhs = self.parse_expr(pow.get())?;

					let lspan = lhs.span;
					let rspan = rhs.span;

					lhs = match token.kind {
						TokenKind::Plus    => Node::Add,
						TokenKind::Minus   => Node::Sub,
						TokenKind::Star    => Node::Mul,
						TokenKind::Slash   => Node::Div,
						TokenKind::Percent => Node::Mod,
						_ => unreachable!(),
					}(lhs.into(), rhs.into()).span(lspan.extend(&rspan));
				},
				_ => return ReportKind::UnexpectedToken
					.title(format!("Expected operator, got {:?}", token.text))
					.span(token.span).as_err(),
			}
		}

		Ok(lhs)
	}

	fn parse_lambda(&mut self) -> Result<Sp<Node<'src>>> {
		let start = self.current().span;

		let args = match self.current().kind {
			TokenKind::PipePipe => Vec::new(),
			TokenKind::Pipe => {
				self.advance();
				let token = self.current();

				let mut args = Vec::new();
				loop {
					let token = self.current();
					match token.kind {
						TokenKind::Pipe => break,
						TokenKind::Comma => self.advance(),
						TokenKind::Identifier => {
							let name = token.text.span(token.span);
							self.advance();

							let ty = match self.current().kind {
								TokenKind::Colon => {
									self.advance();
									Some(self.parse_type()?)
								},
								TokenKind::Comma | TokenKind::Pipe => None,
								_ => return ReportKind::UnexpectedToken
									.title("Expected ',' or '|'")
									.span(self.current().span).as_err(),
							};

							args.push((name, ty));
						},
						_ => return ReportKind::UnexpectedToken
							.title("Expected identifier")
							.span(token.span).as_err(),
					}
				}
				args
			},
			_ => return ReportKind::UnexpectedToken
				.title("Expected identifier")
				.span(self.current().span).as_err(),
		};

		self.advance();

		let (body, ret) = match self.current().kind {
			TokenKind::Colon     => {
				self.advance();
				(vec![self.parse_expr(0)?], None)
			},
			TokenKind::LBrace    => {
				self.advance();
				(self.parse_block(false), None)
			},
			TokenKind::Semicolon => {
				self.advance();
				(Vec::new(), None)
			},
			_ => {
				let ty = self.parse_type()?;

				let token = self.current();
				self.advance();

				(match token.kind {
					TokenKind::Colon     => vec![self.parse_expr(0)?],
					TokenKind::LBrace    => self.parse_block(false),
					TokenKind::Semicolon => Vec::new(),
					_ => return ReportKind::UnexpectedToken
						.title("Expected '{', ';', or ':'")
						.span(token.span).as_err()?,
				}, Some(ty))
			},
		};

		Ok(Node::Lambda { args, ret, body }
			.span(start.extend(&self.current().span)))
	}

	fn parse_assign(&mut self, stat: bool) -> Result<Sp<Node<'src>>> {
		self.advance();
		let token = self.current();

		self.advance_if(|t| matches!(t, TokenKind::Identifier)).then_some(())
			.ok_or_else(|| ReportKind::UnexpectedToken
				.title(format!("Expected identifier, got '{:?}'", self.current().kind))
				.span(self.current().span))?;

		let (ty, expr) = match self.current().kind {
			TokenKind::Equals => {
				self.advance();
				(None, self.parse_expr(0)?)
			},
			TokenKind::Colon  => {
				self.advance();
				let ty = Some(self.parse_type()?);
				self.advance_if(|t| matches!(t, TokenKind::Equals)).then_some(())
					.ok_or_else(|| ReportKind::UnexpectedToken
						.title(format!("Expected '=', got '{:?}'", self.current().kind))
						.span(self.current().span))?;
				(ty, self.parse_expr(0)?)
			},
			_ => return ReportKind::UnexpectedToken
				.title(format!("Expected ':' or '=', got '{:?}'", self.current().kind))
				.span(self.current().span).as_err(),
		};

		Ok(Node::Assign { 
			ty, stat, 
			expr: expr.into(), 
			name: token.text.span(token.span),
		}.span(token.span.extend(&self.current().span)))
	}

	fn parse_atom(&mut self) -> Result<Sp<Node<'src>>> {
		let token = self.current();
		self.advance();

		Ok(match token.kind {
			TokenKind::Identifier => Node::Ident(token.text),
			TokenKind::DecimalIntLiteral => 
				Node::IntLit(token.text.parse::<IBig>()
					.map_err(|e| ReportKind::InvalidNumber
						.title(format!("Invalid integer literal: {e}"))
						.span(token.span))?),
			TokenKind::StringLiteral => {
				let text = token.text;

				let mut new_text = String::with_capacity(text.len());

				let mut escape_flag = false;
				for c in text.chars() {
					if escape_flag {
						new_text.push(parse_char(c));
						escape_flag = false; 
					} else if c == '\\' {
						escape_flag = true;
					} else {
						new_text.push(c);
					}
				}

				self.advance();
				Node::StrLit(new_text)
			},
			_ => return ReportKind::UnexpectedToken
				.title(format!("Expected atom, got {:?}", token.text))
				.span(token.span).as_err(),
		}.span(token.span))
	}

	// 	let ast = match self.current().kind {
	// 		TokenKind::KWRet => {
	// 			self.advance();
	//
	// 			match self.current().kind {
	// 				TokenKind::Semicolon => Node::Ret(None),
	// 				_ => Node::Ret(Some(Box::new(self.parse_expr()?))),
	// 			}.span(self.current().span)
	// 		},
	// 		TokenKind::Identifier => {
	// 			let tok = self.current();
	// 			self.advance();
	//
	// 			match self.current().kind {
	// 				TokenKind::Equals => {
	// 					self.advance();
	// 					Node::Store {
	// 						name: tok.text.span(tok.span),
	// 						value: Box::new(self.parse_expr()?),
	// 					}.span(tok.span.extend(&self.current().span))
	// 				},
	// 				_ => todo!()
	// 			}
	// 		}
	// 	};

	// 		TokenKind::LBracket => {
	// 			self.advance();
	//
	// 			let mut exprs = Vec::new();
	// 			loop {
	// 				let expr = self.parse_expr()?;
	//
	// 				let token = self.current();
	// 				self.advance();
	// 				exprs.push(expr);
	// 				match token.kind {
	// 					TokenKind::Comma => (),
	// 					TokenKind::RBracket => break,
	// 					_ => return ReportKind::UnexpectedToken
	// 						.title(format!("Expected ',' or ']', got '{:?}'", token.kind))
	// 						.span(token.span).as_err(),
	// 				}
	// 			}
	//
	// 			Node::ArrayLit(exprs)
	// 		},
	// }

	fn parse_type(&mut self) -> Result<Sp<Type<'src>>> {
		let token = self.current();
		self.advance();

		Ok(match token.kind {
			TokenKind::Star => Type::Ptr(Box::new(self.parse_type()?)).span(token.span),
			TokenKind::LBracket => {
				let ty = self.parse_type()?;

				if matches!(self.current().kind, TokenKind::RBracket) {
					self.advance();
					return Ok(Type::Arr(Box::new(ty), None).span(token.span.extend(&self.current().span)));
				}

				let expr = self.parse_expr(0)
					.map_err(|e| e.help("Expected array size"))?;

				let Node::IntLit(ref size) = *expr else {
					return ReportKind::UnexpectedToken
						.title("Expected integer literal")
						.span(expr.span).as_err();
				};


				let size = size.try_into()
					.map_err(|_| ReportKind::InvalidNumber
						.title("Invalid integer in array size")
						.span(expr.span))?;

				self.advance_if(|t| matches!(t, TokenKind::RBracket)).then_some(())
					.ok_or_else(|| ReportKind::UnexpectedToken
						.title(format!("Expected ']' or array size, got '{:?}'", self.current().kind))
						.span(self.current().span))?;

				Type::Arr(Box::new(ty), Some(size)).span(token.span.extend(&self.current().span))
			},
			TokenKind::Identifier => match token.text {
				"isize" => Type::Isize,
				"usize" => Type::Usize,
				n if n.starts_with('u') => Type::U(n[1..].parse()
					.map_err(|_| ReportKind::InvalidNumber
						.title("Invalid integer in primitive type")
						.label("try 'u8'")
						.span(token.span))?),
				n if n.starts_with('i') => Type::I(n[1..].parse()
					.map_err(|_| ReportKind::InvalidNumber
						.title("Invalid integer in primitive type")
						.label("try 'i8'")
						.span(token.span))?),
				n if n.starts_with('b') => Type::B(n[1..].parse()
					.map_err(|_| ReportKind::InvalidNumber
						.title("Invalid integer in primitive type")
						.label("try 'b8'")
						.span(token.span))?),
				n if n.starts_with('f') => Type::F(n[1..].parse()
					.map_err(|_| ReportKind::InvalidNumber
						.title("Invalid integer in primitive type")
						.label("try 'f8'")
						.span(token.span))?),
				"void"  => Type::Void,
				"never" => Type::Never,
				"opt"   => Type::Opt(Box::new(self.parse_type()?)),
				"mut"   => Type::Mut(Box::new(self.parse_type()?)),
				n => Type::Ident(n),
			}.span(token.span),
			_ => return ReportKind::UnexpectedToken
				.title("Expected type")
				.span(token.span).as_err(),
		})
	}
}

fn parse_char(chunk: char) -> char {
	match chunk {
		'0' | '@' => '\x00',
		'A'       => '\x01',
		'B'       => '\x02',
		'C'       => '\x03',
		'D'       => '\x04',
		'E'       => '\x05',
		'F'       => '\x06',
		'G' | 'a' => '\x07',
		'H' | 'b' => '\x08',
		'I' | 't' => '\x09',
		'J' | 'n' => '\x0A',
		'K' | 'v' => '\x0B',
		'L' | 'f' => '\x0C',
		'M' | 'r' => '\x0D',
		'N'       => '\x0E',
		'O'       => '\x0F',
		'P'       => '\x10',
		'Q'       => '\x11',
		'R'       => '\x12',
		'S'       => '\x13',
		'T'       => '\x14',
		'U'       => '\x15',
		'V'       => '\x16',
		'W'       => '\x17',
		'X'       => '\x18',
		'Y'       => '\x19',
		'Z'       => '\x1A',
		'[' | 'e' => '\x1B',
		'/'       => '\x1C',
		']'       => '\x1D',
		'^'       => '\x1E',
		'_'       => '\x1F',
		'?'       => '\x7F',
		// '"'       => '\\',
		_ => unreachable!(),
	}
}
