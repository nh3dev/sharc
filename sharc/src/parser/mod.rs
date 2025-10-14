use crate::lexer::{Token, TokenKind};
use crate::report::{ReportKind, Result, Report, Reportable};
use crate::span::{Spannable, Sp};
use crate::bigint::IBig;
use bump::Bump;

pub mod ast;
use ast::{Node, Primitive, LambdaArg};

pub struct Parser<'src, 'b, 'r> {
	tokens:   Vec<Token<'src>>,
	index:    usize,
	reporter: &'r mut crate::Reporter,
	bump:     Bump,
	bump_lt:  std::marker::PhantomData<&'b Bump>,
}

impl<'src, 'b, 'r> Parser<'src, 'b, 'r> {
	#[inline]
	fn current(&self) -> Token<'src> {
		self.tokens[self.index]
	}

	#[inline]
	fn advance_if<F: FnOnce(TokenKind) -> bool>(&mut self, f: F) -> bool {
		f(self.current().kind).then(|| self.advance()).is_some()
	}

	#[inline]
	fn advance(&mut self) {
		self.index += 1;
	}

	#[inline]
	fn log(&mut self, report: Report<ReportKind>) {
		self.reporter.nom(report);
	}

	pub fn parse(tokens: Vec<Token<'src>>, reporter: &'r mut crate::Reporter) -> (Node<'src, 'b>, Bump) {
		if tokens.is_empty() { 
			return (Node::Block(&[]), Bump::new());
		}

		let mut parser = Self { reporter, bump: Bump::new(), tokens, index: 0, bump_lt: std::marker::PhantomData };

		let block = parser.parse_block(true);

		(Node::Block(parser.bump.alloc_from_vec(block)), parser.bump)
	}

	fn parse_block(&mut self, global: bool) -> Vec<Sp<Node<'src, 'b>>> {
		let mut exprs = Vec::new();
		let until = if global { TokenKind::EOF } else { TokenKind::RBrace };

		let mut ret = false;

		while self.current().kind != until {
			ret = false;

			match self.parse_expr(0) {
				Ok(expr) => exprs.push(expr),
				// FIXME: this behaviour is causing headaches.. find someone who knows how to do this shit
				Err(report) => {
					self.log(*report);

					// TODO: check if this is correct
					if matches!(self.current().kind, TokenKind::EOF) { break; }
					while !matches!(self.current().kind, 
						TokenKind::Semicolon | TokenKind::RBrace) 
					{ self.advance(); }
					self.advance();
					continue;
				},
			}

			match self.advance_if(|t| matches!(t, TokenKind::Semicolon)) {
				false if self.current().kind == until => ret = true,
				false => self.log(ReportKind::UnexpectedToken
					.title("Expected expression to end with ';'")
					.span(self.current().span)),
				_ => {},
			}
		}

		if !ret {
			let last_span = exprs.last().map_or(self.current().span, |n| n.span);
			exprs.push(Node::None.span(last_span));
		}

		self.advance();
		exprs
	}

	fn parse_expr(&mut self, mbp: u8) -> Result<Sp<Node<'src, 'b>>> {
		let token = self.current();

		let mut lhs = match token.kind {
			TokenKind::LBrace => {
				self.advance();
				let body = self.parse_block(false);
				let span = body.last().map_or(token.span, |n| token.span.extend(&n.span));

				Node::Block(self.bump.alloc_from_vec(body)).span(span)
			},
			TokenKind::Pipe | TokenKind::PipePipe => self.parse_lambda()?,
			TokenKind::KWExtern | TokenKind::KWExport => {
				self.advance();

				if !matches!(self.current().kind, TokenKind::StringLiteral) {
					return ReportKind::UnexpectedToken
						.title("Expected string literal")
						.span(self.current().span).as_err();
				}

				let sym = self.bump.alloc_str(&parse_str(self.current())?).span(self.current().span);
				self.advance();
				let mut lam = self.parse_lambda()?;

				match (token.kind, &mut lam.elem) {
					(TokenKind::KWExport, &mut Node::Lambda { ref mut export, .. }) => *export = Some(sym),
					(TokenKind::KWExtern, &mut Node::Lambda { ref mut ext, .. }) => *ext = Some(sym),
					_ => unreachable!(),
				}

				lam
			},
			TokenKind::KWLoop => {
				self.advance();

				let let_expr = match self.current().kind {
					TokenKind::KWLet => {
						let expr = self.parse_assign(false)?;
						Some(self.bump.alloc(expr))
					},
					_ => None,
				};

				let expr = self.parse_expr(0)?;

				let end_span = expr.span;

				Node::Loop { initlet: let_expr, expr: self.bump.alloc(expr) }
					.span(token.span.extend(&end_span))
			},
			TokenKind::KWLet => self.parse_assign(false)?,
			TokenKind::KWStatic => self.parse_assign(true)?,
			t if let Some(pow) = t.pbind_power() => {
				self.advance();

				let rhs  = self.parse_expr(pow.get())?;
				let span = rhs.span;

				(match token.kind {
					TokenKind::Minus     => Node::Neg,
					TokenKind::KWMut     => Node::Mut,
					TokenKind::KWMove    => Node::Move,
					TokenKind::Star      => Node::Star,
					TokenKind::Ampersand => Node::Amp,
					_ => unreachable!(),
				})(self.bump.alloc(rhs)).span(token.span.extend(&span))
			},
			TokenKind::LBracket => {
				self.advance();

				let mut elems = Vec::new();
				loop {
					match self.current().kind {
						TokenKind::RBracket | TokenKind::Semicolon => break,
						TokenKind::Comma => self.advance(),
						_ => elems.push(self.parse_expr(0)?),
					}
				}

				let size = match self.current().kind {
					TokenKind::RBracket => None,
					TokenKind::Semicolon => {
						self.advance();
						let Node::IntLit(size) = self.parse_atom()?.elem else {
							return ReportKind::UnexpectedToken
								.title("Expected integer literal for array size")
								.span(self.current().span).as_err();
						};

						Some(size.try_as_u64().ok_or_else(|| ReportKind::InvalidNumber
							.title("intager literal too large for array size")
							.label("max size is 2^64-1")
							.span(self.current().span))?)
					},
					_ => return ReportKind::UnexpectedToken
						.title("Expected ']' or ';'")
						.span(self.current().span).as_err(),
				};

				let end = self.current().span;

				self.advance_if(|t| matches!(t, TokenKind::RBracket)).then_some(())
					.ok_or_else(|| ReportKind::UnexpectedToken
						.title(format!("Expected ']', got '{:?}'", self.current().kind))
						.span(self.current().span))?;

				Node::ArrayLit(self.bump.alloc_from_vec(elems), size)
					.span(token.span.extend(&end))
			},
			TokenKind::LParen => {
				enum Kind { Union, Struct, None }
				let mut kind = Kind::None;

				self.advance();
				let mut args = Vec::new();

				loop {
					match self.current().kind {
						TokenKind::RParen | TokenKind::EOF => break,
						TokenKind::Comma if matches!(kind, Kind::Struct) => self.advance(),
						TokenKind::Pipe  if matches!(kind, Kind::Union ) => self.advance(),
						TokenKind::Comma if matches!(kind, Kind::None) => {
							self.advance();
							kind = Kind::Struct;
						},
						TokenKind::Pipe  if matches!(kind, Kind::None) => {
							self.advance();
							kind = Kind::Union;
						},
						_ => args.push(self.parse_expr(0)?),
					}
				}

				self.advance_if(|t| matches!(t, TokenKind::RParen)).then_some(())
					.ok_or_else(|| ReportKind::UnexpectedToken
						.title("Expected ')'")
						.span(self.current().span))?;

				let args = self.bump.alloc_from_vec(args);

				match kind {
					Kind::Struct | Kind::None => Node::StructLit(args),
					Kind::Union  => Node::UnionLit(args),
				}.span(token.span.extend(&self.current().span))
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
								match self.current().kind {
									TokenKind::RParen | TokenKind::EOF => break,
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
								lhs:  self.bump.alloc(lhs),
								args: self.bump.alloc_from_vec(args),
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
						TokenKind::Plus      => Node::Add,
						TokenKind::Minus     => Node::Sub,
						TokenKind::Star      => Node::Mul,
						TokenKind::Slash     => Node::Div,
						TokenKind::Percent   => Node::Mod,
						TokenKind::Colon     => Node::Field,
						TokenKind::KWAs      => Node::As,
						TokenKind::Equals    => {
							lhs = Node::Store(self.bump.alloc_box(lhs), self.bump.alloc_box(rhs))
								.span(lspan.extend(&rspan));
							continue;
						},
						_ => unreachable!(),
					}(self.bump.alloc(lhs), self.bump.alloc(rhs)).span(lspan.extend(&rspan));
				},
				_ => return ReportKind::UnexpectedToken
					.title(format!("Expected operator, got {:?}", token.text))
					.span(token.span).as_err(),
			}
		}

		Ok(lhs)
	}

	fn parse_lambda(&mut self) -> Result<Sp<Node<'src, 'b>>> {
		let start = self.current().span;

		let args = match self.current().kind {
			TokenKind::PipePipe => &[][..],
			TokenKind::Pipe => {
				self.advance();
				let mut args = Vec::new();
				loop {
					let token = self.current();
					match token.kind {
						TokenKind::Pipe => break,
						TokenKind::Comma => self.advance(),
						TokenKind::Identifier => {
							let ident = token.text.span(token.span);
							self.advance();

							let (ty, default) = match self.current().kind {
								TokenKind::Colon => {
									self.advance();
									let expr = self.parse_expr(0)?;
									match expr.elem {
										Node::Store(lhs, rhs) => (Some(lhs.into_inner()), Some(rhs.into_inner())),
										_ => (Some(expr), None),
									}
								},
								TokenKind::Equals => {
									self.advance();
									(None, Some(self.parse_expr(0)?))
								},
								TokenKind::Comma | TokenKind::Pipe => (None, None),
								_ => return ReportKind::UnexpectedToken
									.title("Expected ',', '|', or '='")
									.span(self.current().span).as_err(),
							};

							args.push(LambdaArg { ident, ty, default });
						},
						_ => return ReportKind::UnexpectedToken
							.title("Expected identifier")
							.span(token.span).as_err(),
					}
				}
				self.bump.alloc_from_vec(args)
			},
			_ => return ReportKind::UnexpectedToken
				.title("Expected identifier")
				.span(self.current().span).as_err(),
		};

		self.advance();

		let ret = match self.current().kind {
			TokenKind::Colon => {
				self.advance();
				let expr = self.parse_expr(0)?;
				Some(self.bump.alloc(expr))
			},
			_ => None,
		};

		let body = match self.current().kind {
			TokenKind::Semicolon => None,
			_ => {
				let expr = self.parse_expr(0)?;
				Some(self.bump.alloc(expr))
			},
		};

		Ok(Node::Lambda { args, ret, body, ext: None, export: None }
			.span(start.extend(&self.current().span)))
	}

	fn parse_assign(&mut self, stat: bool) -> Result<Sp<Node<'src, 'b>>> {
		let start = self.current().span;
		self.advance();

		let gener = match self.current().kind {
			TokenKind::LessThan => {
				self.advance();

				let mut out = Vec::with_capacity(4);

				loop {
					let c = self.current();
					match c.kind {
						TokenKind::GreaterThan => break,
						TokenKind::Comma => self.advance(),
						TokenKind::Identifier => out.push(c.text.span(c.span)),
						_ => return ReportKind::UnexpectedToken
							.title("Expected identifier, ',', or '>'")
							.span(c.span).as_err(),
					}
				}
				self.advance();

				self.bump.alloc_from_vec(out)
			},
			_ => &[],
		};

		let Sp { elem: Node::Ident(lit), span } = self.parse_ident()? 
			else { unreachable!() };

		let (ty, expr) = match self.current().kind {
			TokenKind::Equals => {
				self.advance();
				(None, self.parse_expr(0)?)
			},
			TokenKind::Colon  => {
				self.advance();
				let expr = self.parse_expr(0)?;
				match expr.elem {
					Node::Store(lhs, rhs) => (Some(lhs), rhs.into_inner()),
					_ => (None, expr)
				}
			},
			_ => return ReportKind::UnexpectedToken
				.title(format!("Expected ':' or '=', got '{:?}'", self.current().kind))
				.span(self.current().span).as_err(),
		};

		Ok(Node::Let {
			ty, stat, gener,
			expr:  self.bump.alloc(expr), 
			ident: lit.span(span),
		}.span(start.extend(&self.current().span)))
	}

	fn parse_atom(&mut self) -> Result<Sp<Node<'src, 'b>>> {
		let token = self.current();
		self.advance();

		Ok(match token.kind {
			TokenKind::Quote => Node::Quote(token.text).span(token.span),
			TokenKind::Identifier => Node::Primitive(match token.text {
				// FIXME: not an atom, move this to expr
				"fn"    => {
					self.advance_if(|t| matches!(t, TokenKind::LParen)).then_some(())
						.ok_or_else(|| ReportKind::UnexpectedToken
							.title("Expected '(' after 'fn'")
							.span(self.current().span))?;

					let mut args = Vec::new();

					loop {
						match self.current().kind {
							TokenKind::EOF => return ReportKind::UnexpectedToken
								.title("Expected ')', got EOF")
								.span(self.current().span).as_err(),
							TokenKind::RParen => {
								self.advance();
								break;
							},
							TokenKind::Comma => self.advance(),
							_ => args.push(self.parse_expr(0)?),
						}
					}

					let ret = match self.current().kind {
						TokenKind::Colon => {
							self.advance();
							let expr = self.parse_expr(0)?;
							Some(self.bump.alloc(expr))
						},
						_ => None,
					};

					Primitive::Fn(self.bump.alloc_from_vec(args), ret)
				},
				"isize" => Primitive::Isize,
				"usize" => Primitive::Usize,
				"none"  => Primitive::None,
				"never" => Primitive::Never,
				"type"  => Primitive::Type,
				n if let Some(n) = n.strip_prefix('u') => Primitive::U(n.parse()
					.map_err(|_| ReportKind::InvalidNumber
						.title("Invalid integer in primitive type")
						.label("try 'u8'")
						.span(token.span))?),
				n if let Some(n) = n.strip_prefix('i') => Primitive::I(n.parse()
					.map_err(|_| ReportKind::InvalidNumber
						.title("Invalid integer in primitive type")
						.label("try 'i8'")
						.span(token.span))?),
				n if let Some(n) = n.strip_prefix('b') => Primitive::B(n.parse()
					.map_err(|_| ReportKind::InvalidNumber
						.title("Invalid integer in primitive type")
						.label("try 'b8'")
						.span(token.span))?),
				n if let Some(n) = n.strip_prefix('f') => Primitive::F(n.parse()
					.map_err(|_| ReportKind::InvalidNumber
						.title("Invalid integer in primitive type")
						.label("try 'f8'")
						.span(token.span))?),
				_ => {
					self.index = self.index.saturating_sub(1); // FIXME: def wont come to bite me in the arse
					return self.parse_ident();
				},
			}).span(token.span),
			TokenKind::DecimalIntLiteral => Node::IntLit(IBig::from_str(&self.bump, token.text)
				.map_err(|e| ReportKind::InvalidNumber
					.title(format!("Invalid integer literal: {e}"))
					.span(token.span))?)
				.span(token.span),
			TokenKind::StringLiteral =>
				Node::StrLit(self.bump.alloc_str(&parse_str(token)?)).span(token.span),
			_ => return ReportKind::UnexpectedToken
				.title(format!("Expected expression, got {:?}", token.text))
				.span(token.span).as_err(),
		})
	}

	fn parse_ident(&mut self) -> Result<Sp<Node<'src, 'b>>> {
		let token = self.current();
		self.advance_if(|t| matches!(t, TokenKind::Identifier)).then_some(())
			.ok_or_else(|| ReportKind::UnexpectedToken
				.title(format!("Expected identifier, got '{:?}'", self.current().kind))
				.span(self.current().span))?;

		Ok(Node::Ident(token.text).span(token.span))
	}
}

fn parse_str(tok: Token) -> Result<String> {
	let parse_char = |c| Some(match c {
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
		_ => return None,
	});

	let mut new_text = String::with_capacity(tok.text.len());

	let mut escape_flag = false;
	for c in tok.text.chars() {
		match escape_flag {
			true => {
				new_text.push(parse_char(c)
					.ok_or_else(|| ReportKind::UnexpectedCharacter
						.title(format!("Invalid escape sequence: '\\{c}'"))
						.span(tok.span))?);
				escape_flag = false; 
			},
			_ if c == '\\' => escape_flag = true,
			_ => new_text.push(c),
		}
	}

	Ok(new_text)
}
