use crate::lexer::{Token, TokenKind};
use crate::report::{LogHandler, ReportKind, Result};
use crate::span::{Spannable, Sp};
use crate::bigint::IBig;
use crate::bump::Box;

pub mod ast;
use ast::{Node, Primitive, LambdaArg};

pub struct Parser {
	tokens:   Vec<Token>,
	index:    usize,
	handler:  LogHandler,
	filename: &'static str,
}

impl Parser {
	#[inline]
	fn current(&self) -> Token {
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
	fn log(&self, report: crate::report::Report) {
		self.handler.log(report.file(self.filename));
	}

	fn alloc<T>(&self, elem: T) -> Box<T> {
		crate::bump::THREAD_BUMP.with(|a| a.alloc(elem).into_static_unsafe())
	}

	fn alloc_vec<T>(&self, elems: Vec<T>) -> Box<[T]> {
		crate::bump::THREAD_BUMP.with(|a| a.alloc_from_vec(elems).into_static_unsafe())
	}

	pub fn parse(tokens: Vec<Token>, filename: &'static str, handler: LogHandler) -> ast::AST {
		if tokens.is_empty() { 
			return Box::empty_slice();
		}

		let mut parser = Self {
			tokens, handler, filename,
			index: 0,
		};

		parser.parse_block(true)
	}

	fn parse_block(&mut self, global: bool) -> Box<[Sp<Node>]> {
		let mut exprs = Vec::new();
		let until = if global { TokenKind::EOF } else { TokenKind::RBrace };

		while self.current().kind != until {
			match self.parse_stmt() {
				Ok(expr) => exprs.push(expr),
				// FIXME: this behaviour is causing headaches... find someone who knows how to do this shit
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
		self.alloc_vec(exprs)
	}

	fn parse_stmt(&mut self) -> Result<Sp<Node>> {
		let token = self.current();
		let stmt = match token.kind {
			TokenKind::KWLet    => self.parse_assign(false)?,
			TokenKind::KWStatic => self.parse_assign(true)?,
			_ => self.parse_expr(0)?,
		};

		self.advance_if(|t| matches!(t, TokenKind::Semicolon));

		Ok(stmt)
	}

	fn parse_expr(&mut self, mbp: u8) -> Result<Sp<Node>> {
		let token = self.current();

		let mut lhs = match token.kind {
			TokenKind::LBrace => {
				self.advance();
				let body = self.parse_block(false);
				let span = body.last().map_or(token.span, |n| token.span.extend(&n.span));

				Node::Block(body).span(span)
			},
			TokenKind::Pipe | TokenKind::PipePipe => self.parse_lambda()?,
			TokenKind::KWExtern | TokenKind::KWExport => {
				self.advance();

				if !matches!(self.current().kind, TokenKind::StringLiteral) {
					return ReportKind::UnexpectedToken
						.title("Expected string literal")
						.span(self.current().span).as_err();
				}

				let sym = parse_str(self.current())?.span(self.current().span);
				self.advance();
				let mut lam = self.parse_lambda()?;

				match (token.kind, &mut lam.elem) {
					(TokenKind::KWExport, &mut Node::Lambda { ref mut export, .. }) => *export = Some(sym),
					(TokenKind::KWExtern, &mut Node::Lambda { ref mut ext, .. }) => *ext = Some(sym),
					_ => unreachable!(),
				}

				lam
			},
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
				})(self.alloc(rhs)).span(token.span.extend(&span))
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

						Some((&size).try_into().map_err(|_| ReportKind::InvalidNumber
							.title("intager literal too large for array size")
							.label("max size is 2^64-1")
							.span(self.current().span))?)
					},
					_ => return ReportKind::UnexpectedToken
						.title("Expected ']' or ';'")
						.span(self.current().span).as_err(),
				};

				self.advance_if(|t| matches!(t, TokenKind::RBracket)).then_some(())
					.ok_or_else(|| ReportKind::UnexpectedToken
						.title(format!("Expected ']', got '{:?}'", self.current().kind))
						.span(self.current().span))?;

				Node::ArrayLit(self.alloc_vec(elems), size).span(token.span.extend(&self.current().span))
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

				let args = self.alloc_vec(args);

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
								lhs:  self.alloc(lhs),
								args: self.alloc_vec(args),
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
						TokenKind::Equals    => Node::Store,
						TokenKind::Colon     => Node::Field,
						_ => unreachable!(),
					}(self.alloc(lhs), self.alloc(rhs)).span(lspan.extend(&rspan));
				},
				_ => return ReportKind::UnexpectedToken
					.title(format!("Expected operator, got {:?}", token.text))
					.span(token.span).as_err(),
			}
		}

		Ok(lhs)
	}

	fn parse_lambda(&mut self) -> Result<Sp<Node>> {
		let start = self.current().span;

		let args = match self.current().kind {
			TokenKind::PipePipe => Box::empty_slice(),
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
										Node::Store(lhs, rhs) => (Some(Box::into_inner(lhs)), Some(Box::into_inner(rhs))),
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
				self.alloc_vec(args)
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
				Some(self.alloc(expr))
			},
			_ => None,
		};

		let body = match self.current().kind {
			TokenKind::Semicolon => None,
			_ => {
				let expr = self.parse_expr(0)?;
				Some(self.alloc(expr))
			},
		};

		Ok(Node::Lambda { args, ret, body, ext: None, export: None }
			.span(start.extend(&self.current().span)))
	}

	fn parse_assign(&mut self, stat: bool) -> Result<Sp<Node>> {
		self.advance();
		let Sp { elem: Node::Ident { lit, gener }, span } = self.parse_ident()?
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
					Node::Store(lhs, rhs) => (Some(lhs), Box::into_inner(rhs)),
					_ => (None, expr)
				}
			},
			_ => return ReportKind::UnexpectedToken
				.title(format!("Expected ':' or '=', got '{:?}'", self.current().kind))
				.span(self.current().span).as_err(),
		};

		Ok(Node::Let {
			ty, stat, gener,
			expr:  self.alloc(expr), 
			ident: lit,
		}.span(span.extend(&self.current().span)))
	}

	fn parse_atom(&mut self) -> Result<Sp<Node>> {
		let token = self.current();
		self.advance();

		Ok(match token.kind {
			TokenKind::Quote => Node::Quote(token.text).span(token.span),
			TokenKind::Identifier => Node::Primitive(match token.text {
				"isize" => Primitive::Isize,
				"usize" => Primitive::Usize,
				"any"   => Primitive::Any,
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
			TokenKind::DecimalIntLiteral => Node::IntLit(token.text.parse::<IBig>()
				.map_err(|e| ReportKind::InvalidNumber
					.title(format!("Invalid integer literal: {e}"))
					.span(token.span))?)
				.span(token.span),
			TokenKind::StringLiteral =>
				Node::StrLit(parse_str(token)?).span(token.span),
			_ => return ReportKind::UnexpectedToken
				.title(format!("Expected expression, got {:?}", token.text))
				.span(token.span).as_err(),
		})
	}

	fn parse_ident(&mut self) -> Result<Sp<Node>> {
		let token = self.current();
		self.advance_if(|t| matches!(t, TokenKind::Identifier)).then_some(())
			.ok_or_else(|| ReportKind::UnexpectedToken
				.title(format!("Expected identifier, got '{:?}'", self.current().kind))
				.span(self.current().span))?;

		// FIXME: do something about the whole ident<20 vs ident<20>
		let gener = match self.current().kind {
			TokenKind::LessThan => self.parse_generics()?,
			_ => Box::empty_slice(),
		};

		let span = gener.last().map_or(token.span, |g| g.span.extend(&token.span));

		Ok(Node::Ident { lit: token.text.span(token.span), gener }.span(span))
	}

	fn parse_generics(&mut self) -> Result<Box<[Sp<Node>]>> {
		let token = self.current();
		self.advance_if(|t| matches!(t, TokenKind::LessThan)).then_some(())
			.ok_or_else(|| ReportKind::UnexpectedToken
				.title("Expected '<' for generics")
				.span(token.span))?;

		let mut out = Vec::new();

		loop {
			match self.current().kind {
				TokenKind::GreaterThan => break,
				TokenKind::Comma => self.advance(),
				_ => out.push(self.parse_expr(0)?),
			}
		}
		self.advance();

		Ok(self.alloc_vec(out))
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
