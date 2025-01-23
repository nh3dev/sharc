use crate::lexer::{Token, TokenKind};
use crate::report::{LogHandler, ReportKind, Result};
use crate::span::{Spannable, Sp};

pub mod ast;
use ast::{Node, Type, Attrs};

pub struct Parser<'src> {
	tokens:  Vec<Token<'src>>,
	index:   usize,
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
	#[allow(clippy::cast_possible_wrap)]
	fn peek(&self, index: isize) -> Option<&Token<'src>> {
		let index = self.index as isize + index;
		assert!(index >= 0, "peek() out of bounds");
		self.tokens.get(unsafe { std::mem::transmute::<isize, usize>(index) })
	}

	#[inline]
	fn advance(&mut self) {
		self.index += 1;
		// assert!(self.index < self.tokens.len(), "advance() out of bounds");
	}

	pub fn parse(tokens: Vec<Token<'src>>, filename: &'static str, handler: &LogHandler) -> Vec<Sp<Node<'src>>> {
		let mut ast = Vec::new();

		if tokens.is_empty() { return ast; }

		let mut parser = Self {
			tokens,
			index: 0,
		};

		while !matches!(parser.current().kind, TokenKind::EOF) {
			match parser.parse_global() {
				Ok(global)  => ast.push(global),
				Err(report) => {
					handler.log(report.file(filename));
					if matches!(parser.current().kind, TokenKind::EOF) { break; }

					while !matches!(parser.current().kind, 
						TokenKind::Semicolon|TokenKind::RBrace) {
						parser.advance();
					}
				},
			}
		}

		ast
	}

	fn parse_global(&mut self) -> Result<Sp<Node<'src>>> {
		let token = self.current();

		match token.kind {
			TokenKind::KWFn => self.parse_func(),
			TokenKind::KWExtern | TokenKind::KWExport => { // FIXME: come up with a better way to do this
				self.advance();

				if matches!(self.current().kind, TokenKind::EOF) {
					return ReportKind::UnexpectedEOF
						.untitled().span(token.span).as_err();
				}

				let mut r = self.parse_global()?;
				match *r {
					Node::Func { .. } => Ok({
						let Node::Func { ref mut attrs, .. } = *r
							else { unreachable!() };

						attrs.push(match token.kind {
							TokenKind::KWExtern => Attrs::Extern,
							TokenKind::KWExport => Attrs::Export,
							_ => unreachable!(),
						}.span(token.span));

						r.span = token.span.extend(&r.span);
						r
					}),
					_ => unreachable!(),
				}
			},
			s => {
				self.advance();
				ReportKind::UnexpectedToken
					.title(format!("got '{s:?}'"))
					.span(token.span).as_err()
			},
		}
	}

	fn parse_func(&mut self) -> Result<Sp<Node<'src>>> {
		self.advance();

		let token = self.current();
		self.advance_if(|t| matches!(t, TokenKind::Identifier)).then_some(())
			.ok_or_else(|| ReportKind::UnexpectedToken
				.title("Expected identifier")
				.span(token.span))?;

		let name = token.text.span(token.span);

		// TODO: generic parsing

		if self.current().kind != TokenKind::LParen {
			return ReportKind::UnexpectedToken
				.title("Expected '('")
				.span(self.current().span).as_err();
		}

		let mut args = Vec::new();
		loop {
			self.advance();
			let token = self.current();
			match token.kind {
				TokenKind::RParen => break,
				TokenKind::Identifier => {
					let name = token.text.span(token.span);
					self.advance();

					let token = self.current();
					if token.kind != TokenKind::Colon {
						return ReportKind::UnexpectedToken
							.title("Expected ':'")
							.span(token.span).as_err();
					}

					self.advance();
					args.push((name, self.parse_type()?));

					if matches!(self.current().kind, TokenKind::RParen) { break; }
				},
				_ => return ReportKind::UnexpectedToken
					.title("Expected identifier")
					.span(token.span).as_err(),
			}
		}

		self.advance();
		let (body, ret) = match self.current().kind {
			TokenKind::Colon     => (vec![self.parse_stmt()?],  None),
			TokenKind::LBrace    => (self.parse_block()?, None),
			TokenKind::Semicolon => (Vec::new(), None),
			_ => {
				let ty = self.parse_type()?;

				let token = self.current();
				self.advance();

				(match token.kind {
					TokenKind::Colon     => vec![self.parse_stmt()?],
					TokenKind::LBrace    => self.parse_block()?,
					TokenKind::Semicolon => {
						self.advance();
						Vec::new()
					},
					_ => return ReportKind::UnexpectedToken
						.title("Expected '{', ';', or ':'")
						.span(token.span).as_err()?,
				}, Some(ty))
			},
		};

		Ok(Node::Func { name, args, ret, body, attrs: Vec::new() }
			.span(token.span.extend(&self.current().span)))
	}

	fn parse_block(&mut self) -> Result<Vec<Sp<Node<'src>>>> {
		let mut body = Vec::new();

		loop {
			let token = self.current();
			match token.kind {
				TokenKind::RBrace => {
					self.advance();
					break;
				},
				TokenKind::EOF => 
					return ReportKind::UnexpectedEOF
						.title("Expected '}'")
						.span(self.peek(-1).unwrap().span).as_err(),
				_ => body.push(self.parse_stmt()?),
			}
		}

		Ok(body)
	}

	fn parse_stmt(&mut self) -> Result<Sp<Node<'src>>> {
		let ast = match self.current().kind {
			TokenKind::KWLet => {
				self.advance();
				let tok = self.current();

				self.advance_if(|t| matches!(t, TokenKind::Identifier)).then_some(())
					.ok_or_else(|| ReportKind::UnexpectedToken
						.title(format!("Expected identifier, got '{:?}'", self.current().kind))
						.span(self.current().span))?;

				self.advance_if(|t| matches!(t, TokenKind::Colon)).then_some(())
					.ok_or_else(|| ReportKind::UnexpectedToken
						.title(format!("Expected ':', got '{:?}'", self.current().kind))
						.span(self.current().span))?;

				let ty = self.parse_type()?;

				self.advance_if(|t| matches!(t, TokenKind::Equals)).then_some(())
					.ok_or_else(|| ReportKind::UnexpectedToken
						.title(format!("Expected '=', got '{:?}'", self.current().kind))
						.span(self.current().span))?;

				Node::Assign {
					name: tok.text.span(tok.span),
					ty,
					value: Box::new(self.parse_expr()?),
				}.span(tok.span.extend(&self.current().span))
			},
			TokenKind::KWRet => {
				self.advance();

				match self.current().kind {
					TokenKind::Semicolon => Node::Ret(None),
					_ => Node::Ret(Some(Box::new(self.parse_expr()?))),
				}.span(self.current().span)
			},

			_ => self.parse_expr()?,
		};

		self.advance_if(|t| matches!(t, TokenKind::Semicolon)).then_some(())
			.ok_or_else(|| ReportKind::UnexpectedToken
				.title(format!("Expected ';', got '{:?}'", self.current().kind))
				.span(self.current().span))?;

		Ok(ast)
	}

	fn parse_expr(&mut self) -> Result<Sp<Node<'src>>> {
		let token = self.current();

		let ast = match token.kind {
			TokenKind::Dollar => {
				self.advance();
				let token = self.current();

				// TODO: allow expr?
				if token.kind != TokenKind::Identifier {
					return ReportKind::UnexpectedToken
						.title("Expected identifier")
						.span(token.span).as_err();
				}

				let name = token.text.span(token.span);

				// TODO: generic parsing

				self.advance();
				let args = match self.current().kind {
					TokenKind::LParen => {
						self.advance();

						let mut args = Vec::new();
						loop {
							match self.current().kind {
								TokenKind::RParen => {
									self.advance();
									break;
								},
								TokenKind::Comma => self.advance(),
								TokenKind::EOF => return ReportKind::UnexpectedEOF
									.title("Expected ')'")
									.span(self.peek(-1).unwrap().span).as_err(),
								_ => args.push(self.parse_expr()?)
							}
						}
						args
					},
					_ => vec![self.parse_expr()?],
				};

				Node::FuncCall { name, args }
			},
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
			TokenKind::DecimalIntLiteral => {
				let text = token.text;
				self.advance();
				Node::UIntLit(text.parse::<u64>()
					.map_err(|_| ReportKind::InvalidNumber
						.title("Invalid integer literal")
						.span(token.span))?)
			},

			_ => return ReportKind::UnexpectedToken
				.title("Expected expression")
				.span(token.span).as_err(),
			// _ => self.parse_expr()?,
		};

		// TODO:
		// Verify span.
		Ok(ast.span(token.span.extend(&self.current().span)))
	}

	fn parse_type(&mut self) -> Result<Sp<Type<'src>>> {
		let token = self.current();
		self.advance();

		Ok(match token.kind {
			TokenKind::Star => Type::Ptr(Box::new(self.parse_type()?)).span(token.span),
			TokenKind::LBracket => {
				let ty = self.parse_type()?;

				self.advance_if(|t| matches!(t, TokenKind::RBracket)).then_some(())
					.ok_or_else(|| ReportKind::UnexpectedToken
						.title("Expected ']'")
						.span(self.current().span))?;

				// TODO: array size
				Type::Arr(Box::new(ty), None).span(token.span.extend(&self.current().span))
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
