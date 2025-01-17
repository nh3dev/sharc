use crate::lexer::{Token, TokenKind};
use crate::report::{LogHandler, ReportKind, Result};

pub mod ast;
use ast::{Node, NodeKind, Type};

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

	pub fn parse(tokens: Vec<Token<'src>>, filename: &'static str, handler: &LogHandler) -> Vec<Node<'src>> {
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

			parser.advance();
		}

		ast
	}

	fn parse_global(&mut self) -> Result<Node<'src>> {
		let token = self.current();

		match token.kind {
			TokenKind::KWFn => self.parse_func(),
			TokenKind::KWExport => { // FIXME: come up with a better way to do this
				self.advance();

				if matches!(self.current().kind, TokenKind::EOF) {
					return ReportKind::UnexpectedEOF
						.untitled().span(token.span).as_err();
				}

        let mut r = self.parse_global()?;

				match r.kind {
					NodeKind::Func { name, args, ret, body, .. } 
						=> Ok({
                r.kind = NodeKind::Func { name, args, ret, body, export: true };
                r
            }),
					// TODO: const/static
					_ => unreachable!(),
				}
			},
			_ => ReportKind::UnexpectedToken
				.untitled().span(token.span).as_err(),
		}
	}

	fn parse_func(&mut self) -> Result<Node<'src>> {
		self.advance();

		let token = self.current();
		let name = match token.kind {
			TokenKind::Identifier => token.text,
			_ => return ReportKind::UnexpectedToken
				.title("Expected identifier")
				.span(token.span).as_err(),
		};

		self.advance();

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
					let name = token.text;
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
		let (single_stmt, ret) = match self.current().kind {
			TokenKind::Colon  => (true,  None),
			TokenKind::LBrace => (false, None),
			_ => {
				let ty = self.parse_type()?;
				self.advance();
				(matches!(self.current().kind, TokenKind::Colon), Some(ty))
			},
		};

		let body = 
			if single_stmt { vec![self.parse_stmt()?] } 
			else { self.parse_block()? };

		Ok(Node { kind: NodeKind::Func { name, args, ret, body, export: false }, span: token.span.extend(&self.current().span) })
	}

	fn parse_block(&mut self) -> Result<Vec<Node<'src>>> {
		let mut body = Vec::new();

		loop {
			let token = self.current();
			match token.kind {
				TokenKind::RBrace => break,
				TokenKind::EOF => 
					return ReportKind::UnexpectedEOF
						.title("Expected '}'")
						.span(self.peek(-1).unwrap().span).as_err(),
				_ => body.push(self.parse_stmt()?),
			}
		}

		Ok(body)
	}

	fn parse_stmt(&mut self) -> Result<Node<'src>> {
		let ast = self.parse_expr()?;

		let token = self.current();
		if !matches!(token.kind, TokenKind::Semicolon) {
			return ReportKind::UnexpectedToken
				.title(format!("Expected ';', found '{}'", token.text))
				.span(token.span).as_err();
		}

		self.advance();
		Ok(ast)
	}

	fn parse_expr(&mut self) -> Result<Node<'src>> {
		let token = self.current();

		let ast = match token.kind {
			TokenKind::KWRet => {
				self.advance();
        // TODO:
        // Verify
				NodeKind::Ret(Box::new(self.parse_expr()?))
			},

			TokenKind::Dollar => {
				self.advance();
				let token = self.current();

				// TODO: allow expr?
				if token.kind != TokenKind::Identifier {
					return ReportKind::UnexpectedToken
						.title("Expected identifier")
						.span(token.span).as_err();
				}

				let name = token.text;

				self.advance();
				let args = match self.current().kind {
					TokenKind::LParen => {
						let mut args = Vec::new();
						loop {
							self.advance();
							let token = self.current();
							match token.kind {
								TokenKind::RParen => break,
								_ => {
									args.push(self.parse_expr()?);
									if !matches!(self.current().kind, TokenKind::Comma) { break; }
									self.advance();
								},
							}

							if !matches!(self.current().kind, TokenKind::RParen) {
								return ReportKind::UnexpectedToken
									.title("Expected ')'")
									.span(self.current().span).as_err();
							}
						}

						args
					},
					_ => vec![self.parse_expr()?],
				};

				NodeKind::FuncCall { name, args }
			},

			TokenKind::StringLiteral => {
				let text = token.text;
				self.advance();
				NodeKind::StrLit(text)
			},

			TokenKind::DecimalIntLiteral => {
				let text = token.text;
				self.advance();
				NodeKind::UIntLit(text.parse::<u64>()
					.map_err(|_| ReportKind::InvalidNumber
						.title("Invalid integer literal")
						.span(token.span))?)
			},

			_ => {
				return ReportKind::UnexpectedToken
					.title("Expected expression")
					.span(token.span).as_err();
			},
			// _ => self.parse_expr()?,
		};

    // TODO:
    // Verify span.
		Ok(Node { kind: ast, span: token.span.extend(&self.current().span) })
}

	fn parse_type(&mut self) -> Result<Type<'src>> {
		let token = self.current();
		self.advance();

		Ok(match token.kind {
			TokenKind::Star => Type::Ptr(Box::new(self.parse_type()?)),
			TokenKind::LBracket => {
				let ty = self.parse_type()?;

				if self.current().kind != TokenKind::RBracket {
					return ReportKind::UnexpectedToken
						.title("Expected ']'")
						.span(self.current().span).as_err();
				}
				self.advance();

				Type::Arr(Box::new(ty))
			},
			TokenKind::Identifier => match token.text {
				"u8"   => Type::U8,
				"u16"  => Type::U16,
				"u32"  => Type::U32,
				"u64"  => Type::U64,
				"i8"   => Type::I8,
				"i16"  => Type::I16,
				"i32"  => Type::I32,
				"i64"  => Type::I64,
				"b8"   => Type::B8,
				"b16"  => Type::B16,
				"b32"  => Type::B32,
				"b64"  => Type::B64,
				"void" => Type::Void,
				"mut"  => Type::Mut(Box::new(self.parse_type()?)),
				_ => Type::Ident(token.text),
			},
			_ => return ReportKind::UnexpectedToken
				.title("Expected type")
				.span(token.span).as_err(),
		})
	}
}
