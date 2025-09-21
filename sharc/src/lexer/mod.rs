mod token;
pub use token::{Token, TokenKind};

use crate::Reporter;
use crate::report::{ReportKind, Report, Result, Reportable};
use crate::span::Span;

#[allow(clippy::complexity)]
pub struct Lexer<'src, 'r> {
	reporter: &'r mut Reporter,
	contents: &'src str,
	iter:     std::iter::Peekable<std::iter::Map<std::str::CharIndices<'src>, fn((usize, char)) -> usize>>,
	index:    usize,
	tokens:   Vec<Token<'src>>,
}

impl<'src, 'r> Lexer<'src, 'r> {
	fn log(&mut self, report: Report<ReportKind>) {
		self.reporter.nom(report);
	}

	fn next(&mut self) -> Option<&'src str> {
		self.iter.next().map(|i| { 
			self.index = i; 
			&self.contents[i..*self.iter.peek().unwrap_or(&self.contents.len())] 
		})
	}

	fn peek(&mut self) -> Option<&'src str> {
		self.iter.peek().and_then(|i| 
			self.contents.get(*i..*i + match self.contents.get(*i..) {
					Some(s) => s.char_indices().nth(1).map_or(self.contents.len(), |(i, _)| i),
					None => return None,
				}))
	}

	fn push_token(&mut self, kind: TokenKind, start: usize, end: usize) {
		self.tokens.push(Token { 
			kind, 
			span: Span { start, end }, 
			text: &self.contents[start..=end]
		});
	}

	fn slice(&self, start: usize, end: usize) -> &'src str {
		&self.contents[start..end]
	}

	fn span_from(&self, start: usize) -> Span {
		Span::new(start).end(self.index)
	}

	fn push_token_simple(&mut self, kind: TokenKind, len: usize) {
		let index = self.index;
		(0..len-1).for_each(|_| { self.iter.next().expect("Unexpected EOF (this is a bug)"); });
		self.push_token(kind, index, self.index);
	}

	pub fn tokenize(contents: &'src str, reporter: &'r mut Reporter) -> Vec<Token<'src>> {
		let mut lex = Self {
			contents, reporter,
			index: 0,
			iter: contents.char_indices().map((|(i, _)| i) as fn((usize, char)) -> usize).peekable(),
			tokens: Vec::new(),
		};

		while let Some(current) = lex.next() {
			if let Err(e) = lex.tokenize_inner(lex.index, current) { lex.log(*e) }
		}

		lex.tokens.push(Token {
			kind: TokenKind::EOF,
			span: Span::new(lex.index),
			text: "",
		});

		lex.tokens
	}

	fn tokenize_inner(&mut self, index: usize, current: &'src str) -> Result<()> {
		match current {
			c if c.chars().any(char::is_whitespace) => (),

			// TODO: make comments a token
			"/" => match self.peek() {
				Some("/") => while Some("\n") != self.next() { },
				Some("*") => {
					let mut depth = 0;
					loop {
						match self.next() {
							Some("/") if Some("*") == self.peek() => {
								self.next();
								depth += 1;
							},
							Some("*") if Some("/") == self.peek() => {
								self.next();
								depth -= 1;
							},
							None => return ReportKind::UnterminatedMultilineComment
								.title(format!("{depth} comments never terminated"))
								.span(Span::new(index).end(self.index))
								.as_err(),
							_ => (),
						}

						if depth == 0 { break; }
					}
				},
				_ => self.push_token_simple(TokenKind::Slash, 1),
			},

			c if c.chars().any(|c| c.is_ascii_alphabetic()) => {
				while let Some(c) = self.peek() {
					if c.chars().any(|c| c.is_ascii_alphanumeric() || c == '_') {
						self.next();
						continue;
					}
					break;
				}

				
				let ident = self.slice(index, self.index + 1);
				let kind = match ident {
					"let"    => TokenKind::KWLet,
					"static" => TokenKind::KWStatic,
					"export" => TokenKind::KWExport,
					"ret"    => TokenKind::KWRet,
					"impl"   => TokenKind::KWImpl,
					"extern" => TokenKind::KWExtern,
					"mut"    => TokenKind::KWMut,
					"move"   => TokenKind::KWMove,
					"as"     => TokenKind::KWAs,
					"if"     => TokenKind::KWIf,
					"fat"    => TokenKind::KWFat,
					"raw"    => TokenKind::KWRaw,
					"loop"	=> TokenKind::KWLoop,
					_ => TokenKind::Identifier,
				};

				self.push_token(kind, index, self.index);
			},

			"\"" => {
				let Some(&start) = self.iter.peek() else {
					return ReportKind::UnterminatedLiteral
						.untitled().span(self.span_from(index))
						.as_err();
				};

				let mut end = start;
				loop { 
					match self.next() {
						Some("\"") => break,
						None => return ReportKind::UnterminatedLiteral
							.untitled().span(self.span_from(index))
							.as_err(),
						_ => end = self.index,
					}
				}

				self.push_token(TokenKind::StringLiteral, start, end);
			},

			"'" => {
				let Some(&start) = self.iter.peek() else {
					return ReportKind::UnterminatedLiteral
						.untitled().span(self.span_from(index))
						.as_err();
				};

				if matches!(self.peek(), Some("'")) {
					return ReportKind::EmptyLiteral
						.untitled()
						.span(self.span_from(index))
						.as_err();
				}

				match self.next().unwrap() {
					"\\" => {
						if matches!(self.next(), Some("'")) && !matches!(self.peek(), Some("'")) {
							self.next();
							return ReportKind::UnterminatedLiteral
								.untitled()
								.span(self.span_from(index))
								.help("Remove the escape character")
								.as_err();
						}

						self.next();
					},

					"\n" => return ReportKind::UnterminatedLiteral
						.untitled().span(self.span_from(index))
						.as_err(),

					_ if matches!(self.peek(), Some("'")) => {
						let end = self.index;
						self.next();

						self.push_token(TokenKind::CharLiteral, start, end);
					},

					_ => {
						while matches!(self.peek(), Some(c) if c.chars().any(|c| c.is_ascii_alphanumeric() || c == '_')) {
							self.next();
						}
						self.push_token(TokenKind::Quote, start, self.index);
					},
				}

			},

			"0" if self.peek().filter(|c| "box".contains(c)).is_some() => {
				let (kind, base) = match self.peek() {
					Some("b") => (TokenKind::BinaryIntLiteral, 2),
					Some("o") => (TokenKind::OctalIntLiteral, 8),
					Some("x") => (TokenKind::HexadecimalIntLiteral, 16),
					_ => unreachable!(),
				};

				self.next();
				let start = self.index;

				self.lex_integer(base)?;
				self.push_token(kind, start, self.index);
			},

			c if c.chars().any(|c| c.is_ascii_digit()) => {
				let start = self.index;
				self.lex_integer(10)?;

				if matches!(self.peek(), Some(".")) {
					self.next();
					self.lex_integer(10)?;

					if matches!(self.peek(), Some(".")) {
						let end = self.index;
						self.next();

						return ReportKind::SyntaxError
							.title("Invalid Float Literal")
							.span(Span::new(index).end(end))
							.as_err();
					}

					self.push_token(TokenKind::FloatLiteral, start, self.index);
					return Ok(());
				}

				self.push_token(TokenKind::DecimalIntLiteral, start, self.index);
			},

			s => {
				let (token, len) = match s {
					"." => (TokenKind::Dot, 1),
					"!" => match self.peek() {
						Some("=") => (TokenKind::NotEquals, 2),
						_ => (TokenKind::Bang, 1),
					},
					"~" => (TokenKind::Tilde, 1),
					"@" => (TokenKind::At, 1),
					"#" => (TokenKind::Pound, 1),
					"$" => (TokenKind::Dollar, 1),
					"%" => (TokenKind::Percent, 1),
					"^" => match self.peek() {
						Some("^") => (TokenKind::CaretCaret, 2),
						_ => (TokenKind::Caret, 1),
					},
					"&" => match self.peek() {
						Some("&") => (TokenKind::AmpersandAmpersand, 2),
						_ => (TokenKind::Ampersand, 1),
					},
					"*" => (TokenKind::Star, 1),
					"(" => (TokenKind::LParen, 1),
					")" => (TokenKind::RParen, 1),
					"-" => match self.peek() {
						Some(">") => (TokenKind::ArrowRight, 2),
						_ => (TokenKind::Minus, 1),
					},
					"_" => (TokenKind::Underscore, 1),
					"+" => (TokenKind::Plus, 1),
					"[" => (TokenKind::LBracket, 1),
					"]" => (TokenKind::RBracket, 1),
					"{" => (TokenKind::LBrace, 1),
					"}" => (TokenKind::RBrace, 1),
					"|" => match self.peek() {
						Some("|") => (TokenKind::PipePipe, 2),
						_ => (TokenKind::Pipe, 1),
					},
					";" => (TokenKind::Semicolon, 1),
					":" => (TokenKind::Colon, 1),
					"," => (TokenKind::Comma, 1),
					"=" => match self.peek() {
						Some("=") => (TokenKind::EqualsEquals, 2),
						Some(">") => (TokenKind::FatArrowRight, 2),
						_ => (TokenKind::Equals, 1),
					},
					"<" => match self.peek() {
						Some("=") => (TokenKind::LessThanEquals, 2),
						Some("-") => (TokenKind::ArrowLeft, 2),
						Some("<") => (TokenKind::ShiftLeft, 2),
						_ => (TokenKind::LessThan, 1),
					},
					">" => match self.peek() {
						Some("=") => (TokenKind::GreaterThanEquals, 2),
						Some(">") => (TokenKind::ShiftRight, 2),
						_ => (TokenKind::GreaterThan, 1),
					},
					"?" => (TokenKind::Question, 1),
					c => return ReportKind::UnexpectedCharacter
						.title(format!("'{c}'"))
						.span(self.span_from(index))
						.as_err(),
				};

				self.push_token_simple(token, len);
			}
		}
		Ok(())
	}

	fn lex_integer(&mut self, base: usize) -> Result<()> {
		while let Some(c) = self.peek() {
			match c.chars().next().unwrap() {
				c if match base {
					2  => matches!(c, '1' | '0' | '_'),
					8  => matches!(c, '0'..='7' | '_'),
					10 => matches!(c, '0'..='9' | '_'),
					16 => matches!(c, '0'..='9' | 'a'..='f' | 'A'..='F' | '_'),
					_  => unreachable!(),
				} => self.next(),

				c if c.is_ascii_alphanumeric() => {
					return ReportKind::SyntaxError
						.title("Invalid Integer Literal")
						.span(self.span_from(self.index - 1))
						.label(format!("{c:?} not valid for base{base} Integer Literal"))
						.as_err();
				},

				_ => break,
			};
		}
		Ok(())
	}
}
