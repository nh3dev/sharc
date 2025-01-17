mod token;
pub use token::{Token, TokenKind};

use crate::report::{LogHandler, ReportKind};
use crate::span::Span;

#[allow(clippy::complexity)]
pub struct Lexer<'src> {
	contents: &'src str,
	iter:     std::iter::Peekable<std::iter::Map<std::str::CharIndices<'src>, fn((usize, char)) -> usize>>,
	index:    usize,
	tokens:   Vec<Token<'src>>,
}

impl<'src> Lexer<'src> {
	fn next(&mut self) -> Option<&'src str> {
		self.iter.next().map(|i| { self.index = i; &self.contents[i..=i] })
	}

	fn peek(&mut self) -> Option<&'src str> {
		self.iter.peek().map(|i| &self.contents[*i..=*i])
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

	pub fn tokenize(contents: &'src str, handler: &LogHandler) -> Vec<Token<'src>> {
		let mut lex = Self {
			contents,
			index: 0,
			iter: contents.char_indices().map((|(i, _)| i) as fn((usize, char)) -> usize).peekable(),
			tokens: Vec::new(),
		};

		'outer: while let Some(current) = lex.next() {
			let index = lex.index;
			match current {
				c if c.chars().any(char::is_whitespace) => (),

				// TODO: make comments a token
				"/" => match lex.peek() {
					Some("/") => while Some("\n") != lex.next() { },
					Some("*") => {
						let mut depth = 0;
						loop {
							match lex.next() {
								Some("/") if Some("*") == lex.peek() => {
									lex.next();
									depth += 1;
								},
								Some("*") if Some("/") == lex.peek() => {
									lex.next();
									depth -= 1;
								},
								None => handler.log(
									ReportKind::UnterminatedMultilineComment
										.title(format!("{depth} comments never terminated"))
										.span(Span::new(index).end(lex.index))),
								_ => (),
							}

							if depth == 0 { break; }
						}
					},
					_ => lex.push_token_simple(TokenKind::Slash, 1),
				},

				c if c.chars().any(|c| c.is_ascii_alphabetic()) => {
					while let Some(c) = lex.peek() {
						if c.chars().any(|c| c.is_ascii_alphanumeric() || c == '_') {
							lex.next();
							continue;
						}
						break;
					}

					
					let ident = lex.slice(index, lex.index + 1);
					let kind = match ident {
						"fn"     => TokenKind::KWFn,
						"export" => TokenKind::KWExport,
						"ret"    => TokenKind::KWRet,
						"struct" => TokenKind::KWStruct,
						"enum"   => TokenKind::KWEnum,
						"impl"   => TokenKind::KWImpl,
						"type"   => TokenKind::KWType,
						"extern" => TokenKind::KWExtern,
						_ => TokenKind::Identifier,
					};

					lex.push_token(kind, index, lex.index);
				},

				"\"" => {
					let Some(&start) = lex.iter.peek() else {
						handler.log(
							ReportKind::UnterminatedLiteral
								.untitled()
								.span(lex.span_from(index)));
						continue;
					};

					let mut end = start;
					loop { 
						match lex.next() {
							Some("\"") => break,
							None => {
								handler.log(
									ReportKind::UnterminatedLiteral
										.untitled()
										.span(lex.span_from(index)));
								continue 'outer;
							},
							_ => end = lex.index,
						}
					}

					lex.push_token(TokenKind::StringLiteral, start, end);
				},

				"'" => {
					let Some(&start) = lex.iter.peek() else {
						handler.log(
							ReportKind::UnterminatedLiteral
								.untitled()
								.span(lex.span_from(index)));
						continue;
					};

					if matches!(lex.peek(), Some("'")) {
						handler.log(
							ReportKind::EmptyLiteral
								.untitled()
								.span(lex.span_from(index)));
						continue;
					}

					let mut end = start;
					match lex.next().unwrap() {
						"\\" => {
							if matches!(lex.next(), Some("'")) && !matches!(lex.peek(), Some("'")) {
								lex.next();
								handler.log(
									ReportKind::UnterminatedLiteral
										.untitled()
										.span(lex.span_from(index))
										.help("Remove the escape character"));
								continue;
							}

							lex.next();
						},

						"\n" => {
							handler.log(
								ReportKind::UnterminatedLiteral
									.untitled().span(lex.span_from(index)));
							continue;
						},

						_ => {
							if !matches!(lex.peek(), Some("'")) {
								handler.log(
									ReportKind::UnterminatedLiteral
										.untitled().span(lex.span_from(index)));
								continue;
							}
							
							end = lex.index;

							lex.next();
						},
					}

					lex.push_token(TokenKind::CharLiteral, start, end);
				},

				"0" if lex.peek().filter(|c| "box".contains(c)).is_some() => {
					let (kind, base) = match lex.peek() {
						Some("b") => (TokenKind::BinaryIntLiteral, 2),
						Some("o") => (TokenKind::OctalIntLiteral, 8),
						Some("x") => (TokenKind::HexadecimalIntLiteral, 16),
						_ => unreachable!(),
					};

					lex.next();
					let start = lex.index;

					if !lex.lex_integer(handler, base) { continue; }

					lex.push_token(kind, start, lex.index);
				},

				c if c.chars().any(|c| c.is_ascii_digit()) => {
					let start = lex.index;
					if !lex.lex_integer(handler, 10) { continue; }

					if matches!(lex.peek(), Some(".")) {
						lex.next();

						if !lex.lex_integer(handler, 10) { continue; }

						if matches!(lex.peek(), Some(".")) {
							handler.log(
								ReportKind::SyntaxError
									.title("Invalid Float Literal")
									.span(lex.span_from(index)));
							lex.next();
							continue;
						}

						lex.push_token(TokenKind::FloatLiteral, start, lex.index);
						continue;
					}

					lex.push_token(TokenKind::DecimalIntLiteral, start, lex.index);
				},

				s => {
					let (token, len) = match s {
						"." => (TokenKind::Dot, 1),
						"'" => (TokenKind::Apostrophe, 1),
						"~" => match lex.peek() {
							Some("=") => (TokenKind::NotEquals, 2),
							_ => (TokenKind::Tilde, 1),
						},
						"!" => (TokenKind::Bang, 1),
						"@" => (TokenKind::At, 1),
						"#" => (TokenKind::Pound, 1),
						"$" => (TokenKind::Dollar, 1),
						"%" => (TokenKind::Percent, 1),
						"^" => match lex.peek() {
							Some("^") => (TokenKind::CaretCaret, 2),
							_ => (TokenKind::Caret, 1),
						},
						"&" => match lex.peek() {
							Some("&") => (TokenKind::AmpersandAmpersand, 2),
							_ => (TokenKind::Ampersand, 1),
						},
						"*" => (TokenKind::Star, 1),
						"(" => (TokenKind::LParen, 1),
						")" => (TokenKind::RParen, 1),
						"-" => match lex.peek() {
							Some(">") => (TokenKind::ArrowRight, 2),
							Some("-") => (TokenKind::MinusMinus, 2),
							_ => (TokenKind::Minus, 1),
						},
						"_" => (TokenKind::Underscore, 1),
						"+" => match lex.peek() {
							Some("+") => (TokenKind::PlusPlus, 2),
							_ => (TokenKind::Plus, 1),
						},
						"[" => (TokenKind::LBracket, 1),
						"]" => (TokenKind::RBracket, 1),
						"{" => (TokenKind::LBrace, 1),
						"}" => (TokenKind::RBrace, 1),
						"|" => match lex.peek() {
							Some("|") => (TokenKind::PipePipe, 2),
							_ => (TokenKind::Pipe, 1),
						},
						";" => (TokenKind::Semicolon, 1),
						":" => (TokenKind::Colon, 1),
						"," => (TokenKind::Comma, 1),
						"=" => match lex.peek() {
							// Some("=") => (TokenKind::EqualsEquals, 2),
							Some(">") => (TokenKind::FatArrowRight, 2),
							_ => (TokenKind::Equals, 1),
						},
						"<" => match lex.peek() {
							Some("=") => (TokenKind::LessThanEquals, 2),
							Some("-") => (TokenKind::ArrowLeft, 2),
							Some("<") => (TokenKind::ShiftLeft, 2),
							_ => (TokenKind::LessThan, 1),
						},
						">" => match lex.peek() {
							Some("=") => (TokenKind::GreaterThanEquals, 2),
							Some(">") => (TokenKind::ShiftRight, 2),
							_ => (TokenKind::GreaterThan, 1),
						},
						"?" => (TokenKind::Question, 1),
						c => {
							handler.log(ReportKind::UnexpectedCharacter.title(c).span(lex.span_from(index)));
							continue;
						},
					};

					lex.push_token_simple(token, len);
				}
			}
		}

		lex.tokens.push(Token {
			kind: TokenKind::EOF,
			span: Span::new(lex.index),
			text: "",
		});

		lex.tokens
	}

	fn lex_integer(&mut self, handler: &LogHandler, base: usize) -> bool {
		const CHARS: [char; 16] =
			['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'];

		let f = match base {
			2  => |c| CHARS[..1].contains(&c),
			8  => |c| CHARS[..7].contains(&c),
			10 => |c| CHARS[..9].contains(&c),
			16 => |c| CHARS.contains(&c),
			_  => unreachable!(),
		};

		while let Some(c) = self.peek() {
			match c.to_ascii_lowercase().chars().next().unwrap() {
				c if f(c) || c == '_' => self.next(),

				c if c.is_ascii_alphanumeric() => {
					handler.log(
						ReportKind::SyntaxError
							.title("Invalid Integer Literal")
							.span(self.span_from(self.index - 1))
							.label(format!("{c:?} not valid for base{base} Integer Literal")));
					return false;
				},

				_ => break,
			};
		}
		true
	}
}
