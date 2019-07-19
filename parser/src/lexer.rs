// use codespan::{ByteSpan, FileMap};
use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::diagnostics::{ByteIndex, ByteOffset, FileId, Files, RawOffset, Span};
use crate::symbol::{Ident, Symbol};

use std::convert::TryInto;
use std::fmt;
use std::str::{CharIndices, FromStr};

// use codespan::{ByteIndex, ByteOffset, RawOffset};
use unicode_xid::UnicodeXID;

fn is_symbol(ch: char) -> bool {
    match ch {
        '&' | '!' | ':' | ',' | '.' | '=' | '/' | '>' | '<' | '-' | '|' | '+' | ';' | '*' | '^'
        | '?' | '%' | '@' | '#' | '$' => true,
        _ => false,
    }
}

fn is_ident_start(ch: char) -> bool {
    UnicodeXID::is_xid_start(ch) || ch == '_' || ch == '-'
}

fn is_ident_continue(ch: char) -> bool {
    UnicodeXID::is_xid_continue(ch) || ch == '_' || ch == '-'
}

fn is_bin_digit(ch: char) -> bool {
    ch.is_digit(2)
}

fn is_oct_digit(ch: char) -> bool {
    ch.is_digit(8)
}

fn is_dec_digit(ch: char) -> bool {
    ch.is_digit(10)
}

fn is_hex_digit(ch: char) -> bool {
    ch.is_digit(16)
}

/// An error that occurred while lexing the source file
#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum LexerError {
    #[error(display = "An unexpected character {:?} was found.", found)]
    UnexpectedCharacter { start: ByteIndex, found: char },
    #[error(display = "Unexpected end of file.")]
    UnexpectedEof { end: ByteIndex },
    #[error(display = "Unterminated string literal.")]
    UnterminatedStringLiteral { span: Span },
    #[error(display = "Unterminated character literal.")]
    UnterminatedCharLiteral { span: Span },
    #[error(display = "Unterminated binary literal.")]
    UnterminatedBinLiteral { span: Span },
    #[error(display = "Unterminated octal literal.")]
    UnterminatedOctLiteral { span: Span },
    #[error(display = "Unterminated hexidecimal literal.")]
    UnterminatedHexLiteral { span: Span },
    #[error(display = "Empty character literal.")]
    EmptyCharLiteral { span: Span },
    #[error(display = "An unknown escape code \\{} was found.", found)]
    UnknownEscapeCode { start: ByteIndex, found: char },
    #[error(
        display = "An integer literal {} was too large for the target type.",
        value
    )]
    IntegerLiteralOverflow { span: Span, value: String },
}

impl LexerError {
    /// Return the span of source code that this error originated from
    pub fn span(&self) -> Span {
        match *self {
            LexerError::UnexpectedCharacter { start, found }
            | LexerError::UnknownEscapeCode { start, found } => {
                Span::new(start, start + ByteOffset::from_char_len(found))
            }
            LexerError::UnexpectedEof { end } => Span::new(end, end),
            LexerError::UnterminatedStringLiteral { span }
            | LexerError::UnterminatedCharLiteral { span }
            | LexerError::UnterminatedBinLiteral { span }
            | LexerError::UnterminatedOctLiteral { span }
            | LexerError::UnterminatedHexLiteral { span }
            | LexerError::EmptyCharLiteral { span }
            | LexerError::IntegerLiteralOverflow { span, .. } => span,
        }
    }

    pub fn to_diagnostic(&self, file_id: FileId) -> Diagnostic {
        match *self {
            LexerError::UnexpectedCharacter { start, found } => {
                let char_span = Span::new(start, start + ByteOffset::from_char_len(found));
                Diagnostic::new_error(
                    format!("unexpected character {:?}", found),
                    Label::new(file_id, char_span, "unexpected character"),
                )
            }
            LexerError::UnexpectedEof { end } => Diagnostic::new_error(
                "unexpected end of file",
                Label::new(file_id, Span::new(end, end), "end of file"),
            ),
            LexerError::UnterminatedStringLiteral { span } => Diagnostic::new_error(
                "unterminated string literal",
                Label::new(file_id, span, "unterminated string literal"),
            ),
            LexerError::UnterminatedCharLiteral { span } => Diagnostic::new_error(
                "unterminated character literal",
                Label::new(file_id, span, "unterminated character literal"),
            ),
            LexerError::UnterminatedBinLiteral { span } => Diagnostic::new_error(
                "unterminated binary literal",
                Label::new(file_id, span, "unterminated binary literal"),
            ),
            LexerError::UnterminatedOctLiteral { span } => Diagnostic::new_error(
                "unterminated octal literal",
                Label::new(file_id, span, "unterminated octal literal"),
            ),
            LexerError::UnterminatedHexLiteral { span } => Diagnostic::new_error(
                "unterminated hexadecimal literal",
                Label::new(file_id, span, "unterminated hexadecimal literal"),
            ),
            LexerError::EmptyCharLiteral { span } => Diagnostic::new_error(
                "empty character literal",
                Label::new(file_id, span, "empty character literal"),
            ),
            LexerError::UnknownEscapeCode { start, found } => {
                let char_span = Span::new(start, start + ByteOffset::from_char_len(found));
                Diagnostic::new_error(
                    format!("unknown escape code \\{}", found),
                    Label::new(file_id, char_span, "unknown escape code"),
                )
            }
            LexerError::IntegerLiteralOverflow { span, ref value } => Diagnostic::new_error(
                format!("integer literal overflow with value `{}`", value),
                Label::new(file_id, span, "overflowing literal"),
            ),
        }
    }
}

/// A token in the source file, to be emitted by the `Lexer`
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    // Data
    Ident(Symbol),
    DocComment(String),
    StringLiteral(String),
    CharLiteral(char),
    BinIntLiteral(i64),
    OctIntLiteral(i64),
    DecIntLiteral(i64),
    HexIntLiteral(i64),
    DecFloatLiteral(f64),

    // Boolean literals
    True,  // true
    False, // false

    // Keywords
    // As,   // as
    // Case, // case
    Match,  // match
    If,     // if
    Else,   // else
    Import, // import
    In,     // in
    Let,    // let
    // Record,     // record
    // RecordType, // Record
    // Then,       // then
    Type, // Type
    // Where, // where
    Fn, // fn

    // Symbols and punctuation
    Plus,    // +
    Minus,   // -
    Star,    // *
    Slash,   // /
    BSlash,  // \
    Percent, // %
    Caret,   // ^
    Not,     // !
    And,     // &
    Or,      // |
    AndAnd,  // &&
    OrOr,    // ||
    // Shl,       // <<
    // Shr,       // >>
    // PlusEq,    // +=
    // MinusEq,   // -=
    // StarEq,    // *=
    // SlashEq, PercentEq, CaretEq, AndEq, OrEq, ShlEq, ShrEq
    Eq,         // =
    EqEq,       // ==
    Ne,         // !=
    Gt,         // >
    Lt,         // <
    Ge,         // >=
    Le,         // <=
    At,         // @
    Underscore, // _
    Dot,        // .
    DotDot,     // ..
    DotDotDot,  // ...
    Comma,      // ,
    Semi,       // ;
    Colon,      // :
    PathSep,    // ::
    RArrow,     // ->
    FatArrow,   // =>
    Pound,      // #
    Dollar,     // $
    Question,   // ?

    // Delimiters
    LParen,   // (
    RParen,   // )
    LBrace,   // {
    RBrace,   // }
    LBracket, // [
    RBracket, // ]
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::Ident(ref name) => write!(f, "{}", name),
            Token::DocComment(ref comment) => write!(f, "/// {}", comment),
            Token::StringLiteral(ref value) => write!(f, "{:?}", value),
            Token::CharLiteral(ref value) => write!(f, "'{:?}'", value),
            Token::BinIntLiteral(ref value) => write!(f, "{:b}", value),
            Token::OctIntLiteral(ref value) => write!(f, "{:o}", value),
            Token::DecIntLiteral(ref value) => write!(f, "{}", value),
            Token::HexIntLiteral(ref value) => write!(f, "{:x}", value),
            Token::DecFloatLiteral(ref value) => write!(f, "{}", value),
            Token::Else => write!(f, "else"),
            Token::If => write!(f, "if"),
            Token::Let => write!(f, "let"),
            Token::In => write!(f, "in"),
            Token::Import => write!(f, "import"),
            Token::Type => write!(f, "type"),
            Token::Match => write!(f, "match"),
            Token::Fn => write!(f, "fn"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::BSlash => write!(f, "\\"),
            Token::Percent => write!(f, "%"),
            Token::Caret => write!(f, "^"),
            Token::Not => write!(f, "!"),
            Token::And => write!(f, "&"),
            Token::Or => write!(f, "|"),
            Token::AndAnd => write!(f, "&&"),
            Token::OrOr => write!(f, "||"),
            Token::Eq => write!(f, "="),
            Token::EqEq => write!(f, "=="),
            Token::Ne => write!(f, "!="),
            Token::Gt => write!(f, ">"),
            Token::Lt => write!(f, "<"),
            Token::Ge => write!(f, ">="),
            Token::Le => write!(f, "<="),
            Token::At => write!(f, "@"),
            Token::Underscore => write!(f, "_"),
            Token::Dot => write!(f, "."),
            Token::DotDot => write!(f, ".."),
            Token::DotDotDot => write!(f, "..."),
            Token::Comma => write!(f, ","),
            Token::Semi => write!(f, ";"),
            Token::Colon => write!(f, ":"),
            Token::PathSep => write!(f, "::"),
            Token::RArrow => write!(f, "->"),
            Token::FatArrow => write!(f, "=>"),
            Token::Pound => write!(f, "#"),
            Token::Dollar => write!(f, "$"),
            Token::Question => write!(f, "?"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
        }
    }
}

/// An iterator over a source string that yields `Token`s for subsequent use by
/// the parser
pub struct Lexer<'input> {
    file: &'input str,
    chars: CharIndices<'input>,
    lookahead: Option<(usize, char)>,
}

impl<'input> Lexer<'input> {
    /// Create a new lexer from the source string
    pub fn new(file: &'input str) -> Self {
        let mut chars = file.char_indices();

        Lexer {
            file,
            lookahead: chars.next(),
            chars,
        }
    }

    /// Returns the index of the end of the file
    fn eof(&self) -> ByteIndex {
        ByteIndex(self.file.len().try_into().unwrap())
    }

    /// Return the next character in the source string
    fn lookahead(&self) -> Option<(ByteIndex, char)> {
        self.lookahead
            .map(|(index, ch)| (ByteIndex(index as u32), ch))
    }

    /// Bump the current position in the source string by one character,
    /// returning the current character and byte position.
    fn bump(&mut self) -> Option<(ByteIndex, char)> {
        let current = self.lookahead();
        self.lookahead = self.chars.next();
        current
    }

    /// Return a slice of the source string
    fn slice(&self, start: ByteIndex, end: ByteIndex) -> &'input str {
        &self.file[start.to_usize()..end.to_usize()]
    }

    /// Test a predicate against the next character in the source
    fn test_lookahead<F>(&self, mut pred: F) -> bool
    where
        F: FnMut(char) -> bool,
    {
        self.lookahead.map_or(false, |(_, ch)| pred(ch))
    }

    /// Consume characters while the predicate matches for the current
    /// character, then return the consumed slice and the end byte
    /// position.
    fn take_while<F>(&mut self, start: ByteIndex, mut keep_going: F) -> (ByteIndex, &'input str)
    where
        F: FnMut(char) -> bool,
    {
        self.take_until(start, |ch| !keep_going(ch))
    }

    /// Consume characters until the predicate matches for the next character
    /// in the lookahead, then return the consumed slice and the end byte
    /// position.
    fn take_until<F>(&mut self, start: ByteIndex, mut terminate: F) -> (ByteIndex, &'input str)
    where
        F: FnMut(char) -> bool,
    {
        while let Some((end, ch)) = self.lookahead() {
            if terminate(ch) {
                return (end, self.slice(start, end));
            } else {
                self.bump();
            }
        }

        let eof = self.eof();
        (eof, self.slice(start, eof))
    }

    /// Consume a doc comment
    fn doc_comment(&mut self, start: ByteIndex) -> SpannedToken<'input> {
        let (end, mut comment) =
            self.take_until(start + ByteOffset::from_str_len("|||"), |ch| ch == '\n');

        // Skip preceding space
        if comment.starts_with(' ') {
            comment = &comment[1..];
        }

        (start, Token::DocComment(comment.to_string()), end)
    }

    /// Consume an identifier
    fn ident(&mut self, start: ByteIndex) -> SpannedToken<'input> {
        let (end, ident) = self.take_while(start, is_ident_continue);

        let token = match ident {
            "if" => Token::If,
            "else" => Token::Else,
            "let" => Token::Let,
            "type" => Token::Type,
            "in" => Token::In,
            "import" => Token::Import,
            "fn" => Token::Fn,
            "true" => Token::True,
            "false" => Token::False,
            ident => Token::Ident(Symbol::intern(ident)),
        };

        (start, token, end)
    }

    /// Consume an escape code
    fn escape_code(&mut self, start: ByteIndex) -> Result<char, LexerError> {
        match self.bump() {
            Some((_, '\'')) => Ok('\''),
            Some((_, '"')) => Ok('"'),
            Some((_, '\\')) => Ok('\\'),
            Some((_, '/')) => Ok('/'),
            Some((_, 'n')) => Ok('\n'),
            Some((_, 'r')) => Ok('\r'),
            Some((_, 't')) => Ok('\t'),
            // TODO: Unicode escape codes
            Some((start, ch)) => Err(LexerError::UnknownEscapeCode { start, found: ch }),
            None => Err(LexerError::UnexpectedEof { end: start }),
        }
    }

    /// Consume a string literal
    fn string_literal(&mut self, start: ByteIndex) -> Result<SpannedToken<'input>, LexerError> {
        let mut string = String::new();
        let mut end = start;

        while let Some((next, ch)) = self.bump() {
            end = next + ByteOffset::from_char_len(ch);
            match ch {
                '\\' => string.push(self.escape_code(next)?),
                '"' => return Ok((start, Token::StringLiteral(string), end)),
                ch => string.push(ch),
            }
        }

        Err(LexerError::UnterminatedStringLiteral {
            span: Span::new(start, end),
        })
    }

    /// Consume a character literal
    fn char_literal(&mut self, start: ByteIndex) -> Result<SpannedToken<'input>, LexerError> {
        let ch = match self.bump() {
            Some((next, '\\')) => self.escape_code(next)?,
            Some((next, '\'')) => {
                return Err(LexerError::EmptyCharLiteral {
                    span: Span::new(start, next + ByteOffset::from_char_len('\'')),
                });
            }
            Some((_, ch)) => ch,
            None => return Err(LexerError::UnexpectedEof { end: start }),
        };

        match self.bump() {
            Some((end, '\'')) => Ok((
                start,
                Token::CharLiteral(ch),
                end + ByteOffset::from_char_len('\''),
            )),
            Some((next, ch)) => Err(LexerError::UnterminatedCharLiteral {
                span: Span::new(start, next + ByteOffset::from_char_len(ch)),
            }),
            None => Err(LexerError::UnexpectedEof { end: start }),
        }
    }

    /// Consume a binary literal token
    fn bin_literal(
        &mut self,
        start: ByteIndex,
    ) -> Result<(ByteIndex, Token, ByteIndex), LexerError> {
        self.bump(); // skip 'b'
        let (end, src) = self.take_while(start + ByteOffset(2), is_bin_digit);
        if src.is_empty() {
            Err(LexerError::UnterminatedBinLiteral {
                span: Span::new(start, end),
            })
        } else {
            let int = i64::from_str_radix(src, 2).unwrap();
            Ok((start, Token::BinIntLiteral(int), end))
        }
    }

    /// Consume a octal literal token
    fn oct_literal(
        &mut self,
        start: ByteIndex,
    ) -> Result<(ByteIndex, Token, ByteIndex), LexerError> {
        self.bump(); // skip 'o'
        let (end, src) = self.take_while(start + ByteOffset(2), is_oct_digit);
        if src.is_empty() {
            Err(LexerError::UnterminatedOctLiteral {
                span: Span::new(start, end),
            })
        } else {
            let int = i64::from_str_radix(src, 8).unwrap();
            Ok((start, Token::OctIntLiteral(int), end))
        }
    }

    /// Consume a decimal literal
    fn dec_literal(&mut self, start: ByteIndex) -> Result<SpannedToken<'input>, LexerError> {
        let (end, src) = self.take_while(start, is_dec_digit);

        if let Some((_, '.')) = self.lookahead() {
            self.bump(); // skip '.'
            let (end, src) = self.take_while(start, is_dec_digit);

            match f64::from_str(src) {
                Ok(value) => Ok((start, Token::DecFloatLiteral(value), end)),
                Err(_) => unimplemented!(),
            }
        } else {
            match i64::from_str_radix(src, 10) {
                Ok(value) => Ok((start, Token::DecIntLiteral(value), end)),
                Err(_) => Err(LexerError::IntegerLiteralOverflow {
                    span: Span::new(start, end),
                    value: src.to_string(),
                }),
            }
        }
    }

    /// Consume a hexadecimal literal token
    fn hex_literal(
        &mut self,
        start: ByteIndex,
    ) -> Result<(ByteIndex, Token, ByteIndex), LexerError> {
        self.bump(); // skip 'x'
        let (end, src) = self.take_while(start + ByteOffset(2), is_hex_digit);
        if src.is_empty() {
            Err(LexerError::UnterminatedHexLiteral {
                span: Span::new(start, end),
            })
        } else {
            let int = i64::from_str_radix(src, 16).unwrap();
            Ok((start, Token::HexIntLiteral(int), end))
        }
    }
}

pub type SpannedToken<'input> = (ByteIndex, Token, ByteIndex);

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<(ByteIndex, Token, ByteIndex), LexerError>;

    #[allow(clippy::cognitive_complexity)]
    fn next(&mut self) -> Option<Result<SpannedToken<'input>, LexerError>> {
        while let Some((start, ch)) = self.bump() {
            let end = start + ByteOffset::from_char_len(ch);

            return Some(match ch {
                ch if is_symbol(ch) => {
                    let (end, symbol) = self.take_while(start, is_symbol);

                    match symbol {
                        "+" => Ok((start, Token::Plus, end)),
                        "-" => Ok((start, Token::Minus, end)),
                        "*" => Ok((start, Token::Star, end)),
                        "%" => Ok((start, Token::Percent, end)),
                        "^" => Ok((start, Token::Caret, end)),
                        "!" => Ok((start, Token::Not, end)),
                        "&" => Ok((start, Token::And, end)),
                        "|" => Ok((start, Token::Or, end)),
                        "&&" => Ok((start, Token::AndAnd, end)),
                        "||" => Ok((start, Token::OrOr, end)),
                        "=" => Ok((start, Token::Eq, end)),
                        "==" => Ok((start, Token::EqEq, end)),
                        "!=" => Ok((start, Token::Ne, end)),
                        ">" => Ok((start, Token::Gt, end)),
                        "<" => Ok((start, Token::Lt, end)),
                        ">=" => Ok((start, Token::Ge, end)),
                        "<=" => Ok((start, Token::Le, end)),
                        "@" => Ok((start, Token::At, end)),

                        "." => Ok((start, Token::Dot, end)),
                        ".." => Ok((start, Token::DotDot, end)),
                        "..." => Ok((start, Token::DotDotDot, end)),
                        "," => Ok((start, Token::Comma, end)),
                        ";" => Ok((start, Token::Semi, end)),
                        ":" => Ok((start, Token::Colon, end)),
                        "::" => Ok((start, Token::PathSep, end)),
                        "->" => Ok((start, Token::RArrow, end)),
                        "=>" => Ok((start, Token::FatArrow, end)),
                        "#" => Ok((start, Token::Pound, end)),
                        "$" => Ok((start, Token::Dollar, end)),
                        "?" => Ok((start, Token::Question, end)),
                        "/" => Ok((start, Token::Slash, end)),
                        symbol if symbol.starts_with("///") => Ok(self.doc_comment(start)),
                        symbol if symbol.starts_with("//") => {
                            self.take_until(start, |ch| ch == '\n');
                            continue;
                        }
                        _ => Err(LexerError::UnexpectedCharacter { start, found: ch }),
                    }
                }
                '_' => Ok((start, Token::Underscore, end)),
                '\\' => Ok((start, Token::BSlash, end)),
                '(' => Ok((start, Token::LParen, end)),
                ')' => Ok((start, Token::RParen, end)),
                '{' => Ok((start, Token::LBrace, end)),
                '}' => Ok((start, Token::RBrace, end)),
                '[' => Ok((start, Token::LBracket, end)),
                ']' => Ok((start, Token::RBracket, end)),
                '"' => self.string_literal(start),
                '\'' => self.char_literal(start),
                '0' if self.test_lookahead(|x| x == 'b') => self.bin_literal(start),
                '0' if self.test_lookahead(|x| x == 'o') => self.oct_literal(start),
                '0' if self.test_lookahead(|x| x == 'x') => self.hex_literal(start),
                ch if is_ident_start(ch) => Ok(self.ident(start)),
                ch if is_dec_digit(ch) => self.dec_literal(start),
                ch if ch.is_whitespace() => continue,
                _ => Err(LexerError::UnexpectedCharacter { start, found: ch }),
            });
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use codespan::Files;
    use codespan::RawIndex;

    use super::*;

    /// A handy macro to give us a nice syntax for declaring test cases
    ///
    /// This was inspired by the tests in the LALRPOP lexer
    macro_rules! test {
        ($src:expr, $($span:expr => $token:expr,)*) => {{
            let lexed_tokens: Vec<_> = Lexer::new(&$src).collect();
            let expected_tokens = vec![$({
                let start = ByteIndex($span.find("~").unwrap() as RawIndex + 0);
                let end = ByteIndex($span.rfind("~").unwrap() as RawIndex + 1);
                Ok((start, $token, end))
            }),*];

            assert_eq!(lexed_tokens, expected_tokens);
        }};
    }

    #[test]
    fn data() {
        test! {
            "  hello-hahaha8ABC  ",
            "  ~~~~~~~~~~~~~~~~  " => Token::Ident(Symbol::intern("hello-hahaha8ABC")),
        };
    }

    #[test]
    fn comment() {
        test! {
            "       // hello this is dog\n  ",
        };
    }

    #[test]
    fn doc_comment() {
        test! {
            "       /// hello this is dog",
            "       ~~~~~~~~~~~~~~~~~~~~~" => Token::DocComment("hello this is dog".to_string()),
        };
    }

    #[test]
    fn string_literal() {
        test! {
            r#"  "a" "\t"  "#,
            r#"  ~~~       "# => Token::StringLiteral("a".to_owned()),
            r#"      ~~~~  "# => Token::StringLiteral("\t".to_owned()),
        };
    }

    #[test]
    fn char_literal() {
        test! {
            r"  'a' '\t'  ",
            r"  ~~~       " => Token::CharLiteral('a'),
            r"      ~~~~  " => Token::CharLiteral('\t'),
        };
    }

    #[test]
    fn bin_literal() {
        test! {
            "  0b010110  ",
            "  ~~~~~~~~  " => Token::BinIntLiteral(0b010110),
        };
    }

    #[test]
    fn oct_literal() {
        test! {
            "  0o12371  ",
            "  ~~~~~~~  " => Token::OctIntLiteral(0o12371),
        };
    }

    #[test]
    fn dec_literal() {
        test! {
            "  123  ",
            "  ~~~  " => Token::DecIntLiteral(123),
        };
    }

    #[test]
    fn hex_literal() {
        test! {
            "  0x123AF  ",
            "  ~~~~~~~  " => Token::HexIntLiteral(0x123AF),
        };
    }

    #[test]
    fn float_literal() {
        test! {
            "  122.345  ",
            "  ~~~~~~~  " => Token::DecFloatLiteral(122.345),
        };
    }

    #[test]
    fn keywords() {
        test! {
            "  else if import in let type fn true false ",
            "  ~~~~                                     " => Token::Else,
            "       ~~                                  " => Token::If,
            "          ~~~~~~                           " => Token::Import,
            "                 ~~                        " => Token::In,
            "                    ~~~                    " => Token::Let,
            "                        ~~~~               " => Token::Type,
            "                             ~~            " => Token::Fn,
            "                                ~~~~       " => Token::True,
            "                                     ~~~~~ " => Token::False,
        };
    }

    #[test]
    fn symbols() {
        test! {
            r" \ ^ : , .. = -> => ? ; ",
            r" ~                      " => Token::BSlash,
            r"   ~                    " => Token::Caret,
            r"     ~                  " => Token::Colon,
            r"       ~                " => Token::Comma,
            r"         ~~             " => Token::DotDot,
            r"            ~           " => Token::Eq,
            r"              ~~        " => Token::RArrow,
            r"                 ~~     " => Token::FatArrow,
            r"                    ~   " => Token::Question,
            r"                      ~ " => Token::Semi,
        }
    }

    #[test]
    fn delimiters() {
        test! {
            " ( ) { } [ ] ",
            " ~           " => Token::LParen,
            "   ~         " => Token::RParen,
            "     ~       " => Token::LBrace,
            "       ~     " => Token::RBrace,
            "         ~   " => Token::LBracket,
            "           ~ " => Token::RBracket,
        }
    }
}
