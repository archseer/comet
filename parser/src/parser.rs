pub use crate::lexer::{LexerError, Token};
use lalrpop_util::ParseError as LalrpopError;

use crate::diagnostics::{FileMap, Pos, Span};
use codespan_reporting::{Diagnostic, Label};

use crate::{ast::Expr, grammar::ExprParser, lexer::Lexer};

pub fn parse<'input>(filemap: &'input FileMap) -> Result<(Expr, Vec<String>), Vec<ParseError>> {
    let mut import_paths = Vec::new();
    let mut errors = Vec::new();
    let lexer = Lexer::new(filemap).map(|x| x.map_err(ParseError::from));
    let value = ExprParser::new()
        .parse(&mut import_paths, &mut errors, lexer)
        .unwrap_or_else(|err| {
            errors.push(err.into());
            Expr::Error(filemap.span())
        });

    if errors.is_empty() {
        Ok((value, import_paths))
    } else {
        Err(errors)
    }
}

/// An error that occurred while lexing the source file
#[derive(Error, Debug, Clone, PartialEq)]
pub enum ParseError {
    #[error(display = "{}", _0)]
    Lexer(#[cause] LexerError),
    // #[error(display = "Unknown repl command `:{}` found.", command)]
    // UnknownReplCommand { span: Span, command: String },
    #[error(display = "Unexpected EOF, expected one of: {}.", expected)]
    UnexpectedEof { end: Pos, expected: ExpectedTokens },
    #[error(
        display = "Unexpected token {}, found, expected one of: {}.",
        token,
        expected
    )]
    UnexpectedToken {
        span: Span,
        token: Token,
        expected: ExpectedTokens,
    },
    #[error(display = "Extra token {} found", token)]
    ExtraToken { span: Span, token: Token },
}

/// Flatten away an LALRPOP error, leaving the inner `ParseError` behind
impl<T> From<LalrpopError<Pos, T, ParseError>> for ParseError
where
    T: Into<Token>,
{
    fn from(err: LalrpopError<Pos, T, ParseError>) -> ParseError {
        match err {
            LalrpopError::User { error } => error,
            LalrpopError::InvalidToken { .. } => unreachable!(),
            LalrpopError::UnrecognizedEOF { location, expected } => ParseError::UnexpectedEof {
                end: location,
                expected: ExpectedTokens(expected),
            },
            LalrpopError::UnrecognizedToken {
                token: (start, token, end),
                expected,
            } => ParseError::UnexpectedToken {
                span: Span::new(start, end),
                token: token.into(),
                expected: ExpectedTokens(expected),
            },
            LalrpopError::ExtraToken {
                token: (start, token, end),
            } => ParseError::ExtraToken {
                span: Span::new(start, end),
                token: token.into(),
            },
        }
    }
}

impl ParseError {
    /// Return the span of source code that this error originated from
    pub fn span(&self) -> Span {
        match *self {
            ParseError::Lexer(ref err) => err.span(),
            ParseError::UnexpectedToken { span, .. }
            // | ParseError::UnknownReplCommand { span, .. }
            | ParseError::ExtraToken { span, .. } => span,
            ParseError::UnexpectedEof { end, .. } => Span::new(end, end),
        }
    }

    /// Convert the error into a diagnostic message
    pub fn to_diagnostic(&self) -> Diagnostic {
        match *self {
            ParseError::Lexer(ref err) => err.to_diagnostic(),
            // ParseError::UnknownReplCommand { span, ref command } => {
            //     Diagnostic::new_error(format!("unknown repl command `:{}`", command))
            //         .with_label(Label::new_primary(span).with_message("unexpected command"))
            // }
            ParseError::UnexpectedToken {
                span,
                ref token,
                ref expected,
            } => Diagnostic::new_error(format!("expected one of {}, found `{}`", expected, token))
                .with_label(Label::new_primary(span).with_message("unexpected token")),
            ParseError::UnexpectedEof { end, ref expected } => {
                Diagnostic::new_error(format!("expected one of {}, found `EOF`", expected))
                    .with_label(
                        Label::new_primary(Span::new(end, end)).with_message("unexpected EOF"),
                    )
            }
            ParseError::ExtraToken { span, ref token } => {
                Diagnostic::new_error(format!("extra token `{}`", token))
                    .with_label(Label::new_primary(span).with_message("extra token"))
            }
        }
    }
}

impl From<LexerError> for ParseError {
    fn from(src: LexerError) -> ParseError {
        ParseError::Lexer(src)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpectedTokens(pub Vec<String>);

impl std::fmt::Display for ExpectedTokens {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for (i, token) in self.0.iter().enumerate() {
            match i {
                0 => write!(f, "{}", token)?,
                i if i < self.0.len() - 1 => write!(f, ", {}", token)?,
                _ => write!(f, ", or {}", token)?,
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::{assert_eq, assert_ne};

    #[test]
    fn expr_test() {
        use crate::ast::*;
        use crate::diagnostics::{CodeMap, FileName};
        use crate::grammar::ExprParser;
        use crate::lexer::Lexer;

        macro_rules! assert_parse {
            ($res:expr, $src:expr) => {{
                let mut codemap = CodeMap::new();
                let filemap = codemap.add_filemap(FileName::virtual_("test.ct"), $src.into());
                let value = parse(&filemap).map(|(val, paths)| val);

                assert_eq!($res, value);
            }};
        };

        assert_parse!(Ok(Expr::Int(42)), "42");

        assert_parse!(Ok(Expr::Int(255)), "0xFF");

        assert_parse!(Ok(Expr::Int(129)), "0b10000001"); // allow _ later

        assert_parse!(Ok(Expr::Bool(true)), "true");

        assert_parse!(Ok(Expr::Bool(false)), "false");

        // let expr = "3 + 2 * 4";
        // assert_parse!(&format!("{:?}", expr), "(3 + (2 * 4))");

        // assert_parse!(
        //     Ok(Expr::Int(129)),
        //     "fn (a, b, c) -> 1 + 2 * 3"
        // );

        // assert_parse!(
        //     Ok(Expr::Int(129)),
        //     "test(1 + 2, 2, 3)"
        // );

        assert_parse!(
            Ok(Expr::Int(12)),
            "fn (a, b) {
                let x: int = 1 + 2
                x

                if x > 2 {
                    1
                } else {
                    0
                }
            }"
        );
    }
}
