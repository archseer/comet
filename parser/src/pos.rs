//! Source code locations (taken from gluon, which in turn borrowed from rustc's [libsyntax_pos])
//!
//! [gluon]: https://github.com/gluon-lang/gluon/blob/master/base/src/pos.rs
//! [libsyntax_pos]: https://github.com/rust-lang/rust/blob/master/src/libsyntax_pos/lib.rs

use std::fmt;

pub use codespan::ByteSpan as Span;

pub use codespan::{
    ByteIndex as Pos, ByteOffset, ColumnIndex as Column, ColumnOffset, LineIndex as Line,
    LineOffset,
};

/// A location in a source file
#[derive(Copy, Clone, Default, Eq, PartialEq, Debug, Hash, Ord, PartialOrd)]
pub struct Location {
    // TODO: file
    pub line: Line,
    pub column: Column,
    pub absolute: Pos,
}

impl Location {
    pub fn shift(&mut self, ch: u8) {
        if ch == b'\n' {
            self.line += LineOffset(1);
            self.column = Column(1);
        } else {
            self.column += ColumnOffset(1);
        }
        self.absolute += ByteOffset(1);
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Line: {}, Column: {}",
            self.line.number(),
            self.column.number()
        )
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Default)]
pub struct Spanned<T> {
    pub span: Span,
    pub value: T,
}

impl<T> Spanned<T> {
    pub fn map<U, F>(self, mut f: F) -> Spanned<U>
    where
        F: FnMut(T) -> U,
    {
        Spanned {
            span: self.span,
            value: f(self.value),
        }
    }
}

impl<T: fmt::Display> fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.span.start(), self.value)
    }
}

pub fn span(start: Pos, end: Pos) -> Span {
    Span::new(start, end)
}

pub fn spanned<T>(span: Span, value: T) -> Spanned<T> {
    Spanned { span, value }
}

pub fn spanned2<T>(start: Pos, end: Pos, value: T) -> Spanned<T> {
    Spanned {
        span: span(start, end),
        value,
    }
}

pub trait HasSpan {
    fn span(&self) -> Span;
}
