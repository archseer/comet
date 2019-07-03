pub use codespan::{
    ByteIndex, ByteOffset, ColumnIndex, ColumnOffset, FileId, Files, LineIndex, LineOffset,
    RawOffset, Span,
};

// pub const DUMMY_SPAN: Span = Span::new(ByteIndex(0), ByteIndex(0));

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
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

impl<T: std::fmt::Display> std::fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}: {}", self.span.start(), self.value)
    }
}

pub fn span(start: ByteIndex, end: ByteIndex) -> Span {
    Span::new(start, end)
}

pub fn spanned<T>(span: Span, value: T) -> Spanned<T> {
    Spanned { span, value }
}

pub fn spanned2<T>(start: ByteIndex, end: ByteIndex, value: T) -> Spanned<T> {
    Spanned {
        span: span(start, end),
        value,
    }
}

// pub trait HasSpan {
//     fn span(&self) -> Span;
// }
