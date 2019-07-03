pub use codespan::{
    ByteIndex as Pos, ByteOffset, ByteSpan as Span, CodeMap, ColumnIndex, ColumnOffset, FileMap,
    FileName, LineIndex, LineOffset, RawOffset,
};

// pub const DUMMY_SPAN: Span = Span::new(Pos(0), Pos(0));

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

impl<T: std::fmt::Display> std::fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
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

// pub trait HasSpan {
//     fn span(&self) -> Span;
// }
