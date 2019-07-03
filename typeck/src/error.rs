use crate::typecheck::Type;
use comet_parser::{ast::Ident, diagnostics::Span};

#[derive(Debug, PartialEq)]
pub enum Error {
    UnknownVariable { span: Span, name: Ident },
    UnknownType { span: Span, name: Ident },
    UndefinedFn { span: Span, name: Ident },
    NotFn,
    RecursiveType,
    CouldNotUnify { t1: Type, t2: Type },
    UnexpectedArity { expected: usize, given: usize },
}
