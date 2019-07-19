#[macro_use]
extern crate lalrpop_util;

#[macro_use]
extern crate err_derive;

lalrpop_mod!(#[allow(clippy::all)] pub grammar);
pub mod ast;
pub mod diagnostics;
pub mod lexer;
pub mod parser;
pub mod symbol;
