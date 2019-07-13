pub mod fun;
pub mod builder;
pub mod lower;
pub mod graph;

use comet_parser::{symbol::Symbol};
use std::collections::HashMap;

struct Module {
    pub name: Symbol,
    functions: HashMap<Symbol, fun::Function>
}

