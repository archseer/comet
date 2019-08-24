pub mod builder;
pub mod fun;
pub mod graph;
pub mod lower;

pub use builder::Builder;
pub use fun::{
    Block, BlockData, Function, Module, OpType, Primop, PrimopData, Value, ValueData, ValueType,
};
pub use lower::lower_module;
