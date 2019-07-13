use cranelift_entity::{ EntityRef, PrimaryMap, ListPool, EntityList, entity_impl };

use comet_parser::{symbol::Symbol, diagnostics::Span};

/// Block/continuation
#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Block(u32);
entity_impl!(Block, "block");

/// Either a SSA variable, abstraction or a constant
#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Value(u32);
entity_impl!(Value, "value");

#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Primop(u32);
entity_impl!(Primop, "primop");

// continuations == functions.
// each contination has a target/terminator: another func, param, a select(cond, t, f)
// primops aren't scheduled: we need to schedule them later on
// primops have params/operands

pub struct BlockData {
    pub args: EntityList<Value>, // Parameter { index, value, owner }

    pub primops: EntityList<Primop>,
    // statements: Vec<Statement<'tcx>>
    //
    // TODO: block keeps track of the terminal primop, we schedule backwards?
    // since it's linked up

    // pub terminator: Option<Terminator<'tcx>>,
}

pub struct ValueData {
    pub kind: ValueType,
    // category
    // owner? continuation or primop
}

// values act as params to blocks/operands to primops
// common operation: find uses
pub enum ValueType {
    Constant(i64),
    Argument(Block),
    // / Parameter
    Continuation(Block),
    Primop(Primop),
    Operand(Primop),
}

// Function
// Continuation / BasicBlock
// Value
// Primop -> Specialize a terminator value?

pub struct Function {
    pub name: Symbol,
    pub span: Span,

    pub blocks: PrimaryMap<Block, BlockData>,
    pub values: PrimaryMap<Value, ValueData>,
    pub primops: PrimaryMap<Primop, PrimopData>,

    // memory locations on the stack, function arguments, local arguments and temporaries.
    // locals:  ,
    // expressions that identify a location in memory, like _1 or _1.f.
    // places: ,
    // expressions that produce a value. The "R" stands for the fact that these are the "right-hand side" of an assignment. 
    // rvalues: ,

    /// A pool of values. Stores all values referenced in basic blocks.
    pub value_lists: ListPool<Value>,
    pub primop_lists: ListPool<Primop>,

    // constant_values
    //
    // cache: predecessors/successors
}

impl Function {
    pub fn new(name: Symbol, span: Span) -> Self {
        Function {
            name,
            span,
            blocks: PrimaryMap::new(),
            values: PrimaryMap::new(),
            primops: PrimaryMap::new(),

            value_lists: ListPool::new(),
            primop_lists: ListPool::new(),
        }
    }
}

/// Block 0 is always the entry block.
pub const START: Block = Block(0);

pub struct PrimopData {
    pub kind: OpType,
    /// Operands: Typed values.
    pub params: EntityList<Value>, // TODO Operand { value, owner }
    /// Result, if any. `None` if side-effect only.
    pub result: Option<Value>,
}

pub enum OpType {
    Call,
    Return,
    Eq,
    Branch
}