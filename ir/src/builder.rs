use crate::fun::*;

use comet_parser::symbol::Symbol;

use cranelift_entity::{entity_impl, EntityList, EntityRef, ListPool, PrimaryMap};

// enum Type {
//     Int
// }

pub struct Builder<'a> {
    fun: &'a mut Function,
}

impl<'a> Builder<'a> {
    pub fn new(fun: &'a mut Function) -> Self {
        Builder { fun }
    }

    pub fn arg_insert(&mut self, cont: Block) -> Value {
        let len = self.fun.blocks[cont].args.len(&self.fun.value_lists);

        let value = self.fun.values.push(ValueData {
            kind: ValueType::Argument(cont, len),
        });
        self.fun.blocks[cont]
            .args
            .push(value, &mut self.fun.value_lists);
        value
    }

    pub fn constant(&mut self, c: i64) -> Value {
        self.fun.values.push(ValueData {
            kind: ValueType::Constant(c),
        })
    }

    pub fn lambda(&mut self /*, ty: Type*/) -> (Block, Value) {
        // TODO: allocate param values
        let args = EntityList::new();

        let block = self.fun.blocks.push(BlockData {
            args,
            primops: EntityList::new(),
        });

        let value = self.fun.values.push(ValueData {
            kind: ValueType::Continuation(block),
        });

        (block, value)
    }

    pub fn cmp_eq(&mut self, v1: Value, v2: Value) -> (Primop, Value) {
        let mut operands = EntityList::new();
        operands.push(v1, &mut self.fun.value_lists);
        operands.push(v2, &mut self.fun.value_lists);

        let primop = self.fun.primops.push(PrimopData {
            kind: OpType::Eq,
            operands,
        });

        let value = self.fun.values.push(ValueData {
            kind: ValueType::Primop(primop),
        });

        (primop, value)
    }

    pub fn branch(&mut self, main: Block, cond: Value, if_then: Value, if_else: Value) -> Primop {
        let mut operands = EntityList::new();
        operands.push(cond, &mut self.fun.value_lists);
        operands.push(if_then, &mut self.fun.value_lists);
        operands.push(if_else, &mut self.fun.value_lists);

        let primop = self.fun.primops.push(PrimopData {
            kind: OpType::Branch,
            operands,
        });

        self.fun.blocks[main]
            .primops
            .push(primop, &mut self.fun.primop_lists);
        primop
    }

    pub fn call(&mut self, main: Block, fun: Symbol, args: &[Value]) -> (Primop, Value) {
        let mut operands = EntityList::new();

        let fun = self.fun.values.push(ValueData {
            kind: ValueType::Function(fun),
        });

        operands.push(fun, &mut self.fun.value_lists);
        operands.extend(args.iter().copied(), &mut self.fun.value_lists);

        let primop = self.fun.primops.push(PrimopData {
            kind: OpType::Call,
            operands,
        });

        self.fun.blocks[main]
            .primops
            .push(primop, &mut self.fun.primop_lists);

        let value = self.fun.values.push(ValueData {
            kind: ValueType::Primop(primop),
        });

        (primop, value)
    }

    pub fn jump(&mut self, main: Block, to: Value, args: &[Value]) -> Primop {
        let mut operands = EntityList::new();
        operands.push(to, &mut self.fun.value_lists);
        operands.extend(args.iter().copied(), &mut self.fun.value_lists);

        let primop = self.fun.primops.push(PrimopData {
            kind: OpType::Jump,
            operands,
        });

        self.fun.blocks[main]
            .primops
            .push(primop, &mut self.fun.primop_lists);
        primop
    }

    pub fn implicit_return(&mut self, main: Block, ret: Value) -> Option<Primop> {
        let block = &self.fun.blocks[main];

        if let Some(&op) = self.fun.block_primops(block).last() {
            if let Some(PrimopData {
                kind: OpType::Return,
                ..
            }) = self.fun.primops.get(op)
            {
                // ok, already has a return
                return None;
            }
        }

        let mut operands = EntityList::new();
        operands.push(ret, &mut self.fun.value_lists);

        // else insert a return
        let primop = self.fun.primops.push(PrimopData {
            kind: OpType::Return,
            operands,
        });

        self.fun.blocks[main]
            .primops
            .push(primop, &mut self.fun.primop_lists);

        Some(primop)
    }
}
