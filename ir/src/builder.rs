use crate::fun::*;

use cranelift_entity::{ EntityRef, PrimaryMap, ListPool, EntityList, entity_impl };

// enum Type {
//     Int
// }

pub struct Builder {
    fun: Function,
}


impl Builder {
    pub fn new(fun: Function) -> Self {
       Builder {
           fun
       } 
    }

    pub fn extract(self) -> Function {
        self.fun
    }

    pub fn arg_insert(&mut self, cont: Block) -> Value {
        let value = self.fun.values.push(ValueData {
            kind: ValueType::Argument(cont)
        });
        self.fun.blocks[cont].args.push(value, &mut self.fun.value_lists);
        value
    }

    pub fn constant(&mut self, c: i64) -> Value {
        self.fun.values.push(ValueData {
            kind: ValueType::Constant(c)
        })
    }

    pub fn lambda(&mut self/*, ty: Type*/) -> (Block, Value) {
        // TODO: allocate param values
        let args = EntityList::new();

        let block = self.fun.blocks.push(BlockData {
            args,
            primops: EntityList::new()
        });

        let value = self.fun.values.push(ValueData {
            kind: ValueType::Continuation(block)
        });

        (block, value)
    }

    pub fn cmp_eq(&mut self, v1: Value, v2: Value) -> (Primop, Value) {
        let mut params = EntityList::new();
        params.push(v1, &mut self.fun.value_lists);
        params.push(v2, &mut self.fun.value_lists);


        let primop = self.fun.primops.push(PrimopData {
            kind: OpType::Eq,
            params,
            result: None,
        });

        let value = self.fun.values.push(ValueData {
            kind: ValueType::Operand(primop)
        });

        self.fun.primops[primop].result = Some(value);

        let value = self.fun.values.push(ValueData {
            kind: ValueType::Primop(primop)
        });

        (primop, value)
    }

    pub fn branch(&mut self, main: Block, cond: Value, if_then: Value, if_else: Value) -> Primop {
        let mut params = EntityList::new();
        params.push(cond, &mut self.fun.value_lists);
        params.push(if_then, &mut self.fun.value_lists);
        params.push(if_else, &mut self.fun.value_lists);

        // TODO: tie to main
        let primop = self.fun.primops.push(PrimopData {
            kind: OpType::Branch,
            params,
            result: None,
        });

        self.fun.blocks[main].primops.push(primop, &mut self.fun.primop_lists);
        primop
    }

    pub fn jump(&mut self, main: Block, to: Value, args: &[Value]) -> Primop {
        let mut params = EntityList::new();
        params.push(to, &mut self.fun.value_lists);
        params.extend(args.iter().copied(), &mut self.fun.value_lists);

        // TODO: tie to main

        let primop = self.fun.primops.push(PrimopData {
            kind: OpType::Call,
            params,
            result: None
        });

        self.fun.blocks[main].primops.push(primop, &mut self.fun.primop_lists);
        primop
    }
}