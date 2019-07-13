use comet_parser as parser;
use parser::ast::{Expr, Opcode, Ident};
use parser::diagnostics::{ByteIndex, Span};
use parser::symbol::Symbol;

use std::collections::HashMap;

use crate::fun::*;
use crate::builder::Builder;

struct Context {
    vars: HashMap<Symbol, Value>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            vars: HashMap::new()
        }
    }
}

impl Context {
    pub fn lower_fn(&mut self, builder: &mut Builder, span: Span, name: Symbol, args: &Vec<Expr>, body: &Vec<Expr>) -> Result<Function, ()> {
        let entry = builder.lambda();


        // lower param patterns
    
        // lower body
        unimplemented!()
    }

    pub fn lower_block(&mut self, builder: &mut Builder, exprs: &Vec<Expr>, entry: Block) -> Result<(Block, Value), ()> {
        // loop over expr with lower_expr
        let mut block = entry;
        let mut value = None;

        for expr in exprs {
            let (new_block, new_val) = self.lower_expr(builder, expr, block)?;
            block = new_block;
            value = Some(new_val)
        }
        Ok((block, value.unwrap()))
    }

    // need to carefully thread through the blocks:
    pub fn lower_expr(&mut self, builder: &mut Builder, expr: &Expr, entry: Block) -> Result<(Block, Value), ()> {
        match expr {
            Expr::Int(i) => {
                Ok((entry, builder.constant(*i)))
            }
            Expr::Op(v1, Opcode::Equal, v2) => {
                // TODO: these shouldn't use entry
                let (_, v1_val) = self.lower_expr(builder, v1, entry)?;
                let (_, v2_val) = self.lower_expr(builder, v2, entry)?;
                let (_, val) = builder.cmp_eq(v1_val, v2_val);
                Ok((entry, val))
            }
            Expr::Let { name, ty, value } => {
                // compiles to a `store` primop
                unimplemented!()
            }
            Expr::If(cond, then_expr, else_expr) => {
                let (then_cont, then_val) = builder.lambda();
                let (else_cont, else_val) = builder.lambda();
                let (join_cont, join_val) = builder.lambda();
                let join_ret = builder.arg_insert(join_cont);

                // join is probably called with the result val
                // though that would be ssa: thorin / impala impl seems to skip that
                // seems to thread through cur_mem / mem_type
                // (equal to then_cont / else_cont param 0)

                // emit cond branch onto entry
                let (_, cond) = self.lower_expr(builder, cond, entry)?;
                // probably a primop branch
                let _ = builder.branch(entry, cond, then_val, else_val);
                // branch compiles to jump(world.branch(), {cond, t, f}), a compiler intrinsic

                // emit then blocks
                // TODO: getting rets here makes no sense
                let (then_cont, then_ret) = self.lower_block(builder, then_expr, then_cont)?;
                builder.jump(then_cont, join_val, &[then_ret]);
                // and the then return with a jump to join
    
                // emit else blocks
                let (else_cont, else_ret) = self.lower_block(builder, else_expr, else_cont)?;
                builder.jump(else_cont, join_val, &[else_ret]);
                // and the return with a jump to join

                Ok((join_cont, join_ret))
            }
            a => unimplemented!("{:?}", a)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::Graph;
    use comet_parser as parser;
    use parser::symbol::Symbol;
    use parser::diagnostics::Span;
    use cranelift_entity::{ EntityRef, PrimaryMap, ListPool, EntityList, entity_impl };
 
    #[test]
    fn cond_if_test() {
        let mut fun = Function::new(Symbol::intern("test"), Span::initial());
        let entry = fun.blocks.push(BlockData { args: EntityList::new(), primops: EntityList::new() });

        let mut builder = Builder::new(fun);

        let mut ctx = Context::new();
        let input = "if 1 == 2 { 42 } else { 23 }";
        let expr = parser::parser::parse(input).expect("syntax error");
        ctx.lower_expr(&mut builder, &expr, entry).unwrap();

        // let main = builder.lambda(/*Type::Fn((Type::Int, Type::Fn((Type::Int))))*/);
        // let if_then = builder.lambda();
        // let if_else = builder.lambda();
        // let next = builder.lambda(/*Type::Fn(Type::Int)*/);

        // let cmp = builder.cmp_eq(main.param(0), builder.literal(0));

        // main.push(builder.branch(cmp, if_then, if_else));
        // if_then.push(builder.jump(next, (builder.literal(23))));
        // if_else.push(builder.jump(next, (builder.literal(42))));
        // next.push(builder.jump(main.param(1), (next.param(0))));

        let fun = builder.extract();

        let mut buf = Vec::new();
        Graph::new(&fun).to_dot(&mut buf).unwrap();
        let res = std::str::from_utf8(&buf).unwrap();

        println!("{}", res);
    }
}
