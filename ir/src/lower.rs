use comet_parser as parser;
use parser::ast::{self, Expr, Ident, Opcode, Statement};
use parser::diagnostics::{ByteIndex, Span};
use parser::symbol::Symbol;

use cranelift_entity::{entity_impl, EntityList, EntityRef, ListPool, PrimaryMap};

use std::collections::HashMap;

use crate::builder::Builder;
use crate::fun::*;

#[derive(Debug, PartialEq)]
pub enum Error {
    // unimpl
}

pub fn lower_module(module: &ast::Module, name: &str) -> Result<Module, Error> {
    let mut ir_module = Module::new(Symbol::intern(name));
    let mut ctx = Context::new();

    for fun in module.statements.iter() {
        match fun {
            Statement::Fn { name, args, body } => {
                let name = name.0; // TEMP: until we get rid of the Ident(Sym) wrapper
                let span = Span::initial(); // TODO
                let fun = Function::new(name, span);
                ir_module.functions.insert(name, fun);
                let fun = ir_module.functions.get_mut(&name).unwrap();

                let mut builder = Builder::new(fun);

                ctx.lower_fn(&mut builder, span, name, args, body) // .unwrap();
            }
        }
    }

    Ok(ir_module)
}

struct Context {
    vars: HashMap<Symbol, Value>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            vars: HashMap::new(),
        }
    }
}

impl Context {
    pub fn lower_fn(
        &mut self,
        builder: &mut Builder,
        span: Span,
        name: Symbol,
        args: &[Ident],
        body: &[Expr],
        // ) -> Result<Function, Error> {
    ) -> () {
        let (entry_cont, entry_val) = builder.lambda();

        // lower param patterns
        for arg in args {
            let val = builder.arg_insert(entry_cont);
            self.vars.insert(arg.0, val);
        }

        // lower body
        let (cont, val) = self.lower_block(builder, body, entry_cont).unwrap();

        // insert implicit return if last block doesn't return
        builder.implicit_return(cont, val);
    }

    pub fn lower_block(
        &mut self,
        builder: &mut Builder,
        exprs: &[Expr],
        entry: Block,
    ) -> Result<(Block, Value), Error> {
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
    pub fn lower_expr(
        &mut self,
        builder: &mut Builder,
        expr: &Expr,
        entry: Block,
    ) -> Result<(Block, Value), Error> {
        match expr {
            Expr::Int(i) => Ok((entry, builder.constant(*i))),
            Expr::Op(v1, Opcode::Equal, v2) => {
                // TODO: these shouldn't use entry
                let (_, v1_val) = self.lower_expr(builder, v1, entry)?;
                let (_, v2_val) = self.lower_expr(builder, v2, entry)?;
                let (_, val) = builder.cmp_eq(v1_val, v2_val);
                Ok((entry, val))
            }
            Expr::Let { name, ty, value } => {
                // compiles to a `store` primop
                let (_, value_val) = self.lower_expr(builder, value, entry)?;
                self.vars.insert(name.0, value_val);
                Ok((entry, value_val)) // TODO: should return `bottom` val ()
            }
            Expr::Var(name) => {
                // compiles to a 'load' primop?
                Ok((entry, *self.vars.get(&name.0).expect("undefined variable")))
            }
            Expr::Call { name, args } => {
                // let fun = self.lookup(name.0).expected("function not found");

                let args: Vec<_> = args
                    .iter()
                    .map(|arg| {
                        let (_, val) = self.lower_expr(builder, arg, entry).unwrap();
                        val
                    })
                    .collect();

                // TODO: might need to resolve fun here already
                let (_primop, result_val) = builder.call(entry, name.0, &args);

                Ok((entry, result_val))
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
            a => unimplemented!("{:?}", a),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::Graph;
    use comet_parser as parser;
    use cranelift_entity::{entity_impl, EntityList, EntityRef, ListPool, PrimaryMap};
    use parser::diagnostics::Span;
    use parser::symbol::Symbol;

    #[test]
    fn cond_if_test() {
        let mut fun = Function::new(Symbol::intern("test"), Span::initial());
        let entry = fun.blocks.push(BlockData {
            args: EntityList::new(),
            primops: EntityList::new(),
        });

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
