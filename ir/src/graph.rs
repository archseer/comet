use crate::fun::*;

pub struct Graph<'a> {
    fun: &'a Function,
}

impl<'a> Graph<'a> {
    pub fn new(fun: &'a Function) -> Graph<'a> {
        Graph { fun }
    }

    pub fn to_dot(&self, w: &mut dyn std::io::Write) -> std::io::Result<()> {
        write!(w, "digraph g {{\n")?;
        write!(w, "entry [ label=<entry> ];\n")?;
        write!(w, "entry -> blk_{};\n\n", START)?;
        for (block, data) in self.fun.blocks.iter() {
            for primop in data.primops.as_slice(&self.fun.primop_lists) {
                write!(w, "blk_{} -> prim_{};\n\n", block, primop)?;
            }
        }
        for (primop, data) in self.fun.primops.iter() {
            for param in data.operands.as_slice(&self.fun.value_lists) {
                match self.fun.values[*param] {
                    ValueData {
                        kind: ValueType::Continuation(block),
                    } => write!(w, "prim_{} -> blk_{};\n\n", primop, block)?,
                    ValueData {
                        kind: ValueType::Primop(p),
                    } => write!(w, "prim_{} -> prim_{};\n\n", primop, p)?,
                    ValueData {
                        kind: ValueType::Constant(p),
                    } => write!(w, "prim_{} -> const_{};\n\n", primop, p)?,
                    _ => unimplemented!(),
                }
            }
        }
        write!(w, "}}")?;

        Ok(())
    }
}
