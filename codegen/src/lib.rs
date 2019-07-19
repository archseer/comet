use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, FloatValue, FunctionValue, PhiValue, PointerValue};
use inkwell::{FloatPredicate, OptimizationLevel};

use std::collections::HashMap;

use comet_ir as ir;

#[derive(Debug, PartialEq)]
pub enum Error {
    FunctionInvalid,
}

pub struct Compiler<'a> {
    pub context: &'a Context,
    pub builder: &'a Builder,
    pub fpm: PassManager<FunctionValue>,

    // pub function: &'a Function,
    pub module: &'a Module,

    variables: HashMap<ir::Value, PointerValue>,
    fn_value_opt: Option<FunctionValue>,

    blocks: HashMap<ir::Block, BasicBlock>,
    phis: HashMap<(ir::Block, usize), PhiValue>,
    vals: HashMap<ir::Value, BasicValueEnum>,
}

impl<'a> Compiler<'a> {
    pub fn emit_module(&mut self, module: &ir::Module) -> () {
        // loop over funcs

        // emit proto for each fn in advance

        // emit each fun
        unimplemented!()
    }

    pub fn emit_proto(&mut self, fun: &ir::Function) -> Result<FunctionValue, Error> {
        let entry = fun.entry();

        let ret_type = self.context.i64_type(); // TODO
        let args_types = std::iter::repeat(ret_type)
            .take(entry.args.len(&fun.value_lists))
            .map(|f| f.into())
            .collect::<Vec<BasicTypeEnum>>();
        let args_types = args_types.as_slice();

        let fn_type = self.context.i64_type().fn_type(args_types, false);
        let fn_val = self.module.add_function(&fun.name.as_str(), fn_type, None);

        // set arguments names
        // for (i, arg) in fn_val.get_param_iter().enumerate() {
        //     arg.into_float_value()
        //         .set_name(entry.args.get(i, &fun.value_lists).as_str());
        // }

        // finally return built prototype
        Ok(fn_val)
    }

    pub fn emit_fn(&mut self, fun: &ir::Function) -> Result<FunctionValue, Error> {
        let function = self.emit_proto(fun)?;

        // got external function, returning only compiled prototype
        // if fun.body.is_none() {
        //     return Ok(function);
        // }

        let entry_cont = fun.entry();

        let entry = self.context.append_basic_block(&function, "entry");

        self.builder.position_at_end(&entry);

        // update fn field
        self.fn_value_opt = Some(function);

        // build variables map
        self.variables
            .reserve(entry_cont.args.len(&fun.value_lists));

        for (i, arg) in function.get_param_iter().enumerate() {
            // let arg_name = entry_cont.args.get(i, &fun.value_lists).unwrap().as_str();
            let arg_name = "arg";
            let alloca = self.create_entry_block_alloca(arg_name, Some(&entry));

            self.builder.build_store(alloca, arg);

            self.variables
                .insert(entry_cont.args.get(i, &fun.value_lists).unwrap(), alloca);
        }

        // -> compile body

        // predefine all the continuation blocks
        for (block_id, block) in fun.blocks.iter() {
            let bb = self
                .context
                .append_basic_block(&function, &format!("{}", block_id));

            self.blocks.insert(block_id, bb);

            // insert phi args for block
            self.builder.position_at_end(&bb);
            for (i, arg) in fun.block_args(block).iter().enumerate() {
                // insert arg (phi node, then store that into phis and phi.to_value() into vals)

                // TODO: resolve type
                let typ = self.context.i64_type();
                let phi = self.builder.build_phi(typ, &format!("{}", arg));
                self.phis.insert((block_id, i), phi);
                self.vals.insert(*arg, phi.as_basic_value());
            }
        }

        // then emit them
        for (block_id, block) in fun.blocks.iter() {
            self.emit_block(fun, block_id, block);
        }

        // return the whole thing after verification and optimization
        if function.verify(true) {
            self.fpm.run_on(&function);

            Ok(function)
        } else {
            unsafe {
                function.delete();
            }

            Err(Error::FunctionInvalid)
        }
    }

    fn emit_block(&mut self, fun: &ir::Function, id: ir::Block, block: &ir::BlockData) {
        let bb = self.blocks[&id];
        self.builder.position_at_end(&bb);

        for op in fun.block_primops(block) {
            self.emit_primop(fun, *op)
        }
    }

    fn emit_primop(&mut self, fun: &ir::Function, op: ir::Primop) {
        use ir::OpType::*;
        let op = &fun.primops[op];

        match op.kind {
            Call => {
                // a
                unimplemented!()
            }
            Jump => {
                // a
                unimplemented!()
            }
            Return => {
                // a
                unimplemented!()
            }
            Eq => {
                // a
                unimplemented!()
            }
            Branch => {
                // a
                unimplemented!()
            }
        }
    }

    /// Creates a new stack allocation instruction in the entry block of the function.
    fn create_entry_block_alloca(&self, name: &str, entry: Option<&BasicBlock>) -> PointerValue {
        unimplemented!()
        // let builder = self.context.create_builder();

        // let owned_entry = self.fn_value().get_entry_basic_block();
        // let entry = owned_entry.as_ref().or(entry).unwrap();

        // match entry.get_first_instruction() {
        //     Some(first_instr) => builder.position_before(&first_instr),
        //     None => builder.position_at_end(entry),
        // }

        // builder.build_alloca(self.context.i64_type(), name) // TODO
    }
}

pub fn compile(module: &ir::Module) -> Result<Module, Error> {
    let context = Context::create();
    let llvm_module = context.create_module(&module.name.as_str());
    let builder = context.create_builder();

    let fpm = PassManager::create(&llvm_module);

    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_gvn_pass();
    fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();

    fpm.initialize();

    let mut compiler = Compiler {
        context: &context,
        builder: &builder,
        fpm,
        module: &llvm_module,
        // function,
        fn_value_opt: None,
        variables: HashMap::new(),

        blocks: HashMap::new(),
        phis: HashMap::new(),
        vals: HashMap::new(),
    };

    compiler.emit_module(module);

    Ok(llvm_module)
}
