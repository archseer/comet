use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{
    AnyValueEnum, BasicValue, BasicValueEnum, FloatValue, FunctionValue, PhiValue, PointerValue,
};
use inkwell::{IntPredicate, OptimizationLevel};

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

    variables: HashMap<ir::Value, BasicValueEnum>,
    fn_value_opt: Option<FunctionValue>,

    blocks: HashMap<ir::Block, BasicBlock>,
    phis: HashMap<(ir::Block, usize), PhiValue>,
}

impl<'a> Compiler<'a> {
    /// Returns the `FunctionValue` representing the function being compiled.
    #[inline]
    fn fn_value(&self) -> FunctionValue {
        self.fn_value_opt.unwrap()
    }

    pub fn emit_module(&mut self, module: &ir::Module) -> () {
        // emit proto for each fn in advance
        for fun in module.functions.values() {
            self.emit_proto(fun).unwrap();
        }

        // emit each fun
        for fun in module.functions.values() {
            self.emit_fn(fun).unwrap();
        }
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

        self.blocks.insert(ir::fun::START, entry);

        // update fn field
        self.fn_value_opt = Some(function);

        // build variables map
        self.variables
            .reserve(entry_cont.args.len(&fun.value_lists));

        for (i, arg) in function.get_param_iter().enumerate() {
            // let arg_name = entry_cont.args.get(i, &fun.value_lists).unwrap().as_str();
            let arg_name = "arg";
            let alloca = self.create_entry_block_alloca(arg_name);

            self.builder.build_store(alloca, arg);

            self.variables.insert(
                entry_cont.args.get(i, &fun.value_lists).unwrap(),
                alloca.as_basic_value_enum(),
            );

            // TODO: insert variables as vals
        }

        // -> compile body

        // predefine all the continuation blocks
        for (block_id, block) in fun.blocks.iter() {
            // skip entry block which we already defined
            if block_id == ir::fun::START {
                continue;
            }

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
                self.variables.insert(*arg, phi.as_basic_value());
            }
        }

        // then emit them
        for (block_id, block) in fun.blocks.iter() {
            self.emit_block(fun, block_id, block);
        }

        function.print_to_stderr();

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
            self.emit_primop(fun, *op); // ignore value
        }
        // TODO: last value should be as block result? tho usually last val is ctrl flow terminator
    }

    // TODO: possibly use AnyValueEnum?
    // compiles the value, regardless of what it is
    fn emit_value(&mut self, fun: &ir::Function, val: ir::Value) -> BasicValueEnum {
        use ir::ValueType::*;
        let value = &fun.values[val];

        match value.kind {
            Constant(c) => self
                .context
                .i64_type()
                .const_int(c as u64, false)
                .as_basic_value_enum(),
            // TODO: build load
            Argument(_block, _index) => self.builder.build_load(self.variables[&val], "argvar"),
            Continuation(block) => {
                println!("c");
                // a
                unimplemented!()
            }
            Primop(primop) => self
                .emit_primop(fun, primop)
                .expect("primop with no value!"),
            Function(name) => {
                println!("a");
                // a
                unimplemented!()
            }
        }
    }

    fn emit_primop(&mut self, fun: &ir::Function, op: ir::Primop) -> Option<BasicValueEnum> {
        use ir::OpType::*;
        let op = &fun.primops[op];
        let args = op.operands.as_slice(&fun.value_lists);

        // TODO: call is emitted twice?
        match op.kind {
            Call => {
                // 0 = fun
                // 1.. args
                let f = &fun.values[args[0]];

                // TODO: use a (module, fn_name) symbol keys map
                match self.module.get_function(&f.to_sym().as_str()) {
                    Some(callee) => {
                        let argv: Vec<_> = args[1..]
                            .iter()
                            .map(|arg| {
                                self.emit_value(fun, *arg)
                                // TODO: maybe always emit basic value
                                // .try_as_basic_value()
                                // .left()
                                // .unwrap()
                            })
                            .collect();

                        let ret_val = self
                            .builder
                            .build_call(callee, argv.as_slice(), "fun_call")
                            .try_as_basic_value()
                            .left()
                            .unwrap();

                        Some(ret_val)
                    }
                    None => panic!("unknown function!"),
                }
            }
            Jump => {
                let cont = fun.continuation(args[0]);
                self.builder.build_unconditional_branch(&self.blocks[&cont]);
                // TODO maybe no return?
                None
            }
            Return => {
                println!("building return");
                let return_val = self.emit_value(fun, args[0]);
                self.builder.build_return(Some(&return_val));
                // TODO maybe no return?
                None
            }
            Eq => {
                // let call = ctx.builder.build_call(
                //     self.types.enif_compare,
                //     &[
                //         lhs,
                //         rhs,
                //     ],
                //     "",
                // );
                // let res = call.try_as_basic_value().left().unwrap();

                let cond = self.builder.build_int_compare(
                    IntPredicate::EQ,
                    self.emit_value(fun, args[0]).into_int_value(),
                    self.emit_value(fun, args[1]).into_int_value(),
                    "eq",
                );

                // let cond = self.builder.build_int_compare(
                //     IntPredicate::EQ,
                //     res.into_int_value(),
                //     self.context.i32_type().const_int(0, false),
                //     "eq",
                // );

                Some(cond.as_basic_value_enum())
            }
            Branch => {
                let cond = self.emit_value(fun, args[0]);
                let then_cont = fun.continuation(args[1]);
                let else_cont = fun.continuation(args[2]);

                self.builder.build_conditional_branch(
                    *cond.as_int_value(),
                    &self.blocks[&then_cont],
                    &self.blocks[&else_cont],
                );
                None
                // TODO: maybe no return?
            }
        }
    }

    /// Creates a new stack allocation instruction in the entry block of the function.
    fn create_entry_block_alloca(&self, name: &str) -> PointerValue {
        let builder = self.context.create_builder();

        let entry = self.fn_value().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(&entry),
        }

        builder.build_alloca(self.context.i64_type(), name) // TODO
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
    };

    compiler.emit_module(module);

    Ok(llvm_module)
}
