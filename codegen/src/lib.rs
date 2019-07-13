use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, FloatValue, FunctionValue, PointerValue};
use inkwell::{OptimizationLevel, FloatPredicate};

use comet_ir as ir;

pub struct Compiler<'a> {
    pub context: &'a Context,
    pub builder: &'a Builder,
    pub fpm: &'a PassManager<FunctionValue>,
    pub module: &'a Module,
    pub function: &'a Function,

    variables: HashMap<String, PointerValue>,
    fn_value_opt: Option<FunctionValue>
}

impl<'a> Compiler<'a> {
    pub fn emit_module(&mut module: &ir::Module) -> () {
        unimplemented!()
    }

    pub fn emit_proto(&mut self, fun: &ir::Fn) {
        unimplemented!()
    }

    pub fn emit_fn(&mut self, fun: &ir::Fn) -> Result<FunctionValue, &'static str> {
        let function = self.emit_proto(fun.name, fun.args)?;

        // got external function, returning only compiled prototype
        // if fun.body.is_none() {
        //     return Ok(function);
        // }

        let entry = self.context.append_basic_block(&function, "entry");

        self.builder.position_at_end(&entry);

        // update fn field
        self.fn_value_opt = Some(function);

        // build variables map
        self.variables.reserve(fun.args.len());

        for (i, arg) in function.get_param_iter().enumerate() {
            let arg_name = fun.args[i].as_str();
            let alloca = self.create_entry_block_alloca(arg_name, Some(&entry));

            self.builder.build_store(alloca, arg);

            self.variables.insert(fun.args[i].clone(), alloca);
        }

        // compile body
        let body = self.emit_expr(fun.body.as_ref().unwrap())?;

        self.builder.build_return(Some(&body));

        // return the whole thing after verification and optimization
        if function.verify(true) {
            self.fpm.run_on(&function);

            Ok(function)
        } else {
            unsafe {
                function.delete();
            }

            Err("Invalid generated function.")
        }
    }

    pub fn emit_expr(&mut self, &mut module: &ir::Expr) -> () {
        unimplemented!()
    }
}

pub fn compile() -> Result<FunctionValue, &'static str> {
    let mut compiler = Compiler {

    }

    compiler.emit_module(module);
}
