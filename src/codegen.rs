use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;

struct CodeGen<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
}
