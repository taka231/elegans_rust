use crate::ast::*;
use crate::token::*;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::IntValue;

pub struct Compile<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
}

impl<'a, 'ctx> Compile<'a, 'ctx> {
    fn compile_expr(&self, expr: Expr) -> IntValue {
        match expr {
            Expr::Number(num) => self
                .context
                .i64_type()
                .const_int(num.try_into().unwrap(), false),
            Expr::BinOp(Token::Op(str), lh, rh) => match &str as &str {
                "+" => self.builder.build_int_add(
                    self.compile_expr(*lh),
                    self.compile_expr(*rh),
                    "add",
                ),
                "-" => self.builder.build_int_sub(
                    self.compile_expr(*lh),
                    self.compile_expr(*rh),
                    "sub",
                ),

                "*" => self.builder.build_int_mul(
                    self.compile_expr(*lh),
                    self.compile_expr(*rh),
                    "sub",
                ),
                "/" => self.builder.build_int_signed_div(
                    self.compile_expr(*lh),
                    self.compile_expr(*rh),
                    "sub",
                ),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }
    pub fn add_main(&self, ast: Expr) {
        let main_type = self.context.i64_type().fn_type(&[], false);
        let main = self.module.add_function("main", main_type, None);
        let main_block = self.context.append_basic_block(main, "entry");
        self.builder.position_at_end(main_block);
        let ret = self.compile_expr(ast);
        self.builder.build_return(Some(&ret));
    }
    pub fn print(&self) {
        self.module.print_to_stderr()
    }
}
