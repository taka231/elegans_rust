use crate::ast::*;
use crate::token::*;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::BasicValueEnum;
use inkwell::values::BasicValueEnum::*;
use std::collections::HashMap;

pub struct Compile<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
}

impl<'a, 'ctx> Compile<'a, 'ctx> {
    pub fn new(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
    ) -> Compile<'a, 'ctx> {
        Compile {
            context: context,
            builder: builder,
            module: module,
        }
    }
    fn compile_stmt(
        &self,
        stmts: &'a [Stmt],
        env: &mut HashMap<&'a str, BasicValueEnum<'ctx>>,
    ) -> BasicValueEnum {
        let (init_stmts, last_stmt) = stmts.split_at(stmts.len() - 1);
        for stmt in init_stmts {
            match stmt {
                Stmt::ExprStmt(expr) => {
                    self.compile_expr(expr, env);
                    continue;
                }
                Stmt::Assign(varname, expr) => {
                    let rh = self.compile_expr(expr, env);
                    env.insert(&varname, rh);
                }
            }
        }
        match &last_stmt[0] {
            Stmt::ExprStmt(expr) => self.compile_expr(expr, env),
            _ => panic!(),
        }
    }
    fn compile_expr(
        &self,
        expr: &Expr,
        env: &mut HashMap<&'a str, BasicValueEnum<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        match expr {
            Expr::Number(num) => IntValue(self.context.i64_type().const_int(*num as u64, false)),
            Expr::BinOp(Token::Op(str), lh, rh) => {
                match (self.compile_expr(lh, env), self.compile_expr(rh, env)) {
                    (IntValue(lhval), IntValue(rhval)) => match &str as &str {
                        "+" => IntValue(self.builder.build_int_add(lhval, rhval, "add")),
                        "-" => IntValue(self.builder.build_int_sub(lhval, rhval, "sub")),
                        "*" => IntValue(self.builder.build_int_mul(lhval, rhval, "mul")),
                        "/" => IntValue(self.builder.build_int_signed_div(lhval, rhval, "div")),
                        _ => unimplemented!(),
                    },
                    _ => unimplemented!(),
                }
            }

            Expr::Var(varname) => match env.get(&(&varname as &str)) {
                Some(expr) => *expr,
                None => panic!(),
            },

            _ => unimplemented!(),
        }
    }
    pub fn add_main(&self, ast: &[Stmt]) {
        let main_type = self.context.i64_type().fn_type(&[], false);
        let main = self.module.add_function("main", main_type, None);
        let main_block = self.context.append_basic_block(main, "entry");
        self.builder.position_at_end(main_block);
        let ret = self.compile_stmt(&ast, &mut HashMap::new());
        self.builder.build_return(Some(&ret));
    }
    pub fn print(&self) {
        self.module.print_to_stderr()
    }
}
