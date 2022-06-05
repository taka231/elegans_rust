use crate::ast::*;
use crate::token::*;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::IntValue;
use std::collections::HashMap;

pub struct Compile<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
}

impl<'a, 'ctx: 'a> Compile<'a, 'ctx> {
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
        &'ctx self,
        stmts: &'a [Stmt],
        env: &mut std::collections::HashMap<&'a str, inkwell::values::IntValue<'a>>,
    ) -> IntValue {
        let (init_stmts, last_stmt) = stmts.split_at(stmts.len() - 1);
        for stmt in init_stmts {
            match stmt {
                Stmt::ExprStmt(expr) => {
                    self.compile_expr(expr);
                    continue;
                }
                Stmt::Assign(varname, expr) => {
                    let rh = self.compile_expr(expr);
                    env.insert(&varname, rh);
                }
            }
        }
        match &last_stmt[0] {
            Stmt::ExprStmt(expr) => self.compile_expr(expr),
            _ => panic!(),
        }
    }
    fn compile_expr(&'ctx self, expr: &'ctx Expr) -> IntValue<'ctx> {
        match expr {
            Expr::Number(num) => self.context.i64_type().const_int(*num as u64, false),
            Expr::BinOp(Token::Op(str), lh, rh) => match &str as &str {
                "+" => {
                    self.builder
                        .build_int_add(self.compile_expr(lh), self.compile_expr(rh), "add")
                }
                "-" => {
                    self.builder
                        .build_int_sub(self.compile_expr(lh), self.compile_expr(rh), "sub")
                }

                "*" => {
                    self.builder
                        .build_int_mul(self.compile_expr(lh), self.compile_expr(rh), "sub")
                }
                "/" => self.builder.build_int_signed_div(
                    self.compile_expr(lh),
                    self.compile_expr(rh),
                    "sub",
                ),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }
    pub fn add_main(&'ctx self, ast: &'ctx [Stmt]) {
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
