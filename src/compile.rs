use crate::ast::*;
use crate::token::*;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::BasicValueEnum;
use inkwell::values::BasicValueEnum::*;
use inkwell::values::FunctionValue;
use inkwell::IntPredicate as IntP;
use std::collections::HashMap;

pub struct Compile<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,

    fn_value_opt: Option<FunctionValue<'ctx>>,
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
            fn_value_opt: None,
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
    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt.unwrap()
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
                        ">" => {
                            IntValue(
                                self.builder
                                    .build_int_compare(IntP::SGT, lhval, rhval, "sgt"),
                            )
                        }
                        _ => unimplemented!(),
                    },
                    _ => unimplemented!(),
                }
            }

            Expr::If(cond_expr, then_expr, else_expr) => {
                let parent = self.fn_value();
                let cond = self.compile_expr(cond_expr, env);
                let then_bb = self.context.append_basic_block(parent, "then");
                let else_bb = self.context.append_basic_block(parent, "else");
                let cont_bb = self.context.append_basic_block(parent, "ifcont");
                if let IntValue(cond_) = cond {
                    self.builder
                        .build_conditional_branch(cond_, then_bb, else_bb);

                    self.builder.position_at_end(then_bb);
                    let then_val = self.compile_expr(then_expr, env);
                    self.builder.build_unconditional_branch(cont_bb);
                    let then_bb = self.builder.get_insert_block().unwrap();

                    self.builder.position_at_end(else_bb);
                    let else_val = self.compile_expr(else_expr, env);
                    self.builder.build_unconditional_branch(cont_bb);

                    let else_bb = self.builder.get_insert_block().unwrap();

                    self.builder.position_at_end(cont_bb);

                    let phi = self.builder.build_phi(self.context.i64_type(), "iftmp");

                    phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

                    phi.as_basic_value()
                } else {
                    panic!()
                }
            }

            Expr::Var(varname) => match env.get(&(&varname as &str)) {
                Some(expr) => *expr,
                None => panic!(),
            },

            _ => unimplemented!(),
        }
    }
    pub fn add_main(&mut self, ast: &[Stmt]) {
        let main_type = self.context.i64_type().fn_type(&[], false);
        let main = self.module.add_function("main", main_type, None);
        self.fn_value_opt = Some(main);
        let main_block = self.context.append_basic_block(main, "entry");
        self.builder.position_at_end(main_block);
        let ret = self.compile_stmt(&ast, &mut HashMap::new());
        self.builder.build_return(Some(&ret));
    }
    pub fn print(&self) {
        self.module.print_to_stderr()
    }
}
