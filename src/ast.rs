use crate::token::*;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    BinOp(Token, Box<Expr>, Box<Expr>),
    Number(i32),
    Var(String),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Stmt {
    Assign(String, Expr),
    ExprStmt(Expr),
}
