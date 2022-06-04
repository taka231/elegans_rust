use crate::token::*;

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    BinOp(Token, Box<Expr>, Box<Expr>),
    Number(i32),
    Var(String),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Stmt {
    Assign(String, Expr),
    ExprStmt(Expr),
}
