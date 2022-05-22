use crate::token::*;

#[derive(Debug, PartialEq, Eq)]
enum Expr {
    BinOp(Token, Box<Expr>, Box<Expr>),
    Number(i32),
}
