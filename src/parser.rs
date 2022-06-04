use crate::ast::*;
use crate::token::*;
use std::iter::Peekable;
use std::slice::Iter;

pub fn parse(tokens: &[Token]) -> Vec<Stmt> {
    let mut stmt_vec: Vec<Stmt> = Vec::new();
    let mut stmts = tokens.split(|token| *token == Token::Semicolon);
    for stmt in stmts {
        if *stmt == [] {
            continue;
        }
        let mut stmt_iter = stmt.iter().peekable();
        let expr = parse_expr(&mut stmt_iter);
        match expr {
            Expr::Var(varname) if stmt_iter.next() == Some(&Token::Op("=".to_string())) => {
                let rh = parse_expr(&mut stmt_iter);
                stmt_vec.push(Stmt::Assign(varname, rh));
            }
            _ => {
                if stmt_iter.next() == None {
                    stmt_vec.push(Stmt::ExprStmt(expr));
                } else {
                    panic!()
                }
            }
        }
    }
    stmt_vec
}

fn parse_expr(tokens: &mut Peekable<Iter<Token>>) -> Expr {
    parse_additive_expr(tokens)
}

fn parse_additive_expr(tokens: &mut Peekable<Iter<Token>>) -> Expr {
    let mut left_expr = parse_multiplicative_expr(tokens);
    loop {
        match tokens.peek() {
            Some(Token::Op(op)) if &op as &str == "+" || &op as &str == "-" => {
                tokens.next();
                let right_expr = parse_multiplicative_expr(tokens);
                left_expr = Expr::BinOp(
                    Token::Op(op.clone()),
                    Box::new(left_expr),
                    Box::new(right_expr),
                );
            }
            _ => break,
        }
    }

    left_expr
}

fn parse_multiplicative_expr(tokens: &mut Peekable<Iter<Token>>) -> Expr {
    let mut left_expr = parse_term(tokens);
    loop {
        match tokens.peek() {
            Some(Token::Op(op)) if &op as &str == "*" || &op as &str == "/" => {
                tokens.next();
                let right_expr = parse_term(tokens);
                left_expr = Expr::BinOp(
                    Token::Op(op.clone()),
                    Box::new(left_expr),
                    Box::new(right_expr),
                );
            }
            _ => break,
        }
    }

    left_expr
}

fn parse_term(tokens: &mut Peekable<Iter<Token>>) -> Expr {
    match tokens.peek() {
        Some(Token::Number(num)) => {
            tokens.next();
            Expr::Number(*num)
        }
        Some(Token::Ident(str)) => {
            tokens.next();
            Expr::Var(str.to_string())
        }
        Some(Token::LParen) => {
            tokens.next();
            let expr = parse_expr(tokens);
            if tokens.next() == Some(&&Token::RParen) {
                expr
            } else {
                panic!("Expected LParen")
            }
        }
        _ => panic!("Expected term"),
    }
}

#[cfg(test)]
mod expr_tests {
    use super::*;
    use Expr::*;

    macro_rules! parse_expr_test {
        ($($describe:ident: $value:expr, )*) => {
            $(
                #[test]
                fn $describe() {
                    let (string, expr) = $value;
                    assert_eq!(parse_expr(&mut tokenize(string).iter().peekable()), expr);
                }
            )*
        }
    }

    parse_expr_test! {
        num: ("10", Expr::Number(10)),
        num_plus_num: (
            "10+12",
            Expr::BinOp(
                Token::Op("+".to_string()),
                Box::new( Expr::Number(10) ),
                Box::new( Expr::Number(12) )
                )
            ),
        num_minus_num: (
            "10-12",
            Expr::BinOp(
                Token::Op("-".to_string()),
                Box::new( Expr::Number(10) ),
                Box::new( Expr::Number(12) )
                )
            ),
        num_plus_num_minus_num: (
            "10-12+5",
            Expr::BinOp(
                Token::Op("+".to_string()),
                Box::new(
                    Expr::BinOp(
                        Token::Op("-".to_string()),
                        Box::new(Expr::Number(10)),
                        Box::new(Expr::Number(12))
                        )
                    ),
                Box::new(Expr::Number(5))
                )
            ),
        num_plus_num_mul_num: (
            "10+12*5",
            Expr::BinOp(
                Token::Op("+".to_string()),
                Box::new(Expr::Number(10)),
                Box::new(
                    Expr::BinOp(
                        Token::Op("*".to_string()),
                        Box::new(Expr::Number(12)),
                        Box::new(Expr::Number(5))
                        )
                    )
                )
            ),
        parens: (
            "(10+12)*5",
            Expr::BinOp(
                Token::Op("*".to_string()),
                Box::new(
                    Expr::BinOp(
                        Token::Op("+".to_string()),
                        Box::new(Expr::Number(10)),
                        Box::new(Expr::Number(12))
                        )
                    ),
                Box::new(Expr::Number(5)),
                )
            ),
        var: (
            "a + b",
            BinOp(
                Token::Op("+".to_string()),
                Box::new(Var("a".to_string())),
                Box::new(Var("b".to_string()))
            )
        ),
    }
}

#[cfg(test)]
mod stmt_tests {
    use super::*;
    use Expr::*;
    use Stmt::*;
    macro_rules! parse_stmt_test {
        ($($describe:ident: $value:expr, )*) => {
            $(
                #[test]
                fn $describe() {
                    let (string, stmt) = $value;
                    assert_eq!(parse(&tokenize(string)), stmt);
                }
            )*
        }
    }
    parse_stmt_test! {
        num: ("3; 4;", [ExprStmt(Number(3)), ExprStmt(Number(4))]),
        num_plus_num: ("3+4;3+4;", [
            ExprStmt(BinOp(Token::Op("+".to_string()), Box::new(Number(3)), Box::new(Number(4)))),
            ExprStmt(BinOp(Token::Op("+".to_string()), Box::new(Number(3)), Box::new(Number(4))))
        ]),
        assign: ("a = 3; a + 2", [
            Assign("a".to_string(), Number(3)),
            ExprStmt(BinOp(Token::Op("+".to_string()), Box::new(Var("a".to_string())), Box::new(Number(2))))
        ]),
    }
}
