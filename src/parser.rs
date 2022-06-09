use crate::ast::*;
use crate::token::*;
use std::iter::Peekable;
use std::slice::Iter;

pub fn parse(tokens: &[Token]) -> Vec<Stmt> {
    let mut stmt_vec: Vec<Stmt> = Vec::new();
    let stmts = tokens.split(|token| *token == Token::Semicolon);
    for stmt in stmts {
        if *stmt == [] {
            continue;
        }
        let mut stmt_iter = stmt.iter().peekable();
        let expr = parse_expr(&mut stmt_iter);
        match expr {
            Expr::Var(varname) if stmt_iter.peek() == Some(&&Token::Op("=".to_string())) => {
                stmt_iter.next();
                let rh = parse_expr(&mut stmt_iter);
                stmt_vec.push(Stmt::Assign(varname, rh));
            }
            Expr::FnCall(fnname, args)
                if stmt_iter.peek() == Some(&&Token::Op("=".to_string())) =>
            {
                stmt_iter.next();
                match *fnname {
                    Expr::Var(fnname) => {
                        let body = parse_expr(&mut stmt_iter);
                        let args = args
                            .iter()
                            .map(|expr| match expr {
                                Expr::Var(argname) => argname.to_string(),
                                _ => panic!("expected ident"),
                            })
                            .collect();
                        stmt_vec.push(Stmt::FnDef(fnname, args, body))
                    }
                    _ => panic!("expected ident"),
                }
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

pub fn parse_expr(tokens: &mut Peekable<Iter<Token>>) -> Expr {
    parse_compare_expr(tokens)
}

fn parse_compare_expr(tokens: &mut Peekable<Iter<Token>>) -> Expr {
    let mut left_expr = parse_additive_expr(tokens);
    loop {
        match tokens.peek() {
            Some(Token::Op(op))
                if &op as &str == ">"
                    || &op as &str == "<"
                    || &op as &str == "=="
                    || &op as &str == "/=" =>
            {
                tokens.next();
                let right_expr = parse_additive_expr(tokens);
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
    let mut left_expr = parse_app_expr(tokens);
    loop {
        match tokens.peek() {
            Some(Token::Op(op)) if &op as &str == "*" || &op as &str == "/" => {
                tokens.next();
                let right_expr = parse_app_expr(tokens);
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

fn parse_app_expr(tokens: &mut Peekable<Iter<Token>>) -> Expr {
    let expr = parse_term(tokens);
    let mut args: Vec<Expr> = Vec::new();
    loop {
        match tokens.peek() {
            Some(Token::Op(_)) | Some(Token::RParen) | Some(Token::Then) | Some(Token::Else)
            | None => break,
            _ => args.push(parse_term(tokens)),
        }
    }
    if args.len() == 0 {
        expr
    } else {
        Expr::FnCall(Box::new(expr), args)
    }
}

fn parse_term(tokens: &mut Peekable<Iter<Token>>) -> Expr {
    let expr = match tokens.peek() {
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
        Some(Token::If) => parse_if(tokens),
        _ => panic!("Expected term"),
    };
    expr
}

fn parse_if(tokens: &mut Peekable<Iter<Token>>) -> Expr {
    expect_token(tokens, Token::If);
    let cond = parse_expr(tokens);
    expect_token(tokens, Token::Then);
    let then_expr = parse_expr(tokens);
    expect_token(tokens, Token::Else);
    let else_expr = parse_expr(tokens);
    Expr::If(Box::new(cond), Box::new(then_expr), Box::new(else_expr))
}

fn expect_token(tokens: &mut Peekable<Iter<Token>>, token: Token) {
    if tokens.next() != Some(&token) {
        panic!()
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
        if_then_else: (
            "if a then b else c",
            If(
                Box::new(Var("a".to_string())),
                Box::new(Var("b".to_string())),
                Box::new(Var("c".to_string()))
            )
        ),
        gt: (
            "1 > 2 + 2",
            BinOp(
                Token::Op(">".to_string()),
                Box::new(Number(1)),
                Box::new(
                    BinOp(
                        Token::Op("+".to_string()),
                        Box::new(Number(2)),
                        Box::new(Number(2))
                    )
                )
            )
        ),
        fn_call: (
            "add 2 3",
            FnCall(Box::new(Var("add".to_string())), vec![Number(2), Number(3)])
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
        fn_def: ("add a b = a + b;", [
            FnDef("add".to_string(), vec!["a".to_string(), "b".to_string()],
                BinOp(Token::Op("+".to_string()), Box::new( Var("a".to_string()) ), Box::new(Var("b".to_string()))))
        ]),
        fn_def_and_fn_call: ("add a b = a + b; add 2 3", [
            FnDef("add".to_string(), vec!["a".to_string(), "b".to_string()],
                BinOp(Token::Op("+".to_string()), Box::new( Var("a".to_string()) ), Box::new(Var("b".to_string())))),
            ExprStmt(FnCall(Box::new(Var("add".to_string())), vec![Number(2), Number(3)]))
        ]),
    }
}
