use crate::ast::*;
use crate::token::*;
use std::iter::Peekable;
use std::slice::Iter;

pub fn parse(tokens: &[Token]) -> Expr {
    parse_expr(&mut tokens.iter().peekable())
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
    match tokens.next() {
        Some(Token::Number(num)) => Expr::Number(*num),
        _ => panic!("Expected term"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    macro_rules! parse_test {
        ($($describe:ident: $value:expr, )*) => {
            $(
                #[test]
                fn $describe() {
                    let (string, expr) = $value;
                    assert_eq!(parse(&tokenize(string)), expr);
                }
            )*
        }
    }
    parse_test! {
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
    }
}
