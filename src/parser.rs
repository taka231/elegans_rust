use crate::ast::*;
use crate::token::*;
use std::iter::Peekable;
use std::slice::Iter;

fn parse(tokens: &[Token]) -> Expr {
    parse_expr(&mut tokens.iter().peekable())
}

fn parse_expr(tokens: &mut Peekable<Iter<Token>>) -> Expr {
    parse_additive_expr(tokens)
}

fn parse_additive_expr(tokens: &mut Peekable<Iter<Token>>) -> Expr {
    let left_expr = parse_term(tokens);
    let operation: Token;
    match tokens.next() {
        Some(Token::Op(op)) => match &op as &str {
            "+" | "-" => operation = Token::Op(op.clone()),
            _op => panic!("unsapported operator"),
        },

        _ => panic!("Expected operation"),
    }
    let right_expr = parse_term(tokens);

    Expr::BinOp(operation, Box::new(left_expr), Box::new(right_expr))
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
        num_plus_num: ("10+12", Expr::BinOp(Token::Op("+".to_string()), Box::new( Expr::Number(10) ), Box::new( Expr::Number(12) ))),
        num_minus_num: ("10-12", Expr::BinOp(Token::Op("-".to_string()), Box::new( Expr::Number(10) ), Box::new( Expr::Number(12) ))),
    }
}
