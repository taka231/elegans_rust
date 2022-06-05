#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    Number(i32),
    Op(String),
    LParen,
    RParen,
    Ident(String),
    Semicolon,
    If,
    Then,
    Else,
}

struct Lexer {
    input: Vec<char>,
    current_pos: usize,
}

impl Lexer {
    fn new(input: &str) -> Lexer {
        Lexer {
            input: input.chars().collect::<Vec<char>>(),
            current_pos: 0,
        }
    }
    fn next(&mut self) {
        self.current_pos += 1
    }
    fn is_eof(&self) -> bool {
        self.current_pos >= self.input.len()
    }
    fn current_char(&self) -> char {
        self.input[self.current_pos]
    }
    fn consume(&mut self, strs: &str) -> bool {
        let strs_vec: Vec<char> = strs.chars().collect();
        let input_slice = &(self.input[self.current_pos..]);
        if input_slice.starts_with(&strs_vec) {
            self.current_pos += strs_vec.len();
            true
        } else {
            false
        }
    }
    fn currentchar_fulfill(&self, f: fn(char) -> bool) -> bool {
        !self.is_eof() && f(self.current_char())
    }
    fn consume_while(&mut self, strs: &str) {
        while !self.is_eof() && !self.consume(strs) {
            self.next()
        }
    }
    fn take_while(&mut self, f: fn(char) -> bool) -> String {
        let mut string: String = String::new();
        while self.currentchar_fulfill(f) {
            string.push(self.current_char());
            self.next();
        }
        string
    }
}

fn is_operator(c: char) -> bool {
    let operators = vec!['+', '-', '*', '/', '='];
    operators.contains(&c)
}

fn is_leading_char(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_ident_char(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_' || c.is_digit(10)
}

fn is_keyword(str: &str) -> Option<Token> {
    match str {
        "if" => Some(Token::If),
        "then" => Some(Token::Then),
        "else" => Some(Token::Else),
        _ => None,
    }
}

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut lexer = Lexer::new(input);

    while !lexer.is_eof() {
        match lexer.current_char() {
            ' ' | '\n' => {
                lexer.next();
                continue;
            }
            '(' => tokens.push(Token::LParen),
            ')' => tokens.push(Token::RParen),
            ';' => tokens.push(Token::Semicolon),
            c => {
                if c.is_digit(10) {
                    let number_string = lexer.take_while(|c| c.is_digit(10));

                    let number: i32 = number_string.parse().expect("invalid number");
                    tokens.push(Token::Number(number));

                    // Since we `i += 1` at end of the last loop, we have to skip over
                    // the following i += 1 at the end of the current while loop.
                    continue;
                } else if is_operator(c) {
                    if lexer.consume("//") {
                        lexer.consume_while("\n");
                        continue;
                    } else {
                        let operator_string = lexer.take_while(is_operator);
                        tokens.push(Token::Op(operator_string));
                        continue;
                    }
                } else if is_leading_char(c) {
                    let mut ident = lexer.take_while(is_ident_char);
                    if lexer.currentchar_fulfill(|c| c == '\'') {
                        ident.push_str("'");
                        lexer.next();
                    }
                    if let Some(token) = is_keyword(&ident) {
                        tokens.push(token)
                    } else {
                        tokens.push(Token::Ident(ident));
                    }
                    continue;
                }
            }
        }
        lexer.next();
    }

    tokens
}

#[cfg(test)]
mod tests {
    use super::Token::*;
    use super::*;
    macro_rules! tokenize_test {
        ($($describe:ident: $value:expr, )*) => {
            $(
                #[test]
                fn $describe() {
                    let (string, token_vec) = $value;
                    assert_eq!(tokenize(string), token_vec);
                }
            )*
        }
    }
    tokenize_test! {
        num_plus_num: ("3+12", [Number(3), Op("+".to_string()), Number(12)]),
        space_consume: ("3 +\n 12", [Number(3), Op("+".to_string()), Number(12)]),
        num_sub_num: ("3-12", [Number(3), Op("-".to_string()), Number(12)]),
        num_mul_num: ("3*12", [Number(3), Op("*".to_string()), Number(12)]),
        num_div_num: ("3/12", [Number(3), Op("/".to_string()), Number(12)]),
        line_comment: ("//3+5\n3", [Number(3)]),
        parens: ("()", [LParen, RParen]),
        ident: ("abc", [Ident("abc".to_string())]),
        ident_containing_underscore: ("_a_b_c_", [Ident("_a_b_c_".to_string())]),
        ident_containing_num: ("a03b", [Ident("a03b".to_string())]),
        ident_end_with_singlequote: ("abc'", [Ident("abc'".to_string())]),
        assign: ("a = b", [Ident("a".to_string()), Op("=".to_string()), Ident("b".to_string())]),
        if_then_else: ("if then else", [If, Then, Else]),
    }
}
