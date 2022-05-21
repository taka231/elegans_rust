#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Number(i32),
    Plus,
    Sub,
    Mul,
    Div,
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
    fn peak(&self, f: fn(char) -> bool) -> bool {
        !self.is_eof() && f(self.current_char())
    }
    fn consume_while(&mut self, strs: &str) {
        while !self.is_eof() && !self.consume(strs) {
            self.next()
        }
    }
}

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut lexer = Lexer::new(input);

    while !lexer.is_eof() {
        match lexer.current_char() {
            '+' => tokens.push(Token::Plus),
            '-' => tokens.push(Token::Sub),
            '*' => tokens.push(Token::Mul),
            '/' => {
                if lexer.consume("//") {
                    lexer.consume_while("\n");
                    continue;
                } else {
                    tokens.push(Token::Div)
                }
            }
            ' ' => {
                lexer.next();
                continue;
            }
            c => {
                if c.is_digit(10) {
                    // if c is a digit in base 10
                    let mut number_string: String = c.to_string();
                    lexer.next(); // consume c

                    while lexer.peak(|c| c.is_digit(10)) {
                        number_string.push(lexer.current_char());
                        lexer.next();
                    }

                    let number: i32 = number_string.parse().expect("invalid number");
                    tokens.push(Token::Number(number));

                    // Since we `i += 1` at end of the last loop, we have to skip over
                    // the following i += 1 at the end of the current while loop.
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
        num_plus_num: ("3+12", [Number(3), Plus, Number(12)]),
        space_consume: ("3 + 12", [Number(3), Plus, Number(12)]),
    }
}
