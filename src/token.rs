#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Number(i32),
    Plus,
}

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let chars = input.chars().collect::<Vec<char>>();

    let mut i: usize = 0;
    while i < chars.len() {
        match chars[i] {
            '+' => tokens.push(Token::Plus),
            ' ' => {
                i += 1;
                continue;
            }
            c => {
                if c.is_digit(10) {
                    // if c is a digit in base 10
                    let mut number_string: String = c.to_string();
                    i += 1; // consume c

                    while i < chars.len() && chars[i].is_digit(10) {
                        number_string.push(chars[i]);
                        i += 1;
                    }

                    let number: i32 = number_string.parse().expect("invalid number");
                    tokens.push(Token::Number(number));

                    // Since we `i += 1` at end of the last loop, we have to skip over
                    // the following i += 1 at the end of the current while loop.
                    continue;
                }
            }
        }
        i += 1;
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
