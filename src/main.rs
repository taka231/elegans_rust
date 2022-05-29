extern crate inkwell;

mod ast;
mod codegen;
mod parser;
mod token;

use std::io::{self, Write};

macro_rules! print_flush {
    ( $( $x:expr ),* ) => {
        print!( $($x, )* );

        std::io::stdout().flush().expect("Could not flush to standard output.");
    };
}

fn main() {
    loop {
        println!();
        print_flush!("?> ");

        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Could not read from standard input.");

        if &input == ":q\n" || &input == ":quit\n" {
            break;
        } else if input.chars().all(char::is_whitespace) {
            continue;
        }

        println!("{:?}", parser::parse(&token::tokenize(&input)))
    }
}
