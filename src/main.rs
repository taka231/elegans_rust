extern crate inkwell;

mod ast;
mod compile;
mod parser;
mod token;

use inkwell::context::Context;
use std::env;
use std::io::{self, Write};

macro_rules! print_flush {
    ( $( $x:expr ),* ) => {
        print!( $($x, )* );

        std::io::stdout().flush().expect("Could not flush to standard output.");
    };
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if &args[1] == "repl" {
        repl()
    } else {
        let ast = parser::parse(&token::tokenize(&args[1]));
        let context = Context::create();
        let builder = context.create_builder();
        let module = context.create_module("main");
        let compile = compile::Compile::new(&context, &builder, &module);
        compile.add_main(&ast);
        compile.print();
    }
}

fn repl() {
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
