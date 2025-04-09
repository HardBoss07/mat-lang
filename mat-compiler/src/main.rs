mod lexer;
mod parser;
mod code_generator;
mod enums;

use std::fs;
use lexer::Lexer;
use parser::Parser;
use code_generator::CodeGenerator;

fn main() {
    let input = fs::read_to_string("input.mat").expect("Failed to read file");

    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    println!("Tokens: {:?}\n-------------------------------------------", tokens);

    let mut parser = Parser::new(tokens);
    let ast = parser.parse();
    println!("Abstract Tree: {:?}\n-------------------------------------------", ast);

    let mut generator = CodeGenerator::new(ast, parser.variables.clone());
    let rust_code = generator.generate();

    fs::write("output.rs", rust_code).expect("Failed to write to output file");

    println!("Compilation successful! Generated Rust code saved to output.rs");
}