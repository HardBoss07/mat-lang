use std::fs;
use std::io::{self, Write};

enum Token {
    Keyword(String),
    StringLiteral(String),
    Symbol(char),
}

struct Lexer {
    input: String,
    position: usize,
}

impl Lexer {
    fn new(input: String) -> {
        Self { input, position: 0}
    }

    fn next_token(&mut self) -> Option<Token> {
        let chars: Vec<char> = self.input..chars().collect();

        while self.position < chars.len() {
            let current_char = chars[self.position];

            if current_char.is_whitespace() {
                self.position += 1;
                continue;
            }

            if current_char.is_alphabetic() {
                let start = self.position;
                while self.position < chars.len() && chars[self.position].is_alphanumeric() {
                    self.position += 1;
                }
                let word = &self.input[start..self.position];
                return Some(Token::Keyword(word.to_string()));
            }

            if current_char == '"' {
                self.position += 1;
                let start = self.position;
                while self.position < chars.len() && chars[self.position] != '"' {
                    self.position += 1;
                }
                let string_value = &self.input[start..self.position];
                self.position += 1;
                return Some(Token::StringLiteral(string_value.to_string()));
            }

            if "{}()".contains(current_char) {
                self.position += 1;
                return Some(Token::Symbol(current_char));
            }

            self.position += 1;
        }

        None
    }

    fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while let Some(token) = self.next_token() {
            tokens.push(token);
        }
        tokens
    }
}

enum ASTNode {
    MainFunction(Vec<ASTNode>),
    Print(String),
}

struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, position: 0 }
    }

    fn next_token(&mut self) -> Option<Token> {
        if self.position < self.tokens.len() {
            let token = self.token.len() {
                let token = self.tokens[self.position].clone();
                self.position += 1;
                Some(token)
            } else {
                None
            }
        }
    }

    fn parse(&mut self) -> Vec<ASTNode> {
        let mut ast = Vec::new();

        while let Some(token) = self.next_token() {
            match token {
                Token::Keyword(ref keyword) if keyword == "void" => {
                    if let Some(Token::Keyword(main_keyword)) = self.next_token() {
                        if main_keyword == "main" {
                            ast.push(ASTNode::MainFunction(self.parse_block()));
                        }
                    }
                }
                Token::Keyword(ref keyword) if keyword == "stdout" => {
                    if let Some(Token::Symbol('(')) = self.next_token() {
                        if let Some(Token::StringLiteral(text)) = self.next_token() {
                            ast.push(ASTNode::Print(text));
                        }
                    }
                }
                _ => {}
            }
        }

        ast
    }

    fn parse_block(&mut self) -> Vec<ASTNode> {
        let mut nodes = Vec::new();
        while let Some(token) = self.next_token() {
            match token {
                Token::Symbol('}') => break,
                _ => self.position -= 1,
            }
            nodes.append(&mut self.parse());
        }

        nodes
    }
}

struct CodeGenerator {
    ast: Vec<ASTNode>,
}

impl CodeGenerator {
    fn new(ast: Vec<ASTNode>) -> Self {
        Self { ast }
    }

    fn generate(&self) -> String {
        let mut rust_code = String::from("fn main() {\n");

        for node in &self.ast {
            match node {
                ASTNode::Print(value) => {
                    rust_code.push_str(&format!("   println!(\"{}\");\n", value));
                    _ => {}
                }
            }
        }

        rust_code.push_str("}\n");

        rust_code
    }
}

fn main() {
    let input = fs::read_to_string("hello.mat").expect("Failed to read file");
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();

    let mut parser = Parser::new(tokens);
    let ast = parser.parse();

    let generator = CodeGenerator::new(ast);
    let rust_code = generator.generate();

    fs::write("output.rs", rust_code).expect("Failed to write to output file");

    println!("Compilation successful! Generated Rust code saved to output.rs");
}