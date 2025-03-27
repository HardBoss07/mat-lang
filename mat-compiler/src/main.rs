use std::fs;
use std::io::{self, Write};

#[derive(Debug, Clone)]
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
    fn new(input: String) -> Self {
        Self { input, position: 0}
    }

    fn next_token(&mut self) -> Option<Token> {
        let chars: Vec<char> = self.input.chars().collect();

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

            if "{}();".contains(current_char) {
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

#[derive(Debug)]
enum ASTNode {
    MainFunction(Vec<ASTNode>),
    Print(Vec<PrintPart>),
}

#[derive(Debug)]
enum PrintPart {
    Literal(String),
    Variable(String),
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
            let token = self.tokens[self.position].clone();
            self.position += 1;
            Some(token)
        } else {
            None
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
                Token::Keyword(ref keyword) if keyword == "sout" => {
                    if let Some(Token::Symbol('(')) = self.next_token() {
                        let mut parts = Vec::new();

                        if let Some(Token::StringLiteral(string)) = self.next_token() {
                            let mut current_literal = String::new();
                            let mut in_interpolation = false;
                            let mut variable_name = String::new();

                            for c in string.chars() {
                                if c == '{' {
                                    if !current_literal.is_empty() {
                                        parts.push(PrintPart::Literal(current_literal.clone()));
                                        current_literal.clear();
                                    }
                                    in_interpolation = true;
                                } else if c == '}' {
                                    if !variable_name.is_empty() {
                                        parts.push(PrintPart::Variable(variable_name.clone()));
                                        variable_name.clear();
                                    }
                                    in_interpolation = false;
                                } else {
                                    if in_interpolation {
                                        variable_name.push(c);
                                    } else {
                                        current_literal.push(c);
                                    }
                                }
                            }

                            if !current_literal.is_empty() {
                                parts.push(PrintPart::Literal(current_literal));
                            }

                            if let Some(Token::Symbol(')')) = self.next_token() {
                                if let Some(Token::Symbol(';')) = self.next_token() {
                                    ast.push(ASTNode::Print(parts));
                                }
                            }
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
                ASTNode::MainFunction(body) => {
                    for inner_node in body {
                        match inner_node {
                            ASTNode::Print(parts) => {
                                let mut format_string = String::new();
                                let mut variables = Vec::new();
    
                                for part in parts {
                                    match part {
                                        PrintPart::Literal(lit) => format_string.push_str(&lit),
                                        PrintPart::Variable(var) => {
                                            format_string.push_str("{}");
                                            variables.push(var);
                                        }
                                    }
                                }

                                rust_code.push_str(&format!(
                                    "   println!(\"{}\", {});\n",
                                    format_string,
                                    variables.iter().map(|v| v.as_str()).collect::<Vec<&str>>().join(", ")
                                ))
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
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
    println!("Tokens: {:?}", tokens);

    let mut parser = Parser::new(tokens);
    let ast = parser.parse();
    println!("Abstract Tree: {:?}", ast);

    let generator = CodeGenerator::new(ast);
    let rust_code = generator.generate();

    fs::write("output.rs", rust_code).expect("Failed to write to output file");

    println!("Compilation successful! Generated Rust code saved to output.rs");
}