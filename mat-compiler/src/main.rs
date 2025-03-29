use std::fs;
use std::io::{self, Write};
use std::collections::HashMap;

#[derive(Debug, Clone)]
enum Token {
    Keyword(String),
    StringLiteral(String),
    Symbol(char),
    Identifier(String),
    Number(i32),
    Operator(char),
    Character(char),
    Float(f32),
}

#[derive(Debug)]
enum ASTNode {
    MainFunction(Vec<ASTNode>),
    Print(Vec<PrintPart>),
    VariableDeclaration(String, VariableType),
    VariableChangeValue(String, VariableType),
    Operation(char, String, VariableType),
}

#[derive(Debug, Clone)]
enum VariableType {
    Integer(i32),
    Character(char),
    Float(f32),
}

#[derive(Debug)]
enum PrintPart {
    Literal(String),
    Variable(String),
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

                return Some(if ["void", "int", "float", "char", "sout"].contains(&word) {
                    Token::Keyword(word.to_string())
                } else {
                    Token::Identifier(word.to_string())
                });
            }

            if current_char.is_digit(10) {
                let start = self.position;
                while self.position < chars.len() && chars[self.position].is_digit(10) {
                    self.position += 1;
                }
                let number = self.input[start..self.position].parse::<i32>().unwrap();
                return Some(Token::Number(number));
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

            if current_char == '\'' {
                self.position += 1;
                if self.position < chars.len() && chars[self.position] != '\'' {
                    let char_literal = chars[self.position];
                    self.position += 1;
                    if self.position < chars.len() && chars[self.position] == '\'' {
                        self.position += 1;
                        return Some(Token::Character(char_literal));
                    }
                }
            }

            if "+-*/".contains(current_char) {
                self.position += 1;
                return Some(Token::Operator(current_char));
            }

            if "{}();=".contains(current_char) {
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

struct Parser {
    tokens: Vec<Token>,
    position: usize,
    variables: HashMap<String, VariableType>,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { 
            tokens,
            position: 0,
            variables: HashMap::new()
        }
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
                    if let Some(Token::Identifier(main_keyword)) = self.next_token() {
                        if main_keyword == "main" {
                            if let Some(Token::Symbol('(')) = self.next_token() {
                                if let Some(Token::Symbol(')')) = self.next_token() {
                                    if let Some(Token::Symbol('{')) = self.next_token() {
                                        let body = self.parse_block();
                                        ast.push(ASTNode::MainFunction(body));
                                    }
                                }
                            }
                        }
                    }
                }
                Token::Keyword(ref keyword) if keyword == "int" => {
                    if let Some(Token::Identifier(var_name)) = self.next_token() {
                        if let Some(Token::Symbol('=')) = self.next_token() {
                            if let Some(Token::Number(value)) = self.next_token() {
                                if let Some(Token::Symbol(';')) = self.next_token() {
                                    self.variables.insert(var_name.clone(), VariableType::Integer(value));
                                    ast.push(ASTNode::VariableDeclaration(var_name, VariableType::Integer(value)));
                                }
                            }
                        }
                    }
                }
                Token::Keyword(ref keyword) if keyword == "char" => {
                    if let Some(Token::Identifier(var_name)) = self.next_token() {
                        if let Some(Token::Symbol('=')) = self.next_token() {
                            if let Some(Token::Character(value)) = self.next_token() {
                                if let Some(Token::Symbol(';')) = self.next_token() {
                                    self.variables.insert(var_name.clone(), VariableType::Character(value));
                                    ast.push(ASTNode::VariableDeclaration(var_name, VariableType::Character(value)));
                                }
                            }
                        }
                    }
                }
                Token::Keyword(ref keyword) if keyword == "float" => {
                    if let Some(Token::Identifier(var_name)) = self.next_token() {
                        if let Some(Token::Symbol('=')) = self.next_token() {
                            if let Some(Token::Number(value_before)) = self.next_token() {
                                if let Some(Token::Number(value_after)) = self.next_token() {
                                    if let Some(Token::Symbol(';')) = self.next_token() {
                                        let decimal_value = 10f32.powf(value_after.ilog(10) as f32 + 1.0);
                                        let float_value = value_before as f32 + (value_after as f32 / decimal_value);
                                        self.variables.insert(var_name.clone(), VariableType::Float(float_value));
                                        ast.push(ASTNode::VariableDeclaration(var_name, VariableType::Float(float_value)));
                                    }
                                }
                            }
                        }
                    }
                }
                Token::Identifier(ref identifier) => {
                    if let Some(Token::Symbol('=')) = self.next_token() {
                        if let Some(Token::Number(value)) = self.next_token() {
                            self.variables.insert(identifier.clone(), VariableType::Integer(value));
                            ast.push(ASTNode::VariableChangeValue(identifier.to_string(), VariableType::Integer(value)));
                        }
                    } 
                    if let Some(Token::Operator(operator)) = self.next_token() {
                        if let Some(Token::Symbol('=')) = self.next_token() {
                            if let Some(Token::Number(value)) = self.next_token() {
                                if let Some(next_token) = self.next_token() {
                                    match next_token {
                                        Token::Symbol(';') => ast.push(ASTNode::Operation(operator, identifier.to_string(), VariableType::Integer(value))),
                                        Token::Number(value_after) => {
                                            if let Some(Token::Symbol(';')) = self.next_token() {
                                                let decimal_value = 10f32.powf(value_after.ilog(10) as f32 + 1.0);
                                                let float_value = value as f32 + (value_after as f32 / decimal_value);
                                                ast.push(ASTNode::Operation(operator, identifier.to_string(), VariableType::Float(float_value)));
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
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
                Token::Keyword(ref keyword) if keyword == "int" => {
                    if let Some(Token::Identifier(var_name)) = self.next_token() {
                        if let Some(Token::Symbol('=')) = self.next_token() {
                            if let Some(Token::Number(value)) = self.next_token() {
                                if let Some(Token::Symbol(';')) = self.next_token() {
                                    self.variables.insert(var_name.clone(), VariableType::Integer(value));
                                    nodes.push(ASTNode::VariableDeclaration(var_name, VariableType::Integer(value)));
                                }
                            }
                        }
                    }
                }
                Token::Keyword(ref keyword) if keyword == "char" => {
                    if let Some(Token::Identifier(var_name)) = self.next_token() {
                        if let Some(Token::Symbol('=')) = self.next_token() {
                            if let Some(Token::Character(value)) = self.next_token() {
                                if let Some(Token::Symbol(';')) = self.next_token() {
                                    self.variables.insert(var_name.clone(), VariableType::Character(value));
                                    nodes.push(ASTNode::VariableDeclaration(var_name, VariableType::Character(value)));
                                }
                            }
                        }
                    }
                }
                Token::Keyword(ref keyword) if keyword == "float" => {
                    if let Some(Token::Identifier(var_name)) = self.next_token() {
                        if let Some(Token::Symbol('=')) = self.next_token() {
                            if let Some(Token::Number(value_before)) = self.next_token() {
                                if let Some(Token::Number(value_after)) = self.next_token() {
                                    if let Some(Token::Symbol(';')) = self.next_token() {
                                        let decimal_value = 10f32.powf(value_after.ilog(10) as f32 + 1.0);
                                        let float_value = value_before as f32 + (value_after as f32 / decimal_value);
                                        self.variables.insert(var_name.clone(), VariableType::Float(float_value));
                                        nodes.push(ASTNode::VariableDeclaration(var_name, VariableType::Float(float_value)));
                                    }
                                }
                            }
                        }
                    }
                }
                Token::Identifier(ref identifier) if self.variables.contains_key(identifier) => {
                    if let Some(next_token) = self.next_token() {
                        match next_token {
                            Token::Operator(operator) => {
                                if let Some(Token::Symbol('=')) = self.next_token() {
                                    if let Some(Token::Number(value)) = self.next_token() {
                                        if let Some(next_token) = self.next_token() {
                                            match next_token {
                                                Token::Symbol(';') => nodes.push(ASTNode::Operation(operator, identifier.to_string(), VariableType::Integer(value))),
                                                Token::Number(value_after) => {
                                                    if let Some(Token::Symbol(';')) = self.next_token() {
                                                        let decimal_value = 10f32.powf(value_after.ilog(10) as f32 + 1.0);
                                                        let float_value = value as f32 + (value_after as f32 / decimal_value);
                                                        nodes.push(ASTNode::Operation(operator, identifier.to_string(), VariableType::Float(float_value)));
                                                    }
                                                }
                                                _ => {}
                                            }
                                        }
                                    }
                                }
                            }
                            Token::Symbol('=') => {
                                if let Some(Token::Number(value)) = self.next_token() {
                                    if let Some(Token::Symbol(';')) = self.next_token() {
                                        self.variables.insert(identifier.clone(), VariableType::Integer(value));
                                        nodes.push(ASTNode::VariableChangeValue(identifier.to_string(), VariableType::Integer(value)));
                                    }
                                }
                            }
                            _ => {
                                self.position -= 1;
                            }
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
                                    nodes.push(ASTNode::Print(parts));
                                }
                            }
                        }
                    }
                }
                _ => {}
            }
        }
    
        nodes
    }    
}

struct CodeGenerator {
    ast: Vec<ASTNode>,
    variables: HashMap<String, VariableType>
}

impl CodeGenerator {
    fn new(ast: Vec<ASTNode>, variables: HashMap<String, VariableType>) -> Self {
        Self { ast, variables }
    }

    fn generate(&mut self) -> String {
        let mut rust_code = String::from("fn main() {\n");
    
        for node in &self.ast {
            match node {
                ASTNode::MainFunction(body) => {
                    for inner_node in body {
                        match inner_node {
                            ASTNode::VariableDeclaration(name, val) => {
                                self.variables.insert(name.clone(), val.clone());
                                match val {
                                    VariableType::Integer(v) => rust_code.push_str(&format!("   let mut {} = {};\n", name, v)),
                                    VariableType::Character(c) => rust_code.push_str(&format!("   let mut {} = '{}';\n", name, c)),
                                    VariableType::Float(f) => rust_code.push_str(&format!("   let mut {} = {};\n", name, f)),
                                }
                            }
                            ASTNode::VariableChangeValue(name, val) => {
                                self.variables.insert(name.clone(), val.clone());
                                match val {
                                    VariableType::Integer(v) => rust_code.push_str(&format!("   {} = {};\n", name, v)),
                                    VariableType::Character(c) => rust_code.push_str(&format!("   {} = '{}';\n", name, c)),
                                    VariableType::Float(f) => rust_code.push_str(&format!("   {} = {};\n", name, f)),
                                }
                            }
                            ASTNode::Operation(operator, identifier, variable) => {
                                match variable {
                                    VariableType::Integer(v) => rust_code.push_str(&format!("   {} {}= {};\n", identifier, operator, v)),
                                    VariableType::Character(c) => rust_code.push_str(&format!("   {} {}= '{}';\n", identifier, operator, c)),
                                    VariableType::Float(f) => rust_code.push_str(&format!("   {} {}= {};\n", identifier, operator, f)),
                                }
                            }                            
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

    let mut generator = CodeGenerator::new(ast, parser.variables.clone());
    let rust_code = generator.generate();

    fs::write("output.rs", rust_code).expect("Failed to write to output file");

    println!("Compilation successful! Generated Rust code saved to output.rs");
}