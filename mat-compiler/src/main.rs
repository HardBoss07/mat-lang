use std::fs;
use std::collections::HashMap;

#[derive(Debug, Clone)]
enum Token {
    Keyword(String),
    StringLiteral(String),
    Symbol(char),
    Identifier(String),
    Integer(i32),
    Operator(char),
    Character(char),
    Float(f64),
    Bool(bool),
    Condition(String),
}

#[derive(Debug)]
enum ASTNode {
    MainFunction(Vec<ASTNode>),
    Print(Vec<PrintPart>),
    VariableDeclaration(String, VariableType),
    VariableChangeValue(String, VariableType),
    Operation(char, String, VariableType),
    IfStatement(String, Vec<ASTNode>),
}

#[derive(Debug, Clone)]
enum VariableType {
    Integer(i32),
    Character(char),
    Float(f64),
    Bool(bool),
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
        Self { input, position: 0 }
    }

    fn next_token(&mut self, prev_token: Option<Token>) -> Option<Token> {
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

                return Some(match word {
                    "void" | "int" | "float" | "char" | "sout" | "bool" | "if" => Token::Keyword(word.to_string()),
                    "tru" => Token::Bool(true),
                    "fal" => Token::Bool(false),
                    _ => Token::Identifier(word.to_string()),
                });
            }

            if current_char.is_digit(10) {
                let start = self.position;
                while self.position < chars.len() && chars[self.position].is_digit(10) {
                    self.position += 1;
                }

                if self.position < chars.len() && chars[self.position] == '.' {
                    self.position += 1;
                    if self.position < chars.len() && chars[self.position].is_digit(10) {
                        while self.position < chars.len() && chars[self.position].is_digit(10) {
                            self.position += 1;
                        }

                        let float_value = self.input[start..self.position].parse::<f64>().unwrap();
                        return Some(Token::Float(float_value));
                    }
                }

                let number = self.input[start..self.position].parse::<i32>().unwrap();
                return Some(Token::Integer(number));
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

            if "{}();=><".contains(current_char) {
                //TODO if prev_tonen = keyword::if parse codition instead of tokens
                if current_char == '(' {
                    match prev_token {
                        Some(Token::Keyword(ref keyword)) if keyword == "if" => {
                            self.position += 1;
                            let start = self.position;
                            while self.position < chars.len() && chars[self.position] != ')' {
                                self.position += 1;
                            }
                            let condition = &self.input[start..self.position];
                            self.position += 1;
                            return Some(Token::Condition(condition.to_string()));
                        },
                        _ => {}
                    }
                }
                
                self.position += 1;
                return Some(Token::Symbol(current_char));
            }

            self.position += 1;
        }

        None
    }

    fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while let Some(token) = self.next_token(tokens.last().cloned()) {
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
                    ast.push(self.parse_void().expect("NO VOID FOUND"));
                }
                Token::Keyword(ref keyword) if keyword == "if" => {

                }
                Token::Keyword(ref keyword) if ["int", "char", "float", "bool"].contains(&keyword.as_str()) => {
                    if let Some(ast_node) = self.parse_variable_declaration(keyword) {
                        ast.push(ast_node);
                    }
                }
                Token::Identifier(ref identifier) => {
                    if let Some(ast_node) = self.parse_identifier(identifier.to_string()) {
                        ast.push(ast_node);
                    }
                }             
                Token::Keyword(ref keyword) if keyword == "sout" => {
                    if let Some(ast_node) = self.parse_sout() {
                        ast.push(ast_node);
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
                Token::Keyword(ref keyword) if ["int", "char", "float", "bool"].contains(&keyword.as_str()) => {
                    if let Some(ast_node) = self.parse_variable_declaration(keyword) {
                        nodes.push(ast_node);
                    }
                }
                Token::Identifier(ref identifier) if self.variables.contains_key(identifier) => {
                    if let Some(ast_node) = self.parse_identifier(identifier.to_string()) {
                        nodes.push(ast_node);
                    }
                }
                Token::Keyword(ref keyword) if keyword == "if" => {
                    if let Some(ast_node) = self.parse_if_statement() {
                        nodes.push(ast_node);
                    }
                }
                Token::Keyword(ref keyword) if keyword == "sout" => {
                    if let Some(ast_node) = self.parse_sout() {
                        nodes.push(ast_node);
                    }
                }
                _ => {}
            }
        }
    
        nodes
    }

    fn parse_if_statement(&mut self) -> Option<ASTNode> {
        if let Some(Token::Condition(condition)) = self.next_token() {
            if let Some(Token::Symbol('{')) = self.next_token() {
                let body = self.parse_block();
                return Some(ASTNode::IfStatement(condition, body));
            }
        }
        None
    }

    fn parse_variable_declaration(&mut self, var_type: &str) -> Option<ASTNode> {
        if let Some(Token::Identifier(var_name)) = self.next_token() {
            if let Some(Token::Symbol('=')) = self.next_token() {
                let value = match var_type {
                    "int" => self.next_token().and_then(|t| if let Token::Integer(v) = t { Some(VariableType::Integer(v)) } else { None }),
                    "char" => self.next_token().and_then(|t| if let Token::Character(v) = t { Some(VariableType::Character(v)) } else { None }),
                    "float" => self.next_token().and_then(|t| if let Token::Float(v) = t { Some(VariableType::Float(v)) } else { None }),
                    "bool" => self.next_token().and_then(|t| if let Token::Bool(v) = t { Some(VariableType::Bool(v)) } else { None }),
                    _ => None,
                };

                if let Some(var_value) = value {
                    if let Some(Token::Symbol(';')) = self.next_token() {
                        self.variables.insert(var_name.clone(), var_value.clone());
                        return Some(ASTNode::VariableDeclaration(var_name, var_value));
                    }
                }
            }
        }
        None
    }

    fn parse_identifier(&mut self, identifier: String) -> Option<ASTNode> {
        if let Some(next_token) = self.next_token() {
            match next_token {
                Token::Operator(operator) => {
                    if let Some(Token::Symbol('=')) = self.next_token() {
                        if let Some(value_token) = self.next_token() {
                            match value_token {
                                Token::Integer(v) => {
                                    if let Some(Token::Symbol(';')) = self.next_token() {
                                        return Some(ASTNode::Operation(operator, identifier, VariableType::Integer(v)));
                                    }
                                }
                                Token::Float(v) => {
                                    if let Some(Token::Symbol(';')) = self.next_token() {
                                        return Some(ASTNode::Operation(operator, identifier, VariableType::Float(v)));
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
                Token::Symbol('=') => {
                    if let Some(value_token) = self.next_token() {
                        match value_token {
                            Token::Integer(v) => {
                                if let Some(Token::Symbol(';')) = self.next_token() {
                                    self.variables.insert(identifier.clone(), VariableType::Integer(v));
                                    return Some(ASTNode::VariableChangeValue(identifier, VariableType::Integer(v)));
                                }
                            }
                            Token::Character(v) => {
                                if let Some(Token::Symbol(';')) = self.next_token() {
                                    self.variables.insert(identifier.clone(), VariableType::Character(v));
                                    return Some(ASTNode::VariableChangeValue(identifier, VariableType::Character(v)));
                                }
                            }
                            Token::Float(v) => {
                                if let Some(Token::Symbol(';')) = self.next_token() {
                                    self.variables.insert(identifier.clone(), VariableType::Float(v));
                                    return Some(ASTNode::VariableChangeValue(identifier, VariableType::Float(v)));
                                }
                            }
                            Token::Bool(v) => {
                                if let Some(Token::Symbol(';')) = self.next_token() {
                                    self.variables.insert(identifier.clone(), VariableType::Bool(v));
                                    return Some(ASTNode::VariableChangeValue(identifier, VariableType::Bool(v)));
                                }
                            }
                            _ => {}
                        }
                    }
                }
                _ => {
                    self.position -= 1;
                }
            }
        }
        return None
    }

    fn parse_void(&mut self) -> Option<ASTNode> {
        if let Some(Token::Identifier(main_keyword)) = self.next_token() {
            if main_keyword == "main" {
                if let Some(Token::Symbol('(')) = self.next_token() {
                    if let Some(Token::Symbol(')')) = self.next_token() {
                        if let Some(Token::Symbol('{')) = self.next_token() {
                            let body = self.parse_block();
                            return Some(ASTNode::MainFunction(body));
                        }
                    }
                }
            }
        }
        return None;
    }

    fn parse_sout(&mut self) -> Option<ASTNode> {
        if let Some(Token::Symbol('(')) = self.next_token() {
            let mut parts = Vec::new();
    
            if let Some(Token::StringLiteral(string)) = self.next_token() {
                let mut current_literal = String::new();
                let mut in_interpolation = false;
                let mut variable_name = String::new();
    
                for c in string.chars() {
                    match c {
                        '{' => {
                            if in_interpolation {
                                return None;
                            }
                            if !current_literal.is_empty() {
                                parts.push(PrintPart::Literal(current_literal.clone()));
                                current_literal.clear();
                            }
                            in_interpolation = true;
                        }
                        '}' => {
                            if !in_interpolation {
                                return None;
                            }
                            if !variable_name.is_empty() {
                                parts.push(PrintPart::Variable(variable_name.clone()));
                                variable_name.clear();
                            }
                            in_interpolation = false;
                        }
                        _ => {
                            if in_interpolation {
                                variable_name.push(c);
                            } else {
                                current_literal.push(c);
                            }
                        }
                    }
                }
    
                if !current_literal.is_empty() {
                    parts.push(PrintPart::Literal(current_literal));
                }
    
                if let Some(Token::Symbol(')')) = self.next_token() {
                    if let Some(Token::Symbol(';')) = self.next_token() {
                        return Some(ASTNode::Print(parts));
                    }
                }
            }
        }
        return None;
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
                                    VariableType::Bool(b) => rust_code.push_str(&format!("   let mut {} = {};\n", name, b)),
                                }
                            }
                            ASTNode::VariableChangeValue(name, val) => {
                                self.variables.insert(name.clone(), val.clone());
                                match val {
                                    VariableType::Integer(v) => rust_code.push_str(&format!("   {} = {};\n", name, v)),
                                    VariableType::Character(c) => rust_code.push_str(&format!("   {} = '{}';\n", name, c)),
                                    VariableType::Float(f) => rust_code.push_str(&format!("   {} = {};\n", name, f)),
                                    VariableType::Bool(b) => rust_code.push_str(&format!("   {} = {};\n", name, b)),
                                }
                            }
                            ASTNode::Operation(operator, identifier, variable) => {
                                match variable {
                                    VariableType::Integer(v) => rust_code.push_str(&format!("   {} {}= {};\n", identifier, operator, v)),
                                    VariableType::Character(c) => rust_code.push_str(&format!("   {} {}= '{}';\n", identifier, operator, c)),
                                    VariableType::Float(f) => rust_code.push_str(&format!("   {} {}= {};\n", identifier, operator, f)),
                                    VariableType::Bool(b) => rust_code.push_str(&format!("   {} {}= {};\n", identifier, operator, b)),
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
    println!("Tokens: {:?}\n-------------------------------------------", tokens);

    let mut parser = Parser::new(tokens);
    let ast = parser.parse();
    println!("Abstract Tree: {:?}\n-------------------------------------------", ast);

    let mut generator = CodeGenerator::new(ast, parser.variables.clone());
    let rust_code = generator.generate();

    fs::write("output.rs", rust_code).expect("Failed to write to output file");

    println!("Compilation successful! Generated Rust code saved to output.rs");
}