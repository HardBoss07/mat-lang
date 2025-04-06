use crate::enums::{ASTNode, Token, VariableType, PrintPart};
use std::collections::HashMap;

pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
    pub variables: HashMap<String, VariableType>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
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

    pub fn parse(&mut self) -> Vec<ASTNode> {
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