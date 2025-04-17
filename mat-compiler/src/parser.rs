use crate::enums::{ASTNode, Token, PrimitiveVariable};
use crate::common;
use std::collections::HashMap;

pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
    pub variables: HashMap<String, PrimitiveVariable>,
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
                    if let Some(ast_node) = self.parse_if_statement() {
                        ast.push(ast_node);
                    }
                }
                Token::Keyword(ref keyword) if keyword == "else" => {
                    if let Some(ast_node) = self.parse_else_statement() {
                        ast.push(ast_node);
                    }
                }
                Token::Keyword(ref keyword) if ["int", "char", "float", "bool", "string"].contains(&keyword.as_str()) => {
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
                Token::Keyword(ref keyword) if ["int", "char", "float", "bool", "string"].contains(&keyword.as_str()) => {
                    let Some(nt) = self.next_token();
                    match nt {
                        Some(Token::Identifier(identifier)) => {
                            if let Some(ast_node) = self.parse_variable_declaration(keyword, identifier) {
                                nodes.push(ast_node);
                            }
                        }
                        Some(Token::Symbol('[')) => {
                            if let Some(ast_node) = self.parse_array_declaration(keyword) {
                                nodes.push(ast_node);
                            }
                        }
                    }
                }
                Token::Identifier(ref identifier) => {
                    if let Some(ast_node) = self.parse_identifier(identifier.to_string()) {
                        nodes.push(ast_node);
                    }
                }
                Token::Keyword(ref keyword) if keyword == "if" => {
                    if let Some(ast_node) = self.parse_if_statement() {
                        nodes.push(ast_node);
                    }
                }
                Token::Keyword(ref keyword) if keyword == "else" => {
                    if let Some(ast_node) = self.parse_else_statement() {
                        nodes.push(ast_node);
                    }
                }
                Token::Keyword(ref keyword) if keyword == "while" => {
                    if let Some(ast_node) = self.parse_while_loop() {
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

    fn parse_else_statement(&mut self) -> Option<ASTNode> {
        if let Some(Token::Symbol('{')) = self.next_token() {
            let body = self.parse_block();
            return Some(ASTNode::ElseStatement(body));
        }
        None
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

    fn parse_while_loop(&mut self) -> Option<ASTNode> {
        if let Some(Token::Condition(condition)) = self.next_token() {
            if let Some(Token::Symbol('{')) = self.next_token() {
                let body = self.parse_block();
                return Some(ASTNode::WhileLoop(condition, body));
            }
        }
        None
    }

    fn parse_array_declaration(&mut self, var_type: &str) -> Option<ASTNode> {
        if let Some(Token::Integer(length)) = self.next_token() {
            if let Some(Token::Symbol(']')) = self.next_token() {
                if let Some(Token::Identifier(var_name)) = self.next_token() {
                    if let Some(Token::Symbol('=')) = self.next_token() {
                        if let Some(Token::Symbol('{')) = self.next_token() {
                            let mut array = Vec::new();
                            for i in 0..length {
                                let value = match var_type {
                                    "int" => self.next_token().and_then(|t| if let Token::Integer(v) = t { Some(PrimitiveVariable::Integer(v)) } else { None }),
                                    "char" => self.next_token().and_then(|t| if let Token::Character(v) = t { Some(PrimitiveVariable::Character(v)) } else { None }),
                                    "float" => self.next_token().and_then(|t| if let Token::Float(v) = t { Some(PrimitiveVariable::Float(v)) } else { None }),
                                    "bool" => self.next_token().and_then(|t| if let Token::Bool(v) = t { Some(PrimitiveVariable::Bool(v)) } else { None }),
                                    "string" => self.next_token().and_then(|t| if let Token::StringLiteral(v) = t { Some(PrimitiveVariable::String(v)) } else { None }),
                                    _ => None,
                                };
                                if let Some(Token::Symbol(',')) {
                                    array.push(value);
                                } 
                            }

                            if let Some(Token::Symbol(';')) = self.next_token() {
                                return Some(ASTNode::ArrayDeclaration(ComplexVariable::Array(var_name, length, array)));
                            }
                        }
                    }
                }
            }
        }
        None
    }

    fn parse_variable_declaration(&mut self, var_type: &str, identifier: String) -> Option<ASTNode> {
            if let Some(Token::Symbol('=')) = self.next_token() {
            let value = match var_type {
                "int" => self.next_token().and_then(|t| if let Token::Integer(v) = t { Some(PrimitiveVariable::Integer(v)) } else { None }),
                "char" => self.next_token().and_then(|t| if let Token::Character(v) = t { Some(PrimitiveVariable::Character(v)) } else { None }),
                "float" => self.next_token().and_then(|t| if let Token::Float(v) = t { Some(PrimitiveVariable::Float(v)) } else { None }),
                "bool" => self.next_token().and_then(|t| if let Token::Bool(v) = t { Some(PrimitiveVariable::Bool(v)) } else { None }),
                "string" => self.next_token().and_then(|t| if let Token::StringLiteral(v) = t { Some(PrimitiveVariable::String(v)) } else { None }),
                _ => None,
            };

            if let Some(var_value) = value {
                if let Some(Token::Symbol(';')) = self.next_token() {
                    self.variables.insert(var_name.clone(), var_value.clone());
                    return Some(ASTNode::VariableDeclaration(identifier, var_value));
                }
            }
        }
        None
    }

    fn parse_identifier(&mut self, identifier: String) -> Option<ASTNode> {
        if let Some(next_token) = self.next_token() {
            if next_token == Token::Symbol('(') {
                if let Some(Token::Symbol(')')) = self.next_token() {
                    if let Some(Token::Symbol(';')) = self.next_token() {
                        return Some(ASTNode::FunctionCall(identifier));
                    }
                }
            } else {
                self.position -= 1;
            }
        }
        if let Some(next_token) = self.next_token() {
            match next_token {
                Token::Operator(operator) => {
                    if let Some(Token::Symbol('=')) = self.next_token() {
                        if let Some(value_token) = self.next_token() {
                            match value_token {
                                Token::Integer(v) => {
                                    if let Some(Token::Symbol(';')) = self.next_token() {
                                        return Some(ASTNode::Operation(operator, identifier, PrimitiveVariable::Integer(v)));
                                    }
                                }
                                Token::Float(v) => {
                                    if let Some(Token::Symbol(';')) = self.next_token() {
                                        return Some(ASTNode::Operation(operator, identifier, PrimitiveVariable::Float(v)));
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
                                    self.variables.insert(identifier.clone(), PrimitiveVariable::Integer(v));
                                    return Some(ASTNode::VariableChangeValue(identifier, PrimitiveVariable::Integer(v)));
                                }
                            }
                            Token::Character(v) => {
                                if let Some(Token::Symbol(';')) = self.next_token() {
                                    self.variables.insert(identifier.clone(), PrimitiveVariable::Character(v));
                                    return Some(ASTNode::VariableChangeValue(identifier, PrimitiveVariable::Character(v)));
                                }
                            }
                            Token::Float(v) => {
                                if let Some(Token::Symbol(';')) = self.next_token() {
                                    self.variables.insert(identifier.clone(), PrimitiveVariable::Float(v));
                                    return Some(ASTNode::VariableChangeValue(identifier, PrimitiveVariable::Float(v)));
                                }
                            }
                            Token::Bool(v) => {
                                if let Some(Token::Symbol(';')) = self.next_token() {
                                    self.variables.insert(identifier.clone(), PrimitiveVariable::Bool(v));
                                    return Some(ASTNode::VariableChangeValue(identifier, PrimitiveVariable::Bool(v)));
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
        return None;
    }

    fn parse_void(&mut self) -> Option<ASTNode> {
        if let Some(Token::Identifier(fn_name)) = self.next_token() {
            if let Some(Token::Symbol('(')) = self.next_token() {
                if let Some(Token::Symbol(')')) = self.next_token() {
                    if let Some(Token::Symbol('{')) = self.next_token() {
                        let body = self.parse_block();
                            return Some(ASTNode::Function(fn_name, body));
                    }
                }
            }
        }
        return None;
    }

    fn parse_sout(&mut self) -> Option<ASTNode> {
        if let Some(Token::Symbol('(')) = self.next_token() {
            if let Some(Token::StringLiteral(string)) = self.next_token() {
                if let Some(parts) = common::s_lit_to_s_parts(string) {
                    if let Some(Token::Symbol(')')) = self.next_token() {
                        if let Some(Token::Symbol(';')) = self.next_token() {
                            return Some(ASTNode::Print(parts));
                        }
                    }
                }
            }
        }
        return None;
    }
}
