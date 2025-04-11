use crate::enums::{ASTNode, PrintPart, VariableType};
use std::collections::HashMap;

pub struct CodeGenerator {
    ast: Vec<ASTNode>,
    variables: HashMap<String, VariableType>
}

impl CodeGenerator {
    pub fn new(ast: Vec<ASTNode>, variables: HashMap<String, VariableType>) -> Self {
        Self { ast, variables }
    }

    pub fn generate(&mut self) -> String {
        let mut rust_code = String::from("fn main() {\n");
    
        for node in self.ast.clone() {
            if let ASTNode::MainFunction(body) = node {
                for inner_node in body {
                    rust_code.push_str(&self.generate_node(&inner_node, 1));
                }
            }
        }

        rust_code.push_str("}\n");

        rust_code
    }

    fn generate_node(&mut self, node: &ASTNode, indent_level: usize) -> String {
        match node {
            ASTNode::VariableDeclaration(name, value) => {
                self.variables.insert(name.clone(), value.clone());
                self.generate_variable_declaration(name, value, indent_level)
            }
            ASTNode::VariableChangeValue(name, value) => {
                self.variables.insert(name.clone(), value.clone());
                self.generate_variable_change(name, value, indent_level)
            }
            ASTNode::Operation(operator, identifier, value) => {
                self.generate_operation(&operator.to_string(), identifier, value, indent_level)
            }
            ASTNode::Print(parts) => {
                self.generate_print(parts, indent_level)
            }
            ASTNode::IfStatement(condition, body_nodes) => {
                self.generate_if(condition, body_nodes, indent_level)
            }
            ASTNode::ElseStatement(body_nodes) => {
                self.generate_else(body_nodes, indent_level)
            }
            ASTNode::WhileLoop(condition, body_nodes) => {
                self.generate_while(condition, body_nodes, indent_level)
            }
            _ => String::new(),
        }
    }

    fn generate_variable_declaration(&self, name: &str, value: &VariableType, indent_level: usize) -> String {
        let indent = "   ".repeat(indent_level);
        match value {
            VariableType::Integer(v) => format!("{}let mut {} = {};\n", indent, name, v),
            VariableType::Character(v) => format!("{}let mut {} = '{}';\n", indent, name, v),
            VariableType::Float(v) => format!("{}let mut {} = {};\n", indent, name, v),
            VariableType::Bool(v) => format!("{}let mut {} = {};\n", indent, name, v),
        }
    }

    fn generate_variable_change(&self, name: &str, value: &VariableType, indent_level: usize) -> String {
        let indent = "   ".repeat(indent_level);
        match value {
            VariableType::Integer(v) => format!("{}{} = {};\n", indent, name, v),
            VariableType::Character(v) => format!("{}{} = '{}';\n", indent, name, v),
            VariableType::Float(v) => format!("{}{} = {};\n", indent, name, v),
            VariableType::Bool(v) => format!("{}{} = {};\n", indent, name, v),
        }
    }

    fn generate_operation(&self, operator: &str, identifier: &str, value: &VariableType, indent_level: usize) -> String {
        let indent = "   ".repeat(indent_level);
        match value {
            VariableType::Integer(v) => format!("{}{} {}= {};\n", indent, identifier, operator, v),
            VariableType::Character(v) => format!("{}{} {}= '{}';\n", indent, identifier, operator, v),
            VariableType::Float(v) => format!("{}{} {}= {};\n", indent, identifier, operator, v),
            VariableType::Bool(v) => format!("{}{} {}= {};\n", indent, identifier, operator, v),
        }
    }

    fn generate_print(&self, parts: &[PrintPart], indent_level: usize) -> String {
        let indent = "   ".repeat(indent_level);
        let mut format_string = String::new();
        let mut variables = Vec::new();

        for part in parts {
            match part {
                PrintPart::Literal(lit) => format_string.push_str(lit),
                PrintPart::Variable(var) => {
                    format_string.push_str("{}");
                    variables.push(var.as_str());
                }
            }
        }

        format!(
            "{}println!(\"{}\", {});\n",
            indent,
            format_string,
            variables.join(", ")
        )
    }

    fn generate_if(&mut self, condition: &str, _body_nodes: &Vec<ASTNode>, indent_level: usize) -> String {
        let indent = "   ".repeat(indent_level);
        let mut code = format!("{}if {} {{\n", indent, condition);

        for node in _body_nodes {
            code.push_str(&self.generate_node(&node, indent_level + 1));
        }

        code.push_str(&format!("{}}}\n", indent));
        
        code
    }

    fn generate_else(&mut self, _body_nodes: &Vec<ASTNode>, indent_level: usize) -> String {
        let indent = "   ".repeat(indent_level);
        let mut code = format!("{}else {{\n", indent);

        for node in _body_nodes {
            code.push_str(&self.generate_node(&node, indent_level + 1));
        }

        code.push_str(&format!("{}}}\n", indent));

        code
    }

    fn generate_while(&mut self, condition: &str, _body_nodes: &Vec<ASTNode>, indent_level: usize) -> String {
        let indent = "   ".repeat(indent_level);
        let mut code = format!("{}while {} {{\n", indent, condition);
        
        for node in _body_nodes {
            code.push_str(&self.generate_node(&node, indent_level + 1));
        }

        code.push_str(&format!("{}}}\n", indent));

        code
    }
}