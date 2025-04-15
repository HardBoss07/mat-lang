use crate::enums::{ASTNode, StringPart, VariableType};
use crate::common;
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
        let mut rust_code = String::new();
    
        for node in self.ast.clone() {
            rust_code.push_str(&self.generate_node(&node, 0));
        }

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
            ASTNode::Function(fn_name, body_nodes) => {
                self.generate_function(fn_name, body_nodes, indent_level)
            }
            ASTNode::FunctionCall(fn_name) => {
                self.generate_function_call(fn_name, indent_level)
            }
        }
    }

    fn generate_function_call(&self, fn_name: &str, indent_level: usize) -> String {
        let indent = "   ".repeat(indent_level);
        format!("{}{}();\n", indent, fn_name)
    }

    fn generate_variable_declaration(&self, name: &str, value: &VariableType, indent_level: usize) -> String {
        let indent = "   ".repeat(indent_level);
        match value {
            VariableType::Integer(v) => format!("{}let mut {} = {};\n", indent, name, v),
            VariableType::Character(v) => format!("{}let mut {} = '{}';\n", indent, name, v),
            VariableType::Float(v) => format!("{}let mut {} = {};\n", indent, name, v),
            VariableType::Bool(v) => format!("{}let mut {} = {};\n", indent, name, v),
            VariableType::String(v) => format!("{}let mut {} = format!({});\n", indent, name, common::s_parts_to_string(common::s_lit_to_s_parts(v.to_string()).expect("ERROR WITH COMMON.RS"))),
        }
    }

    fn generate_variable_change(&self, name: &str, value: &VariableType, indent_level: usize) -> String {
        let indent = "   ".repeat(indent_level);
        match value {
            VariableType::Integer(v) => format!("{}{} = {};\n", indent, name, v),
            VariableType::Character(v) => format!("{}{} = '{}';\n", indent, name, v),
            VariableType::Float(v) => format!("{}{} = {};\n", indent, name, v),
            VariableType::Bool(v) => format!("{}{} = {};\n", indent, name, v),
            VariableType::String(v) => format!("{}{} = format!({});\n",  indent, name, common::s_parts_to_string(common::s_lit_to_s_parts(v.to_string()).expect("ERROR WITH COMMON.RS"))),
        }
    }

    fn generate_operation(&self, operator: &str, identifier: &str, value: &VariableType, indent_level: usize) -> String {
        let indent = "   ".repeat(indent_level);
        match value {
            VariableType::Integer(v) => format!("{}{} {}= {};\n", indent, identifier, operator, v),
            VariableType::Character(v) => format!("{}{} {}= '{}';\n", indent, identifier, operator, v),
            VariableType::Float(v) => format!("{}{} {}= {};\n", indent, identifier, operator, v),
            VariableType::Bool(v) => format!("{}{} {}= {};\n", indent, identifier, operator, v),
            _ => String::new()
        }
    }

    fn generate_print(&self, parts: &[StringPart], indent_level: usize) -> String {
        let indent = "   ".repeat(indent_level);
        let content = common::s_parts_to_string(parts.to_vec());

        format!(
            "{}println!({});\n",
            indent,
            content,
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

    fn generate_function(&mut self, fn_name: &str, _body_nodes: &Vec<ASTNode>, indent_level: usize) -> String {
        let indent = "   ".repeat(indent_level);
        let mut code = format!("{}fn {}() {{\n", indent, fn_name);

        for node in _body_nodes {
            code.push_str(&self.generate_node(node, indent_level + 1));
        }

        code.push_str(&format!("{}}}\n", indent));

        code
    }
}