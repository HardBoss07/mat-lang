use crate::enums::Token;

pub struct Lexer {
    input: String,
    position: usize,
}

impl Lexer {
    pub fn new(input: String) -> Self {
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

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while let Some(token) = self.next_token(tokens.last().cloned()) {
            tokens.push(token);
        }
        tokens
    }
}