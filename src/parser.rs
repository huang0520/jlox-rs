use std::iter::Peekable;

use crate::{expr::Expr, token::Token, token_type::TokenType};

pub struct Parser<I: Iterator<Item = Token>> {
    tokens: Peekable<I>,
    errors: Vec<ParseError>,
}

impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
            errors: Vec::new(),
        }
    }
    pub fn parse(&mut self) -> Result<Expr, Vec<ParseError>> {
        while self
            .tokens
            .peek()
            .is_some_and(|t| t.token_type != TokenType::Eof)
        {
            match self.expression() {
                Ok(expr) => {
                    if self.errors.is_empty() {
                        return Ok(*expr);
                    }
                }
                Err(e) => {
                    self.errors.push(e);
                    self.sync();
                }
            }
        }
        if self.errors.is_empty() {
            Err(vec![ParseError::EmptyFile])
        } else {
            Err(self.errors.clone())
        }
    }

    fn expression(&mut self) -> Result<Box<Expr>, ParseError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Box<Expr>, ParseError> {
        self.parse_binary_op(
            |p| p.comparison(),
            &[TokenType::BangEqual, TokenType::EqualEqual],
        )
    }

    fn comparison(&mut self) -> Result<Box<Expr>, ParseError> {
        self.parse_binary_op(
            |p| p.term(),
            &[
                TokenType::Greater,
                TokenType::GreaterEqual,
                TokenType::Less,
                TokenType::LessEqual,
            ],
        )
    }

    fn term(&mut self) -> Result<Box<Expr>, ParseError> {
        self.parse_binary_op(|p| p.factor(), &[TokenType::Minus, TokenType::Plus])
    }

    fn factor(&mut self) -> Result<Box<Expr>, ParseError> {
        self.parse_binary_op(|p| p.unary(), &[TokenType::Slash, TokenType::Star])
    }

    fn unary(&mut self) -> Result<Box<Expr>, ParseError> {
        if let Some(TokenType::Bang | TokenType::Minus) = self.tokens.peek().map(|t| t.token_type) {
            let operator = self.tokens.next().expect("operator exist").clone();
            let right = self.unary()?;
            Ok(Box::new(Expr::Unary { operator, right }))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Box<Expr>, ParseError> {
        let peek_token = self.tokens.peek();
        match peek_token.map(|t| t.token_type) {
            Some(
                TokenType::True
                | TokenType::False
                | TokenType::Nil
                | TokenType::Number
                | TokenType::String,
            ) => Ok(Box::new(Expr::Literal {
                value: self
                    .tokens
                    .next()
                    .expect("operator exist")
                    .literal
                    .expect("literal exist"),
            })),
            Some(TokenType::LeftParen) => {
                self.tokens.next();
                let expr = self.expression()?;
                match self.tokens.peek() {
                    Some(Token {
                        token_type: TokenType::RightParen,
                        ..
                    }) => {
                        self.tokens.next();
                        Ok(Box::new(Expr::Grouping { expression: expr }))
                    }
                    Some(t) => Err(ParseError::LackRightParan { token: t.clone() }),
                    None => unreachable!("shouldn't pass EOF token"),
                }
            }
            Some(_) => Err(ParseError::NotExpression {
                token: peek_token.expect("token exist").clone(),
            }),
            None => unreachable!("shouldn't pass EOF token"),
        }
    }

    fn sync(&mut self) {
        while let Some(t) = self.tokens.peek() {
            match t.token_type {
                TokenType::Semicolon => {
                    self.tokens.next();
                    break;
                }
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return
                | TokenType::Eof => {
                    break;
                }

                _ => {
                    self.tokens.next();
                }
            }
        }
    }

    // Helper function
    fn parse_binary_op<F>(
        &mut self,
        mut parse_operand: F,
        operators: &[TokenType],
    ) -> Result<Box<Expr>, ParseError>
    where
        F: FnMut(&mut Self) -> Result<Box<Expr>, ParseError>,
    {
        let mut expr = parse_operand(self)?;

        while let Some(token) = self.tokens.peek() {
            if !operators.contains(&token.token_type) {
                break;
            }

            let operator = self.tokens.next().expect("operator exist");
            let right = parse_operand(self)?;
            expr = Box::new(Expr::Binary {
                left: expr,
                operator,
                right,
            });
        }

        Ok(expr)
    }
}

#[derive(Debug, thiserror::Error, Clone, PartialEq)]
pub enum ParseError {
    #[error("expect ')' after expression")]
    LackRightParan { token: Token },
    #[error("expect expression")]
    NotExpression { token: Token },
    #[error("empty file")]
    EmptyFile,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expr::Expr;
    use crate::token::Token;
    use crate::token_type::Literal;
    use crate::token_type::TokenType;

    // Helper to create boolean literal tokens
    fn token_bool(value: bool, line: usize) -> Token {
        Token {
            token_type: if value {
                TokenType::True
            } else {
                TokenType::False
            },
            lexeme: if value { "true" } else { "false" }.to_string(),
            line,
            literal: Some(if value {
                Literal::Boolean(true)
            } else {
                Literal::Boolean(false)
            }),
        }
    }

    // Helper to create nil token
    fn token_nil(line: usize) -> Token {
        Token {
            token_type: TokenType::Nil,
            lexeme: "nil".to_string(),
            line,
            literal: Some(Literal::Nil),
        }
    }

    // Helper for EOF token
    fn token_eof(line: usize) -> Token {
        Token::new_eof(line)
    }

    // Helper to assert successful parsing
    fn assert_parse_success(tokens: Vec<Token>, expected: Expr) {
        let mut parser = Parser::new(tokens.into_iter());
        match parser.parse() {
            Ok(expr) => assert_eq!(expr, expected),
            Err(errors) => panic!("Expected parse to succeed but got errors: {:?}", errors),
        }
    }

    // Helper to assert parsing error
    fn assert_parse_error(tokens: Vec<Token>) -> Vec<ParseError> {
        let mut parser = Parser::new(tokens.into_iter());
        match parser.parse() {
            Ok(expr) => panic!("Expected parse to fail but got: {:?}", expr),
            Err(errors) => errors,
        }
    }

    mod literals {
        use super::*;

        #[test]
        fn number() {
            let tokens = vec![Token::new_number(42.0, "42", 1), token_eof(1)];
            let expected = Expr::Literal {
                value: Literal::Number(42.0),
            };
            assert_parse_success(tokens, expected);
        }

        #[test]
        fn string() {
            let tokens = vec![Token::new_string("hello", "\"hello\"", 1), token_eof(1)];
            let expected = Expr::Literal {
                value: Literal::String("hello".to_string()),
            };
            assert_parse_success(tokens, expected);
        }

        #[test]
        fn boolean_true() {
            let tokens = vec![token_bool(true, 1), token_eof(1)];
            let expected = Expr::Literal {
                value: Literal::Boolean(true),
            };
            assert_parse_success(tokens, expected);
        }

        #[test]
        fn boolean_false() {
            let tokens = vec![token_bool(false, 1), token_eof(1)];
            let expected = Expr::Literal {
                value: Literal::Boolean(false),
            };
            assert_parse_success(tokens, expected);
        }

        #[test]
        fn nil() {
            let tokens = vec![token_nil(1), token_eof(1)];
            let expected = Expr::Literal {
                value: Literal::Nil,
            };
            assert_parse_success(tokens, expected);
        }
    }

    mod unary_operators {
        use super::*;

        #[test]
        fn logical_not() {
            let bang = Token::new_simple(TokenType::Bang, "!", 1);
            let tokens = vec![bang.clone(), token_bool(false, 1), token_eof(1)];
            let expected = Expr::Unary {
                operator: bang,
                right: Box::new(Expr::Literal {
                    value: Literal::Boolean(false),
                }),
            };
            assert_parse_success(tokens, expected);
        }

        #[test]
        fn negation() {
            let minus = Token::new_simple(TokenType::Minus, "-", 1);
            let tokens = vec![
                minus.clone(),
                Token::new_number(42.0, "42", 1),
                token_eof(1),
            ];
            let expected = Expr::Unary {
                operator: minus,
                right: Box::new(Expr::Literal {
                    value: Literal::Number(42.0),
                }),
            };
            assert_parse_success(tokens, expected);
        }

        #[test]
        fn nested_unary() {
            let bang = Token::new_simple(TokenType::Bang, "!", 1);
            let minus = Token::new_simple(TokenType::Minus, "-", 1);
            let tokens = vec![
                bang.clone(),
                minus.clone(),
                Token::new_number(42.0, "42", 1),
                token_eof(1),
            ];
            let expected = Expr::Unary {
                operator: bang,
                right: Box::new(Expr::Unary {
                    operator: minus,
                    right: Box::new(Expr::Literal {
                        value: Literal::Number(42.0),
                    }),
                }),
            };
            assert_parse_success(tokens, expected);
        }
    }

    mod binary_operators {
        use super::*;

        #[test]
        fn addition() {
            let plus = Token::new_simple(TokenType::Plus, "+", 1);
            let tokens = vec![
                Token::new_number(1.0, "1", 1),
                plus.clone(),
                Token::new_number(2.0, "2", 1),
                token_eof(1),
            ];
            let expected = Expr::Binary {
                left: Box::new(Expr::Literal {
                    value: Literal::Number(1.0),
                }),
                operator: plus,
                right: Box::new(Expr::Literal {
                    value: Literal::Number(2.0),
                }),
            };
            assert_parse_success(tokens, expected);
        }

        #[test]
        fn subtraction() {
            let minus = Token::new_simple(TokenType::Minus, "-", 1);
            let tokens = vec![
                Token::new_number(5.0, "5", 1),
                minus.clone(),
                Token::new_number(3.0, "3", 1),
                token_eof(1),
            ];
            let expected = Expr::Binary {
                left: Box::new(Expr::Literal {
                    value: Literal::Number(5.0),
                }),
                operator: minus,
                right: Box::new(Expr::Literal {
                    value: Literal::Number(3.0),
                }),
            };
            assert_parse_success(tokens, expected);
        }

        #[test]
        fn multiplication() {
            let star = Token::new_simple(TokenType::Star, "*", 1);
            let tokens = vec![
                Token::new_number(4.0, "4", 1),
                star.clone(),
                Token::new_number(7.0, "7", 1),
                token_eof(1),
            ];
            let expected = Expr::Binary {
                left: Box::new(Expr::Literal {
                    value: Literal::Number(4.0),
                }),
                operator: star,
                right: Box::new(Expr::Literal {
                    value: Literal::Number(7.0),
                }),
            };
            assert_parse_success(tokens, expected);
        }

        #[test]
        fn division() {
            let slash = Token::new_simple(TokenType::Slash, "/", 1);
            let tokens = vec![
                Token::new_number(10.0, "10", 1),
                slash.clone(),
                Token::new_number(2.0, "2", 1),
                token_eof(1),
            ];
            let expected = Expr::Binary {
                left: Box::new(Expr::Literal {
                    value: Literal::Number(10.0),
                }),
                operator: slash,
                right: Box::new(Expr::Literal {
                    value: Literal::Number(2.0),
                }),
            };
            assert_parse_success(tokens, expected);
        }

        #[test]
        fn equality_equal() {
            // Use literals instead of identifiers: 1 == 2
            let eq_eq = Token::new_simple(TokenType::EqualEqual, "==", 1);
            let tokens = vec![
                Token::new_number(1.0, "1", 1),
                eq_eq.clone(),
                Token::new_number(2.0, "2", 1),
                token_eof(1),
            ];
            let expected = Expr::Binary {
                left: Box::new(Expr::Literal {
                    value: Literal::Number(1.0),
                }),
                operator: eq_eq,
                right: Box::new(Expr::Literal {
                    value: Literal::Number(2.0),
                }),
            };
            assert_parse_success(tokens, expected);
        }

        #[test]
        fn equality_not_equal() {
            // Use literals instead of identifiers: 1 != 2
            let bang_eq = Token::new_simple(TokenType::BangEqual, "!=", 1);
            let tokens = vec![
                Token::new_number(1.0, "1", 1),
                bang_eq.clone(),
                Token::new_number(2.0, "2", 1),
                token_eof(1),
            ];
            let expected = Expr::Binary {
                left: Box::new(Expr::Literal {
                    value: Literal::Number(1.0),
                }),
                operator: bang_eq,
                right: Box::new(Expr::Literal {
                    value: Literal::Number(2.0),
                }),
            };
            assert_parse_success(tokens, expected);
        }

        #[test]
        fn comparison_greater() {
            // Use literals instead of identifiers: 1 > 2
            let gt = Token::new_simple(TokenType::Greater, ">", 1);
            let tokens = vec![
                Token::new_number(1.0, "1", 1),
                gt.clone(),
                Token::new_number(2.0, "2", 1),
                token_eof(1),
            ];
            let expected = Expr::Binary {
                left: Box::new(Expr::Literal {
                    value: Literal::Number(1.0),
                }),
                operator: gt,
                right: Box::new(Expr::Literal {
                    value: Literal::Number(2.0),
                }),
            };
            assert_parse_success(tokens, expected);
        }

        #[test]
        fn comparison_greater_equal() {
            // Use literals instead of identifiers: 1 >= 2
            let gte = Token::new_simple(TokenType::GreaterEqual, ">=", 1);
            let tokens = vec![
                Token::new_number(1.0, "1", 1),
                gte.clone(),
                Token::new_number(2.0, "2", 1),
                token_eof(1),
            ];
            let expected = Expr::Binary {
                left: Box::new(Expr::Literal {
                    value: Literal::Number(1.0),
                }),
                operator: gte,
                right: Box::new(Expr::Literal {
                    value: Literal::Number(2.0),
                }),
            };
            assert_parse_success(tokens, expected);
        }
    }

    mod precedence {
        use super::*;

        #[test]
        fn multiplication_over_addition() {
            // 1 + 2 * 3 parses as 1 + (2 * 3)
            let tokens = vec![
                Token::new_number(1.0, "1", 1),
                Token::new_simple(TokenType::Plus, "+", 1),
                Token::new_number(2.0, "2", 1),
                Token::new_simple(TokenType::Star, "*", 1),
                Token::new_number(3.0, "3", 1),
                token_eof(1),
            ];
            let expected = Expr::Binary {
                left: Box::new(Expr::Literal {
                    value: Literal::Number(1.0),
                }),
                operator: Token::new_simple(TokenType::Plus, "+", 1),
                right: Box::new(Expr::Binary {
                    left: Box::new(Expr::Literal {
                        value: Literal::Number(2.0),
                    }),
                    operator: Token::new_simple(TokenType::Star, "*", 1),
                    right: Box::new(Expr::Literal {
                        value: Literal::Number(3.0),
                    }),
                }),
            };
            assert_parse_success(tokens, expected);
        }

        #[test]
        fn left_associative() {
            // 1 - 2 - 3 parses as (1 - 2) - 3
            let tokens = vec![
                Token::new_number(1.0, "1", 1),
                Token::new_simple(TokenType::Minus, "-", 1),
                Token::new_number(2.0, "2", 1),
                Token::new_simple(TokenType::Minus, "-", 1),
                Token::new_number(3.0, "3", 1),
                token_eof(1),
            ];
            let expected = Expr::Binary {
                left: Box::new(Expr::Binary {
                    left: Box::new(Expr::Literal {
                        value: Literal::Number(1.0),
                    }),
                    operator: Token::new_simple(TokenType::Minus, "-", 1),
                    right: Box::new(Expr::Literal {
                        value: Literal::Number(2.0),
                    }),
                }),
                operator: Token::new_simple(TokenType::Minus, "-", 1),
                right: Box::new(Expr::Literal {
                    value: Literal::Number(3.0),
                }),
            };
            assert_parse_success(tokens, expected);
        }
    }

    mod grouping {
        use super::*;

        #[test]
        fn simple_grouping() {
            let left_paren = Token::new_simple(TokenType::LeftParen, "(", 1);
            let right_paren = Token::new_simple(TokenType::RightParen, ")", 1);
            let tokens = vec![
                left_paren.clone(),
                token_bool(true, 1),
                right_paren.clone(),
                token_eof(1),
            ];
            let expected = Expr::Grouping {
                expression: Box::new(Expr::Literal {
                    value: Literal::Boolean(true),
                }),
            };
            assert_parse_success(tokens, expected);
        }

        #[test]
        fn grouping_changes_precedence() {
            // (1 + 2) * 3
            let tokens = vec![
                Token::new_simple(TokenType::LeftParen, "(", 1),
                Token::new_number(1.0, "1", 1),
                Token::new_simple(TokenType::Plus, "+", 1),
                Token::new_number(2.0, "2", 1),
                Token::new_simple(TokenType::RightParen, ")", 1),
                Token::new_simple(TokenType::Star, "*", 1),
                Token::new_number(3.0, "3", 1),
                token_eof(1),
            ];
            let expected = Expr::Binary {
                left: Box::new(Expr::Grouping {
                    expression: Box::new(Expr::Binary {
                        left: Box::new(Expr::Literal {
                            value: Literal::Number(1.0),
                        }),
                        operator: Token::new_simple(TokenType::Plus, "+", 1),
                        right: Box::new(Expr::Literal {
                            value: Literal::Number(2.0),
                        }),
                    }),
                }),
                operator: Token::new_simple(TokenType::Star, "*", 1),
                right: Box::new(Expr::Literal {
                    value: Literal::Number(3.0),
                }),
            };
            assert_parse_success(tokens, expected);
        }

        #[test]
        fn nested_grouping() {
            let tokens = vec![
                Token::new_simple(TokenType::LeftParen, "(", 1),
                Token::new_simple(TokenType::LeftParen, "(", 1),
                token_bool(true, 1),
                Token::new_simple(TokenType::RightParen, ")", 1),
                Token::new_simple(TokenType::RightParen, ")", 1),
                token_eof(1),
            ];
            let expected = Expr::Grouping {
                expression: Box::new(Expr::Grouping {
                    expression: Box::new(Expr::Literal {
                        value: Literal::Boolean(true),
                    }),
                }),
            };
            assert_parse_success(tokens, expected);
        }
    }

    mod errors {
        use super::*;

        #[test]
        fn missing_right_paren() {
            let tokens = vec![
                Token::new_simple(TokenType::LeftParen, "(", 1),
                token_bool(true, 1),
                token_eof(1),
            ];
            let errors = assert_parse_error(tokens);
            assert_eq!(errors.len(), 1);
            match &errors[0] {
                ParseError::LackRightParan { token } => {
                    assert_eq!(token.token_type, TokenType::Eof);
                }
                _ => panic!("Expected LackRightParan error"),
            }
        }

        #[test]
        fn unexpected_token() {
            let tokens = vec![
                Token::new_simple(TokenType::RightParen, ")", 1),
                token_eof(1),
            ];
            let errors = assert_parse_error(tokens);
            assert_eq!(errors.len(), 1);
            match &errors[0] {
                ParseError::NotExpression { token } => {
                    assert_eq!(token.token_type, TokenType::RightParen);
                }
                _ => panic!("Expected NotExpression error"),
            }
        }

        #[test]
        fn empty_file() {
            let tokens = vec![token_eof(1)];
            let errors = assert_parse_error(tokens);
            assert_eq!(errors.len(), 1);
            assert!(matches!(errors[0], ParseError::EmptyFile));
        }

        #[test]
        fn invalid_primary() {
            let tokens = vec![
                Token::new_simple(TokenType::Semicolon, ";", 1),
                token_eof(1),
            ];
            let errors = assert_parse_error(tokens);
            assert_eq!(errors.len(), 1);
            match &errors[0] {
                ParseError::NotExpression { token } => {
                    assert_eq!(token.token_type, TokenType::Semicolon);
                }
                _ => panic!("Expected NotExpression error"),
            }
        }
    }

    mod complex_expressions {
        use super::*;

        #[test]
        fn mixed_operators() {
            // !-(1 + 2)
            let tokens = vec![
                Token::new_simple(TokenType::Bang, "!", 1),
                Token::new_simple(TokenType::Minus, "-", 1),
                Token::new_simple(TokenType::LeftParen, "(", 1),
                Token::new_number(1.0, "1", 1),
                Token::new_simple(TokenType::Plus, "+", 1),
                Token::new_number(2.0, "2", 1),
                Token::new_simple(TokenType::RightParen, ")", 1),
                token_eof(1),
            ];
            let expected = Expr::Unary {
                operator: Token::new_simple(TokenType::Bang, "!", 1),
                right: Box::new(Expr::Unary {
                    operator: Token::new_simple(TokenType::Minus, "-", 1),
                    right: Box::new(Expr::Grouping {
                        expression: Box::new(Expr::Binary {
                            left: Box::new(Expr::Literal {
                                value: Literal::Number(1.0),
                            }),
                            operator: Token::new_simple(TokenType::Plus, "+", 1),
                            right: Box::new(Expr::Literal {
                                value: Literal::Number(2.0),
                            }),
                        }),
                    }),
                }),
            };
            assert_parse_success(tokens, expected);
        }

        #[test]
        fn complex_precedence() {
            // 1 + 2 * 3 - 4 / 5
            let tokens = vec![
                Token::new_number(1.0, "1", 1),
                Token::new_simple(TokenType::Plus, "+", 1),
                Token::new_number(2.0, "2", 1),
                Token::new_simple(TokenType::Star, "*", 1),
                Token::new_number(3.0, "3", 1),
                Token::new_simple(TokenType::Minus, "-", 1),
                Token::new_number(4.0, "4", 1),
                Token::new_simple(TokenType::Slash, "/", 1),
                Token::new_number(5.0, "5", 1),
                token_eof(1),
            ];
            // Should parse as: (1 + (2 * 3)) - (4 / 5)
            let mult = Expr::Binary {
                left: Box::new(Expr::Literal {
                    value: Literal::Number(2.0),
                }),
                operator: Token::new_simple(TokenType::Star, "*", 1),
                right: Box::new(Expr::Literal {
                    value: Literal::Number(3.0),
                }),
            };
            let add = Expr::Binary {
                left: Box::new(Expr::Literal {
                    value: Literal::Number(1.0),
                }),
                operator: Token::new_simple(TokenType::Plus, "+", 1),
                right: Box::new(mult),
            };
            let div = Expr::Binary {
                left: Box::new(Expr::Literal {
                    value: Literal::Number(4.0),
                }),
                operator: Token::new_simple(TokenType::Slash, "/", 1),
                right: Box::new(Expr::Literal {
                    value: Literal::Number(5.0),
                }),
            };
            let expected = Expr::Binary {
                left: Box::new(add),
                operator: Token::new_simple(TokenType::Minus, "-", 1),
                right: Box::new(div),
            };
            assert_parse_success(tokens, expected);
        }
    }
}
