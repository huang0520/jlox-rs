use crate::token::Token;
use crate::token_type::{Literal, TokenType};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum State {
    Start,
    String,
    Integer,
    Decimal,
    Identifier,
    CommentLine,
    CommentBlock,
}

pub enum Transition {
    Advance, // Increase Current but keep Start
    Skip,    // Increase Current and Start
    Emit(Token),
    Error(ScanError),
}

#[derive(Debug)]
pub struct Scanner {
    source: Vec<char>,
    state: State,
    line: usize,
    start: usize,
    current: usize,
}

impl Scanner {
    pub fn new(source: &str) -> Self {
        Scanner {
            source: source.chars().collect(),
            state: State::Start,
            line: 1,
            start: 0,
            current: 0,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>, Vec<ScanError>> {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        while self.current < self.source.len() {
            let transition = self.scan_token();

            match transition {
                Transition::Emit(token) => {
                    tokens.push(token);
                    self.start = self.current;
                    self.state = State::Start;
                }
                Transition::Advance => {}
                Transition::Skip => {
                    self.start = self.current;
                    self.state = State::Start;
                }
                Transition::Error(e) => {
                    errors.push(e);
                    self.start = self.current;
                    self.state = State::Start;
                }
            };
        }

        tokens.push(Token::new_eof(self.line));

        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(tokens)
        }
    }

    fn scan_token(&mut self) -> Transition {
        let c = self.advance();
        match self.state {
            State::Start => self.start(c),
            State::String => self.string(),
            State::Integer => self.integer(),
            State::Decimal => self.decimal(),
            State::Identifier => self.identifier(),
            State::CommentLine => self.comment_line(),
            State::CommentBlock => self.comment_block(c),
        }
    }

    fn start(&mut self, c: char) -> Transition {
        match c {
            // Single-char token
            '(' | ')' | '{' | '}' | ',' | '.' | '-' | '+' | ';' | '*' => Transition::Emit(
                Token::new_simple(self.check_single_lexeme(&c), c, self.line),
            ),

            // Double-char token
            '!' => self.emit_if_next('=', TokenType::BangEqual, TokenType::Bang),
            '=' => self.emit_if_next('=', TokenType::EqualEqual, TokenType::Equal),
            '>' => self.emit_if_next('=', TokenType::GreaterEqual, TokenType::Greater),
            '<' => self.emit_if_next('=', TokenType::LessEqual, TokenType::Less),
            '/' => {
                if self.peek() == Some('/') {
                    self.state = State::CommentLine;
                    Transition::Advance
                } else if self.peek() == Some('*') {
                    self.state = State::CommentBlock;
                    self.advance();
                    Transition::Advance
                } else {
                    Transition::Emit(Token::new_simple(TokenType::Slash, '/', self.line))
                }
            }

            // Whitespace
            ' ' | '\r' | '\t' => Transition::Skip,
            '\n' => {
                self.line += 1;
                Transition::Skip
            }

            // State transitions
            '"' => {
                self.state = State::String;
                Transition::Advance
            }
            '0'..='9' => match self.peek() {
                Some('0'..='9') => {
                    self.state = State::Integer;
                    Transition::Advance
                }
                Some('.') => {
                    self.state = State::Decimal;
                    Transition::Advance
                }
                _ => Transition::Emit(self.make_number_token()),
            },
            'a'..='z' | 'A'..='Z' | '_' => {
                if self
                    .peek()
                    .is_some_and(|c| c.is_ascii_alphanumeric() || c == '_')
                {
                    self.state = State::Identifier;
                    Transition::Advance
                } else {
                    Transition::Emit(self.make_keyword_token())
                }
            }

            _ => Transition::Error(ScanError::UnexpectedChar {
                found: c,
                line: self.line,
            }),
        }
    }

    fn comment_line(&mut self) -> Transition {
        let next_c = self.peek();
        if next_c == Some('\n') || next_c.is_none() {
            Transition::Skip
        } else {
            Transition::Advance
        }
    }

    fn comment_block(&mut self, c: char) -> Transition {
        if c == '*' && self.peek().is_some_and(|c| c == '/') {
            self.advance();
            return Transition::Skip;
        }

        match self.peek() {
            Some('\n') => {
                self.line += 1;
                Transition::Advance
            }
            Some(_) => Transition::Advance,
            None => Transition::Error(ScanError::UnterminatedString { line: self.line }),
        }
    }

    fn string(&mut self) -> Transition {
        match self.peek() {
            Some('"') => {
                self.advance();
                Transition::Emit(self.make_string_token())
            }
            Some('\n') => {
                self.line += 1;
                Transition::Advance
            }
            Some(_) => Transition::Advance,
            None => Transition::Error(ScanError::UnterminatedString { line: self.line }),
        }
    }

    fn integer(&mut self) -> Transition {
        match self.peek() {
            Some('0'..='9') => Transition::Advance,
            Some('.') => match self.peek_next() {
                Some('0'..='9') => {
                    self.state = State::Decimal;
                    Transition::Advance
                }
                _ => Transition::Emit(self.make_number_token()),
            },
            _ => Transition::Emit(self.make_number_token()),
        }
    }

    fn decimal(&self) -> Transition {
        match self.peek() {
            Some('0'..='9') => Transition::Advance,
            _ => Transition::Emit(self.make_number_token()),
        }
    }

    fn identifier(&mut self) -> Transition {
        if self
            .peek()
            .is_some_and(|c| c.is_ascii_alphanumeric() || c == '_')
        {
            Transition::Advance
        } else {
            Transition::Emit(self.make_keyword_token())
        }
    }

    // helper function
    fn advance(&mut self) -> char {
        let c = self.source[self.current];
        self.current += 1;
        c
    }
    fn peek(&self) -> Option<char> {
        self.source.get(self.current).copied()
    }

    fn peek_next(&self) -> Option<char> {
        self.source.get(self.current + 1).copied()
    }

    fn emit_if_next(
        &mut self,
        expected: char,
        then_type: TokenType,
        else_type: TokenType,
    ) -> Transition {
        let token_type = if self.peek() == Some(expected) {
            self.advance();
            then_type
        } else {
            else_type
        };
        Transition::Emit(Token::new_simple(
            token_type,
            self.current_lexeme(),
            self.line,
        ))
    }

    fn current_lexeme(&self) -> String {
        self.source[self.start..self.current].iter().collect()
    }

    fn make_string_token(&self) -> Token {
        let lexeme = self.current_lexeme();
        let value = &lexeme.clone()[1..lexeme.len() - 1];
        Token::new_string(value, lexeme, self.line)
    }

    fn make_number_token(&self) -> Token {
        let lexeme = self.current_lexeme();
        let value: f64 = lexeme.parse().expect("Should be able to parse");
        Token::new_number(value, lexeme, self.line)
    }

    fn make_keyword_token(&self) -> Token {
        let lexeme = self.current_lexeme();
        let token_type = self.check_keyword(&lexeme);
        match token_type {
            TokenType::True => Token::new(token_type, lexeme, self.line, Some(Literal::True)),
            TokenType::False => Token::new(token_type, lexeme, self.line, Some(Literal::False)),
            TokenType::Nil => Token::new(token_type, lexeme, self.line, Some(Literal::Nil)),
            _ => Token::new_simple(token_type, lexeme, self.line),
        }
    }

    fn check_single_lexeme(&self, lexeme: &char) -> TokenType {
        match lexeme {
            '(' => TokenType::LeftParen,
            ')' => TokenType::RightParen,
            '{' => TokenType::LeftBrace,
            '}' => TokenType::RightBrace,
            ',' => TokenType::Comma,
            '.' => TokenType::Dot,
            '-' => TokenType::Minus,
            '+' => TokenType::Plus,
            ';' => TokenType::Semicolon,
            '*' => TokenType::Star,
            _ => unreachable!("Invalid single-char lexeme: {}", lexeme),
        }
    }

    fn check_keyword(&self, lexeme: &str) -> TokenType {
        match lexeme {
            "and" => TokenType::And,
            "class" => TokenType::Class,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "fun" => TokenType::Fun,
            "if" => TokenType::If,
            "nil" => TokenType::Nil,
            "or" => TokenType::Or,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "this" => TokenType::This,
            "true" => TokenType::True,
            "var" => TokenType::Var,
            "while" => TokenType::While,
            _ => TokenType::Identifier,
        }
    }
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum ScanError {
    #[error("unexpected character '{found}'")]
    UnexpectedChar { found: char, line: usize },

    #[error("unterminated string")]
    UnterminatedString { line: usize },
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token_type::Literal;

    // Helper function to create expected tokens easily
    fn token(token_type: TokenType, lexeme: &str, line: usize) -> Token {
        let literal = match token_type {
            TokenType::Number => Some(Literal::Number(lexeme.parse().unwrap())),
            TokenType::String => Some(Literal::String(lexeme.to_string())),
            _ => None,
        };
        Token::new(token_type, lexeme.to_string(), line, literal)
    }

    #[test]
    fn test_single_char_tokens() {
        let mut scanner = Scanner::new("(){},.-+;*");
        let tokens = scanner.scan_tokens().unwrap();

        let expected = [
            token(TokenType::LeftParen, "(", 1),
            token(TokenType::RightParen, ")", 1),
            token(TokenType::LeftBrace, "{", 1),
            token(TokenType::RightBrace, "}", 1),
            token(TokenType::Comma, ",", 1),
            token(TokenType::Dot, ".", 1),
            token(TokenType::Minus, "-", 1),
            token(TokenType::Plus, "+", 1),
            token(TokenType::Semicolon, ";", 1),
            token(TokenType::Star, "*", 1),
            token(TokenType::Eof, "", 1),
        ];

        assert_eq!(tokens.len(), expected.len());
        for (actual, expected) in tokens.iter().zip(expected.iter()) {
            assert_eq!(actual.token_type, expected.token_type);
            assert_eq!(actual.lexeme, expected.lexeme);
            assert_eq!(actual.line, expected.line);
        }
    }

    #[test]
    fn test_double_char_tokens() {
        let mut scanner = Scanner::new("! != = == > >= < <=");
        let tokens = scanner.scan_tokens().unwrap();

        let expected = [
            token(TokenType::Bang, "!", 1),
            token(TokenType::BangEqual, "!=", 1),
            token(TokenType::Equal, "=", 1),
            token(TokenType::EqualEqual, "==", 1),
            token(TokenType::Greater, ">", 1),
            token(TokenType::GreaterEqual, ">=", 1),
            token(TokenType::Less, "<", 1),
            token(TokenType::LessEqual, "<=", 1),
            token(TokenType::Eof, "", 1),
        ];

        assert_eq!(tokens.len(), expected.len());
        for (actual, expected) in tokens.iter().zip(expected.iter()) {
            assert_eq!(actual.token_type, expected.token_type);
            assert_eq!(actual.lexeme, expected.lexeme);
        }
    }

    #[test]
    fn test_string_literal() {
        let mut scanner = Scanner::new(r#""hello world""#);
        let tokens = scanner.scan_tokens().unwrap();

        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].token_type, TokenType::String);
        assert_eq!(tokens[0].lexeme, "\"hello world\"");
        assert_eq!(tokens[1].token_type, TokenType::Eof);
    }

    #[test]
    fn test_string_multiline() {
        let mut scanner = Scanner::new("\"hello\nworld\"");
        let tokens = scanner.scan_tokens().unwrap();

        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].token_type, TokenType::String);
        assert_eq!(tokens[0].line, 2); // Line should increment
        assert_eq!(tokens[1].token_type, TokenType::Eof);
    }

    #[test]
    fn test_unterminated_string_error() {
        let mut scanner = Scanner::new(r#""unterminated"#);
        let result = scanner.scan_tokens();

        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);

        let scan_error = &errors[0];
        match scan_error {
            ScanError::UnterminatedString { line } => {
                assert_eq!(*line, 1);
            }
            _ => panic!("Expected UnterminatedString, got {:?}", scan_error),
        }
    }

    #[test]
    fn test_integer_number() {
        let mut scanner = Scanner::new("123 456.");
        let tokens = scanner.scan_tokens().unwrap();

        let expected = [
            token(TokenType::Number, "123", 1),
            token(TokenType::Number, "456", 1),
            token(TokenType::Dot, ".", 1),
            token(TokenType::Eof, "", 1),
        ];

        assert_eq!(tokens.len(), expected.len());
        for (actual, expected) in tokens.iter().zip(expected.iter()) {
            assert_eq!(actual.token_type, expected.token_type);
            assert_eq!(actual.lexeme, expected.lexeme);
        }
    }

    #[test]
    fn test_decimal_number() {
        let mut scanner = Scanner::new("123.45 0.99.");
        let tokens = scanner.scan_tokens().unwrap();

        let expected = [
            token(TokenType::Number, "123.45", 1),
            token(TokenType::Number, "0.99", 1),
            token(TokenType::Dot, ".", 1),
            token(TokenType::Eof, "", 1),
        ];

        assert_eq!(tokens.len(), expected.len());
        for (actual, expected) in tokens.iter().zip(expected.iter()) {
            assert_eq!(actual.token_type, expected.token_type);
            assert_eq!(actual.lexeme, expected.lexeme);
        }
    }

    #[test]
    fn test_identifier() {
        let mut scanner = Scanner::new("my_var _test abc123");
        let tokens = scanner.scan_tokens().unwrap();

        let expected = [
            token(TokenType::Identifier, "my_var", 1),
            token(TokenType::Identifier, "_test", 1),
            token(TokenType::Identifier, "abc123", 1),
            token(TokenType::Eof, "", 1),
        ];

        assert_eq!(tokens.len(), expected.len());
        for (actual, expected) in tokens.iter().zip(expected.iter()) {
            assert_eq!(actual.token_type, expected.token_type);
            assert_eq!(actual.lexeme, expected.lexeme);
        }
    }

    #[test]
    fn test_keywords() {
        let input = "and class else false for fun if nil or print return super this true var while";
        let mut scanner = Scanner::new(input);
        let tokens = scanner.scan_tokens().unwrap();

        let expected_types = [
            TokenType::And,
            TokenType::Class,
            TokenType::Else,
            TokenType::False,
            TokenType::For,
            TokenType::Fun,
            TokenType::If,
            TokenType::Nil,
            TokenType::Or,
            TokenType::Print,
            TokenType::Return,
            TokenType::Super,
            TokenType::This,
            TokenType::True,
            TokenType::Var,
            TokenType::While,
            TokenType::Eof,
        ];

        assert_eq!(tokens.len(), expected_types.len());
        for (actual, expected) in tokens.iter().zip(expected_types.iter()) {
            assert_eq!(&actual.token_type, expected);
        }
    }

    #[test]
    fn test_line_comment() {
        let mut scanner = Scanner::new("// this is a comment\n123");
        let tokens = scanner.scan_tokens().unwrap();

        let expected = [
            token(TokenType::Number, "123", 2), // Line should be 2 after newline
            token(TokenType::Eof, "", 2),
        ];

        assert_eq!(tokens.len(), expected.len());
        assert_eq!(tokens[0].token_type, TokenType::Number);
        assert_eq!(tokens[0].line, 2);
    }

    #[test]
    fn test_block_comment() {
        let mut scanner = Scanner::new("/* this is a\nblock comment */123");
        let tokens = scanner.scan_tokens().unwrap();

        let expected = [
            token(TokenType::Number, "123", 2), // Line should be 2 after newline
            token(TokenType::Eof, "", 2),
        ];

        assert_eq!(tokens.len(), expected.len());
        assert_eq!(tokens[0].token_type, TokenType::Number);
        assert_eq!(tokens[0].line, 2);
    }

    #[test]
    fn test_slash_vs_comment() {
        let mut scanner = Scanner::new("/ //");
        let tokens = scanner.scan_tokens().unwrap();

        let expected = [
            token(TokenType::Slash, "/", 1),
            token(TokenType::Eof, "", 1),
        ];

        assert_eq!(tokens.len(), expected.len());
        assert_eq!(tokens[0].token_type, TokenType::Slash);
    }

    #[test]
    fn test_unterminated_comment() {
        let mut scanner = Scanner::new("/*/");
        let result = scanner.scan_tokens();

        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);

        let scan_error = &errors[0];
        match scan_error {
            ScanError::UnterminatedString { line } => {
                assert_eq!(*line, 1);
            }
            _ => panic!("Expected UnterminatedString, got {:?}", scan_error),
        }
    }

    #[test]
    fn test_whitespace_handling() {
        let mut scanner = Scanner::new("  \t\n   \t\n 123 \t ");
        let tokens = scanner.scan_tokens().unwrap();

        let expected = [
            token(TokenType::Number, "123", 3), // Should be on line 3 after newlines
            token(TokenType::Eof, "", 3),
        ];

        assert_eq!(tokens.len(), expected.len());
        assert_eq!(tokens[0].token_type, TokenType::Number);
        assert_eq!(tokens[0].line, 3);
    }

    #[test]
    fn test_empty_input() {
        let mut scanner = Scanner::new("");
        let tokens = scanner.scan_tokens().unwrap();

        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].token_type, TokenType::Eof);
    }

    #[test]
    fn test_complex_expression() {
        let mut scanner = Scanner::new(
            r#"var x = 10 + 5.5; // initialize x
            if (x > 10) {
                print "hello";
            }"#,
        );
        let tokens = scanner.scan_tokens().unwrap();

        // Just verify we get the right sequence without panicking
        assert!(!tokens.is_empty());
        assert_eq!(tokens[0].token_type, TokenType::Var);
        assert_eq!(tokens[1].token_type, TokenType::Identifier);
        assert_eq!(tokens[2].token_type, TokenType::Equal);
        assert_eq!(tokens[tokens.len() - 1].token_type, TokenType::Eof);
    }

    #[test]
    fn test_unexpected_char() {
        let mut scanner = Scanner::new("@");
        let result = scanner.scan_tokens();

        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);

        let scan_error = &errors[0];
        match scan_error {
            ScanError::UnexpectedChar { found, line } => {
                assert_eq!(*found, '@');
                assert_eq!(*line, 1);
            }
            _ => panic!("Expected UnexpectedChar, got {:?}", scan_error),
        }
    }
}
