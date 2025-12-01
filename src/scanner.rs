use std::iter::Peekable;
use std::str::CharIndices;

use crate::literal::Literal;
use crate::token::Token;
use crate::token_type::TokenType;

#[derive(Debug)]
pub struct Scanner<'src> {
    source: &'src str,
    chars: Peekable<CharIndices<'src>>,
    line: usize,
    start: usize,
    current: usize,
}

impl<'src> Scanner<'src> {
    pub fn scan_tokens(source: &'src str) -> Result<Vec<Token<'src>>, Vec<ScanError>> {
        let mut tokens: Vec<Token<'src>> = Vec::new();
        let mut errors = Vec::new();

        let mut scanner = Scanner {
            source,
            chars: source.char_indices().peekable(),
            line: 1,
            start: 0,
            current: 0,
        };

        loop {
            scanner.start = scanner.current;
            match scanner.advance() {
                Some(c) => match scanner.scan_token(c) {
                    Ok(Some(t)) => tokens.push(t),
                    Ok(None) => {}
                    Err(e) => errors.push(e),
                },
                None => break,
            }
        }

        tokens.push(Token::new_eof(scanner.line));
        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(errors)
        }
    }

    fn scan_token(&mut self, c: char) -> Result<Option<Token<'src>>, ScanError> {
        match c {
            // Single-char token
            '(' | ')' | '{' | '}' | ',' | '.' | '-' | '+' | ';' | '*' => {
                Ok(Some(Token::new_simple(
                    self.check_single_lexeme(&c),
                    self.current_lexeme(),
                    self.line,
                )))
            }

            // Double-char token
            '!' => Ok(Some(self.lookahead_token(
                '=',
                TokenType::BangEqual,
                TokenType::Bang,
            ))),
            '=' => Ok(Some(self.lookahead_token(
                '=',
                TokenType::EqualEqual,
                TokenType::Equal,
            ))),
            '>' => Ok(Some(self.lookahead_token(
                '=',
                TokenType::GreaterEqual,
                TokenType::Greater,
            ))),
            '<' => Ok(Some(self.lookahead_token(
                '=',
                TokenType::LessEqual,
                TokenType::Less,
            ))),
            '/' => {
                if let Some((_, nc)) = self.chars.next_if(|&(_, c)| matches!(c, '/' | '*')) {
                    self.advance();
                    if nc == '/' {
                        while self.chars.next_if(|&(_, c)| c != '\n').is_some() {}
                    } else {
                        self.scan_comment_block()?;
                    }
                    Ok(None)
                } else {
                    Ok(Some(Token::new_simple(
                        TokenType::Slash,
                        self.current_lexeme(),
                        self.line,
                    )))
                }
            }

            // Whitespace
            ' ' | '\r' | '\t' => Ok(None),
            '\n' => {
                self.line += 1;
                Ok(None)
            }

            // State transitions
            '"' => Ok(Some(self.scan_string()?)),
            '0'..='9' => Ok(Some(self.scan_number())),
            'a'..='z' | 'A'..='Z' | '_' => Ok(Some(self.scan_identifier())),

            _ => Err(ScanError::UnexpectedChar {
                found: c,
                line: self.line,
            }),
        }
    }

    fn scan_comment_block(&mut self) -> Result<(), ScanError> {
        while let Some((_, c)) = self.chars.next() {
            if c == '*' && self.chars.peek().is_some_and(|&(_, c)| c == '/') {
                self.advance();
                return Ok(());
            }

            if c == '\n' {
                self.line += 1;
            }
        }
        Err(ScanError::UnterminatedCommentBlock { line: self.line })
    }

    fn scan_string(&mut self) -> Result<Token<'src>, ScanError> {
        while self.peek().is_some() {
            let c = self.advance().expect("next char exist");
            match c {
                '"' => {
                    let lexeme = self.current_lexeme();
                    let value = &lexeme[1..lexeme.len() - 1];
                    return Ok(Token::new_string(value, lexeme, self.line));
                }
                '\n' => self.line += 1,
                _ => {}
            }
        }
        Err(ScanError::UnterminatedString { line: self.line })
    }

    fn scan_number(&mut self) -> Token<'src> {
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                self.advance();
            } else {
                break;
            }
        }

        // Check decimal
        if self.peek() == Some('.') {
            // Check ahead
            let mut ahead_iter = self.chars.clone();
            ahead_iter.next();

            if ahead_iter.next().map(|(_, c)| c).is_some() {
                // Consume '.'
                self.advance();
                while let Some(c) = self.peek() {
                    if c.is_ascii_digit() {
                        self.advance();
                    } else {
                        break;
                    }
                }
            }
        }

        let lexeme = self.current_lexeme();
        let value: f64 = lexeme.parse().expect("Should be able to parse");
        Token::new_number(value, lexeme, self.line)
    }

    fn scan_identifier(&mut self) -> Token<'src> {
        while self
            .peek()
            .is_some_and(|c| c.is_ascii_alphanumeric() || c == '_')
        {
            self.advance();
        }

        let lexeme = self.current_lexeme();
        let token_type = self.check_keyword(lexeme);
        match token_type {
            TokenType::True => {
                Token::new(token_type, lexeme, self.line, Some(Literal::Boolean(true)))
            }
            TokenType::False => {
                Token::new(token_type, lexeme, self.line, Some(Literal::Boolean(false)))
            }
            TokenType::Nil => Token::new(token_type, lexeme, self.line, Some(Literal::Nil)),
            _ => Token::new_simple(token_type, lexeme, self.line),
        }
    }

    // helper function
    fn advance(&mut self) -> Option<char> {
        let (idx, c) = self.chars.next()?;
        self.current = idx + c.len_utf8();
        Some(c)
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|&(_, c)| c)
    }

    fn lookahead_token(
        &mut self,
        expected: char,
        match_type: TokenType,
        default_type: TokenType,
    ) -> Token<'src> {
        let token_type = if self.peek().is_some_and(|c| c == expected) {
            self.advance();
            match_type
        } else {
            default_type
        };
        Token::new_simple(token_type, self.current_lexeme(), self.line)
    }

    fn current_lexeme(&self) -> &'src str {
        &self.source[self.start..self.current]
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

#[derive(Debug, Clone, thiserror::Error, PartialEq)]
pub enum ScanError {
    #[error("line {line}: unexpected character '{found}'")]
    UnexpectedChar { found: char, line: usize },

    #[error("line {line}: unterminated string")]
    UnterminatedString { line: usize },

    #[error("line {line}: unterminated comment block")]
    UnterminatedCommentBlock { line: usize },
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::literal::Literal;
    use crate::token::Token;
    use crate::token_type::TokenType;

    // Helper to run the scanner
    fn scan<'a>(source: &'a str) -> Result<Vec<Token<'a>>, Vec<ScanError>> {
        Scanner::scan_tokens(source)
    }

    // Helper to assert a successful scan with expected tokens (EOF auto-added)
    fn assert_tokens(
        source: &str,
        expected_types: &[TokenType],
        expected_literals: Option<&[Option<Literal>]>,
    ) {
        let tokens = scan(source).unwrap();
        assert_eq!(tokens.len(), expected_types.len() + 1); // +1 for EOF
        for (i, &tt) in expected_types.iter().enumerate() {
            assert_eq!(tokens[i].token_type, tt);
            if let Some(lits) = expected_literals {
                assert_eq!(tokens[i].literal, lits[i]);
            }
        }
        assert_eq!(tokens.last().unwrap().token_type, TokenType::Eof);
    }

    // Helper to assert errors
    fn assert_errors(source: &str, expected_errors: &[ScanError]) {
        match scan(source) {
            Ok(_) => panic!("Expected errors, got tokens"),
            Err(errors) => assert_eq!(errors, expected_errors),
        }
    }

    #[test]
    fn test_single_char_tokens() {
        assert_tokens(
            "() {} , . - + ; *",
            &[
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::RightBrace,
                TokenType::Comma,
                TokenType::Dot,
                TokenType::Minus,
                TokenType::Plus,
                TokenType::Semicolon,
                TokenType::Star,
            ],
            None,
        );
    }

    #[test]
    fn test_double_char_tokens() {
        assert_tokens(
            "! = == != > >= < <=",
            &[
                TokenType::Bang,
                TokenType::Equal,
                TokenType::EqualEqual,
                TokenType::BangEqual,
                TokenType::Greater,
                TokenType::GreaterEqual,
                TokenType::Less,
                TokenType::LessEqual,
            ],
            None,
        );
    }

    #[test]
    fn test_divide_vs_comment() {
        // / as slash
        assert_tokens("/", &[TokenType::Slash], None);

        // // comment skipped
        assert_tokens(
            "var x = 1; // this is a comment",
            &[
                TokenType::Var,
                TokenType::Identifier,
                TokenType::Equal,
                TokenType::Number,
                TokenType::Semicolon,
            ],
            Some(&[None, None, None, Some(Literal::Number(1.0)), None]),
        );

        // /* */ block comment skipped
        assert_tokens(
            "var x = 1; /* multi line comment */",
            &[
                TokenType::Var,
                TokenType::Identifier,
                TokenType::Equal,
                TokenType::Number,
                TokenType::Semicolon,
            ],
            Some(&[None, None, None, Some(Literal::Number(1.0)), None]),
        );

        // Nested or unterminated handled as per impl (no nesting, error on unterm)
    }

    #[test]
    fn test_unterminated_comment_block() {
        let source = "/* unterminated";
        let expected_error = ScanError::UnterminatedCommentBlock { line: 1 };
        assert_errors(source, &[expected_error]);
    }

    #[test]
    fn test_strings() {
        // Valid string
        assert_tokens(
            r#""hello""#,
            &[TokenType::String],
            Some(&[Some(Literal::String("hello".to_string()))]),
        );

        // String with newline (increments line)
        assert_tokens(
            r#""hello
world""#,
            &[TokenType::String],
            Some(&[Some(Literal::String("hello\nworld".to_string()))]), // Line 2 for EOF
        );

        // Unterminated string
        let source = r#""unterminated"#;
        let expected_error = ScanError::UnterminatedString { line: 1 };
        assert_errors(source, &[expected_error]);
    }

    #[test]
    fn test_numbers() {
        // Integer
        assert_tokens(
            "123",
            &[TokenType::Number],
            Some(&[Some(Literal::Number(123.0))]),
        );

        // Decimal
        assert_tokens(
            "1.23",
            &[TokenType::Number],
            Some(&[Some(Literal::Number(1.23))]),
        );

        // Multiple numbers
        assert_tokens(
            "1 2.0 3",
            &[TokenType::Number, TokenType::Number, TokenType::Number],
            Some(&[
                Some(Literal::Number(1.0)),
                Some(Literal::Number(2.0)),
                Some(Literal::Number(3.0)),
            ]),
        );

        assert_tokens(
            "1.",
            &[TokenType::Number, TokenType::Dot],
            Some(&[Some(Literal::Number(1.0)), None]),
        );

        assert_tokens(
            ".5",
            &[TokenType::Dot, TokenType::Number],
            Some(&[None, Some(Literal::Number(5.0))]),
        );
    }

    #[test]
    fn test_identifiers_and_keywords() {
        // Keywords
        assert_tokens(
            "and class else false for fun if nil or print return super this true var while",
            &[
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
            ],
            Some(&[
                None,
                None,
                None,
                Some(Literal::Boolean(false)),
                None,
                None,
                None,
                Some(Literal::Nil),
                None,
                None,
                None,
                None,
                None,
                Some(Literal::Boolean(true)),
                None,
                None,
            ]),
        );

        // Identifier
        assert_tokens("variable", &[TokenType::Identifier], None);

        // With underscore/alphanum
        assert_tokens("_var123", &[TokenType::Identifier], None);
    }

    #[test]
    fn test_whitespace_and_newlines() {
        assert_tokens(
            "let \n x = 1; \t\r\n",
            &[
                TokenType::Identifier,
                TokenType::Identifier,
                TokenType::Equal,
                TokenType::Number,
                TokenType::Semicolon,
            ],
            Some(&[None, None, None, Some(Literal::Number(1.0)), None]),
        );
        // EOF on line 3
    }

    #[test]
    fn test_unexpected_char() {
        let source = "@";
        let expected_error = ScanError::UnexpectedChar {
            found: '@',
            line: 1,
        };
        assert_errors(source, &[expected_error]);
    }

    #[test]
    fn test_empty_source() {
        let tokens = scan("").unwrap();
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].token_type, TokenType::Eof);
        assert_eq!(tokens[0].line, 1);
    }

    #[test]
    fn test_mixed_input() {
        let source = r#"
            var x = "hello"; /* comment */ if (true) { print x; } // end
        "#;
        assert_tokens(
            source,
            &[
                TokenType::Var,
                TokenType::Identifier,
                TokenType::Equal,
                TokenType::String,
                TokenType::Semicolon,
                TokenType::If,
                TokenType::LeftParen,
                TokenType::True,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::Print,
                TokenType::Identifier,
                TokenType::Semicolon,
                TokenType::RightBrace,
            ],
            Some(&[
                None,
                None,
                None,
                Some(Literal::String("hello".to_string())),
                None,
                None,
                None,
                Some(Literal::Boolean(true)),
                None,
                None,
                None,
                None,
                None,
                None,
            ]),
        );
        // EOF on line 3
    }

    #[test]
    fn test_multiple_errors() {
        let source = r#"@ # "unterm@str"#;
        let expected_errors = vec![
            ScanError::UnexpectedChar {
                found: '@',
                line: 1,
            },
            ScanError::UnexpectedChar {
                found: '#',
                line: 1,
            },
            ScanError::UnterminatedString { line: 1 },
        ];
        assert_errors(source, &expected_errors);
    }
}
