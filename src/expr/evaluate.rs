use crate::{
    expr::Expr,
    token_type::{Literal, LiteralError, TokenType},
};

impl Expr {
    pub fn evaluate(&self) -> Result<Literal, RuntimeError> {
        match self {
            Expr::Literal { value } => Ok(value.clone()),
            Expr::Grouping { expression } => expression.evaluate(),
            Expr::Unary { operator, right } => {
                let right_lit = right.evaluate()?;

                match operator.token_type {
                    TokenType::Minus => Ok(Literal::Number(-right_lit.try_into()?)),
                    TokenType::Bang => Ok(Literal::Boolean(!Into::<bool>::into(right_lit))),
                    _ => unreachable!("only minus and bang are unary operator"),
                }
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left_lit = left.evaluate()?;
                let right_lit = right.evaluate()?;

                match operator.token_type {
                    TokenType::Minus => Ok(Literal::Number(
                        TryInto::<f64>::try_into(left_lit)? - TryInto::<f64>::try_into(right_lit)?,
                    )),
                    TokenType::Slash => Ok(Literal::Number(
                        TryInto::<f64>::try_into(left_lit)? / TryInto::<f64>::try_into(right_lit)?,
                    )),
                    TokenType::Star => Ok(Literal::Number(
                        TryInto::<f64>::try_into(left_lit)? * TryInto::<f64>::try_into(right_lit)?,
                    )),
                    TokenType::Plus => match (left_lit, right_lit) {
                        (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Number(l + r)),
                        (Literal::String(l), Literal::String(r)) => {
                            Ok(Literal::String(format!("{l}{r}")))
                        }
                        _ => todo!(),
                    },
                    TokenType::Greater => Ok(Literal::Boolean(
                        TryInto::<f64>::try_into(left_lit)? > TryInto::<f64>::try_into(right_lit)?,
                    )),
                    TokenType::GreaterEqual => Ok(Literal::Boolean(
                        TryInto::<f64>::try_into(left_lit)? >= TryInto::<f64>::try_into(right_lit)?,
                    )),
                    TokenType::Less => Ok(Literal::Boolean(
                        TryInto::<f64>::try_into(left_lit)? < TryInto::<f64>::try_into(right_lit)?,
                    )),
                    TokenType::LessEqual => Ok(Literal::Boolean(
                        TryInto::<f64>::try_into(left_lit)? <= TryInto::<f64>::try_into(right_lit)?,
                    )),
                    TokenType::EqualEqual => Ok(Literal::Boolean(left_lit == right_lit)),
                    TokenType::BangEqual => Ok(Literal::Boolean(left_lit != right_lit)),
                    _ => unreachable!(),
                }
            }
            _ => todo!(),
        }
    }
}

#[derive(Debug, thiserror::Error, PartialEq)]
enum RuntimeError {
    #[error(transparent)]
    LiteralError(#[from] LiteralError),
}

#[cfg(test)]
mod evaluate_tests {
    use super::*;
    use crate::token::Token;

    // Helper functions for constructing expressions
    fn num_expr(value: f64) -> Expr {
        Expr::Literal {
            value: Literal::Number(value),
        }
    }

    fn str_expr(s: &str) -> Expr {
        Expr::Literal {
            value: Literal::String(s.to_string()),
        }
    }

    fn bool_expr(b: bool) -> Expr {
        Expr::Literal {
            value: Literal::Boolean(b),
        }
    }

    fn nil_expr() -> Expr {
        Expr::Literal {
            value: Literal::Nil,
        }
    }

    fn token(token_type: TokenType, lexeme: &str) -> Token {
        Token::new_simple(token_type, lexeme, 1)
    }

    fn unary_expr(op: TokenType, lexeme: &str, right: Expr) -> Expr {
        Expr::Unary {
            operator: token(op, lexeme),
            right: Box::new(right),
        }
    }

    fn binary_expr(left: Expr, op: TokenType, lexeme: &str, right: Expr) -> Expr {
        Expr::Binary {
            left: Box::new(left),
            operator: token(op, lexeme),
            right: Box::new(right),
        }
    }

    fn grouping_expr(expr: Expr) -> Expr {
        Expr::Grouping {
            expression: Box::new(expr),
        }
    }

    #[test]
    fn literal_evaluation() {
        assert_eq!(num_expr(42.5).evaluate(), Ok(Literal::Number(42.5)));
        assert_eq!(
            str_expr("hello").evaluate(),
            Ok(Literal::String("hello".to_string()))
        );
        assert_eq!(bool_expr(true).evaluate(), Ok(Literal::Boolean(true)));
        assert_eq!(bool_expr(false).evaluate(), Ok(Literal::Boolean(false)));
        assert_eq!(nil_expr().evaluate(), Ok(Literal::Nil));
    }

    #[test]
    fn grouping_evaluation() {
        assert_eq!(
            grouping_expr(num_expr(42.0)).evaluate(),
            Ok(Literal::Number(42.0))
        );
        assert_eq!(
            grouping_expr(grouping_expr(num_expr(42.0))).evaluate(),
            Ok(Literal::Number(42.0))
        );
    }

    #[test]
    fn unary_minus() {
        assert_eq!(
            unary_expr(TokenType::Minus, "-", num_expr(42.0)).evaluate(),
            Ok(Literal::Number(-42.0))
        );
        assert_eq!(
            unary_expr(TokenType::Minus, "-", num_expr(-42.0)).evaluate(),
            Ok(Literal::Number(42.0))
        );
        assert_eq!(
            unary_expr(TokenType::Minus, "-", num_expr(0.0)).evaluate(),
            Ok(Literal::Number(0.0))
        );
    }

    #[test]
    fn unary_bang() {
        // false and nil are falsey
        assert_eq!(
            unary_expr(TokenType::Bang, "!", bool_expr(false)).evaluate(),
            Ok(Literal::Boolean(true))
        );
        assert_eq!(
            unary_expr(TokenType::Bang, "!", nil_expr()).evaluate(),
            Ok(Literal::Boolean(true))
        );

        // everything else is truthy
        assert_eq!(
            unary_expr(TokenType::Bang, "!", bool_expr(true)).evaluate(),
            Ok(Literal::Boolean(false))
        );
        assert_eq!(
            unary_expr(TokenType::Bang, "!", num_expr(0.0)).evaluate(),
            Ok(Literal::Boolean(false))
        );
        assert_eq!(
            unary_expr(TokenType::Bang, "!", num_expr(42.0)).evaluate(),
            Ok(Literal::Boolean(false))
        );
        assert_eq!(
            unary_expr(TokenType::Bang, "!", str_expr("")).evaluate(),
            Ok(Literal::Boolean(false))
        );
        assert_eq!(
            unary_expr(TokenType::Bang, "!", str_expr("hello")).evaluate(),
            Ok(Literal::Boolean(false))
        );
    }

    #[test]
    fn binary_arithmetic() {
        // Addition
        assert_eq!(
            binary_expr(num_expr(10.0), TokenType::Plus, "+", num_expr(32.0)).evaluate(),
            Ok(Literal::Number(42.0))
        );

        // Subtraction
        assert_eq!(
            binary_expr(num_expr(50.0), TokenType::Minus, "-", num_expr(8.0)).evaluate(),
            Ok(Literal::Number(42.0))
        );

        // Multiplication
        assert_eq!(
            binary_expr(num_expr(21.0), TokenType::Star, "*", num_expr(2.0)).evaluate(),
            Ok(Literal::Number(42.0))
        );

        // Division
        assert_eq!(
            binary_expr(num_expr(84.0), TokenType::Slash, "/", num_expr(2.0)).evaluate(),
            Ok(Literal::Number(42.0))
        );
    }

    #[test]
    fn binary_string_concatenation() {
        assert_eq!(
            binary_expr(
                str_expr("Hello, "),
                TokenType::Plus,
                "+",
                str_expr("World!")
            )
            .evaluate(),
            Ok(Literal::String("Hello, World!".to_string()))
        );

        assert_eq!(
            binary_expr(str_expr(""), TokenType::Plus, "+", str_expr("test")).evaluate(),
            Ok(Literal::String("test".to_string()))
        );

        assert_eq!(
            binary_expr(str_expr("foo"), TokenType::Plus, "+", str_expr("")).evaluate(),
            Ok(Literal::String("foo".to_string()))
        );
    }

    #[test]
    fn binary_comparisons() {
        // Greater
        assert_eq!(
            binary_expr(num_expr(50.0), TokenType::Greater, ">", num_expr(42.0)).evaluate(),
            Ok(Literal::Boolean(true))
        );
        assert_eq!(
            binary_expr(num_expr(42.0), TokenType::Greater, ">", num_expr(42.0)).evaluate(),
            Ok(Literal::Boolean(false))
        );

        // GreaterEqual
        assert_eq!(
            binary_expr(
                num_expr(50.0),
                TokenType::GreaterEqual,
                ">=",
                num_expr(42.0)
            )
            .evaluate(),
            Ok(Literal::Boolean(true))
        );
        assert_eq!(
            binary_expr(
                num_expr(42.0),
                TokenType::GreaterEqual,
                ">=",
                num_expr(42.0)
            )
            .evaluate(),
            Ok(Literal::Boolean(true))
        );
        assert_eq!(
            binary_expr(
                num_expr(41.0),
                TokenType::GreaterEqual,
                ">=",
                num_expr(42.0)
            )
            .evaluate(),
            Ok(Literal::Boolean(false))
        );

        // Less
        assert_eq!(
            binary_expr(num_expr(42.0), TokenType::Less, "<", num_expr(50.0)).evaluate(),
            Ok(Literal::Boolean(true))
        );
        assert_eq!(
            binary_expr(num_expr(42.0), TokenType::Less, "<", num_expr(42.0)).evaluate(),
            Ok(Literal::Boolean(false))
        );

        // LessEqual
        assert_eq!(
            binary_expr(num_expr(42.0), TokenType::LessEqual, "<=", num_expr(50.0)).evaluate(),
            Ok(Literal::Boolean(true))
        );
        assert_eq!(
            binary_expr(num_expr(42.0), TokenType::LessEqual, "<=", num_expr(42.0)).evaluate(),
            Ok(Literal::Boolean(true))
        );
        assert_eq!(
            binary_expr(num_expr(43.0), TokenType::LessEqual, "<=", num_expr(42.0)).evaluate(),
            Ok(Literal::Boolean(false))
        );
    }

    #[test]
    fn binary_equality() {
        // EqualEqual
        assert_eq!(
            binary_expr(num_expr(42.0), TokenType::EqualEqual, "==", num_expr(42.0)).evaluate(),
            Ok(Literal::Boolean(true))
        );
        assert_eq!(
            binary_expr(num_expr(42.0), TokenType::EqualEqual, "==", num_expr(43.0)).evaluate(),
            Ok(Literal::Boolean(false))
        );
        assert_eq!(
            binary_expr(
                str_expr("hello"),
                TokenType::EqualEqual,
                "==",
                str_expr("hello")
            )
            .evaluate(),
            Ok(Literal::Boolean(true))
        );
        assert_eq!(
            binary_expr(
                str_expr("hello"),
                TokenType::EqualEqual,
                "==",
                str_expr("world")
            )
            .evaluate(),
            Ok(Literal::Boolean(false))
        );
        assert_eq!(
            binary_expr(num_expr(42.0), TokenType::EqualEqual, "==", str_expr("42")).evaluate(),
            Ok(Literal::Boolean(false))
        );
        assert_eq!(
            binary_expr(nil_expr(), TokenType::EqualEqual, "==", nil_expr()).evaluate(),
            Ok(Literal::Boolean(true))
        );

        // BangEqual
        assert_eq!(
            binary_expr(num_expr(42.0), TokenType::BangEqual, "!=", num_expr(42.0)).evaluate(),
            Ok(Literal::Boolean(false))
        );
        assert_eq!(
            binary_expr(num_expr(42.0), TokenType::BangEqual, "!=", num_expr(43.0)).evaluate(),
            Ok(Literal::Boolean(true))
        );
        assert_eq!(
            binary_expr(num_expr(42.0), TokenType::BangEqual, "!=", str_expr("42")).evaluate(),
            Ok(Literal::Boolean(true))
        );
    }

    #[test]
    fn division_by_zero() {
        assert_eq!(
            binary_expr(num_expr(42.0), TokenType::Slash, "/", num_expr(0.0)).evaluate(),
            Ok(Literal::Number(f64::INFINITY))
        );

        assert_eq!(
            binary_expr(num_expr(-42.0), TokenType::Slash, "/", num_expr(0.0)).evaluate(),
            Ok(Literal::Number(f64::NEG_INFINITY))
        );
    }

    #[test]
    fn type_error_cases() {
        // Unary minus on non-number returns error
        assert!(
            unary_expr(TokenType::Minus, "-", str_expr("not a number"))
                .evaluate()
                .is_err()
        );

        // Comparison on non-numbers returns error
        assert!(
            binary_expr(str_expr("a"), TokenType::Greater, ">", str_expr("b"))
                .evaluate()
                .is_err()
        );
    }
}
