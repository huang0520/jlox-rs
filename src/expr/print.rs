use crate::expr::Expr;

impl<'src> Expr<'src> {
    pub fn print(&self) -> String {
        match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => self.parenthesize(&operator.lexeme, &[left.as_ref(), right.as_ref()]),
            Expr::Grouping { expression } => self.parenthesize("group", &[expression.as_ref()]),
            Expr::Literal { value } => value.to_string(),
            Expr::Unary { operator, right } => {
                self.parenthesize(&operator.lexeme, &[right.as_ref()])
            }

            _ => todo!(),
        }
    }

    pub fn print_rpn(&self) -> String {
        match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => self.to_rpn(&operator.lexeme, &[left.as_ref(), right.as_ref()]),
            Expr::Grouping { expression } => self.to_rpn("group", &[expression.as_ref()]),
            Expr::Literal { value } => value.to_string(),
            Expr::Unary { operator, right } => self.to_rpn(&operator.lexeme, &[right.as_ref()]),

            _ => todo!(),
        }
    }

    fn parenthesize(&self, name: &str, exprs: &[&Expr]) -> String {
        let mut rst = String::new();
        rst.push('(');
        rst.push_str(name);
        for expr in exprs {
            rst.push(' ');
            rst.push_str(&expr.print());
        }
        rst.push(')');
        rst
    }

    fn to_rpn(&self, name: &str, exprs: &[&Expr]) -> String {
        let mut rst = String::new();
        for expr in exprs {
            rst.push_str(&expr.print_rpn());
            rst.push(' ');
        }
        rst.push_str(name);
        rst
    }
}

#[cfg(test)]
mod tests {
    use crate::{literal::Literal, token::Token, token_type::TokenType};

    use super::*;

    // Helper to create number literal expression
    fn num_expr(value: f64) -> Expr<'static> {
        Expr::Literal {
            value: Literal::Number(value),
        }
    }

    // Helper to create operator token
    fn op_token(op: &str) -> Token {
        match op {
            "+" => Token::new_simple(TokenType::Plus, "+", 1),
            "-" => Token::new_simple(TokenType::Minus, "-", 1),
            "*" => Token::new_simple(TokenType::Star, "*", 1),
            _ => panic!("Unknown operator"),
        }
    }

    #[test]
    fn test_print_literal() {
        assert_eq!(num_expr(42.0).print(), "42");
        assert_eq!(
            Expr::Literal {
                value: Literal::Nil
            }
            .print(),
            "nil"
        );
    }

    #[test]
    fn test_print_unary() {
        let expr = Expr::Unary {
            operator: op_token("-"),
            right: Box::new(num_expr(5.0)),
        };
        assert_eq!(expr.print(), "(- 5)");
    }

    #[test]
    fn test_print_binary() {
        let expr = Expr::Binary {
            left: Box::new(num_expr(1.0)),
            operator: op_token("+"),
            right: Box::new(num_expr(2.0)),
        };
        assert_eq!(expr.print(), "(+ 1 2)");
    }

    #[test]
    fn test_print_grouping() {
        let expr = Expr::Grouping {
            expression: Box::new(num_expr(42.0)),
        };
        assert_eq!(expr.print(), "(group 42)");
    }

    #[test]
    fn test_print_complex() {
        // (- (+ 1 2))
        let inner = Expr::Binary {
            left: Box::new(num_expr(1.0)),
            operator: op_token("+"),
            right: Box::new(num_expr(2.0)),
        };
        let expr = Expr::Unary {
            operator: op_token("-"),
            right: Box::new(inner),
        };
        assert_eq!(expr.print(), "(- (+ 1 2))");
    }

    #[test]
    fn test_print_rpn_literal() {
        assert_eq!(num_expr(42.0).print_rpn(), "42");
        assert_eq!(
            Expr::Literal {
                value: Literal::Nil
            }
            .print_rpn(),
            "nil"
        );
    }

    #[test]
    fn test_print_rpn_unary() {
        let expr = Expr::Unary {
            operator: op_token("-"),
            right: Box::new(num_expr(5.0)),
        };
        assert_eq!(expr.print_rpn(), "5 -");
    }

    #[test]
    fn test_print_rpn_binary() {
        let expr = Expr::Binary {
            left: Box::new(num_expr(1.0)),
            operator: op_token("+"),
            right: Box::new(num_expr(2.0)),
        };
        assert_eq!(expr.print_rpn(), "1 2 +");
    }

    #[test]
    fn test_print_rpn_grouping() {
        let expr = Expr::Grouping {
            expression: Box::new(num_expr(42.0)),
        };
        assert_eq!(expr.print_rpn(), "42 group");
    }

    #[test]
    fn test_print_rpn_complex() {
        // (1 + 2) * 3
        let grouped = Expr::Grouping {
            expression: Box::new(Expr::Binary {
                left: Box::new(num_expr(1.0)),
                operator: op_token("+"),
                right: Box::new(num_expr(2.0)),
            }),
        };
        let expr = Expr::Binary {
            left: Box::new(grouped),
            operator: op_token("*"),
            right: Box::new(num_expr(3.0)),
        };
        assert_eq!(expr.print_rpn(), "1 2 + group 3 *");
    }
}
