use crate::environment::{Environment, EnvironmentError};
use crate::expr::Expr;
use crate::literal::{Literal, TypeError};
use crate::token_type::TokenType;

impl<'src> Expr<'src> {
    pub fn evaluate(&self, environment: &mut Environment) -> Result<Literal, ExprError> {
        match self {
            Expr::Variable { name } => environment.get(name).map_err(ExprError::UndefinedVariable),
            Expr::Literal { value } => Ok(value.clone()),
            Expr::Grouping { expression } => expression.evaluate(environment),
            Expr::Unary { operator, right } => {
                let right_lit = right.evaluate(environment)?;

                match operator.token_type {
                    TokenType::Minus => Ok(Literal::Number(-right_lit.try_into()?)),
                    TokenType::Bang => Ok(Literal::Boolean(!right_lit.into_truthy())),
                    _ => unreachable!("only minus and bang are unary operator"),
                }
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left_lit = left.evaluate(environment)?;
                let right_lit = right.evaluate(environment)?;

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
                    TokenType::Plus => Ok(self.handle_plus(&left_lit, &right_lit)?),
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
            Expr::Assign { name, value } => {
                let value = value.evaluate(environment)?;
                environment
                    .assign(name, value.clone())
                    .map_err(ExprError::UndefinedVariable)?;
                Ok(value)
            }
            _ => todo!(),
        }
    }

    fn handle_plus(&self, left: &Literal, right: &Literal) -> Result<Literal, ExprError> {
        match (left, right) {
            (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Number(l + r)),
            (Literal::String(_), _) | (_, Literal::String(_)) => {
                Ok(Literal::String(format!("{}{}", left, right)))
            }
            _ => todo!(),
        }
    }
}

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum ExprError {
    #[error(transparent)]
    Type(#[from] TypeError),
    #[error(transparent)]
    UndefinedVariable(EnvironmentError),
}
