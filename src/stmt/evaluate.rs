use crate::{environment::Environment, expr::evaluate::ExprError, stmt::Stmt};

impl<'src> Stmt<'src> {
    pub fn evaluate(&self, environment: &mut Environment) -> Result<(), StmtError> {
        match self {
            Stmt::Expression(expr) => {
                expr.evaluate(environment)?;
                Ok(())
            }
            Stmt::Print(expr) => {
                println!("{}", expr.evaluate(environment)?);
                Ok(())
            }
            Stmt::Var { name, initializer } => {
                let value = initializer.evaluate(environment)?;
                environment.define(name, value);
                Ok(())
            }
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum StmtError {
    #[error(transparent)]
    Expr(#[from] ExprError),
}
