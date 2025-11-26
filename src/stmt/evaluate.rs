use crate::{expr::evaluate::ExprError, stmt::Stmt};

impl<'src> Stmt<'src> {
    pub fn evaluate(&self) -> Result<(), StmtError> {
        match self {
            Stmt::Expression(expr) => {
                expr.evaluate()?;
                Ok(())
            }
            Stmt::Print(expr) => {
                println!("{}", expr.evaluate()?);
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
