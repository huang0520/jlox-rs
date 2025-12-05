mod expr;
mod stmt;

use crate::token::{BorrowedToken, OwnedToken, Token};

pub use expr::{BorrowedExpr, Expr, OwnedExpr};
pub use stmt::{BorrowedStmt, OwnedStmt, Stmt};

pub type BorrowedNode<'src> = ASTNode<BorrowedToken<'src>>;
pub type OwnedNode = ASTNode<OwnedToken>;

#[derive(Debug)]
pub enum ASTNode<T: Token> {
    Stmt(Stmt<T>),
    Expr(Expr<T>),
}

impl From<BorrowedNode<'_>> for OwnedNode {
    fn from(node: BorrowedNode) -> Self {
        match node {
            ASTNode::Stmt(stmt) => OwnedNode::Stmt(stmt.into()),
            ASTNode::Expr(expr) => OwnedNode::Expr(expr.into()),
        }
    }
}
