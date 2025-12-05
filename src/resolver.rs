use std::collections::{HashMap, VecDeque};

use snafu::Snafu;

use crate::ast::{Expr, OwnedExpr, OwnedStmt, Stmt};
use crate::literal::Literal;
use crate::token::{OwnedToken, Token};

type Result<T> = std::result::Result<T, SemanticError>;

#[derive(Debug, Default)]
pub struct Resolver {
    scopes: VecDeque<HashMap<String, bool>>,
    locals: HashMap<OwnedExpr, isize>,
}

impl Resolver {
    fn resolve_stmts(&mut self, statements: &[OwnedStmt]) {
        for statement in statements {
            self.resolve_stmt(statement);
        }
    }
    fn resolve_stmt(&mut self, statement: &OwnedStmt) {
        match statement {
            Stmt::Block(statements) => {
                self.begin_scope();
                self.resolve_stmts(statements);
                self.end_scope();
            }
            Stmt::Var { name, initializer } => {
                self.declare(name);
                if !matches!(
                    initializer,
                    Expr::Literal {
                        value: Literal::Nil
                    }
                ) {
                    self.resolve_expr(initializer);
                }
                self.define(name);
            }
            _ => todo!(),
        }
    }
    fn resolve_expr(&self, expression: &OwnedExpr) -> Result<()> {
        match expression {
            Expr::Variable { name } => {
                if let Some(scope) = self.scopes.back()
                    && let Some(&defined) = scope.get(name.lexeme())
                    && defined
                {
                    InvalidShadowingSnafu {
                        line: name.line(),
                        name: name.lexeme().to_string(),
                    }
                    .fail()
                } else {
                    todo!()
                }
            }
            _ => todo!(),
        }
    }

    fn declare(&mut self, name: &OwnedToken) {
        if let Some(scope) = self.scopes.back_mut() {
            scope.insert(name.lexeme().to_string(), false);
        }
    }

    fn define(&mut self, name: &OwnedToken) {
        if let Some(scope) = self.scopes.back_mut()
            && let Some(v) = scope.get_mut(name.lexeme())
        {
            *v = true;
        }
    }

    fn resolve_local(&mut self, expression: OwnedExpr, name: OwnedToken) {
        if let Some((i, _)) = self
            .scopes
            .iter()
            .rev()
            .enumerate()
            .find(|&(_, scope)| scope.contains_key(name.lexeme()))
        {
            self.locals.insert()
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push_back(HashMap::new());
    }
    fn end_scope(&mut self) {
        self.scopes.pop_back();
    }
}

#[derive(Debug, Snafu)]
pub enum SemanticError {
    #[snafu(display("  - line: {line}: can't read local vaiable '{name}' in its own initializer"))]
    InvalidShadowing { line: usize, name: String },
}
