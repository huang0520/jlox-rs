use snafu::Snafu;

use crate::ast::{Expr, OwnedExpr, OwnedNode, OwnedStmt, Stmt};
use crate::environment::{Environment, EnvironmentError};
use crate::literal::{Callable, Literal, TypeError, UserFunction};
use crate::token::{Token, TokenType};

type Result<T> = std::result::Result<T, RuntimeError>;

#[derive(Debug)]
pub enum StmtResult {
    Normal,
    Return(Literal),
    Break,
}

pub fn evaluate(nodes: &[OwnedNode], environment: &Environment) -> Result<()> {
    for node in nodes {
        match node {
            OwnedNode::Stmt(statement) => {
                evaluate_stmt(statement, environment)?;
            }
            OwnedNode::Expr(expression) => {
                println!("{}", evaluate_expr(expression, environment)?);
            }
        };
    }
    Ok(())
}

pub fn evaluate_block(
    statements: &[OwnedStmt],
    parent_environment: &Environment,
) -> Result<StmtResult> {
    let local = Environment::new(Some(parent_environment.clone()));

    for stmt in statements {
        match evaluate_stmt(stmt, &local)? {
            StmtResult::Normal => {}
            rst => return Ok(rst),
        }
    }
    Ok(StmtResult::Normal)
}

fn evaluate_stmt(statement: &OwnedStmt, environment: &Environment) -> Result<StmtResult> {
    match statement {
        Stmt::Expression(expression) => {
            evaluate_expr(expression, environment)?;
            Ok(StmtResult::Normal)
        }
        Stmt::Print(expression) => {
            println!("{}", evaluate_expr(expression, environment)?);
            Ok(StmtResult::Normal)
        }
        Stmt::Var { name, initializer } => {
            let value = evaluate_expr(initializer, environment)?;
            environment.define(name.lexeme(), value);
            Ok(StmtResult::Normal)
        }
        Stmt::Block(statements) => evaluate_block(statements, environment),
        Stmt::If {
            condition,
            then_branch,
            else_branch,
        } => {
            if evaluate_expr(condition, environment)?.is_truthy() {
                evaluate_stmt(then_branch, environment)
            } else if let Some(stmt) = else_branch {
                evaluate_stmt(stmt, environment)
            } else {
                Ok(StmtResult::Normal)
            }
        }
        Stmt::While { condition, body } => {
            while evaluate_expr(condition, environment)?.is_truthy() {
                match evaluate_stmt(body, environment)? {
                    StmtResult::Normal => {}
                    StmtResult::Break => break,
                    rst @ StmtResult::Return(..) => return Ok(rst),
                }
            }
            Ok(StmtResult::Normal)
        }
        Stmt::Break => Ok(StmtResult::Break),
        Stmt::Function {
            name,
            parameters,
            body,
        } => {
            let function = UserFunction {
                name: name.lexeme().to_string(),
                parameters: parameters.iter().map(|p| p.lexeme().to_string()).collect(),
                body: statements_of_block(*body.clone()),
                closure: environment.clone(),
            };
            environment.define(name.lexeme(), Literal::Callable(Callable::User(function)));
            Ok(StmtResult::Normal)
        }
        Stmt::Return { value, .. } => Ok(StmtResult::Return(evaluate_expr(value, environment)?)),
        _ => todo!(),
    }
}

fn evaluate_expr(expression: &OwnedExpr, environment: &Environment) -> Result<Literal> {
    match expression {
        Expr::Variable { name } => {
            environment
                .get(name.lexeme())
                .map_err(|e| RuntimeError::UndefinedVariable {
                    line: name.line(),
                    source: e,
                })
        }
        Expr::Literal { value } => Ok(value.clone()),
        Expr::Grouping { expression } => evaluate_expr(expression, environment),
        Expr::Unary { operator, right } => {
            let right_lit = evaluate_expr(right, environment)?;

            match operator.token_type() {
                TokenType::Minus => Ok(Literal::Number(-right_lit.try_into().map_err(|e| {
                    TypeSnafu {
                        line: operator.line(),
                        source: e,
                    }
                    .build()
                })?)),
                TokenType::Bang => Ok(Literal::Boolean(!right_lit.is_truthy())),
                _ => unreachable!("only minus and bang are unary operator"),
            }
        }
        Expr::Binary {
            left,
            operator,
            right,
        } => {
            let left_lit = evaluate_expr(left, environment)?;
            let right_lit = evaluate_expr(right, environment)?;

            let line = operator.line();
            match operator.token_type() {
                TokenType::Minus => Ok(Literal::Number(
                    try_to_f64(left_lit, line)? - try_to_f64(right_lit, line)?,
                )),
                TokenType::Slash => Ok(Literal::Number(
                    try_to_f64(left_lit, line)? / try_to_f64(right_lit, line)?,
                )),
                TokenType::Star => Ok(Literal::Number(
                    try_to_f64(left_lit, line)? * try_to_f64(right_lit, line)?,
                )),
                TokenType::Plus => match (&left_lit, &right_lit) {
                    (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Number(l + r)),
                    (Literal::String(_), _) | (_, Literal::String(_)) => {
                        Ok(Literal::String(format!("{}{}", left_lit, right_lit)))
                    }
                    _ => UnsupportedSnafu { line }.fail(),
                },
                TokenType::Greater => Ok(Literal::Boolean(
                    try_to_f64(left_lit, line)? > try_to_f64(right_lit, line)?,
                )),
                TokenType::GreaterEqual => Ok(Literal::Boolean(
                    try_to_f64(left_lit, line)? >= try_to_f64(right_lit, line)?,
                )),
                TokenType::Less => Ok(Literal::Boolean(
                    try_to_f64(left_lit, line)? < try_to_f64(right_lit, line)?,
                )),
                TokenType::LessEqual => Ok(Literal::Boolean(
                    try_to_f64(left_lit, line)? <= try_to_f64(right_lit, line)?,
                )),
                TokenType::EqualEqual => Ok(Literal::Boolean(left_lit == right_lit)),
                TokenType::BangEqual => Ok(Literal::Boolean(left_lit != right_lit)),
                _ => unreachable!(),
            }
        }
        Expr::Assign { name, value } => {
            let value = evaluate_expr(value, environment)?;
            environment
                .assign(name.lexeme(), value.clone())
                .map_err(|e| RuntimeError::UndefinedVariable {
                    line: name.line(),
                    source: e,
                })?;
            Ok(value)
        }
        Expr::Logical {
            left,
            operator,
            right,
        } => {
            let left = evaluate_expr(left, environment)?;
            if matches!(operator.token_type(), TokenType::Or) {
                if left.is_truthy() {
                    return Ok(left);
                }
            } else if !left.is_truthy() {
                return Ok(left);
            }
            evaluate_expr(right, environment)
        }
        Expr::Call {
            callee,
            paren,
            arguments,
        } => {
            let callee = evaluate_expr(callee, environment)?;

            let arguments: Result<Vec<Literal>> = arguments
                .iter()
                .map(|expr| evaluate_expr(expr, environment))
                .collect();
            let arguments = arguments?;

            match callee {
                Literal::Callable(function) => {
                    if arguments.len() != function.arity() {
                        UnmatchedArgumentsSnafu {
                            line: paren.line(),
                            expect: function.arity(),
                            found: arguments.len(),
                        }
                        .fail()
                    } else {
                        function.call(&arguments)
                    }
                }
                _ => NotCallableSnafu {
                    line: paren.line(),
                    identifier: callee.to_string(),
                }
                .fail(),
            }
        }
        _ => todo!(),
    }
}

fn statements_of_block(block_statement: OwnedStmt) -> Vec<OwnedStmt> {
    match block_statement {
        Stmt::Block(statements) => statements,
        _ => unreachable!(),
    }
}

fn try_to_f64(lit: Literal, line: usize) -> Result<f64> {
    TryInto::<f64>::try_into(lit).map_err(|e| TypeSnafu { line, source: e }.build())
}

#[derive(Debug, Snafu)]
pub enum RuntimeError {
    #[snafu(display("  - line: {line}: {source}"))]
    UndefinedVariable {
        line: usize,
        #[snafu(source(false))]
        source: EnvironmentError,
    },
    #[snafu(display("  - line: {line}: {source}"))]
    Type {
        line: usize,
        #[snafu(source(false))]
        source: TypeError,
    },
    #[snafu(display("  - line: {line}: expected callable, found {identifier}"))]
    NotCallable { line: usize, identifier: String },
    #[snafu(display("  - line: {line}: expected {expect} arguments, found {found}"))]
    UnmatchedArguments {
        line: usize,
        expect: usize,
        found: usize,
    },
    #[snafu(display("  - line: {line}: unsupported operation"))]
    Unsupported { line: usize },
}
