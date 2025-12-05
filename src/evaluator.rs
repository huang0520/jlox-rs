use snafu::{ResultExt, Snafu};

use crate::{
    callable::{Callable, UserFunction},
    environment::{Environment, EnvironmentError},
    expr::RuntimeExpr,
    literal::{Literal, TypeError},
    parser::RuntimeASTNode,
    stmt::RuntimeStmt,
    token_type::TokenType,
};

type Result<T> = std::result::Result<T, RuntimeError>;

#[derive(Debug)]
pub enum StmtResult {
    Normal,
    Return(Literal),
    Break,
}
pub struct Evaluator {}

impl Evaluator {
    pub fn evaluate(nodes: &[RuntimeASTNode], environment: &Environment) -> Result<()> {
        let evaluator = Evaluator {};
        for node in nodes {
            match node {
                RuntimeASTNode::Stmt(statement) => {
                    evaluator.evaluate_stmt(statement, environment)?;
                }
                RuntimeASTNode::Expr(expression) => {
                    println!("{}", evaluator.evaluate_expr(expression, environment)?);
                }
            };
        }
        Ok(())
    }

    fn evaluate_stmt(
        &self,
        statement: &RuntimeStmt,
        environment: &Environment,
    ) -> Result<StmtResult> {
        match statement {
            RuntimeStmt::Expression(expression) => {
                self.evaluate_expr(expression, environment)?;
                Ok(StmtResult::Normal)
            }
            RuntimeStmt::Print(expression) => {
                println!("{}", self.evaluate_expr(expression, environment)?);
                Ok(StmtResult::Normal)
            }
            RuntimeStmt::Var { name, initializer } => {
                let value = self.evaluate_expr(initializer, environment)?;
                environment.define(&name.lexeme, value);
                Ok(StmtResult::Normal)
            }
            RuntimeStmt::Block(statements) => self.evaluate_block(statements, environment),
            RuntimeStmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                if self.evaluate_expr(condition, environment)?.is_truthy() {
                    self.evaluate_stmt(then_branch, environment)
                } else if let Some(stmt) = else_branch {
                    self.evaluate_stmt(stmt, environment)
                } else {
                    Ok(StmtResult::Normal)
                }
            }
            RuntimeStmt::While { condition, body } => {
                while self.evaluate_expr(condition, environment)?.is_truthy() {
                    match self.evaluate_stmt(body, environment)? {
                        StmtResult::Normal => {}
                        StmtResult::Break => break,
                        rst @ StmtResult::Return(..) => return Ok(rst),
                    }
                }
                Ok(StmtResult::Normal)
            }
            RuntimeStmt::Break => Ok(StmtResult::Break),
            RuntimeStmt::Function {
                name,
                parameters,
                body,
            } => {
                let function = UserFunction {
                    name: name.lexeme.clone(),
                    parameters: parameters.iter().map(|p| p.lexeme.clone()).collect(),
                    body: self.statements_of_block(*body.clone()),
                    closure: environment.clone(),
                };
                environment.define(&name.lexeme, Literal::Callable(Callable::User(function)));
                Ok(StmtResult::Normal)
            }
            RuntimeStmt::Return { value, .. } => {
                Ok(StmtResult::Return(self.evaluate_expr(value, environment)?))
            }
            _ => todo!(),
        }
    }

    pub fn evaluate_block(
        &self,
        statements: &[RuntimeStmt],
        parent_environment: &Environment,
    ) -> Result<StmtResult> {
        let local = Environment::new(Some(parent_environment.clone()));

        for stmt in statements {
            match self.evaluate_stmt(stmt, &local)? {
                StmtResult::Normal => {}
                rst => return Ok(rst),
            }
        }
        Ok(StmtResult::Normal)
    }

    fn evaluate_expr(
        &self,
        expression: &RuntimeExpr,
        environment: &Environment,
    ) -> Result<Literal> {
        match expression {
            RuntimeExpr::Variable { name } => {
                environment
                    .get(&name.lexeme)
                    .map_err(|e| RuntimeError::UndefinedVariable {
                        line: name.line,
                        source: e,
                    })
            }
            RuntimeExpr::Literal { value } => Ok(value.clone()),
            RuntimeExpr::Grouping { expression } => self.evaluate_expr(expression, environment),
            RuntimeExpr::Unary { operator, right } => {
                let right_lit = self.evaluate_expr(right, environment)?;

                match operator.token_type {
                    TokenType::Minus => {
                        Ok(Literal::Number(-right_lit.try_into().map_err(|e| {
                            TypeSnafu {
                                line: operator.line,
                                source: e,
                            }
                            .build()
                        })?))
                    }
                    TokenType::Bang => Ok(Literal::Boolean(!right_lit.is_truthy())),
                    _ => unreachable!("only minus and bang are unary operator"),
                }
            }
            RuntimeExpr::Binary {
                left,
                operator,
                right,
            } => {
                let left_lit = self.evaluate_expr(left, environment)?;
                let right_lit = self.evaluate_expr(right, environment)?;

                let line = operator.line;
                match operator.token_type {
                    TokenType::Minus => Ok(Literal::Number(
                        self.try_to_f64(left_lit, line)? - self.try_to_f64(right_lit, line)?,
                    )),
                    TokenType::Slash => Ok(Literal::Number(
                        self.try_to_f64(left_lit, line)? / self.try_to_f64(right_lit, line)?,
                    )),
                    TokenType::Star => Ok(Literal::Number(
                        self.try_to_f64(left_lit, line)? * self.try_to_f64(right_lit, line)?,
                    )),
                    TokenType::Plus => Ok(self.handle_plus(&left_lit, &right_lit)?),
                    TokenType::Greater => Ok(Literal::Boolean(
                        self.try_to_f64(left_lit, line)? > self.try_to_f64(right_lit, line)?,
                    )),
                    TokenType::GreaterEqual => Ok(Literal::Boolean(
                        self.try_to_f64(left_lit, line)? >= self.try_to_f64(right_lit, line)?,
                    )),
                    TokenType::Less => Ok(Literal::Boolean(
                        self.try_to_f64(left_lit, line)? < self.try_to_f64(right_lit, line)?,
                    )),
                    TokenType::LessEqual => Ok(Literal::Boolean(
                        self.try_to_f64(left_lit, line)? <= self.try_to_f64(right_lit, line)?,
                    )),
                    TokenType::EqualEqual => Ok(Literal::Boolean(left_lit == right_lit)),
                    TokenType::BangEqual => Ok(Literal::Boolean(left_lit != right_lit)),
                    _ => unreachable!(),
                }
            }
            RuntimeExpr::Assign { name, value } => {
                let value = self.evaluate_expr(value, environment)?;
                environment
                    .assign(&name.lexeme, value.clone())
                    .map_err(|e| RuntimeError::UndefinedVariable {
                        line: name.line,
                        source: e,
                    })?;
                Ok(value)
            }
            RuntimeExpr::Logical {
                left,
                operator,
                right,
            } => {
                let left = self.evaluate_expr(left, environment)?;
                if operator.token_type == TokenType::Or {
                    if left.is_truthy() {
                        return Ok(left);
                    }
                } else if !left.is_truthy() {
                    return Ok(left);
                }
                self.evaluate_expr(right, environment)
            }
            RuntimeExpr::Call {
                callee,
                paren,
                arguments,
            } => {
                let callee = self.evaluate_expr(callee, environment)?;

                let arguments: Result<Vec<Literal>> = arguments
                    .iter()
                    .map(|expr| self.evaluate_expr(expr, environment))
                    .collect();
                let arguments = arguments?;

                match callee {
                    Literal::Callable(function) => {
                        if arguments.len() != function.arity() {
                            UnmatchedArgumentsSnafu {
                                line: paren.line,
                                expect: function.arity(),
                                found: arguments.len(),
                            }
                            .fail()
                        } else {
                            function.call(self, &arguments)
                        }
                    }
                    _ => NotCallableSnafu {
                        line: paren.line,
                        identifier: callee.to_string(),
                    }
                    .fail(),
                }
            }
            _ => todo!(),
        }
    }

    fn handle_plus(&self, left: &Literal, right: &Literal) -> Result<Literal> {
        match (left, right) {
            (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Number(l + r)),
            (Literal::String(_), _) | (_, Literal::String(_)) => {
                Ok(Literal::String(format!("{}{}", left, right)))
            }
            _ => todo!(),
        }
    }

    fn statements_of_block(&self, block_statement: RuntimeStmt) -> Vec<RuntimeStmt> {
        match block_statement {
            RuntimeStmt::Block(statements) => statements,
            _ => unreachable!(),
        }
    }

    fn try_to_f64(&self, lit: Literal, line: usize) -> Result<f64> {
        TryInto::<f64>::try_into(lit).map_err(|e| TypeSnafu { line, source: e }.build())
    }
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
}
