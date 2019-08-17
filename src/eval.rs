use crate::ast::*;
use crate::error::{Error, Result};
use pest::Span;
use std::collections::HashMap;
use std::fmt;
use std::io::Write;

pub struct Evaluator<'a, W> {
    stdout: &'a mut W,
}

impl<'a, W: Write> Evaluator<'a, W> {
    pub fn evaluate(program: Program<'a>, stdout: &'a mut W) -> Result<'a, ()> {
        let span = program.span.clone();

        let fn_env = build_fn_env(program);
        let call = main_call(span);

        let env = LocalEnv::new();
        Evaluator::new(stdout).eval_call(&call, &fn_env, &env)?;

        Ok(())
    }

    fn new(stdout: &'a mut W) -> Self {
        Evaluator { stdout }
    }

    fn eval_statement(
        &mut self,
        stmt: &Statement<'a>,
        fn_env: &FnEnv<'a, W>,
        env: &mut LocalEnv<'a>,
    ) -> Result<'a, StatementEvalResult> {
        match stmt {
            Statement::Call(call) => {
                self.eval_call(call, fn_env, env)?;
                Ok(StatementEvalResult::Value)
            }

            Statement::Return(return_statement) => {
                let value = self.eval_expr(&return_statement.expr, fn_env, env)?;
                Ok(StatementEvalResult::Return(value))
            }

            Statement::VariableBinding(binding) => {
                let name = &binding.name.name;
                let value = self.eval_expr(&binding.expr, fn_env, env)?;
                env.insert(name, value);
                Ok(StatementEvalResult::Value)
            }
        }
    }

    fn eval_call(
        &mut self,
        call: &Call<'a>,
        fn_env: &FnEnv<'a, W>,
        env: &LocalEnv<'a>,
    ) -> Result<'a, Option<Value>> {
        let name_to_call = call.name.name;

        let function = fn_env
            .get(name_to_call)
            .ok_or_else(|| Error::UndefinedFunction(name_to_call.to_string(), call.span.clone()))?;

        match function {
            FnEnvEntry::Function(f) => {
                let mut inner_env = LocalEnv::new();
                // TODO: assert sizes are equal
                for (param, arg) in f.parameters.0.iter().zip(&call.args) {
                    let value = self.eval_expr(arg, fn_env, env)?;
                    inner_env.insert(param.name, value);
                }

                for statement in &f.body {
                    match self.eval_statement(statement, fn_env, &mut inner_env)? {
                        StatementEvalResult::Value => {}
                        StatementEvalResult::Return(value) => return Ok(Some(value)),
                    }
                }
                Ok(None)
            }
            FnEnvEntry::BuiltIn(f) => {
                let mut inner_env = LocalEnv::new();
                // TODO: assert only given one arg
                for (name, arg) in ["input"].iter().zip(&call.args) {
                    let value = self.eval_expr(arg, fn_env, env)?;
                    inner_env.insert(name, value);
                }

                f(&mut self.stdout, &mut inner_env)
            }
        }
    }

    fn eval_expr(
        &mut self,
        expr: &Expr<'a>,
        fn_env: &FnEnv<'a, W>,
        env: &LocalEnv<'a>,
    ) -> Result<'a, Value> {
        match expr {
            Expr::StringLit(string_lit) => Ok(Value::String(string_lit.contents.to_string())),

            Expr::LocalVariable(ident) => {
                let name = &ident.name.name;
                let value = env.get(name).ok_or_else(|| {
                    Error::UndefinedLocalVariable(name.to_string(), ident.span.clone())
                })?;
                Ok(value.clone())
            }

            Expr::Call(call) => {
                let value = self
                    .eval_call(call, fn_env, env)?
                    .expect("call expression returned void. Should be caught by type checking");
                Ok(value)
            }
        }
    }
}

enum StatementEvalResult {
    Value,
    Return(Value),
}

type FnEnv<'a, W> = HashMap<&'a str, FnEnvEntry<'a, W>>;
type LocalEnv<'a> = HashMap<&'a str, Value>;

type BuiltIn<'a, W> = Box<Fn(&mut W, &mut LocalEnv<'a>) -> Result<'a, Option<Value>>>;

enum FnEnvEntry<'a, W> {
    Function(Function<'a>),
    BuiltIn(BuiltIn<'a, W>),
}

fn build_fn_env<'a, W: Write>(program: Program<'a>) -> FnEnv<'a, W> {
    let mut fn_env = HashMap::new();

    for function in program.functions {
        fn_env.insert(function.name.name, FnEnvEntry::Function(function));
    }

    fn_env.insert("println", FnEnvEntry::BuiltIn(println_built_in()));

    fn_env
}

fn println_built_in<'a, W: Write>() -> BuiltIn<'a, W> {
    Box::new(|stdout: &mut W, env: &mut LocalEnv<'a>| {
        let input = env
            .get("input")
            .expect("Built-in `println` argument not found");
        writeln!(stdout, "{}", input);
        Ok(None)
    })
}

fn main_call(span: Span) -> Call {
    Call {
        name: Ident {
            name: "main",
            span: span.clone(),
        },
        args: vec![],
        span: span.clone(),
    }
}

#[derive(Debug, Clone)]
enum Value {
    String(String),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::String(s) => write!(f, "{}", s),
        }
    }
}
