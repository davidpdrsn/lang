use crate::ast::*;
use crate::error::{Error, Result};
use pest::Span;
use std::collections::HashMap;
use std::fmt;
use std::io::Write;

pub struct Evaluator;

impl Evaluator {
    pub fn evaluate<'a, W: Write>(program: Program<'a>, stdout: &mut W) -> Result<'a, ()> {
        let span = program.span.clone();

        let fn_env = build_fn_env(program);
        let call = main_call(span);

        let mut env = LocalEnv::new();
        Evaluator.eval_call(&call, &fn_env, &mut env, stdout)?;

        Ok(())
    }
}

impl Evaluator {
    fn eval_call<'a, W: Write>(
        &self,
        call: &Call<'a>,
        fn_env: &FnEnv<'a, W>,
        env: &mut LocalEnv<'a>,
        stdout: &mut W,
    ) -> Result<'a, Value> {
        let name_to_call = call.name.name;

        let function = fn_env
            .get(name_to_call)
            .ok_or_else(|| Error::UndefinedFunction(name_to_call.to_string(), call.span.clone()))?;

        match function {
            FnEnvEntry::Function(f) => {
                let mut inner_env = LocalEnv::new();
                for (param, arg) in f.parameters.0.iter().zip(&call.args) {
                    let value = self.eval_expr(arg, fn_env, env, stdout)?;
                    inner_env.insert(param.name, value);
                }

                let mut return_value = Value::Void;
                for statement in &f.body {
                    return_value =
                        self.eval_statement(statement, fn_env, &mut inner_env, stdout)?;
                }
                Ok(return_value)
            }
            FnEnvEntry::BuiltIn(f) => {
                let mut inner_env = LocalEnv::new();
                for (name, arg) in ["input"].iter().zip(&call.args) {
                    let value = self.eval_expr(arg, fn_env, env, stdout)?;
                    inner_env.insert(name, value);
                }

                f(stdout, &mut inner_env)
            }
        }
    }

    fn eval_statement<'a, W: Write>(
        &self,
        stmt: &Statement<'a>,
        fn_env: &FnEnv<'a, W>,
        env: &mut LocalEnv<'a>,
        stdout: &mut W,
    ) -> Result<'a, Value> {
        match stmt {
            Statement::Call(call) => self.eval_call(call, fn_env, env, stdout),
        }
    }

    fn eval_expr<'a, W: Write>(
        &self,
        expr: &Expr<'a>,
        fn_env: &FnEnv<'a, W>,
        env: &LocalEnv<'a>,
        stdout: &mut W,
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
        }
    }
}

type FnEnv<'a, W> = HashMap<&'a str, FnEnvEntry<'a, W>>;
type LocalEnv<'a> = HashMap<&'a str, Value>;

type BuiltIn<'a, W> = Box<Fn(&mut W, &mut LocalEnv<'a>) -> Result<'a, Value>>;

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
        let input = env.get("input").expect("Built-in `println` argument not found");
        writeln!(stdout, "{}", input);
        Ok(Value::Void)
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
    Void,
    String(String),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Void => write!(f, "void"),
            Value::String(s) => write!(f, "{}", s),
        }
    }
}
