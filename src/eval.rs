use crate::ast::*;
use crate::error::{Error, Result};
use pest::Span;
use std::collections::HashMap;
use std::hash::Hash;
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
                Ok(StatementEvalResult::Void)
            }

            Statement::Return(return_statement) => {
                let value = self.eval_expr(&return_statement.expr, fn_env, env)?;
                Ok(StatementEvalResult::Return(value))
            }

            Statement::VariableBinding(binding) => {
                let name = &binding.name.name;
                let value = self.eval_expr(&binding.expr, fn_env, env)?;
                env.insert(name, value);
                Ok(StatementEvalResult::Void)
            }

            Statement::ReassignVariable(binding) => {
                let name = &binding.name.name;

                if !env.contains_key(name) {
                    return Err(Error::UndefinedLocalVariable(
                        name.to_string(),
                        binding.span.clone(),
                    ));
                }

                let value = self.eval_expr(&binding.expr, fn_env, env)?;
                env.insert(name, value);
                Ok(StatementEvalResult::Void)
            }

            Statement::IfStatement(if_stmt) => {
                let condition = self.eval_expr(&if_stmt.condition, fn_env, env)?;

                let branch = if let Value::Boolean(condition) = condition {
                    if condition {
                        Some(&if_stmt.then_branch)
                    } else {
                        if_stmt.else_branch.as_ref()
                    }
                } else {
                    panic!("type error. If condition must be of type bool")
                };

                if let Some(branch) = branch {
                    env.push_env();
                    for stmt in branch {
                        match self.eval_statement(stmt, fn_env, env)? {
                            StatementEvalResult::Void => {}
                            StatementEvalResult::Return(value) => {
                                return Ok(StatementEvalResult::Return(value))
                            }
                        }
                    }
                    env.pop_env();
                }

                Ok(StatementEvalResult::Void)
            }
        }
    }

    fn eval_call(
        &mut self,
        call: &Call<'a>,
        fn_env: &FnEnv<'a, W>,
        env: &LocalEnv<'a>,
    ) -> Result<'a, Option<Value>> {
        let span = call.span.clone();
        let name_to_call = call.name.name;

        let function = fn_env
            .get(name_to_call)
            .ok_or_else(|| Error::UndefinedFunction(name_to_call.to_string(), call.span.clone()))?;

        match function {
            FnEnvEntry::Function(f) => {
                let mut inner_env = LocalEnv::new();

                if f.parameters.0.len() != call.args.len() {
                    return Err(Error::WrongNumberOfArguments {
                        expected: f.parameters.0.len(),
                        got: call.args.len(),
                        span,
                    });
                }

                for (param, arg) in f.parameters.0.iter().zip(&call.args) {
                    let value = self.eval_expr(arg, fn_env, env)?;
                    inner_env.insert(param.name, value);
                }

                for statement in &f.body {
                    match self.eval_statement(statement, fn_env, &mut inner_env)? {
                        StatementEvalResult::Void => {}
                        StatementEvalResult::Return(value) => return Ok(Some(value)),
                    }
                }
                Ok(None)
            }
            FnEnvEntry::BuiltIn(f) => {
                let mut inner_env = LocalEnv::new();

                if call.args.len() != 1 {
                    return Err(Error::WrongNumberOfArguments {
                        expected: 1,
                        got: call.args.len(),
                        span,
                    });
                }

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
            Expr::StringLit(lit) => Ok(Value::String(lit.contents.to_string())),

            Expr::IntegerLit(lit) => Ok(Value::Integer(lit.integer)),

            Expr::BooleanLit(lit) => Ok(Value::Boolean(lit.boolean)),

            Expr::ListLit(lit) => {
                let mut acc = vec![];
                for element in &lit.elements {
                    acc.push(self.eval_expr(&element, fn_env, env)?);
                }
                Ok(Value::List(acc))
            }

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
    Void,
    Return(Value),
}

type FnEnv<'a, W> = HashMap<&'a str, FnEnvEntry<'a, W>>;
type LocalEnv<'a> = EnvStack<&'a str, Value>;

struct EnvStack<K, V> {
    bottom: HashMap<K, V>,
    stack: Vec<HashMap<K, V>>,
}

impl<K: Hash + Ord + Eq, V> EnvStack<K, V> {
    fn new() -> Self {
        EnvStack {
            bottom: HashMap::new(),
            stack: Vec::new(),
        }
    }

    fn insert(&mut self, key: K, value: V) -> Option<V> {
        if let Some(current_value) = self.get_mut(&key) {
            let prev = std::mem::replace(current_value, value);
            return Some(prev);
        }

        if let Some(env) = self.stack.last_mut() {
            env.insert(key, value)
        } else {
            self.bottom.insert(key, value)
        }
    }

    fn get<Q: ?Sized>(&self, key: &Q) -> Option<&V>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq,
    {
        for env in self.stack.iter().rev() {
            if let Some(value) = env.get(key) {
                return Some(value);
            }
        }
        self.bottom.get(key)
    }

    fn get_mut<Q: ?Sized>(&mut self, key: &Q) -> Option<&mut V>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq,
    {
        for env in self.stack.iter_mut().rev() {
            if let Some(value) = env.get_mut(key) {
                return Some(value);
            }
        }
        self.bottom.get_mut(key)
    }

    fn contains_key<Q: ?Sized>(&self, key: &Q) -> bool
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq,
    {
        self.get(key).is_some()
    }

    fn push_env(&mut self) {
        self.stack.push(HashMap::new())
    }

    fn pop_env(&mut self) -> Option<HashMap<K, V>> {
        if !self.stack.is_empty() {
            Some(self.stack.remove(self.stack.len() - 1))
        } else {
            None
        }
    }
}

type BuiltIn<'a, W> = Box<Fn(&mut W, &mut LocalEnv<'a>) -> Result<'a, Option<Value>>>;

enum FnEnvEntry<'a, W> {
    Function(Function<'a>),
    BuiltIn(BuiltIn<'a, W>),
}

fn build_fn_env<W: Write>(program: Program) -> FnEnv<W> {
    let mut fn_env = HashMap::new();

    for function in program.functions {
        fn_env.insert(function.name.name, FnEnvEntry::Function(function));
    }

    fn_env.insert("println", FnEnvEntry::BuiltIn(println_built_in()));
    fn_env.insert(
        "int_to_string",
        FnEnvEntry::BuiltIn(int_to_string_built_in()),
    );
    fn_env.insert(
        "bool_to_string",
        FnEnvEntry::BuiltIn(bool_to_string_built_in()),
    );
    fn_env.insert("length", FnEnvEntry::BuiltIn(length_built_in()));

    fn_env
}

fn println_built_in<'a, W: Write>() -> BuiltIn<'a, W> {
    Box::new(|stdout: &mut W, env: &mut LocalEnv<'a>| {
        let input = env
            .get("input")
            .expect("Built-in `println` argument not found");

        if let Value::String(input) = input {
            writeln!(stdout, "{}", input).expect("`println` failed to write");
            Ok(None)
        } else {
            panic!("Type error. `println` only supports strings")
        }
    })
}

fn int_to_string_built_in<'a, W: Write>() -> BuiltIn<'a, W> {
    Box::new(|_stdout: &mut W, env: &mut LocalEnv<'a>| {
        let input = env
            .get("input")
            .expect("Built-in `int_to_string` argument not found");

        if let Value::Integer(i) = input {
            let string = format!("{}", i);
            Ok(Some(Value::String(string)))
        } else {
            unimplemented!("type error")
        }
    })
}

fn bool_to_string_built_in<'a, W: Write>() -> BuiltIn<'a, W> {
    Box::new(|_stdout: &mut W, env: &mut LocalEnv<'a>| {
        let input = env
            .get("input")
            .expect("Built-in `bool_to_string` argument not found");

        if let Value::Boolean(i) = input {
            let string = format!("{}", i);
            Ok(Some(Value::String(string)))
        } else {
            unimplemented!("type error")
        }
    })
}

fn length_built_in<'a, W: Write>() -> BuiltIn<'a, W> {
    Box::new(|_stdout: &mut W, env: &mut LocalEnv<'a>| {
        let input = env
            .get("input")
            .expect("Built-in `length` argument not found");

        if let Value::List(list) = input {
            Ok(Some(Value::Integer(list.len() as i32)))
        } else {
            unimplemented!("type error")
        }
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
    Integer(i32),
    Boolean(bool),
    List(Vec<Value>),
}
