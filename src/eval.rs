use crate::{
    ast::*,
    error::{Error, Result},
    utils::{EnvStack, LineAndCol, Stack},
};
use pest::Span;
use std::{collections::HashMap, fmt, hash::Hash, io::Write};

type FnEnv<'a, W> = HashMap<&'a str, FnEnvEntry<'a, W>>;
type LocalEnv<'a> = EnvStack<&'a str, Value>;
pub type CallStack<'a> = Stack<StackFrame<'a>>;

pub struct Evaluator<'a, W> {
    stdout: &'a mut W,
    call_stack: CallStack<'a>,
}

impl<'a, W: Write> Evaluator<'a, W> {
    pub fn evaluate(program: Program<'a>, stdout: &'a mut W) -> Result<'a, ()> {
        let span = program.span.clone();

        let fn_env = build_fn_env(program);
        let call = main_call(span);

        let mut eval = Evaluator::new(stdout);
        let result = eval.eval_call(&call, &fn_env, &LocalEnv::new());
        match result {
            Ok(_) => Ok(()),
            Err(error) => {
                Err(Error::WithCallStack(eval.call_stack, Box::new(error)))
            }
        }
    }

    fn new(stdout: &'a mut W) -> Self {
        Evaluator {
            stdout,
            call_stack: CallStack::new(),
        }
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
                let value =
                    self.eval_expr(&return_statement.expr, fn_env, env)?;
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
                let condition =
                    self.eval_expr(&if_stmt.condition, fn_env, env)?;

                let branch = if let Value::Boolean(condition) = condition {
                    if condition {
                        Some(&if_stmt.then_branch)
                    } else {
                        if_stmt.else_branch.as_ref()
                    }
                } else {
                    unreachable!("type error in eval (IfStatement)")
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

        let function = fn_env.get(name_to_call).ok_or_else(|| {
            Error::UndefinedFunction(
                name_to_call.to_string(),
                call.span.clone(),
            )
        })?;

        self.call_stack.push(StackFrame {
            fn_name: name_to_call,
            span: span.clone(),
        });

        let result = match function {
            FnEnvEntry::Function(f) => {
                let mut inner_env = LocalEnv::new();

                if f.parameters.0.len() != call.args.len() {
                    unreachable!("type error in eval (eval_call)")
                }

                for (param, arg) in f.parameters.0.iter().zip(&call.args) {
                    let value = self.eval_expr(arg, fn_env, env)?;
                    inner_env.insert(param.0.name, value);
                }

                for statement in &f.body {
                    match self.eval_statement(
                        statement,
                        fn_env,
                        &mut inner_env,
                    )? {
                        StatementEvalResult::Void => {}
                        StatementEvalResult::Return(value) => {
                            return Ok(Some(value))
                        }
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
        };

        self.call_stack.pop();

        result
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

            Expr::Add { lhs, rhs, .. } => {
                let lhs = self.eval_expr(&lhs, fn_env, env)?.as_integer();
                let rhs = self.eval_expr(&rhs, fn_env, env)?.as_integer();
                Ok(Value::Integer(lhs + rhs))
            }

            Expr::Sub { lhs, rhs, .. } => {
                let lhs = self.eval_expr(&lhs, fn_env, env)?.as_integer();
                let rhs = self.eval_expr(&rhs, fn_env, env)?.as_integer();
                Ok(Value::Integer(lhs - rhs))
            }

            Expr::Mul { lhs, rhs, .. } => {
                let lhs = self.eval_expr(&lhs, fn_env, env)?.as_integer();
                let rhs = self.eval_expr(&rhs, fn_env, env)?.as_integer();
                Ok(Value::Integer(lhs * rhs))
            }

            Expr::Div { lhs, rhs, .. } => {
                let lhs = self.eval_expr(&lhs, fn_env, env)?.as_integer();
                let rhs = self.eval_expr(&rhs, fn_env, env)?.as_integer();
                Ok(Value::Integer(lhs / rhs))
            }

            Expr::And { lhs, rhs, .. } => {
                let lhs = self.eval_expr(&lhs, fn_env, env)?.as_boolean();
                let rhs = self.eval_expr(&rhs, fn_env, env)?.as_boolean();
                Ok(Value::Boolean(lhs && rhs))
            }

            Expr::Or { lhs, rhs, .. } => {
                let lhs = self.eval_expr(&lhs, fn_env, env)?.as_boolean();
                let rhs = self.eval_expr(&rhs, fn_env, env)?.as_boolean();
                Ok(Value::Boolean(lhs || rhs))
            }

            Expr::Eq { lhs, rhs, .. } => {
                let lhs = self.eval_expr(&lhs, fn_env, env)?;
                let rhs = self.eval_expr(&rhs, fn_env, env)?;
                Ok(Value::Boolean(lhs == rhs))
            }

            Expr::NotEq { lhs, rhs, .. } => {
                let lhs = self.eval_expr(&lhs, fn_env, env)?;
                let rhs = self.eval_expr(&rhs, fn_env, env)?;
                Ok(Value::Boolean(lhs != rhs))
            }

            Expr::Lt { lhs, rhs, .. } => {
                let lhs = self.eval_expr(&lhs, fn_env, env)?.as_integer();
                let rhs = self.eval_expr(&rhs, fn_env, env)?.as_integer();
                Ok(Value::Boolean(lhs < rhs))
            }

            Expr::Lte { lhs, rhs, .. } => {
                let lhs = self.eval_expr(&lhs, fn_env, env)?.as_integer();
                let rhs = self.eval_expr(&rhs, fn_env, env)?.as_integer();
                Ok(Value::Boolean(lhs <= rhs))
            }

            Expr::Gt { lhs, rhs, .. } => {
                let lhs = self.eval_expr(&lhs, fn_env, env)?.as_integer();
                let rhs = self.eval_expr(&rhs, fn_env, env)?.as_integer();
                Ok(Value::Boolean(lhs > rhs))
            }

            Expr::Gte { lhs, rhs, .. } => {
                let lhs = self.eval_expr(&lhs, fn_env, env)?.as_integer();
                let rhs = self.eval_expr(&rhs, fn_env, env)?.as_integer();
                Ok(Value::Boolean(lhs >= rhs))
            }

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
                    Error::UndefinedLocalVariable(
                        name.to_string(),
                        ident.span.clone(),
                    )
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

type BuiltIn<'a, W> =
    Box<Fn(&mut W, &mut LocalEnv<'a>) -> Result<'a, Option<Value>>>;

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
    fn_env.insert("length", FnEnvEntry::BuiltIn(length_built_in()));

    fn_env
}

fn println_built_in<'a, W: Write>() -> BuiltIn<'a, W> {
    Box::new(|stdout: &mut W, env: &mut LocalEnv<'a>| {
        let input = env
            .get("input")
            .expect("Built-in `println` argument not found");

        writeln!(stdout, "{}", input).expect("`println` failed to write");
        Ok(None)
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
            unreachable!("type error in eval (length_built_in)")
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

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
enum Value {
    String(String),
    Integer(i32),
    Boolean(bool),
    List(Vec<Value>),
}

impl Value {
    fn as_integer<'a>(self) -> i32 {
        if let Value::Integer(x) = self {
            x
        } else {
            unreachable!("type error in eval (as_integer)")
        }
    }

    fn as_boolean<'a>(self) -> bool {
        if let Value::Boolean(x) = self {
            x
        } else {
            unreachable!("type error in eval (as_boolean)")
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;

        match self {
            String(x) => write!(f, "{}", x),
            Integer(x) => write!(f, "{}", x),
            Boolean(x) => write!(f, "{}", x),
            List(x) => {
                let inner = x
                    .iter()
                    .map(|y| format!("{}", y))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "[{}]", inner)
            }
        }
    }
}

#[derive(Debug)]
pub struct StackFrame<'a> {
    fn_name: &'a str,
    span: Span<'a>,
}

impl<'a> fmt::Display for StackFrame<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (line, col) = self.span.line_and_col();
        write!(f, "{} {}:{}", self.fn_name, line, col)
    }
}
