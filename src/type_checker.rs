use crate::ast::*;
use pest::Span;
use crate::error::{Error, Result};
use crate::eval::EnvStack;
use std::collections::HashMap;

pub struct TypeChecker;

impl TypeChecker {
    pub fn new() -> Self {
        Self
    }

    pub fn check<'a>(&self, program: &Program<'a>) -> Result<'a, ()> {
        let fn_env = build_fn_env(program);

        let main_call = main_call(program.span.clone());
        check_call(&main_call, &fn_env, &Env::new())?;

        for function in &program.functions {
            check_function(function, &fn_env)?;
        }

        Ok(())
    }
}

type FnEnv<'a> = HashMap<&'a str, FunctionType>;
type Env<'a> = EnvStack<&'a str, Type>;

fn check_function<'a>(
    function: &Function<'a>,
    fn_env: &FnEnv<'a>,
) -> Result<'a, ()> {
    let mut env = Env::new();
    for (ident, type_) in &function.parameters.0 {
        env.insert(&ident.name, type_.clone());
    }

    let mut return_type = None::<Type>;

    check_statements(&function.body, fn_env, &mut return_type, &mut env)?;

    let return_type = if let Some(return_type) = return_type {
        return_type
    } else {
        Type::Void
    };

    if &return_type != &function.return_type {
        Err(Error::TypeError {
            expected: function.return_type.clone(),
            got: return_type.clone(),
            span: function.span.clone(),
        })?;
    }

    Ok(())
}

fn check_statements<'a>(
    stmts: &[Statement<'a>],
    fn_env: &FnEnv<'a>,
    return_type: &mut Option<Type>,
    env: &mut Env<'a>,
) -> Result<'a, ()> {
    for stmt in stmts {
        check_statement(stmt, fn_env, return_type, env)?;
    }

    Ok(())
}

fn check_statement<'a>(
    stmt: &Statement<'a>,
    fn_env: &FnEnv<'a>,
    return_type: &mut Option<Type>,
    env: &mut Env<'a>,
) -> Result<'a, ()> {
    match stmt {
        Statement::Call(call) => {
            check_call(call, fn_env, &env)?;
        }

        Statement::Return(inner) => {
            let type_ = check_expr(&inner.expr, fn_env, &env)?;

            match return_type {
                Some(return_type) if { &type_ != return_type } => {
                    Err(Error::TypeError {
                        expected: type_.clone(),
                        got: return_type.clone(),
                        span: inner.span.clone(),
                    })?;
                }
                Some(_) => {}
                None => {
                    *return_type = Some(type_);
                }
            }
        }

        Statement::VariableBinding(binding) => {
            let name = &binding.name.name;
            let type_ = check_expr(&binding.expr, fn_env, &env)?;
            env.insert(name, type_);
        }

        Statement::ReassignVariable(binding) => {
            let type_ = check_expr(&binding.expr, fn_env, &env)?;
            let name = &binding.name.name;

            if let Some(prev_type) = env.get(name) {
                if &type_ != prev_type {
                    Err(Error::TypeError {
                        expected: prev_type.clone(),
                        got: type_.clone(),
                        span: binding.span.clone(),
                    })?;
                }
            } else {
                Err(Error::UndefinedLocalVariable(
                    name.to_string(),
                    binding.span.clone(),
                ))?;
            }
        }

        Statement::IfStatement(if_stmt) => {
            let condition = &if_stmt.condition;
            let cond_type = check_expr(condition, fn_env, &env)?;

            if cond_type != Type::Boolean {
                Err(Error::TypeError {
                    expected: Type::Boolean,
                    got: cond_type.clone(),
                    span: condition.span(),
                })?;
            }

            env.push_env();
            check_statements(&if_stmt.then_branch, fn_env, return_type, env)?;
            env.pop_env();

            if let Some(else_branch) = &if_stmt.else_branch {
                env.push_env();
                check_statements(else_branch, fn_env, return_type, env)?;
                env.pop_env();
            }
        }
    }

    Ok(())
}

fn check_expr<'a>(
    expr: &Expr<'a>,
    fn_env: &FnEnv<'a>,
    env: &Env<'a>,
) -> Result<'a, Type> {
    match expr {
        Expr::StringLit(_) => Ok(Type::String),
        Expr::IntegerLit(_) => Ok(Type::Integer),
        Expr::BooleanLit(_) => Ok(Type::Boolean),

        Expr::ListLit(list) => {
            if list.elements.is_empty() {
                unimplemented!("TODO: unknow type of empty list")
            } else if list.elements.len() == 1 {
                let first = check_expr(&list.elements[0], fn_env, env)?;
                Ok(Type::List(Box::new(first)))
            } else {
                let first = check_expr(&list.elements[0], fn_env, env)?;

                for element in &list.elements[1..] {
                    let other = check_expr(element, fn_env, env)?;
                    if first != other {
                        Err(Error::TypeError {
                            expected: first.clone(),
                            got: other.clone(),
                            span: list.span.clone(),
                        })?;
                    }
                }

                Ok(Type::List(Box::new(first)))
            }
        }

        Expr::LocalVariable(ident) => {
            let name = &ident.name.name;

            if let Some(value) = env.get(name) {
                Ok(value.clone())
            } else {
                Err(Error::UndefinedLocalVariable(
                    name.to_string(),
                    ident.span.clone(),
                ))
            }
        }

        Expr::Call(call) => check_call(call, fn_env, env),
    }
}

fn check_call<'a>(
    call: &Call<'a>,
    fn_env: &FnEnv<'a>,
    env: &Env<'a>,
) -> Result<'a, Type> {
    let span = call.span.clone();
    let name_to_call = call.name.name;

    let function_type = fn_env.get(name_to_call).ok_or_else(|| {
        Error::UndefinedFunction(name_to_call.to_string(), call.span.clone())
    })?;

    if function_type.arg_types.len() != call.args.len() {
        Err(Error::WrongNumberOfArguments {
            expected: function_type.arg_types.len(),
            got: call.args.len(),
            span: span.clone(),
        })?;
    }

    for (required_type, expr) in function_type.arg_types.iter().zip(&call.args)
    {
        let type_ = check_expr(expr, fn_env, &env)?;

        if required_type != &type_ {
            Err(Error::TypeError {
                expected: required_type.clone(),
                got: type_.clone(),
                span: span.clone(),
            })?;
        }
    }

    Ok(function_type.return_type.clone())
}

#[derive(Debug)]
struct FunctionType {
    arg_types: Vec<Type>,
    return_type: Type,
}

fn build_fn_env<'a>(program: &Program<'a>) -> FnEnv<'a> {
    let mut fn_env = HashMap::new();

    for function in &program.functions {
        let arg_types = function
            .parameters
            .0
            .iter()
            .map(|(_, type_)| type_.clone())
            .collect::<Vec<_>>();

        fn_env.insert(
            function.name.name,
            FunctionType {
                arg_types,
                return_type: function.return_type.clone(),
            },
        );
    }

    fn_env.insert(
        "println",
        FunctionType {
            arg_types: vec![Type::String],
            return_type: Type::Void,
        },
    );

    fn_env.insert(
        "int_to_string",
        FunctionType {
            arg_types: vec![Type::Integer],
            return_type: Type::String,
        },
    );

    fn_env.insert(
        "bool_to_string",
        FunctionType {
            arg_types: vec![Type::Boolean],
            return_type: Type::String,
        },
    );

    fn_env.insert(
        "length",
        FunctionType {
            // TODO: Make this work for any list type
            arg_types: vec![Type::List(Box::new(Type::Integer))],
            return_type: Type::Integer,
        },
    );

    fn_env
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
