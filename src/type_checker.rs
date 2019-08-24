use crate::ast::*;
use crate::error::{Error, Result};
use crate::utils::EnvStack;
use pest::Span;
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

    let return_type = &function.return_type;
    check_statements(&function.body, fn_env, return_type, &mut env)?;

    Ok(())
}

fn check_statements<'a>(
    stmts: &[Statement<'a>],
    fn_env: &FnEnv<'a>,
    return_type: &Option<Type>,
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
    return_type: &Option<Type>,
    env: &mut Env<'a>,
) -> Result<'a, ()> {
    match stmt {
        Statement::Call(call) => {
            check_call(call, fn_env, &env)?;
        }

        Statement::Return(inner) => {
            let type_ = check_expr(&inner.expr, &return_type, fn_env, &env)
                .not_void(&inner.span)?;

            if let Some(return_type) = return_type {
                if &type_ != return_type {
                    Err(Error::TypeError {
                        expected: type_.clone(),
                        got: return_type.clone(),
                        span: inner.span.clone(),
                    })?;
                }
            } else {
                Err(Error::ReturnFromVoidFunction(inner.span.clone()))?;
            }
        }

        Statement::VariableBinding(binding) => {
            let name = &binding.name.name;
            let type_hint = &binding.type_;
            let type_ = check_expr(&binding.expr, type_hint, fn_env, &env)
                .not_void(&binding.span)?;
            env.insert(name, type_);
        }

        Statement::ReassignVariable(binding) => {
            let name = &binding.name.name;

            let prev_type = if let Some(prev_type) = env.get(name) {
                prev_type
            } else {
                return Err(Error::UndefinedLocalVariable(
                    name.to_string(),
                    binding.span.clone(),
                ));
            };

            let type_ = check_expr(
                &binding.expr,
                &Some(prev_type.clone()),
                fn_env,
                &env,
            )
            .not_void(&binding.span)?;

            if &type_ != prev_type {
                Err(Error::TypeError {
                    expected: prev_type.clone(),
                    got: type_.clone(),
                    span: binding.span.clone(),
                })?;
            }
        }

        Statement::IfStatement(if_stmt) => {
            let condition = &if_stmt.condition;

            // TODO: What would a type hint here do?
            let cond_type = check_expr(condition, &None, fn_env, &env)
                .not_void(&if_stmt.span)?;

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

fn inner_type(type_: &Type) -> Option<Type> {
    match type_ {
        Type::Boolean | Type::String | Type::Integer => None,
        Type::List(inner) => Some(inner.as_ref().clone()),
    }
}

fn check_expr<'a>(
    expr: &Expr<'a>,
    hint: &Option<Type>,
    fn_env: &FnEnv<'a>,
    env: &Env<'a>,
) -> Result<'a, Option<Type>> {
    match expr {
        Expr::StringLit(_) => Ok(Some(Type::String)),
        Expr::IntegerLit(_) => Ok(Some(Type::Integer)),
        Expr::BooleanLit(_) => Ok(Some(Type::Boolean)),

        Expr::ListLit(list) => {
            let span = list.span.clone();

            if list.elements.is_empty() {
                let type_ = if let Some(type_) = hint.as_ref() {
                    type_
                } else {
                    return Err(Error::CannotInferType(span));
                };
                Ok(Some(type_.clone()))
            } else if list.elements.len() == 1 {
                let inner_type = hint.as_ref().and_then(|h| inner_type(&h));

                let first =
                    check_expr(&list.elements[0], &inner_type, fn_env, env)
                        .not_void(&span)?;

                Ok(Some(Type::List(Box::new(first))))
            } else {
                let inner_type = hint.as_ref().and_then(|h| inner_type(&h));
                let first =
                    check_expr(&list.elements[0], &inner_type, fn_env, env)
                        .not_void(&span)?;

                for element in &list.elements[1..] {
                    let other = check_expr(element, &inner_type, fn_env, env)
                        .not_void(&span)?;

                    if first != other {
                        Err(Error::TypeError {
                            expected: first.clone(),
                            got: other.clone(),
                            span: list.span.clone(),
                        })?;
                    }
                }

                Ok(Some(Type::List(Box::new(first))))
            }
        }

        Expr::LocalVariable(ident) => {
            let name = &ident.name.name;

            if let Some(value) = env.get(name) {
                Ok(Some(value.clone()))
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
) -> Result<'a, Option<Type>> {
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
        let type_ =
            check_expr(expr, &Some(required_type.clone()), fn_env, &env)
                .not_void(&span)?;

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
    return_type: Option<Type>,
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
            return_type: None,
        },
    );

    fn_env.insert(
        "int_to_string",
        FunctionType {
            arg_types: vec![Type::Integer],
            return_type: Some(Type::String),
        },
    );

    fn_env.insert(
        "bool_to_string",
        FunctionType {
            arg_types: vec![Type::Boolean],
            return_type: Some(Type::String),
        },
    );

    fn_env.insert(
        "length",
        FunctionType {
            // TODO: Make this work for any list type
            arg_types: vec![Type::List(Box::new(Type::Integer))],
            return_type: Some(Type::Integer),
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

trait NotVoid<'a> {
    fn not_void(self, span: &Span<'a>) -> Result<'a, Type>;
}

impl<'a> NotVoid<'a> for Result<'a, Option<Type>> {
    fn not_void(self, span: &Span<'a>) -> Result<'a, Type> {
        match self {
            Err(e) => Err(e),
            Ok(Some(t)) => Ok(t),
            Ok(None) => Err(Error::VoidTypeUsed(span.clone())),
        }
    }
}
