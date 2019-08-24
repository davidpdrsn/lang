use crate::{
    ast::{ParseError, Type},
    eval::CallStack,
    utils::LineAndCol,
    Rule,
};
use pest::Span;
use std::fmt;

pub type Result<'a, T> = std::result::Result<T, Error<'a>>;

#[derive(Debug)]
pub enum Error<'a> {
    WithCallStack(CallStack<'a>, Box<Error<'a>>),
    TokenizeError(pest::error::Error<Rule>),
    ParseError(ParseError),
    UndefinedFunction(String, Span<'a>),
    UndefinedLocalVariable(String, Span<'a>),
    WrongNumberOfArguments {
        expected: usize,
        got: usize,
        span: Span<'a>,
    },
    TypeError {
        expected: Type,
        got: Type,
        span: Span<'a>,
    },
    CannotInferType(Span<'a>),
    ReturnFromVoidFunction(Span<'a>),
    VoidTypeUsed(Span<'a>),
}

impl<'a> From<pest::error::Error<Rule>> for Error<'a> {
    fn from(source: pest::error::Error<Rule>) -> Self {
        Error::TokenizeError(source)
    }
}

impl<'a> From<ParseError> for Error<'a> {
    fn from(source: ParseError) -> Self {
        Error::ParseError(source)
    }
}

impl<'a> fmt::Display for Error<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Error::*;

        match self {
            WithCallStack(stack, error) => {
                for frame in stack {
                    writeln!(f, "{}", frame)?;
                }
                write!(f, "{}", error)
            }
            TokenizeError(inner) => write!(f, "{}", inner),
            ParseError(inner) => write!(f, "{}", inner),
            UndefinedFunction(name, span) => {
                let (line, col) = span.line_and_col();
                write!(f, "Undefined function `{}` at {}:{}", name, line, col,)?;
                Ok(())
            }
            UndefinedLocalVariable(name, span) => {
                let (line, col) = span.line_and_col();
                write!(
                    f,
                    "Undefined local variable `{}` at {}:{}",
                    name, line, col,
                )?;
                Ok(())
            }
            WrongNumberOfArguments {
                expected,
                got,
                span,
            } => {
                let (line, col) = span.line_and_col();
                write!(
                    f,
                    "Wrong number of arguments at {}:{}. Expected {} got {}",
                    line, col, expected, got
                )
            }
            TypeError {
                expected,
                got,
                span,
            } => {
                let (line, col) = span.line_and_col();
                write!(
                    f,
                    "Type error at {}:{}. Expected {} got {}",
                    line, col, expected, got
                )
            }
            CannotInferType(span) => {
                let (line, col) = span.line_and_col();
                write!(
                    f,
                    "Type error at {}:{}. The type cannot be inferred. Add type annotation",
                    line, col
                )
            }
            ReturnFromVoidFunction(span) => {
                let (line, col) = span.line_and_col();
                write!(
                    f,
                    "Type error at {}:{}. You cannot return from functions without a return type",
                    line, col
                )
            }
            VoidTypeUsed(span) => {
                let (line, col) = span.line_and_col();
                write!(
                    f,
                    "Type error at {}:{}. Expressions without a type cannot be used in other statements or expressions",
                    line, col
                )
            }
        }
    }
}

impl<'a> std::error::Error for Error<'a> {}
