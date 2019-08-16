use crate::ast::ParseError;
use crate::Rule;
use pest::Span;
use std::fmt;

pub type Result<'a, T> = std::result::Result<T, Error<'a>>;

#[derive(Debug)]
pub enum Error<'a> {
    TokenizeError(pest::error::Error<Rule>),
    ParseError(ParseError),
    NoMainFunction,
    UndefinedFunction(String, Span<'a>),
    UndefinedLocalVariable(String, Span<'a>),
    WrongNumberOfArguments { expected: usize, got: usize },
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
            TokenizeError(inner) => write!(f, "{}", inner),
            ParseError(inner) => write!(f, "{}", inner),
            NoMainFunction => write!(f, "No main function found"),
            UndefinedFunction(name, span) => {
                let (line, col) = span.start_pos().line_col();
                write!(
                    f,
                    "Undefined function `{}` at {}:{}",
                    name,
                    line,
                    col,
                )?;
                Ok(())
            }
            UndefinedLocalVariable(name, span) => {
                let (line, col) = span.start_pos().line_col();
                write!(
                    f,
                    "Undefined local variable `{}` at {}:{}",
                    name,
                    line,
                    col,
                )?;
                Ok(())
            }
            WrongNumberOfArguments { expected, got } => write!(
                f,
                "Wrong number of arguments. Expected {} got {}",
                expected, got
            ),
        }
    }
}

impl<'a> std::error::Error for Error<'a> {}
