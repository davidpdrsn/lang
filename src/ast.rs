use crate::Rule;
use pest::iterators::Pair;
use pest::Span;
use std::fmt::{self, Write};

#[derive(Debug)]
pub struct ParseError;

impl std::error::Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug)]
pub struct Parser;

type ParseResult<T> = std::result::Result<T, ParseError>;

impl Parser {
    pub fn parse(pair: Pair<Rule>) -> ParseResult<Program> {
        Program::parse(pair)
    }
}

trait Parse<'a>: Sized {
    const RULE: Rule;

    fn parse_pair_of_rule(pair: Pair<'a, Rule>) -> ParseResult<Self>;

    fn parse(pair: Pair<'a, Rule>) -> ParseResult<Self> {
        if pair.as_rule() == Self::RULE {
            Self::parse_pair_of_rule(pair)
        } else {
            let mut f = String::new();
            writeln!(f, "Parse error").unwrap();
            writeln!(f, "Expected {:?}", Self::RULE).unwrap();
            writeln!(f, "Got {}", pair.as_str()).unwrap();
            panic!("{}", f)
        }
    }
}

#[derive(Debug)]
pub struct Program<'a> {
    pub functions: Vec<Function<'a>>,
    pub span: Span<'a>,
}

impl<'a> Parse<'a> for Program<'a> {
    const RULE: Rule = Rule::program;

    fn parse_pair_of_rule(program: Pair<'a, Rule>) -> ParseResult<Self> {
        let span = program.as_span();
        let mut functions = vec![];

        for pair in program.into_inner() {
            match pair.as_rule() {
                Rule::function => {
                    functions.push(Function::parse_pair_of_rule(pair)?);
                }
                Rule::EOI => {}
                other => panic!("parse error at\n{:?}", other),
            }
        }

        Ok(Program { functions, span })
    }
}

#[derive(Debug)]
pub struct Function<'a> {
    pub name: Ident<'a>,
    pub parameters: Parameters<'a>,
    pub body: Vec<Statement<'a>>,
    pub span: Span<'a>,
}

impl<'a> Parse<'a> for Function<'a> {
    const RULE: Rule = Rule::function;

    fn parse_pair_of_rule(function: Pair<'a, Rule>) -> ParseResult<Self> {
        let span = function.as_span();
        let mut function = function.into_inner();

        let name = Ident::parse(function.next().unwrap())?;

        let parameters = Parameters::parse(function.next().unwrap())?;
        let _type_ = function.next().unwrap();

        let body = Vec::<Statement>::parse(function.next().unwrap())?;

        Ok(Function {
            name,
            parameters,
            body,
            span,
        })
    }
}

#[derive(Debug)]
pub struct Parameters<'a>(pub Vec<Ident<'a>>);

impl<'a> Parse<'a> for Parameters<'a> {
    const RULE: Rule = Rule::parameters;

    fn parse_pair_of_rule(params: Pair<'a, Rule>) -> ParseResult<Self> {
        let mut idents = vec![];
        for param in params.into_inner() {
            idents.push(Ident::parse(param)?);
        }
        Ok(Parameters(idents))
    }
}

impl<'a> Parse<'a> for Vec<Statement<'a>> {
    const RULE: Rule = Rule::function_body;

    fn parse_pair_of_rule(body: Pair<'a, Rule>) -> ParseResult<Self> {
        let mut acc = vec![];
        for pair in body.into_inner() {
            acc.push(Statement::parse(pair)?);
        }
        Ok(acc)
    }
}

#[derive(Debug)]
pub enum Statement<'a> {
    Call(Call<'a>),
    Return(Return<'a>),
    VariableBinding(VariableBinding<'a>),
}

impl<'a> Parse<'a> for Statement<'a> {
    const RULE: Rule = Rule::statement;

    fn parse_pair_of_rule(statement: Pair<'a, Rule>) -> ParseResult<Self> {
        let inner = statement.into_inner().next().unwrap();

        match inner.as_rule() {
            Rule::function_call => Ok(Statement::Call(Call::parse(inner)?)),
            Rule::return_statement => Ok(Statement::Return(Return::parse(inner)?)),
            Rule::variable_binding => {
                Ok(Statement::VariableBinding(VariableBinding::parse(inner)?))
            }
            other => panic!("statement parse error at {:?}", other),
        }
    }
}

#[derive(Debug)]
pub struct Call<'a> {
    pub name: Ident<'a>,
    pub args: Vec<Expr<'a>>,
    pub span: Span<'a>,
}

impl<'a> Parse<'a> for Call<'a> {
    const RULE: Rule = Rule::function_call;

    fn parse_pair_of_rule(function_call: Pair<'a, Rule>) -> ParseResult<Self> {
        let span = function_call.as_span();
        let mut function_call = function_call.into_inner();

        let name = Ident::parse(function_call.next().unwrap())?;
        let args = Vec::<Expr>::parse(function_call.next().unwrap())?;

        Ok(Call {
            name,
            args,
            span: span.clone(),
        })
    }
}

#[derive(Debug)]
pub struct Return<'a> {
    pub expr: Expr<'a>,
    pub span: Span<'a>,
}

impl<'a> Parse<'a> for Return<'a> {
    const RULE: Rule = Rule::return_statement;

    fn parse_pair_of_rule(return_statement: Pair<'a, Rule>) -> ParseResult<Self> {
        let span = return_statement.as_span();
        let mut return_statement = return_statement.into_inner();

        let expr = Expr::parse(return_statement.next().unwrap())?;

        Ok(Return { expr, span })
    }
}

#[derive(Debug)]
pub struct VariableBinding<'a> {
    pub name: Ident<'a>,
    pub expr: Expr<'a>,
    pub span: Span<'a>,
}

impl<'a> Parse<'a> for VariableBinding<'a> {
    const RULE: Rule = Rule::variable_binding;

    fn parse_pair_of_rule(binding: Pair<'a, Rule>) -> ParseResult<Self> {
        let span = binding.as_span();
        let mut binding = binding.into_inner();

        let name = Ident::parse(binding.next().unwrap())?;
        let expr = Expr::parse(binding.next().unwrap())?;

        Ok(VariableBinding { name, expr, span })
    }
}

#[derive(Debug)]
pub enum Expr<'a> {
    StringLit(StringLit<'a>),
    IntegerLit(IntegerLit<'a>),
    BooleanLit(BooleanLit<'a>),
    LocalVariable(LocalVariable<'a>),
    Call(Call<'a>),
}

impl<'a> Parse<'a> for Vec<Expr<'a>> {
    const RULE: Rule = Rule::arguments;

    fn parse_pair_of_rule(exprs: Pair<'a, Rule>) -> ParseResult<Self> {
        let mut acc = vec![];
        for pair in exprs.into_inner() {
            acc.push(Expr::parse(pair)?);
        }
        Ok(acc)
    }
}

impl<'a> Parse<'a> for Expr<'a> {
    const RULE: Rule = Rule::expression;

    fn parse_pair_of_rule(expr: Pair<'a, Rule>) -> ParseResult<Self> {
        let inner = expr.into_inner().next().unwrap();

        let parsed = match inner.as_rule() {
            Rule::string => Expr::StringLit(StringLit::parse(inner)?),
            Rule::identifier => Expr::LocalVariable(LocalVariable::parse(inner)?),
            Rule::function_call => Expr::Call(Call::parse(inner)?),
            Rule::integer => Expr::IntegerLit(IntegerLit::parse(inner)?),
            Rule::boolean => Expr::BooleanLit(BooleanLit::parse(inner)?),
            other => panic!("expr parse error at {:?}", other),
        };

        Ok(parsed)
    }
}

#[derive(Debug)]
pub struct StringLit<'a> {
    pub contents: &'a str,
    pub span: Span<'a>,
}

impl<'a> Parse<'a> for StringLit<'a> {
    const RULE: Rule = Rule::string;

    fn parse_pair_of_rule(string_lit: Pair<'a, Rule>) -> ParseResult<Self> {
        let span = string_lit.as_span();
        let contents = string_lit.into_inner().next().unwrap().as_span().as_str();
        Ok(StringLit { contents, span })
    }
}

#[derive(Debug)]
pub struct IntegerLit<'a> {
    pub integer: i32,
    pub span: Span<'a>,
}

impl<'a> Parse<'a> for IntegerLit<'a> {
    const RULE: Rule = Rule::integer;

    fn parse_pair_of_rule(integer_lit: Pair<'a, Rule>) -> ParseResult<Self> {
        let span = integer_lit.as_span();
        let integer = integer_lit
            .as_str()
            .parse()
            .expect("failed to parse integer literal as int. Should have been tokenizer error");
        Ok(IntegerLit { integer, span })
    }
}

#[derive(Debug)]
pub struct BooleanLit<'a> {
    pub boolean: bool,
    pub span: Span<'a>,
}

impl<'a> Parse<'a> for BooleanLit<'a> {
    const RULE: Rule = Rule::boolean;

    fn parse_pair_of_rule(lit: Pair<'a, Rule>) -> ParseResult<Self> {
        let span = lit.as_span();
        let boolean = lit
            .as_str()
            .parse()
            .expect("failed to parse boolean literal as bool. Should have been tokenizer error");
        Ok(BooleanLit { boolean, span })
    }
}

#[derive(Debug)]
pub struct LocalVariable<'a> {
    pub name: Ident<'a>,
    pub span: Span<'a>,
}

impl<'a> Parse<'a> for LocalVariable<'a> {
    const RULE: Rule = Rule::identifier;

    fn parse_pair_of_rule(local: Pair<'a, Rule>) -> ParseResult<Self> {
        let ident = Ident::parse(local)?;
        let span = ident.span.clone();
        Ok(LocalVariable { name: ident, span })
    }
}

#[derive(Debug)]
pub struct Ident<'a> {
    pub name: &'a str,
    pub span: Span<'a>,
}

impl<'a> Parse<'a> for Ident<'a> {
    const RULE: Rule = Rule::identifier;

    fn parse_pair_of_rule(pair: Pair<'a, Rule>) -> ParseResult<Self> {
        let span = pair.as_span();
        let name = span.as_str();
        Ok(Ident { name, span })
    }
}

impl<'a> Parse<'a> for Vec<Ident<'a>> {
    const RULE: Rule = Rule::identifier;

    fn parse_pair_of_rule(exprs: Pair<'a, Rule>) -> ParseResult<Self> {
        let mut acc = vec![];
        for pair in exprs.into_inner() {
            acc.push(Ident::parse(pair)?);
        }
        Ok(acc)
    }
}
