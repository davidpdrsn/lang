use crate::Rule;
use pest::{
    iterators::{Pair, Pairs},
    Span,
};
use std::fmt;

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
            panic!(
                "Parse error. Expected {:?} got {:?}",
                Self::RULE,
                pair.as_rule()
            );
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

#[derive(Debug, Clone)]
pub enum Type {
    // Ident(Ident<'a>),
    Boolean,
    Integer,
    String,
    List(Box<Type>),
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        use Type::*;

        match (self, other) {
            (Integer, Integer) => true,
            (Boolean, Boolean) => true,
            (String, String) => true,
            (List(a), List(b)) => a == b,
            _ => false,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Boolean => write!(f, "Bool"),
            Type::Integer => write!(f, "Integer"),
            Type::String => write!(f, "String"),
            Type::List(inner) => write!(f, "[{}]", inner),
        }
    }
}

impl Eq for Type {}

impl<'a> Parse<'a> for Type {
    const RULE: Rule = Rule::type_;

    fn parse_pair_of_rule(type_: Pair<'a, Rule>) -> ParseResult<Self> {
        let mut type_ = type_.into_inner();

        let next = type_.next().unwrap();
        match next.as_rule() {
            Rule::identifier => {
                let name = Ident::parse(next)?;
                match name.name {
                    "Boolean" => Ok(Type::Boolean),
                    "Integer" => Ok(Type::Integer),
                    "String" => Ok(Type::String),
                    // TODO: list
                    _ => unimplemented!("unknown type"),
                }
            }
            Rule::list_type => {
                let inner_type = Type::parse(next.into_inner().next().unwrap())?;
                Ok(Type::List(Box::new(inner_type)))
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct Function<'a> {
    pub name: Ident<'a>,
    pub parameters: Parameters<'a>,
    pub body: Vec<Statement<'a>>,
    pub return_type: Option<Type>,
    pub span: Span<'a>,
}

impl<'a> Parse<'a> for Function<'a> {
    const RULE: Rule = Rule::function;

    fn parse_pair_of_rule(function: Pair<'a, Rule>) -> ParseResult<Self> {
        let span = function.as_span();
        let mut function = function.into_inner();

        let name = Ident::parse(function.next().unwrap())?;

        let parameters = Parameters::parse(function.next().unwrap())?;

        let next = function.next().unwrap();
        let return_type;
        let body;
        match next.as_rule() {
            Rule::type_ => {
                return_type = Some(Type::parse(next)?);
                body = parse_many::<Statement>(
                    function.next().unwrap().into_inner(),
                )?;
            }
            Rule::function_body => {
                return_type = None;
                body = parse_many::<Statement>(next.into_inner())?;
            }
            _ => unreachable!(),
        }

        Ok(Function {
            name,
            parameters,
            body,
            span,
            return_type,
        })
    }
}

fn parse_many<'a, T: Parse<'a>>(pairs: Pairs<'a, Rule>) -> ParseResult<Vec<T>> {
    let mut acc = vec![];
    for pair in pairs {
        acc.push(T::parse(pair)?);
    }
    Ok(acc)
}

#[derive(Debug)]
pub struct Parameters<'a>(pub Vec<(Ident<'a>, Type)>);

impl<'a> Parse<'a> for Parameters<'a> {
    const RULE: Rule = Rule::parameters;

    fn parse_pair_of_rule(params: Pair<'a, Rule>) -> ParseResult<Self> {
        let mut idents = vec![];
        let mut params = params.into_inner();

        loop {
            match (params.next(), params.next()) {
                (Some(ident), Some(type_)) => {
                    let ident = Ident::parse(ident)?;
                    let type_ = Type::parse(type_)?;
                    idents.push((ident, type_));
                }
                (None, None) => break,
                _ => unreachable!(),
            }
        }

        Ok(Parameters(idents))
    }
}

#[derive(Debug)]
pub enum Statement<'a> {
    Call(Call<'a>),
    Return(Return<'a>),
    VariableBinding(VariableBinding<'a>),
    ReassignVariable(ReassignVariable<'a>),
    IfStatement(IfStatement<'a>),
}

impl<'a> Parse<'a> for Statement<'a> {
    const RULE: Rule = Rule::statement;

    fn parse_pair_of_rule(statement: Pair<'a, Rule>) -> ParseResult<Self> {
        let inner = statement.into_inner().next().unwrap();

        match inner.as_rule() {
            Rule::function_call => Ok(Statement::Call(Call::parse(inner)?)),
            Rule::return_statement => {
                Ok(Statement::Return(Return::parse(inner)?))
            }
            Rule::variable_binding => {
                Ok(Statement::VariableBinding(VariableBinding::parse(inner)?))
            }
            Rule::reassign_variable => {
                Ok(Statement::ReassignVariable(ReassignVariable::parse(inner)?))
            }
            Rule::if_statement => {
                Ok(Statement::IfStatement(IfStatement::parse(inner)?))
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
        let args =
            parse_many::<Expr>(function_call.next().unwrap().into_inner())?;

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

    fn parse_pair_of_rule(
        return_statement: Pair<'a, Rule>,
    ) -> ParseResult<Self> {
        let span = return_statement.as_span();
        let mut return_statement = return_statement.into_inner();

        let expr = Expr::parse(return_statement.next().unwrap())?;

        Ok(Return { expr, span })
    }
}

#[derive(Debug)]
pub struct VariableBinding<'a> {
    pub name: Ident<'a>,
    pub type_: Option<Type>,
    pub expr: Expr<'a>,
    pub span: Span<'a>,
}

impl<'a> Parse<'a> for VariableBinding<'a> {
    const RULE: Rule = Rule::variable_binding;

    fn parse_pair_of_rule(binding: Pair<'a, Rule>) -> ParseResult<Self> {
        let span = binding.as_span();
        let mut binding = binding.into_inner();

        let name = Ident::parse(binding.next().unwrap())?;

        let next = binding.next().unwrap();
        match next.as_rule() {
            Rule::type_ => {
                let type_ = Type::parse(next)?;
                let expr = Expr::parse(binding.next().unwrap())?;
                Ok(VariableBinding { name, type_: Some(type_), expr, span })
            },
            Rule::expression => {
                let expr = Expr::parse(next)?;
                Ok(VariableBinding { name, type_: None, expr, span })
            },
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct ReassignVariable<'a> {
    pub name: Ident<'a>,
    pub expr: Expr<'a>,
    pub span: Span<'a>,
}

impl<'a> Parse<'a> for ReassignVariable<'a> {
    const RULE: Rule = Rule::reassign_variable;

    fn parse_pair_of_rule(binding: Pair<'a, Rule>) -> ParseResult<Self> {
        let span = binding.as_span();
        let mut binding = binding.into_inner();

        let name = Ident::parse(binding.next().unwrap())?;
        let expr = Expr::parse(binding.next().unwrap())?;

        Ok(ReassignVariable { name, expr, span })
    }
}

#[derive(Debug)]
pub struct IfStatement<'a> {
    pub condition: Expr<'a>,
    pub then_branch: Vec<Statement<'a>>,
    pub else_branch: Option<Vec<Statement<'a>>>,
    pub span: Span<'a>,
}

impl<'a> Parse<'a> for IfStatement<'a> {
    const RULE: Rule = Rule::if_statement;

    fn parse_pair_of_rule(if_stmt: Pair<'a, Rule>) -> ParseResult<Self> {
        let span = if_stmt.as_span();
        let mut if_stmt = if_stmt.into_inner();

        let condition = Expr::parse(if_stmt.next().unwrap())?;
        let then_branch =
            parse_many::<Statement>(if_stmt.next().unwrap().into_inner())?;
        let else_branch = if_stmt
            .next()
            .map(|pair| parse_many::<Statement>(pair.into_inner()))
            .transpose()?;

        Ok(IfStatement {
            condition,
            then_branch,
            else_branch,
            span,
        })
    }
}

#[derive(Debug)]
pub enum Expr<'a> {
    StringLit(StringLit<'a>),
    IntegerLit(IntegerLit<'a>),
    BooleanLit(BooleanLit<'a>),
    ListLit(ListLit<'a>),
    LocalVariable(LocalVariable<'a>),
    Call(Call<'a>),
}

impl<'a> Expr<'a> {
    pub fn span(&self) -> Span<'a> {
        use Expr::*;

        let span = match self {
            StringLit(x) => &x.span,
            IntegerLit(x) => &x.span,
            BooleanLit(x) => &x.span,
            ListLit(x) => &x.span,
            LocalVariable(x) => &x.span,
            Call(x) => &x.span,
        };

        span.clone()
    }
}

impl<'a> Parse<'a> for Expr<'a> {
    const RULE: Rule = Rule::expression;

    fn parse_pair_of_rule(expr: Pair<'a, Rule>) -> ParseResult<Self> {
        let inner = expr.into_inner().next().unwrap();

        let parsed = match inner.as_rule() {
            Rule::string => Expr::StringLit(StringLit::parse(inner)?),
            Rule::identifier => {
                Expr::LocalVariable(LocalVariable::parse(inner)?)
            }
            Rule::function_call => Expr::Call(Call::parse(inner)?),
            Rule::integer => Expr::IntegerLit(IntegerLit::parse(inner)?),
            Rule::boolean => Expr::BooleanLit(BooleanLit::parse(inner)?),
            Rule::list => Expr::ListLit(ListLit::parse(inner)?),
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
        let contents =
            string_lit.into_inner().next().unwrap().as_span().as_str();
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
pub struct ListLit<'a> {
    pub elements: Vec<Expr<'a>>,
    pub span: Span<'a>,
}

impl<'a> Parse<'a> for ListLit<'a> {
    const RULE: Rule = Rule::list;

    fn parse_pair_of_rule(lit: Pair<'a, Rule>) -> ParseResult<Self> {
        let span = lit.as_span();
        let mut elements = vec![];
        for pair in lit.into_inner() {
            elements.push(Expr::parse(pair)?);
        }
        Ok(ListLit { elements, span })
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
