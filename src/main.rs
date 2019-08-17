#![deny(
    unused_must_use,
    unknown_lints,
    dead_code,
    unused_variables,
    unused_imports
)]
#[macro_use]
extern crate pest_derive;

mod ast;
mod error;
mod eval;

use error::Error;
use eval::Evaluator;
use pest::Parser;
use std::fs;
use std::io;
use std::path::PathBuf;
use structopt::StructOpt;

/// Lang interpreter
#[derive(StructOpt, Debug)]
#[structopt(name = "oops")]
struct Opt {
    /// File to run
    #[structopt(name = "FILE", parse(from_os_str))]
    file: PathBuf,
}

macro_rules! ok_or_exit {
    ( $result:expr ) => {
        match $result {
            Ok(v) => v,
            Err(e) => {
                eprintln!("{}", e);
                std::process::exit(1)
            }
        }
    };
}

fn main() {
    let opt = Opt::from_args();
    let program = ok_or_exit!(fs::read_to_string(opt.file));

    let interpreter = Interpreter::new();
    let mut stdout = io::stdout();
    ok_or_exit!(interpreter.interpret(&program, &mut stdout))
}

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct Tokenizer;

struct Interpreter;

impl Interpreter {
    fn new() -> Self {
        Interpreter
    }

    fn interpret<'a, W: io::Write>(
        self,
        program: &'a str,
        stdout: &'a mut W,
    ) -> Result<(), Error<'a>> {
        let mut pairs = Tokenizer::parse(Rule::program, program)?;
        let pair = pairs.next().unwrap();
        let ast = ast::Parser::parse(pair)?;
        Evaluator::evaluate(ast, stdout)?;

        Ok(())
    }
}

#[cfg(test)]
mod test {
    #[allow(unused_imports)]
    use super::*;

    macro_rules! unwrap_or_panic {
        ($expr:expr) => {
            match $expr {
                Ok(x) => x,
                Err(e) => panic!("{}", e),
            }
        };
    }

    #[test]
    fn do_nothing() {
        let program = "fn main() -> Void {}";

        let mut output = Vec::<u8>::new();
        Interpreter::new().interpret(program, &mut output).unwrap();
        let output = String::from_utf8(output).unwrap();

        assert_eq!(output, "");
    }

    #[test]
    fn hello_world() {
        let program = "fn main() -> Void { println(\"Hello, World!\"); }";

        let mut output = Vec::<u8>::new();
        Interpreter::new().interpret(program, &mut output).unwrap();
        let output = String::from_utf8(output).unwrap();

        assert_eq!(output, "Hello, World!\n");
    }

    #[test]
    fn calling_other_functions() {
        let program = r#"
            fn main() -> Void {
                foo("a", "b");
            }

            fn foo(a, b) -> Void {
                println(a);
                println(b);
            }
        "#;

        let mut output = Vec::<u8>::new();
        unwrap_or_panic!(Interpreter::new().interpret(program, &mut output));
        let output = unwrap_or_panic!(String::from_utf8(output));

        assert_eq!(output, "a\nb\n");
    }

    #[test]
    fn returning_values() {
        let program = r#"
            fn main() -> Void {
                print_it(id("hi"));
            }

            fn print_it(a) -> Void {
                println(a);
            }

            fn id(a) -> String {
                return a;
            }
        "#;

        let mut output = Vec::<u8>::new();
        unwrap_or_panic!(Interpreter::new().interpret(program, &mut output));
        let output = unwrap_or_panic!(String::from_utf8(output));

        assert_eq!(output, "hi\n");
    }

    #[test]
    fn variable_bindings() {
        let program = r#"
            fn main() -> Void {
                let a = "hi";
                println(a);
            }
        "#;

        let mut output = Vec::<u8>::new();
        unwrap_or_panic!(Interpreter::new().interpret(program, &mut output));
        let output = unwrap_or_panic!(String::from_utf8(output));

        assert_eq!(output, "hi\n");
    }

    #[test]
    fn integers() {
        let program = r#"
            fn main() -> Void {
                println(int_to_string(1));
            }
        "#;

        let mut output = Vec::<u8>::new();
        unwrap_or_panic!(Interpreter::new().interpret(program, &mut output));
        let output = unwrap_or_panic!(String::from_utf8(output));

        assert_eq!(output, "1\n");
    }
}