// #![deny(
//     unused_must_use,
//     unknown_lints,
//     dead_code,
//     unused_variables,
//     unused_imports
// )]
#[macro_use]
extern crate pest_derive;

mod utils;
mod ast;
mod error;
mod eval;
mod type_checker;

use error::Error;
use eval::Evaluator;
use pest::Parser;
use std::{fs, io, path::PathBuf};
use structopt::StructOpt;
use type_checker::TypeChecker;

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
        TypeChecker::new().check(&ast)?;
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
        let program = "fn main() {}";

        let mut output = Vec::<u8>::new();
        Interpreter::new().interpret(program, &mut output).unwrap();
        let output = String::from_utf8(output).unwrap();

        assert_eq!(output, "");
    }

    #[test]
    fn hello_world() {
        let program = "fn main() { println(\"Hello, World!\"); }";

        let mut output = Vec::<u8>::new();
        Interpreter::new().interpret(program, &mut output).unwrap();
        let output = String::from_utf8(output).unwrap();

        assert_eq!(output, "Hello, World!\n");
    }

    #[test]
    fn calling_other_functions() {
        let program = r#"
            fn main() {
                foo("a", "b");
            }

            fn foo(a: String, b: String) {
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
            fn main() {
                print_it(id("hi"));
            }

            fn print_it(a: String) {
                println(a);
            }

            fn id(a: String) -> String {
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
            fn main() {
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
            fn main() {
                println(int_to_string(1));
            }
        "#;

        let mut output = Vec::<u8>::new();
        unwrap_or_panic!(Interpreter::new().interpret(program, &mut output));
        let output = unwrap_or_panic!(String::from_utf8(output));

        assert_eq!(output, "1\n");
    }

    #[test]
    fn bools() {
        let program = r#"
            fn main() {
                println(bool_to_string(true));
                println(bool_to_string(false));
            }
        "#;

        let mut output = Vec::<u8>::new();
        unwrap_or_panic!(Interpreter::new().interpret(program, &mut output));
        let output = unwrap_or_panic!(String::from_utf8(output));

        assert_eq!(output, "true\nfalse\n");
    }

    #[test]
    fn lists() {
        let program = r#"
            fn main() {
                let list = [1, 2, 3];
                println(int_to_string(length(list)));
            }
        "#;

        let mut output = Vec::<u8>::new();
        unwrap_or_panic!(Interpreter::new().interpret(program, &mut output));
        let output = unwrap_or_panic!(String::from_utf8(output));

        assert_eq!(output, "3\n");
    }

    #[test]
    fn empty_list() {
        let program = r#"
            fn main() {
                let list: [Integer] = [];
                println(int_to_string(length(list)));
            }
        "#;

        let mut output = Vec::<u8>::new();
        unwrap_or_panic!(Interpreter::new().interpret(program, &mut output));
        let output = unwrap_or_panic!(String::from_utf8(output));

        assert_eq!(output, "0\n");
    }

    #[test]
    fn empty_list_as_arg() {
        let program = r#"
            fn main() {
                let l = foo([]);
                println(int_to_string(l));
            }

            fn foo(as: [Integer]) -> Integer {
                return length(as);
            }
        "#;

        let mut output = Vec::<u8>::new();
        unwrap_or_panic!(Interpreter::new().interpret(program, &mut output));
        let output = unwrap_or_panic!(String::from_utf8(output));

        assert_eq!(output, "0\n");
    }

    #[test]
    fn reassign() {
        let program = r#"
            fn main() {
                let a = "first";
                println(a);
                a = "second";
                println(a);
            }
        "#;

        let mut output = Vec::<u8>::new();
        unwrap_or_panic!(Interpreter::new().interpret(program, &mut output));
        let output = unwrap_or_panic!(String::from_utf8(output));

        assert_eq!(output, "first\nsecond\n");
    }

    #[test]
    fn conditional_statement() {
        let program = r#"
            fn main() {
                if false {
                    println("yep");
                } else {
                    println("nope");
                }
            }
        "#;

        let mut output = Vec::<u8>::new();
        unwrap_or_panic!(Interpreter::new().interpret(program, &mut output));
        let output = unwrap_or_panic!(String::from_utf8(output));

        assert_eq!(output, "nope\n");
    }

    #[test]
    fn optional_else_clause() {
        let program = r#"
            fn main() {
                if false {
                    println("yep");
                }
            }
        "#;

        let mut output = Vec::<u8>::new();
        unwrap_or_panic!(Interpreter::new().interpret(program, &mut output));
        let output = unwrap_or_panic!(String::from_utf8(output));

        assert_eq!(output, "");
    }

    #[test]
    fn if_statement_scoping_reassignment() {
        let program = r#"
            fn main() {
                let a = "og";
                if true {
                    a = "changed";
                }
                println(a);
            }
        "#;

        let mut output = Vec::<u8>::new();
        unwrap_or_panic!(Interpreter::new().interpret(program, &mut output));
        let output = unwrap_or_panic!(String::from_utf8(output));

        assert_eq!(output, "changed\n");
    }

    #[test]
    fn if_statement_scoping_new_binding_not_allowed() {
        let program = r#"
            fn main() {
                if true {
                    let a = "og";
                }
                println(a);
            }
        "#;

        let mut output = Vec::<u8>::new();
        let result = Interpreter::new().interpret(program, &mut output);
        match result {
            Err(Error::UndefinedLocalVariable { .. }) => {}
            other => {
                panic!("Fail: {:?}", other);
            }
        }
    }
}
