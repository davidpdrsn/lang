// #![deny(
//     unused_must_use,
//     unknown_lints,
//     dead_code,
//     unused_variables,
//     unused_imports
// )]
#[macro_use]
extern crate pest_derive;

mod ast;
mod error;
mod eval;
mod type_checker;
mod utils;

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
    fn test_files() {
        let files = test_program_files().unwrap();
        dbg!(&files);

        for (source_file, stdout_file) in files {
            dbg!(&source_file);
            dbg!(&stdout_file);

            let code = std::fs::read_to_string(&source_file).unwrap();

            let mut output = Vec::<u8>::new();
            unwrap_or_panic!(Interpreter::new().interpret(&code, &mut output));
            let output = String::from_utf8(output).unwrap();

            let expected_output =
                std::fs::read_to_string(&stdout_file).unwrap();
            assert_eq!(output, expected_output);
        }
    }

    fn test_program_files() -> std::io::Result<Vec<(String, String)>> {
        let mut source_files = vec![];
        let mut stdout_files = vec![];

        for entry in fs::read_dir("tests/programs")? {
            let dir = entry?;
            let file_name = dir.path().display().to_string();

            if file_name.ends_with(".stdout") {
                stdout_files.push(file_name);
            } else {
                source_files.push(file_name);
            }
        }

        source_files.sort();
        stdout_files.sort();

        assert_eq!(source_files.len(), stdout_files.len());

        let files = source_files
            .into_iter()
            .zip(stdout_files.into_iter())
            .collect::<Vec<_>>();

        Ok(files)
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
