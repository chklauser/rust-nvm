
use std::fmt::{Display, self};
use std::error::Error;

use super::vm::bytecode::Program;

pub mod ast;
pub mod parser;
pub mod codegen;

#[derive(Debug,PartialEq,Eq,Clone)]
#[allow(unused_attributes)]
pub enum FrontendError {
    // The parser error uses references into the input. We can't model that at this level.
    // As a result, the frontend error variant only contains a rendered form of the error message.
    FeParserError(String),
    FeCodeGenError(codegen::CodeGenError),
}

impl Error for FrontendError {
    fn description(&self) -> &str {
        "Frontend error"
    }
    fn cause(&self) -> Option<&Error> {
        match self {
            &FrontendError::FeCodeGenError(ref e) => Some(e),
            _ => None
        }
    }
}

impl Display for FrontendError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &FrontendError::FeParserError(ref txt) => write!(f, "{}", txt),
            &FrontendError::FeCodeGenError(ref e) => write!(f, "{}", e)
        }
    }
}

pub type FrontendResult<T> = Result<T, FrontendError>;

#[allow(dead_code)]
pub fn compile_function(name: String,
                        parameter_names: &[&str],
                        body: &str)
                        -> FrontendResult<Program> {
    debug!("compiling routine {}", name);
    let stmts = try!(parser::parse_routine(body));
    let routine = try!(codegen::compile_isolated_routine(parameter_names, &stmts[..]));
    debug!("compiled routine [ {:?} ]", routine);
    let mut program = Program::new();
    program.routines.push(routine);
    Ok(program)
}

#[allow(dead_code)]
pub fn compile_program(program_text: &str) -> FrontendResult<Program> {
    debug!("parsing program");
    let ast::Program(decls) = try!(parser::parse_program(program_text));
    debug!("generating code for program");
    let program = try!(codegen::compile_program(&decls[..]));
    debug!("program compiled, number of routines: {}, number of instructions {}",
           program.routines.len(),
           program.routines.iter().map(|r| r.instructions.len()).fold(0, |l, r| l + r));
    return Ok(program);
}
