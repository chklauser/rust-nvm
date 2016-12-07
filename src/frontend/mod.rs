
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
