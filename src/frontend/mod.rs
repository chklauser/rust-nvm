
use super::vm::bytecode::Program;

pub mod ast;
pub mod parser;
pub mod codegen;
pub mod analysis;

#[deriving(Show,PartialEq,Eq,Clone)]
pub enum FrontendError {
  FeParserError(String),
  FeCodeGenError(codegen::CodeGenError),
  FeAnalysisError(analysis::AnalysisError)
}

pub type FrontendResult<T> = Result<T,FrontendError>;

#[allow(dead_code)]
pub fn compile_function(name: String, parameter_names: &[&str], body: &str) -> FrontendResult<Program> {
  debug!("compiling routine {}",name)
  let stmts = try_as!(parser::parse_routine(body), FeParserError);
  let mut routine = try_as!(codegen::compile_isolated_routine(parameter_names, stmts.as_slice()), FeCodeGenError);
  try_as!(analysis::compute_max_stack_size(&mut routine), FeAnalysisError);
  debug!("compiled routine [ {} ]",routine);
  let mut program = Program::new();
  program.routines.push(routine);
  Ok(program)
}

#[allow(dead_code)]
pub fn compile_program(program_text: &str) -> FrontendResult<Program> {
  debug!("parsing program");
  let ast::Program(decls) = try_as!(parser::parse_program(program_text), FeParserError);
  debug!("generating code for program");
  let mut program = try_as!(codegen::compile_program(decls.as_slice()),FeCodeGenError);
  debug!("analyzing and optimizing program")
  try_as!(analysis::analyze_program(&mut program),FeAnalysisError);
  debug!("program compiled, number of routines: {}, number of instructions {}", 
      program.routines.len(), 
      program.routines.iter().map(|r| r.instructions.len()).fold(0, |l,r| l+r)
    );
  return Ok(program);
}
