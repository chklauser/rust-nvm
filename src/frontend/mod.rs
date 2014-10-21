
use super::vm::bytecode::Routine;

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

pub fn compile_function(name: String, parameter_names: &[&str], body: &str) -> FrontendResult<Routine> {
  debug!("compiling routine {}",name)
  let stmts = try_as!(parser::parse_routine(body), FeParserError);
  let reg_names = try_as!(codegen::infer_register_names(stmts.as_slice(), parameter_names),  FeCodeGenError);
  let mut routine = try_as!(codegen::compile_routine(name, parameter_names, reg_names.as_slice(), stmts.as_slice()), FeCodeGenError);
  try_as!(analysis::compute_max_stack_size(&mut routine), FeAnalysisError);
  debug!("compiled routine [ {} ]",routine);
  return Ok(routine);
}