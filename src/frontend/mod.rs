
use super::vm::bytecode::Routine;

pub mod ast;
pub mod parser;
pub mod codegen;

#[deriving(Show,PartialEq,Eq,Clone)]
pub enum FrontendError {
  FeParserError(String),
  FeCodeGenError(codegen::CodeGenError)
}

pub type FrontendResult<T> = Result<T,FrontendError>;

pub fn compile_function(name: String, parameter_names: &[&str], body: &str) -> FrontendResult<Routine> {
  debug!("compiling routine {}",name)
  let stmts = try!(parser::parse_routine(body).map_err(|m| FeParserError(m)));
  let reg_names = try!(codegen::infer_register_names(stmts.as_slice(), parameter_names).map_err(|e| FeCodeGenError(e)));
  let routine = try!(codegen::compile_routine(name, parameter_names, reg_names.as_slice(), stmts.as_slice()).map_err(|e| FeCodeGenError(e)));
  debug!("compiled routine [ {} ]",routine);
  return Ok(routine);
}