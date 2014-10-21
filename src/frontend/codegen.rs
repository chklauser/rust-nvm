
use std::collections::{HashMap,HashSet};

use super::ast::{Stmt,Assign,Expr,Addition,Subtraction, Multiplication, Division, Remainder, Variable, Constant};
use super::super::vm::bytecode::{Routine,Instruction,Add,Sub, Mul,Div,Rem,StParam,StReg,LdParam,LdReg,Lit};

struct RoutineContext<'a> {
  parameter_map: HashMap<&'a str, uint>,
  register_map: HashMap<&'a str, uint>,
  instructions: Vec<Instruction>
}

impl<'a> RoutineContext<'a> {
  fn emit(&mut self, ins:Instruction) {
    self.instructions.push(ins);
  }

  fn resolve(&self, name: &str) -> CodeGenResult<SymbolResolution> {
    return self.register_map.find_equiv(&name)
      .map_or_else(
        // No register found, try parameter
        ||self.parameter_map.find_equiv(&name).map(|v| Parameter(*v)) , 
        // Some register found
        |v| Some(Register(*v)))
      .ok_or_else(||UnresolvedSymbol(name.to_string()));
  }

  fn compile_expr(&mut self, expr: &Expr) -> CodeGenResult<()> {
    match expr {
      &Addition(ref lhs, ref rhs) => {
        try!(self.compile_expr(&**lhs));
        try!(self.compile_expr(&**rhs));
        self.emit(Add);
      },
      &Subtraction(ref lhs, ref rhs) => {
        try!(self.compile_expr(&**lhs));
        try!(self.compile_expr(&**rhs));
        self.emit(Sub);
      },
      &Multiplication(ref lhs, ref rhs) => {
        try!(self.compile_expr(&**lhs));
        try!(self.compile_expr(&**rhs));
        self.emit(Mul);
      },
      &Division(ref lhs, ref rhs) => {
        try!(self.compile_expr(&**lhs));
        try!(self.compile_expr(&**rhs));
        self.emit(Div);
      },
      &Remainder(ref lhs, ref rhs) => {
        try!(self.compile_expr(&**lhs));
        try!(self.compile_expr(&**rhs));
        self.emit(Rem);
      },
      &Variable(ref v) => {
        match try!(self.resolve(v[])) {
          Parameter(p) => self.emit(LdParam(p)),
          Register(r) => self.emit(LdReg(r))
        }        
      },
      &Constant(v) => {
        self.emit(Lit(v));
      },
      other => return Err(UnsupportedExpr(other.clone()))
    };
    return Ok(());
  }
}

enum SymbolResolution {
  Parameter(uint),
  Register(uint)
}

#[deriving(Show,PartialEq,Eq,Clone)]
pub enum CodeGenError {
  UnresolvedSymbol(String),
  UnsupportedExpr(Expr)
}

pub type CodeGenResult<T> = Result<T,CodeGenError>;

pub fn infer_register_names<'a>(body: &'a [Stmt], parameter_names: &[&'a str]) -> CodeGenResult<Vec<&'a str>> {
  let mut names = try!(list_variables(body));
  for param in parameter_names.iter() {
    names.remove(param);
  }
  return Ok(FromIterator::from_iter(names.into_iter()))
}

fn list_variables<'a>(body: &'a[Stmt]) -> CodeGenResult<HashSet<&'a str>> {
  fn list_vars_in_expr<'b>(vars: &mut HashSet<&'b str>, expr: &'b Expr) -> CodeGenResult<()> {
    match expr {
      &Variable(ref v) => {
        vars.insert(v.as_slice());
        ()
      },
      &Addition(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(vars, &**lhs));
        try!(list_vars_in_expr(vars, &**rhs));
      },
      &Subtraction(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(vars, &**lhs));
        try!(list_vars_in_expr(vars, &**rhs));
      },
      &Multiplication(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(vars, &**lhs));
        try!(list_vars_in_expr(vars, &**rhs));
      },
      &Division(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(vars, &**lhs));
        try!(list_vars_in_expr(vars, &**rhs));
      },
      &Remainder(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(vars, &**lhs));
        try!(list_vars_in_expr(vars, &**rhs));
      },
      &Constant(_) => (),
      other => return Err(UnsupportedExpr(other.clone()))
    }
    Ok(())
  }
  let mut vars = HashSet::new();

  for stmt in body.iter() {
    match stmt {
      &Assign(ref lhs, ref rhs) => {
        vars.insert(lhs.as_slice());
        try!(list_vars_in_expr(&mut vars,rhs));
      }
    }
  }

  return Ok(vars);
}

pub fn compile_routine<'a>(
  name: String, 
  parameter_names: &'a[&str], 
  register_names: &'a[&str], 
  body: &[Stmt]) 
-> CodeGenResult<Routine> {
  fn compute_map<'a>(names: &'a[&str]) -> HashMap<&'a str,uint> {
    let mut map = HashMap::new();
    let mut i : uint = 0;
    for name in names.iter() {
      map.insert(*name, i);
      i = i + 1;
    }
    return map;
  }
  let mut ctx = RoutineContext {
    parameter_map: compute_map(parameter_names),
    register_map: compute_map(register_names),
    instructions: Vec::new()
  };
  for stmt in body.iter() {
    match stmt {
      &Assign(ref var,ref rhs) => {
        let lhs = try!(ctx.resolve(var[]));
        try!(ctx.compile_expr(rhs));
        match lhs {
          Parameter(r) => ctx.emit(StParam(r)),
          Register(r) => ctx.emit(StReg(r))
        }
      }
    }
  }

  return Ok::<Routine, CodeGenError>(Routine::new(ctx.parameter_map.len(), ctx.register_map.len(), ctx.instructions))
}