
use std::collections::{HashMap,HashSet};

use super::ast::*;
use super::ast;
use super::super::vm::bytecode::*;

struct RoutineContext<'a> {
  parameter_map: HashMap<&'a str, uint>,
  register_map: HashMap<&'a str, uint>,
  instructions: Vec<Instruction>
}

impl<'a> RoutineContext<'a> {
  // emits instruction and returns that instruction's address
  fn emit(&mut self, ins:Instruction) -> uint {
    self.instructions.push(ins);
    return self.instructions.len()-1;
  }

  fn next_address(&self) -> uint {
    return self.instructions.len();
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
        };
      },
      &Constant(v) => {
        self.emit(Lit(v));
      },
      &BinaryNot(ref e) => {
        try!(self.compile_expr(&**e));
        self.emit(BitNot);
      },
      &ast::Not(ref e) => {
        try!(self.compile_expr(&**e));
        self.emit(Not);
      }
      e@&Function(_,_) => return Err(UnsupportedExpr(e.clone()))
    };
    return Ok(());
  }

  fn compile_stmts(&mut self, body: &[Stmt]) -> CodeGenResult<()> {
    for stmt in body.iter() {
      match stmt {
        &Assign(ref var,ref rhs) => {
          let lhs = try!(self.resolve(var[]));
          try!(self.compile_expr(rhs));
          match lhs {
            Parameter(r) => self.emit(StParam(r)),
            Register(r) => self.emit(StReg(r))
          };
        },
        &Condition(ref cond, ref ifbranch, ref elsebranch) => {
          //        <cond>
          //        jumpzero else //<--- needs to be patched
          //    if: <ifbranch>
          //        jump endif //<--- needs to be patched
          //  else: <elsebranch>
          // endif:
          try!(self.compile_expr(cond));
          let cond_jump_addr = self.emit(JumpZero(0));
          try!(self.compile_stmts(ifbranch.as_slice()));          
          let skip_else_jump_addr = self.emit(Jump(0));
          self.instructions[cond_jump_addr] = JumpZero(self.next_address());
          try!(self.compile_stmts(elsebranch.as_slice()));
          self.instructions[skip_else_jump_addr] = Jump(self.next_address());

          // Yes, there are many cases that could be optimized here
        }
        &While(ref cond, ref loop_body) => {
          //           jump continue
          //     loop: <body>
          // continue: <cond>
          //           not
          //           jumpzero loop
          //    break: 
          // We won't know the continue address until we have compiled the body.
          // We insert a placeholder now so that all the other addresses won't shift.
          // once we patch the correct address back in.
          let init_jump_addr = self.emit(Jump(0)); 
          let loop_addr = self.next_address();
          try!(self.compile_stmts(loop_body.as_slice()));
          let continue_addr = self.next_address();
          try!(self.compile_expr(cond));
          self.emit(Not);
          self.emit(JumpZero(loop_addr));

          // patch continue address
          self.instructions[init_jump_addr] = Jump(continue_addr);
        }
      }
    }
    Ok(())
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
      &BinaryNot(ref e) => {
        try!(list_vars_in_expr(vars, &**e));
      },
      &ast::Not(ref e) => {
        try!(list_vars_in_expr(vars, &**e));
      },
      other@&Function(_,_) => return Err(UnsupportedExpr(other.clone()))
    }
    Ok(())
  }
  let mut vars = HashSet::new();

  fn list_vars_in_stmts<'b>(vars: &mut HashSet<&'b str>, stmts: &'b [Stmt]) -> CodeGenResult<()> {
    for stmt in stmts.iter() {
      match stmt {
        &Assign(ref lhs, ref rhs) => {
          vars.insert(lhs.as_slice());
          try!(list_vars_in_expr(vars,rhs));
        },
        &Condition(ref cond, ref ifbranch, ref elsebranch) => {
          try!(list_vars_in_expr(vars, cond));
          try!(list_vars_in_stmts(vars, ifbranch.as_slice()));
          try!(list_vars_in_stmts(vars, elsebranch.as_slice()));
        }
        &While(ref cond, ref loop_body) => {
          try!(list_vars_in_expr(vars, cond));
          try!(list_vars_in_stmts(vars, loop_body.as_slice()))
        }
      }
    }
    Ok(())
  }

  try!(list_vars_in_stmts(&mut vars, body));

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

  try!(ctx.compile_stmts(body));

  return Ok::<Routine, CodeGenError>(Routine::new(ctx.parameter_map.len(), ctx.register_map.len(), ctx.instructions))
}