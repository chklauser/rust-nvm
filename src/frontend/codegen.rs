
use std::collections::{HashMap,HashSet};
use std::cmp::max;
use std::iter::Repeat;

use super::ast::*;
use super::ast;
use super::super::vm::bytecode::*;
use super::super::vm::bytecode;

struct RoutineContext<'a: 'b, 'b> {
  parameter_map: HashMap<&'a str, uint>,
  register_map: HashMap<&'a str, uint>,
  instructions: &'a mut Vec<Instruction>,
  program_ctx: &'b ProgramContext<'a>
}

impl<'a, 'b> RoutineContext<'a, 'b> {
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

  fn emit_load_variable(&mut self, name: &str) -> CodeGenResult<()> {
    match try!(self.resolve(name)) {
      Parameter(p) => self.emit(LdParam(p)),
      Register(r) => self.emit(LdReg(r))
    };
    Ok(())
  }

  fn emit_store_variable(&mut self, name: &str) -> CodeGenResult<()> {
    let lhs = try!(self.resolve(name));
    match lhs {
      Parameter(r) => self.emit(StParam(r)),
      Register(r) => self.emit(StReg(r))
    };
    Ok(())
  }

  fn compile_call(&mut self, routine_name: &str, argv: &[Arg], implicit_return: bool) -> CodeGenResult<()> {
    let info = try!(self.program_ctx.lookup_routine_info(routine_name.as_slice())
      .ok_or_else(|| UnresolvedSymbol(routine_name.to_string())));
    let slot = info.slot;
    let num_params = info.num_parameters;
    let paramoffset = if implicit_return { 1 } else { 0 };
    if argv.len() + paramoffset < num_params {
      return Err(InsufficientArguments(routine_name.to_string(),argv.len(),num_params - paramoffset));
    }

    // Store arguments in call registers
    let mut i = paramoffset;
    for arg in argv.iter() {
      match arg {
        &ByVal(ref value_expr) => {
          try!(self.compile_expr(&*value_expr));
        },
        &ByRef(ref name) => {
          try!(self.emit_load_variable(name[]));
        }
      }
      if i < num_params {
        self.emit(StReg(i));
      } else {
        self.emit(Pop);
      }

      i = i + 1;
    }

    // Call
    self.emit(Call(slot));

    // Restore by-ref
    let mut i = paramoffset;
    for arg in argv.iter() {
      if i < num_params {            
        match arg {
          &ByRef(ref name) => {
            self.emit(LdReg(i));
            try!(self.emit_store_variable(name[]));
          }
          _ => ()
        }
      }

      i = i + 1;
    }

    if implicit_return {
      self.emit(LdReg(0));
    }

    Ok(())
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
      &LessThan(ref lhs, ref rhs) => {
        try!(self.compile_expr(&**lhs));
        try!(self.compile_expr(&**rhs));
        self.emit(Lt);
      },
      &LessThanOrEqualTo(ref lhs, ref rhs) => {
        try!(self.compile_expr(&**lhs));
        try!(self.compile_expr(&**rhs));
        self.emit(Le);
      },
      &EqualTo(ref lhs, ref rhs) => {
        try!(self.compile_expr(&**lhs));
        try!(self.compile_expr(&**rhs));
        self.emit(Eq);
      },
      &NotEqualTo(ref lhs, ref rhs) => {
        try!(self.compile_expr(&**lhs));
        try!(self.compile_expr(&**rhs));
        self.emit(Ne);
      },
      &GreaterThan(ref lhs, ref rhs) => {
        try!(self.compile_expr(&**lhs));
        try!(self.compile_expr(&**rhs));
        self.emit(Gt);
      },
      &GreaterThanOrEqualTo(ref lhs, ref rhs) => {
        try!(self.compile_expr(&**lhs));
        try!(self.compile_expr(&**rhs));
        self.emit(Ge);
      },
      &Variable(ref v) => {
        try!(self.emit_load_variable(v[]));
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
      &Function(ref name, ref argv) => {
        try!(self.compile_call(name[], argv[], true));
      }
    };
    return Ok(());
  }

  fn compile_stmts(&mut self, body: &[Stmt]) -> CodeGenResult<()> {
    for stmt in body.iter() {
      match stmt {
        &Assign(ref var,ref rhs) => {          
          try!(self.compile_expr(rhs));
          try!(self.emit_store_variable(var[]));
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
        },
        &RoutineCall(ref name, ref argv) => {
          try!(self.compile_call(name[], argv[], false));
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
  UnsupportedExpr(Expr),
  DuplicateName(String),
  InternalError(&'static str),  
  InsufficientArguments(String, uint,uint) //routine name, actual, expected (minimum)
}

pub type CodeGenResult<T> = Result<T,CodeGenError>;

fn infer_register_names<'a,'d>(
    body: &'a [Stmt], 
    parameter_names: &[&'a str], 
    program_ctx: &'a ProgramContext) 
    -> CodeGenResult<Vec<&'a str>> {
  let (names,max_parameters) = try!(list_variables(body, program_ctx));
  let mut names = names;
  for param in parameter_names.iter() {
    names.remove(param);
  }
  return Ok(FromIterator::from_iter(
    Repeat::new("<anonymous call parameter register>").take(max_parameters).chain(
    names.into_iter())));
}

fn list_variables<'a: 'p, 'p>(body: &'a[Stmt], program_ctx: &'p ProgramContext<'a>) -> CodeGenResult<(HashSet<&'a str>,uint)> {
  struct Ctx<'c: 'p, 'p> {
    vars: HashSet<&'c str>,
    max_parameters: uint,
    program_ctx: &'p ProgramContext<'c>
  }

  fn list_vars_in_expr<'b, 'p>(ctx: &mut Ctx<'b,'p>, expr: &'b Expr) -> CodeGenResult<()> {
    match expr {
      &Variable(ref v) => {
        ctx.vars.insert(v.as_slice());
        ()
      },
      &Addition(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(ctx, &**lhs));
        try!(list_vars_in_expr(ctx, &**rhs));
      },
      &Subtraction(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(ctx, &**lhs));
        try!(list_vars_in_expr(ctx, &**rhs));
      },
      &Multiplication(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(ctx, &**lhs));
        try!(list_vars_in_expr(ctx, &**rhs));
      },
      &Division(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(ctx, &**lhs));
        try!(list_vars_in_expr(ctx, &**rhs));
      },
      &Remainder(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(ctx, &**lhs));
        try!(list_vars_in_expr(ctx, &**rhs));
      },
      &LessThan(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(ctx, &**lhs));
        try!(list_vars_in_expr(ctx, &**rhs));
      },
      &LessThanOrEqualTo(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(ctx, &**lhs));
        try!(list_vars_in_expr(ctx, &**rhs));
      },
      &EqualTo(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(ctx, &**lhs));
        try!(list_vars_in_expr(ctx, &**rhs));
      },
      &NotEqualTo(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(ctx, &**lhs));
        try!(list_vars_in_expr(ctx, &**rhs));
      },
      &GreaterThan(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(ctx, &**lhs));
        try!(list_vars_in_expr(ctx, &**rhs));
      },
      &GreaterThanOrEqualTo(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(ctx, &**lhs));
        try!(list_vars_in_expr(ctx, &**rhs));
      },
      &Constant(_) => (),
      &BinaryNot(ref e) => {
        try!(list_vars_in_expr(ctx, &**e));
      },
      &ast::Not(ref e) => {
        try!(list_vars_in_expr(ctx, &**e));
      },
      &Function(ref name,ref args) => {
        try!(list_var_in_call(ctx, name[], args[]));
      }
    }
    Ok(())
  }

  fn list_var_in_call<'b,'p>(ctx: &mut Ctx<'b,'p>, routine_name: &str, argv: &'b [Arg]) -> CodeGenResult<()> {
    let num_params = try!(
      ctx.program_ctx.lookup_routine_info(routine_name).map(|x| x.num_parameters)
        .ok_or_else(|| UnresolvedSymbol(routine_name.to_string())));
    ctx.max_parameters = max(num_params, ctx.max_parameters);
    for arg in argv.iter() {
      match arg {
        &ByVal(ref e) => { 
          try!(list_vars_in_expr(ctx, &*e));
        },
        &ByRef(ref v) => {
          ctx.vars.insert(v.as_slice());
        }
      }
    }
    Ok(())
  }

  fn list_vars_in_stmts<'b,'p>(ctx: &mut Ctx<'b,'p>, stmts: &'b [Stmt]) -> CodeGenResult<()> {
    for stmt in stmts.iter() {
      match stmt {
        &Assign(ref lhs, ref rhs) => {
          ctx.vars.insert(lhs.as_slice());
          try!(list_vars_in_expr(ctx,rhs));
        },
        &Condition(ref cond, ref ifbranch, ref elsebranch) => {
          try!(list_vars_in_expr(ctx, cond));
          try!(list_vars_in_stmts(ctx, ifbranch.as_slice()));
          try!(list_vars_in_stmts(ctx, elsebranch.as_slice()));
        }
        &While(ref cond, ref loop_body) => {
          try!(list_vars_in_expr(ctx, cond));
          try!(list_vars_in_stmts(ctx, loop_body.as_slice()))
        },
        &RoutineCall(ref name, ref argv) => {
          try!(list_var_in_call(ctx, name[], argv[]));
        }
      }
    }
    Ok(())
  }

  let mut ctx = Ctx {
    vars: HashSet::new(),
    max_parameters: 0,
    program_ctx: program_ctx
  };
  try!(list_vars_in_stmts(&mut ctx, body));

  return Ok((ctx.vars, ctx.max_parameters));
}

// shortcut
pub fn compile_isolated_routine<'a>(
  parameter_names: &'a[&str],
  body: &[Stmt]) 
-> CodeGenResult<bytecode::Routine> {
  let ctx = ProgramContext::new();
  let register_names = try!(infer_register_names(body, parameter_names, &ctx));
  let mut routine = bytecode::Routine::new(parameter_names.len(), register_names.len(), Vec::new());
  try!(ctx.compile_routine_body(&mut routine, parameter_names, register_names.as_slice(), body));
  Ok(routine)
}

struct ProgramContext<'a> {
  routine_map: HashMap<&'a str, RoutineInfo>
}

impl<'p> ProgramContext<'p> {
  fn new() -> ProgramContext<'p> {
    ProgramContext {
      routine_map: HashMap::new()
    }
  }

  fn lookup_routine_info(&self, name: &'p str) -> Option<&RoutineInfo> {
    self.routine_map.find(&name)
  }

  fn compile_routine_body<'a>(&self, routine: &mut bytecode::Routine,
    parameter_names: &'a[&str], 
    register_names: &'a[&str], 
    body: &[Stmt]) 
  -> CodeGenResult<()> {
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
      instructions: &mut routine.instructions,
      program_ctx: self
    };

    try!(ctx.compile_stmts(body));

    Ok(())
  }
}

struct RoutineInfo {
  slot: uint,
  num_parameters: uint
}

pub fn compile_program(decls: &[Decl]) -> CodeGenResult<Program> {
  let mut program = Program::new();
  let mut ctx = ProgramContext::new();

  // Forward declare stuff
  let mut next_slot = 0;
  for decl in decls.iter() {
    match decl {
      &ast::Routine(ref name,ref parameter_names,_) => {        
        let key = name.as_slice();
        if ctx.routine_map.contains_key(&key) {
          return Err(DuplicateName(name.to_string()));
        }
        ctx.routine_map.insert(key, RoutineInfo {
          slot: next_slot,
          num_parameters: parameter_names.len()
        });
        next_slot = next_slot + 1;
      }
    }
  }

  // Gen code
  for decl in decls.iter() {
    match decl {
      &ast::Routine(ref name,ref parameter_names, ref stmts) => { 
        let info = try!(ctx.routine_map.find(&name.as_slice())
          .ok_or(InternalError("Can't find routine info that I created milliseconds ago.")));
        debug_assert_eq!(program.routines.len(),info.slot);

        // We expect a string slice vector but have a vector to the real thing. 
        // Unfortunately, we need to create a vector that contains just the slices
        let pnames : Vec<&str> = FromIterator::from_iter(parameter_names.iter().map(|x| x.as_slice()));

        // Prepare routine for compilation (compute registers)
        let register_names = try!(infer_register_names(stmts.as_slice(), pnames.as_slice(), &ctx));
        let mut routine = bytecode::Routine::new(parameter_names.len(), register_names.len(), Vec::new());

        // Compile routine
        try!(ctx.compile_routine_body(&mut routine, pnames.as_slice(), register_names.as_slice(), stmts.as_slice()));

        // Store routine in program
        program.routines.push(routine);
      }
    }
  }

  Ok(program)
}