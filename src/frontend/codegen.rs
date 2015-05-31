
use std::collections::{HashMap,HashSet};
use std::cmp::max;
use std::iter::{repeat,FromIterator};

use super::FrontendError;
use super::ast::*;
use super::ast::Decl::*;
use super::ast::Expr::*;
use super::ast::Stmt::*;
use super::ast::Arg::*;
use super::super::vm::bytecode::{Instruction,self,RegisterId};

#[allow(unused_imports)]
use super::super::vm::bytecode::Instruction::{
  Add,
  Sub,
  Mul,
  Div,
  Rem,
  Cmp,
  Neg,
  Lt,
  Le,
  Gt,
  Ge,
  Eq,
  Ne,
  BitNot,
  BitXor,
  BitAnd,
  BitOr,
  Lit,
  StParam,
  LdParam,
  Mov,
  JumpZero,
  Jump,
  Call 
};
use super::super::vm::bytecode::Instruction::Not as BytecodeNot;

struct RoutineContext<'a: 'b, 'b> {
  parameter_map: HashMap<&'a str, usize>,
  register_map: HashMap<&'a str, usize>,
  instructions: &'a mut Vec<Instruction>,
  program_ctx: &'b ProgramContext<'a>,
  eval_stack_ptr: RegisterId
}

impl<'a, 'b> RoutineContext<'a, 'b> {
  // emits instruction and returns that instruction's address
  fn emit(&mut self, ins:Instruction) -> usize {
    self.instructions.push(ins);
    return self.instructions.len()-1;
  }

  fn next_address(&self) -> usize {
    return self.instructions.len();
  }

  fn resolve(&self, name: &'a str) -> CodeGenResult<SymbolResolution> {
    return self.register_map.get(&name)
      .map_or_else(
        // No register found, try parameter
        ||self.parameter_map.get(&name).map(|v| Parameter(*v)) , 
        // Some register found
        |v| Some(Register(*v)))
      .ok_or_else(||UnresolvedSymbol(name.to_string()));
  }

  fn static_eval_stack_min(&self, min: usize) -> CodeGenResult<()> {
    if self.eval_stack_ptr < min {
      Err(InternalError("Underflow of static evaluation stack during compilation."))
    } else {
      Ok(())
    }
  }

  fn emit_load_variable(&mut self, name: &str) -> CodeGenResult<()> {
    let dest = self.eval_stack_ptr;
    match try!(self.resolve(name)) {
      Parameter(p) => self.emit(LdParam(dest, p)),
      Register(r) => self.emit(Mov(dest, r))
    };
    self.eval_stack_ptr = self.eval_stack_ptr + 1;
    Ok(())
  }

  fn emit_store_variable(&mut self, name: &str) -> CodeGenResult<()> {
    if self.eval_stack_ptr == 0 {
      return Err(InternalError("Underflow of static evaluation stack during compilation."));
    }

    let lhs = try!(self.resolve(name));
    let src = self.eval_stack_ptr-1;
    match lhs {
      Parameter(r) => self.emit(StParam(r, src)),
      Register(r) => self.emit(Mov(r, src))
    };
    self.eval_stack_ptr = self.eval_stack_ptr - 1;
    Ok(())
  }

  fn compile_call(&mut self, routine_name: &str, argv: &[Arg], implicit_return: bool) -> CodeGenResult<()> {
    let info = try!(self.program_ctx.lookup_routine_info(&routine_name[..])
      .ok_or_else(|| UnresolvedSymbol(routine_name.to_string())));
    let slot = info.slot;
    let num_params = info.num_parameters;
    let paramoffset = if implicit_return { 1 } else { 0 };
    if argv.len() + paramoffset < num_params {
      return Err(InsufficientArguments(routine_name.to_string(),argv.len(),num_params - paramoffset));
    }

    let stack_ptr_before_call = self.eval_stack_ptr;

    // Store arguments in call registers
    let mut i = paramoffset;
    self.eval_stack_ptr = self.eval_stack_ptr + paramoffset; // implicit result parameter, if present
    for arg in argv.iter() {
      debug!("call {}(#{} {:?}) should end up in register {}",routine_name, i,arg,self.eval_stack_ptr);
      match arg {
        &ByVal(ref value_expr) => {          
          try!(self.compile_expr(&*value_expr));
        },
        &ByRef(ref name) => {
          try!(self.emit_load_variable(&name[..]));
        }
      }

      i = i + 1;
    }

    if self.eval_stack_ptr < num_params {
      return Err(InternalError("Underflow of static evaluation stack during compilation of routine call"));
    }

    // Call
    let param0 = self.eval_stack_ptr - num_params;
    self.emit(Call(slot, param0));

    // Restore by-ref (in reverse order)
    let mut i = paramoffset;
    for arg in argv.iter().rev() {
      let need_to_consume = if i < num_params {            
        match arg {
          &ByRef(ref name) => {
            try!(self.emit_store_variable(&name[..]));
            false
          }
          _ => true
        }
      } else {
        false
      };

      if need_to_consume {
        self.eval_stack_ptr = self.eval_stack_ptr - 1;
      }

      i = i + 1;
    }

    // for function-like calls, the return value is already in the right place.
    // Since it isn't a formal argument, it won't get "popped" in the loop above    

    if stack_ptr_before_call + (if implicit_return { 1 } else { 0 }) != self.eval_stack_ptr {
      return Err(InternalError("Unexpected stack size delta for call."))
    }

    Ok(())
  }

  fn compile_expr(&mut self, expr: &Expr) -> CodeGenResult<()> {
    match expr {
      &Addition(ref lhs, ref rhs) => {
        try!(self.compile_expr(&**lhs));
        try!(self.compile_expr(&**rhs));
        try!(self.static_eval_stack_min(2));
        let lhs = self.eval_stack_ptr-2;
        let rhs = self.eval_stack_ptr-1;
        self.emit(Add(lhs,rhs));
        self.eval_stack_ptr = self.eval_stack_ptr -1 ;
      },
      &Subtraction(ref lhs, ref rhs) => {
        try!(self.compile_expr(&**lhs));
        try!(self.compile_expr(&**rhs));
        try!(self.static_eval_stack_min(2));
        let lhs = self.eval_stack_ptr-2;
        let rhs = self.eval_stack_ptr-1;
        self.emit(Sub(lhs,rhs));
        self.eval_stack_ptr = self.eval_stack_ptr -1 ;
      },
      &Multiplication(ref lhs, ref rhs) => {
        try!(self.compile_expr(&**lhs));
        try!(self.compile_expr(&**rhs));
        try!(self.static_eval_stack_min(2));
        let lhs = self.eval_stack_ptr-2;
        let rhs = self.eval_stack_ptr-1;
        self.emit(Mul(lhs,rhs));
        self.eval_stack_ptr = self.eval_stack_ptr -1 ;
      },
      &Division(ref lhs, ref rhs) => {
        try!(self.compile_expr(&**lhs));
        try!(self.compile_expr(&**rhs));
        try!(self.static_eval_stack_min(2));
        let lhs = self.eval_stack_ptr-2;
        let rhs = self.eval_stack_ptr-1;
        self.emit(Div(lhs,rhs));
        self.eval_stack_ptr = self.eval_stack_ptr -1 ;
      },
      &Remainder(ref lhs, ref rhs) => {
        try!(self.compile_expr(&**lhs));
        try!(self.compile_expr(&**rhs));
        try!(self.static_eval_stack_min(2));
        let lhs = self.eval_stack_ptr-2;
        let rhs = self.eval_stack_ptr-1;
        self.emit(Rem(lhs,rhs));
        self.eval_stack_ptr = self.eval_stack_ptr -1 ;
      },      
      &LessThan(ref lhs, ref rhs) => {
        try!(self.compile_expr(&**lhs));
        try!(self.compile_expr(&**rhs));
        try!(self.static_eval_stack_min(2));
        let lhs = self.eval_stack_ptr-2;
        let rhs = self.eval_stack_ptr-1;
        self.emit(Lt(lhs,rhs));
        self.eval_stack_ptr = self.eval_stack_ptr -1 ;
      },
      &LessThanOrEqualTo(ref lhs, ref rhs) => {
        try!(self.compile_expr(&**lhs));
        try!(self.compile_expr(&**rhs));
        try!(self.static_eval_stack_min(2));
        let lhs = self.eval_stack_ptr-2;
        let rhs = self.eval_stack_ptr-1;
        self.emit(Le(lhs,rhs));
        self.eval_stack_ptr = self.eval_stack_ptr -1 ;
      },
      &EqualTo(ref lhs, ref rhs) => {
        try!(self.compile_expr(&**lhs));
        try!(self.compile_expr(&**rhs));
        try!(self.static_eval_stack_min(2));
        let lhs = self.eval_stack_ptr-2;
        let rhs = self.eval_stack_ptr-1;
        self.emit(Eq(lhs,rhs));
        self.eval_stack_ptr = self.eval_stack_ptr -1 ;
      },
      &NotEqualTo(ref lhs, ref rhs) => {
        try!(self.compile_expr(&**lhs));
        try!(self.compile_expr(&**rhs));
        try!(self.static_eval_stack_min(2));
        let lhs = self.eval_stack_ptr-2;
        let rhs = self.eval_stack_ptr-1;
        self.emit(Ne(lhs,rhs));
        self.eval_stack_ptr = self.eval_stack_ptr -1 ;
      },
      &GreaterThan(ref lhs, ref rhs) => {
        try!(self.compile_expr(&**lhs));
        try!(self.compile_expr(&**rhs));
        try!(self.static_eval_stack_min(2));
        let lhs = self.eval_stack_ptr-2;
        let rhs = self.eval_stack_ptr-1;
        self.emit(Gt(lhs,rhs));
        self.eval_stack_ptr = self.eval_stack_ptr -1 ;
      },
      &GreaterThanOrEqualTo(ref lhs, ref rhs) => {
        try!(self.compile_expr(&**lhs));
        try!(self.compile_expr(&**rhs));
        try!(self.static_eval_stack_min(2));
        let lhs = self.eval_stack_ptr-2;
        let rhs = self.eval_stack_ptr-1;
        self.emit(Ge(lhs,rhs));
        self.eval_stack_ptr = self.eval_stack_ptr -1 ;
      },
      &Variable(ref v) => {
        try!(self.emit_load_variable(&v[..]));
      },
      &Constant(v) => {
        let dest = self.eval_stack_ptr;
        self.emit(Lit(dest,v));
        self.eval_stack_ptr = self.eval_stack_ptr +1 ;
      },
      &BinaryNot(ref e) => {
        try!(self.compile_expr(&**e));
        try!(self.static_eval_stack_min(1));
        let reg = self.eval_stack_ptr-1;
        self.emit(BitNot(reg));
      },
      &Not(ref e) => {
        try!(self.compile_expr(&**e));
        try!(self.static_eval_stack_min(1));
        let reg = self.eval_stack_ptr-1;
        self.emit(BytecodeNot(reg));
      }
      &Function(ref name, ref argv) => {
        try!(self.compile_call(&name[..], &argv[..], true));
      }
    };
    return Ok(());
  }

  fn compile_stmts(&mut self, body: &[Stmt]) -> CodeGenResult<()> {
    for stmt in body.iter() {
      match stmt {
        &Assign(ref var,ref rhs) => {          
          try!(self.compile_expr(rhs));
          try!(self.emit_store_variable(&var[..]));
        },
        &Condition(ref cond, ref ifbranch, ref elsebranch) => {
          //        <cond>
          //        jumpzero else //<--- needs to be patched
          //    if: <ifbranch>
          //        jump endif //<--- needs to be patched
          //  else: <elsebranch>
          // endif:
          try!(self.compile_expr(cond));
          if self.eval_stack_ptr == 0 {
            return Err(InternalError("Underflow of static evaluation stack during compilation of condition."));
          }
          let cond_reg = self.eval_stack_ptr - 1;
          let cond_jump_addr = self.emit(JumpZero(0,cond_reg));          
          self.eval_stack_ptr = cond_reg;

          try!(self.compile_stmts(&ifbranch[..]));          
          let skip_else_jump_addr = self.emit(Jump(0));
          self.instructions[cond_jump_addr] = JumpZero(self.next_address(),cond_reg);
          try!(self.compile_stmts(&elsebranch[..]));
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
          try!(self.compile_stmts(&loop_body[..]));
          let continue_addr = self.next_address();
          try!(self.compile_expr(cond));
          if self.eval_stack_ptr == 0 {
            return Err(InternalError("Underflow of static evaluation stack during compilation of while condition."));
          }
          let expr_reg = self.eval_stack_ptr - 1;
          self.emit(BytecodeNot(expr_reg));
          self.emit(JumpZero(loop_addr,expr_reg));
          self.eval_stack_ptr = expr_reg;

          // patch continue address
          self.instructions[init_jump_addr] = Jump(continue_addr);
        },
        &RoutineCall(ref name, ref argv) => {
          try!(self.compile_call(&name[..], &argv[..], false));
        }
      }
    }
    Ok(())
  }
}

enum SymbolResolution {
  Parameter(usize),
  Register(usize)
}
use self::SymbolResolution::*;

#[derive(Display,Debug,PartialEq,Eq,Clone)]
#[allow(unused_attributes)]
pub enum CodeGenError {
  UnresolvedSymbol(String),
  UnsupportedExpr(Expr),
  DuplicateName(String),
  InternalError(&'static str),  
  InsufficientArguments(String, usize,usize) //routine name, actual, expected (minimum)
}
use self::CodeGenError::*;
impl From<CodeGenError> for FrontendError {
  fn from(e: CodeGenError) -> FrontendError {
    FrontendError::FeCodeGenError(e)
  }
}

pub type CodeGenResult<T> = Result<T,CodeGenError>;

fn infer_register_names<'a,'d>(
  routine_name: &str,
    body: &'a [Stmt], 
    parameter_names: &[&'a str], 
    program_ctx: &'a ProgramContext) 
    -> CodeGenResult<Vec<&'a str>> {
  debug!("INFER REGISTERS for routine {}({:?})",routine_name, parameter_names);
  let (names,max_stack) = try!(list_variables(body, program_ctx));
  let mut names = names;
  for param in parameter_names.iter() {
    names.remove(param);
  }
  let final_register_names : Vec<&str> = FromIterator::from_iter(
    repeat("<anonymous evaluation stack register>").take(max_stack).chain(
    names.into_iter()));
  debug!(" DONE REGISTERS for routine {}({:?})\n  registers ({}):\n    {}",
    routine_name, parameter_names, final_register_names.len(),final_register_names.connect("\n    "));
  return Ok(final_register_names);
}

fn list_variables<'a: 'p, 'p>(body: &'a[Stmt], program_ctx: &'p ProgramContext<'a>) -> CodeGenResult<(HashSet<&'a str>,usize)> {
  struct Ctx<'c: 'p, 'p> {
    vars: HashSet<&'c str>,
    max_stack: usize,
    program_ctx: &'p ProgramContext<'c>
  }  

  fn list_vars_in_expr<'b, 'p>(ctx: &mut Ctx<'b,'p>, expr: &'b Expr, occupied_stack: usize) -> CodeGenResult<()> { 
    // Total stack max stack size when this expression has been computed
    let mut size_for_expr = occupied_stack;
    debug!("begin {:?} with stack size {}", expr, size_for_expr);
    match expr {
      &Variable(ref v) => {
        ctx.vars.insert(&v[..]);
        size_for_expr = size_for_expr + 1;
        ()
      },
      &Addition(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(ctx, &**lhs, size_for_expr));
        size_for_expr = size_for_expr + 1;
        try!(list_vars_in_expr(ctx, &**rhs, size_for_expr));
      },
      &Subtraction(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(ctx, &**lhs, size_for_expr));
        size_for_expr = size_for_expr + 1;
        try!(list_vars_in_expr(ctx, &**rhs, size_for_expr));
      },
      &Multiplication(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(ctx, &**lhs, size_for_expr));
        size_for_expr = size_for_expr + 1;
        try!(list_vars_in_expr(ctx, &**rhs, size_for_expr));
      },
      &Division(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(ctx, &**lhs, size_for_expr));
        size_for_expr = size_for_expr + 1;
        try!(list_vars_in_expr(ctx, &**rhs, size_for_expr));
      },
      &Remainder(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(ctx, &**lhs, size_for_expr));
        size_for_expr = size_for_expr + 1;
        try!(list_vars_in_expr(ctx, &**rhs, size_for_expr));
      },
      &LessThan(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(ctx, &**lhs, size_for_expr));
        size_for_expr = size_for_expr + 1;
        try!(list_vars_in_expr(ctx, &**rhs, size_for_expr));
      },
      &LessThanOrEqualTo(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(ctx, &**lhs, size_for_expr));
        size_for_expr = size_for_expr + 1;
        try!(list_vars_in_expr(ctx, &**rhs, size_for_expr));
      },
      &EqualTo(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(ctx, &**lhs, size_for_expr));
        size_for_expr = size_for_expr + 1;
        try!(list_vars_in_expr(ctx, &**rhs, size_for_expr));
      },
      &NotEqualTo(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(ctx, &**lhs, size_for_expr));
        size_for_expr = size_for_expr + 1;
        try!(list_vars_in_expr(ctx, &**rhs, size_for_expr));
      },
      &GreaterThan(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(ctx, &**lhs, size_for_expr));
        size_for_expr = size_for_expr + 1;
        try!(list_vars_in_expr(ctx, &**rhs, size_for_expr));
      },
      &GreaterThanOrEqualTo(ref lhs, ref rhs) => {
        try!(list_vars_in_expr(ctx, &**lhs, size_for_expr));
        size_for_expr = size_for_expr + 1;
        try!(list_vars_in_expr(ctx, &**rhs, size_for_expr));
      },
      &Constant(_) => {
        size_for_expr = size_for_expr + 1;
      },
      &BinaryNot(ref e) => {
        try!(list_vars_in_expr(ctx, &**e, size_for_expr));
      },
      &Not(ref e) => {
        try!(list_vars_in_expr(ctx, &**e, size_for_expr));
      },
      &Function(ref name,ref args) => {
        try!(list_var_in_call(ctx, &name[..], &args[..], size_for_expr, 1));
        size_for_expr = size_for_expr + 1;
      }
    }

    ctx.max_stack = max(ctx.max_stack, size_for_expr);
    debug!("  end {:?} with stack size {} for a max stack size of {}", expr, size_for_expr, ctx.max_stack);
    Ok(())
  }

  fn list_var_in_call<'b,'p>(ctx: &mut Ctx<'b,'p>, routine_name: &str, argv: &'b [Arg], occupied_stack: usize, implicit_arguments: usize) -> CodeGenResult<()> {
    let num_params = try!(
      ctx.program_ctx.lookup_routine_info(routine_name).map(|x| x.num_parameters)
        .ok_or_else(|| UnresolvedSymbol(routine_name.to_string())));
    let mut stack_size = occupied_stack + implicit_arguments;
    for arg in argv.iter() {
      // Stack size for evaluating argument N
      match arg {
        &ByVal(ref e) => { 
          try!(list_vars_in_expr(ctx, &*e, stack_size));
        },
        &ByRef(ref v) => {
          ctx.vars.insert(&v[..]);
        }
      }
      // Reserve slot on stack for argument N
      stack_size = stack_size + 1;
    }
    // in case we ever have "out"-only arguments with no representation on the stack
    // we still need to reserve that space so that the callee could write to it.
    ctx.max_stack = max(num_params + occupied_stack, ctx.max_stack);
    Ok(())
  }

  fn list_vars_in_stmts<'b,'p>(ctx: &mut Ctx<'b,'p>, stmts: &'b [Stmt]) -> CodeGenResult<()> {
    for stmt in stmts.iter() {
      match stmt {
        &Assign(ref lhs, ref rhs) => {
          ctx.vars.insert(&lhs[..]);
          try!(list_vars_in_expr(ctx,rhs,0));
        },
        &Condition(ref cond, ref ifbranch, ref elsebranch) => {
          try!(list_vars_in_expr(ctx, cond,0));
          try!(list_vars_in_stmts(ctx, &ifbranch[..]));
          try!(list_vars_in_stmts(ctx, &elsebranch[..]));
        }
        &While(ref cond, ref loop_body) => {
          try!(list_vars_in_expr(ctx, cond,0));
          try!(list_vars_in_stmts(ctx, &loop_body[..]))
        },
        &RoutineCall(ref name, ref argv) => {
          try!(list_var_in_call(ctx, &name[..], &argv[..], 0, 0));
        }
      }
    }
    Ok(())
  }

  let mut ctx = Ctx {
    vars: HashSet::new(),
    max_stack: 0,
    program_ctx: program_ctx
  };
  try!(list_vars_in_stmts(&mut ctx, body));

  return Ok((ctx.vars, ctx.max_stack));
}

// shortcut
pub fn compile_isolated_routine<'a>(
  parameter_names: &'a[&str],
  body: &[Stmt]) 
-> CodeGenResult<bytecode::Routine> {
  let ctx = ProgramContext::new();
  let routine_name = String::from_str("main");
  let register_names = try!(infer_register_names(&routine_name[..], body, parameter_names, &ctx));
  let mut routine = bytecode::Routine::new(routine_name, parameter_names.len(), register_names.len(), Vec::new());
  try!(ctx.compile_routine_body(&mut routine, parameter_names, &register_names[..], body));
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
    self.routine_map.get(&name)
  }

  fn compile_routine_body<'a>(&self, routine: &mut bytecode::Routine,
    parameter_names: &'a[&str], 
    register_names: &'a[&str], 
    body: &[Stmt]) 
  -> CodeGenResult<()> {
    fn compute_map<'a>(names: &'a[&str]) -> HashMap<&'a str,usize> {
      let mut map = HashMap::new();
      let mut i : usize = 0;
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
      program_ctx: self,
      eval_stack_ptr: 0
    };

    try!(ctx.compile_stmts(body));

    Ok(())
  }
}

struct RoutineInfo {
  slot: usize,
  num_parameters: usize
}

pub fn compile_program(decls: &[Decl]) -> CodeGenResult<bytecode::Program> {
  let mut program = bytecode::Program::new();
  let mut ctx = ProgramContext::new();

  // Forward declare stuff
  let mut next_slot = 0;
  for decl in decls.iter() {
    match decl {
      &Routine(ref name,ref parameter_names,_) => {        
        let key = &name[..];
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
      &Routine(ref name,ref parameter_names, ref stmts) => { 
        let info = try!(ctx.routine_map.get(&name[..])
          .ok_or(InternalError("Can't find routine info that I created milliseconds ago.")));
        debug_assert_eq!(program.routines.len(),info.slot);

        // We expect a string slice vector but have a vector to the real thing. 
        // Unfortunately, we need to create a vector that contains just the slices
        let pnames : Vec<&str> = FromIterator::from_iter(parameter_names.iter().map(|x| &x[..]));

        // Prepare routine for compilation (compute registers)
        let register_names = try!(infer_register_names(&name[..], &stmts[..], &pnames[..], &ctx));
        let mut routine = bytecode::Routine::new(
          name.clone(), parameter_names.len(), register_names.len(), Vec::new());

        // Compile routine
        try!(ctx.compile_routine_body(&mut routine, &pnames[..], &register_names[..], &stmts[..]));

        // Store routine in program
        program.routines.push(routine);
      }
    }
  }

  Ok(program)
}