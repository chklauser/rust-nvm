use std::vec::Vec;
use std::result::Result;

use super::bytecode::*;


struct StackFrame<'a> {
  parameters: &'a mut [int],
  registers: Vec<int>,
  stack: Vec<int>,
  next_instruction: uint,
  routine: &'a Routine
}

impl<'a> StackFrame<'a> {
  #[inline]
  fn pop(&mut self) -> Result<int,RuntimeError> {
    return self.stack.pop().ok_or_else(|| StackUnderflow);
  }

  #[inline]
  fn push(&mut self, v:int) {
    self.stack.push(v);
  }

  fn store_parameter(&mut self, parameter_name: RegisterId, value: int) -> Result<(),RuntimeError> {
    if parameter_name >= self.parameters.len() {
      return Err(ParameterOutOfRange(parameter_name, self.parameters.len()));
    } else {
      self.parameters[parameter_name] = value;
      debug!("STOR PARAM{:2u} <- {}",parameter_name, value);
      return Ok(());
    }
  }

  fn load_parameter(&mut self, parameter_name: RegisterId) -> Result<int, RuntimeError> {
    if parameter_name >= self.parameters.len() {
      return Err(ParameterOutOfRange(parameter_name, self.parameters.len()));
    } else {
      let value = self.parameters[parameter_name];
      debug!("LOAD PARAM{:2u} = {}",parameter_name, value);
      return Ok(value);
    }
  }
   
  fn store_register(&mut self, register_name: RegisterId, value: int) -> Result<(),RuntimeError> {
    if register_name >= self.registers.len() {
      return Err(RegisterOutOfRange(register_name, self.registers.len()));
    } else {
      self.registers[register_name] = value;
      debug!("STOR REG {:3u} <- {}",register_name, value);
      return Ok(());
    }
  }

  fn load_register(&mut self, register_name: RegisterId) -> Result<int, RuntimeError> {
    if register_name >= self.registers.len() {
      return Err(RegisterOutOfRange(register_name, self.registers.len()));
    } else {
      let value = self.registers[register_name];
      debug!("LOAD REG {:3u} = {}",register_name, value);
      return Ok(value);
    }
  }
  
}

#[deriving(Show,PartialEq,Eq,Clone)]
enum RuntimeError {
  StackUnderflow,
  NotEnoughParameters(uint,uint), //actual, expected
  ParameterOutOfRange(uint,uint), //actual, maximum
  RegisterOutOfRange(uint,uint), //actual, maximum
  DivisionByZero,
  UnsupportedInstruction(Instruction),
  UnknownError(&'static str),
  RoutineSlotOutOfRange(uint,uint)//actual, maximum
}

pub fn execute<'a>(program: &Program, routine_slot: uint, parameters: &'a mut [int]) -> Result<(),RuntimeError> {
  if routine_slot >= program.routines.len() {
    return Err(RoutineSlotOutOfRange(routine_slot, program.routines.len()));
  }
  let routine = &program.routines[routine_slot];

  if parameters.len() < routine.num_parameters {
    return Err(NotEnoughParameters(parameters.len(), routine.num_parameters));
  }

  debug!("execute routine {}({})+{:2u} (instruction count: {})", 
    routine_slot,
    parameters, 
    routine.num_registers, 
    routine.instructions.len());
  let mut frame = StackFrame {
    parameters: parameters,
    registers: Vec::from_elem(routine.num_registers, 0),
    stack: match routine.max_stack_size {
      Some(max) => Vec::with_capacity(max),
      None => Vec::new()
    },
    next_instruction: 0,
    routine: routine
  };

  #[inline]
  fn binary_op(frame: &mut StackFrame, op: |int,int| -> Result<int,RuntimeError>) -> Result<(),RuntimeError> {
    let rhs = try!(frame.pop());
    let lhs = try!(frame.pop());
    frame.push(try!(op(lhs,rhs)));
    return Ok(());
  }

  #[inline]
  fn unary_op(frame: &mut StackFrame, op: |int| -> Result<int,RuntimeError>) -> Result<(),RuntimeError> {
    let operand = try!(frame.pop());
    frame.push(try!(op(operand)));
    return Ok(());
  }

  loop {
    if frame.next_instruction >= frame.routine.instructions.len() {
      debug!("End of routine {} reached.", routine_slot)
      break;
    }

    let ins = frame.routine.instructions[frame.next_instruction];
    debug!("XINS({:3u})+{:2u} {}", frame.next_instruction, frame.stack.len(), ins);
    let mut control_transfer = false;
    match ins {
      Pop => { try!(frame.pop()) ; () }
      Add => try!(binary_op(&mut frame, |lhs,rhs| Ok(lhs + rhs))),
      Sub => try!(binary_op(&mut frame, |lhs,rhs| Ok(lhs - rhs))),
      Mul => try!(binary_op(&mut frame, |lhs,rhs| Ok(lhs * rhs))),
      Div => try!(binary_op(&mut frame, |lhs,rhs| if rhs == 0 { Err(DivisionByZero) } else { Ok(lhs / rhs) } )),
      Rem => try!(binary_op(&mut frame, |lhs,rhs| if rhs == 0 { Err(DivisionByZero) } else { Ok(lhs % rhs) } )),
      Cmp => try!(binary_op(&mut frame, |lhs,rhs| Ok(lhs - rhs))), 
      Neg => try!(unary_op(&mut frame, |op| Ok(-op))),
      Not => try!(unary_op(&mut frame, |op| Ok(if op == 0 { 1 } else { 0 }))),
      Lt => try!(binary_op(&mut frame, |lhs,rhs| Ok( if lhs < rhs { 1 } else { 0 } ))),
      Le => try!(binary_op(&mut frame, |lhs,rhs| Ok( if lhs <= rhs { 1 } else { 0 } ))),
      Eq => try!(binary_op(&mut frame, |lhs,rhs| Ok( if lhs == rhs { 1 } else { 0 } ))),
      Ne => try!(binary_op(&mut frame, |lhs,rhs| Ok( if lhs != rhs { 1 } else { 0 } ))),
      Gt => try!(binary_op(&mut frame, |lhs,rhs| Ok( if lhs > rhs { 1 } else { 0 } ))),
      Ge => try!(binary_op(&mut frame, |lhs,rhs| Ok( if lhs >= rhs { 1 } else { 0 } ))),
      BitNot => try!(unary_op(&mut frame, |op| Ok(!op))),
      BitXor => try!(binary_op(&mut frame, |lhs,rhs| Ok(lhs ^ rhs))),
      BitAnd => try!(binary_op(&mut frame, |lhs,rhs| Ok(lhs & rhs))),
      BitOr => try!(binary_op(&mut frame, |lhs,rhs| Ok(lhs | rhs))),
      Lit(v) => frame.push(v),
      StParam(reg) => {
        let value = try!(frame.pop());
        try!(frame.store_parameter(reg, value));
      },
      LdParam(reg) => {
        let value = try!(frame.load_parameter(reg));
        frame.push(value);
      },
      StReg(reg) => {
        let value = try!(frame.pop());
        try!(frame.store_register(reg, value));
      },
      LdReg(reg) => {
        let value = try!(frame.load_register(reg));
        frame.push(value);
      },
      JumpZero(target) => {
        let value = try!(frame.pop());
        if value == 0 {
          frame.next_instruction = target;
          control_transfer = true;
        }
      },
      Jump(target) => {
        frame.next_instruction = target;
        control_transfer = true;
      },
      Call(slot) => {
        try!(execute(program, slot, frame.registers.as_mut_slice()));        
      }
    }
    debug!("STACK {}",frame.stack);
    if !control_transfer {
      frame.next_instruction = frame.next_instruction + 1;
    }
  }

  return Ok(());
}