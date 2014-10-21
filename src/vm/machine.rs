use std::vec::Vec;
use std::result::Result;

use super::bytecode::{Routine, Instruction, RegisterId, Add, Sub, Mul, Div, Rem, Cmp, Lit, StParam, LdParam, StReg, LdReg};


struct StackFrame<'a> {
  parameters: &'a mut [int],
  registers: Vec<int>,
  stack: Vec<int>,
  next_instruction: uint,
  routine: &'a Routine
}

impl<'a> StackFrame<'a> {
  fn pop(&mut self) -> Result<int,RuntimeError> {
    return self.stack.pop().ok_or_else(|| StackUnderflow);
  }

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
  UnknownError(&'static str)
}

pub fn execute<'a>(routine: &'a Routine, parameters: &'a mut [int]) -> Result<(),RuntimeError> {
  if parameters.len() < routine.num_parameters {
    return Err(NotEnoughParameters(parameters.len(), routine.num_parameters));
  }

  debug!("execute routine({})+{:2u}", parameters, routine.num_registers);
  let mut frame = StackFrame {
    parameters: parameters,
    registers: Vec::from_elem(routine.num_registers, 0),
    stack: Vec::new(),
    next_instruction: 0,
    routine: routine
  };

  fn binary_op(frame: &mut StackFrame, op: |int,int| -> Result<int,RuntimeError>) -> Result<(),RuntimeError> {
    let rhs = try!(frame.pop());
    let lhs = try!(frame.pop());
    frame.push(try!(op(lhs,rhs)));
    return Ok(());
  }

  fn unary_op(frame: &mut StackFrame, op: |int| -> Result<int,RuntimeError>) -> Result<(),RuntimeError> {
    let operand = try!(frame.pop());
    frame.push(try!(op(operand)));
    return Ok(());
  }

  loop {
    if frame.next_instruction >= frame.routine.instructions.len() {
      debug!("End of routine reached.")
      break;
    }

    let ins = frame.routine.instructions[frame.next_instruction];
    debug!("XINS({:3u})+{:2u} {}", frame.next_instruction, frame.stack.len(), ins);
    match ins {
      Add => try!(binary_op(&mut frame, |lhs,rhs| Ok(lhs + rhs))),
      Sub => try!(binary_op(&mut frame, |lhs,rhs| Ok(lhs - rhs))),
      Mul => try!(binary_op(&mut frame, |lhs,rhs| Ok(lhs * rhs))),
      Div => try!(binary_op(&mut frame, |lhs,rhs| if rhs == 0 { Err(DivisionByZero) } else { Ok(lhs / rhs) } )),
      Rem => try!(binary_op(&mut frame, |lhs,rhs| if rhs == 0 { Err(DivisionByZero) } else { Ok(lhs % rhs) } )),
      Cmp => try!(binary_op(&mut frame, |lhs,rhs| Ok(lhs - rhs))), 
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
      }      
    }
    debug!("STACK {}",frame.stack);
    frame.next_instruction = frame.next_instruction + 1;
  }

  return Ok(());
}