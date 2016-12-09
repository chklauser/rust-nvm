use std::vec::Vec;
use std::result::Result;
use std::cmp::min;
use std::iter::{repeat, FromIterator};
use std::error::Error;
use std::fmt::{self, Display};

use super::bytecode::{Instruction, Routine, Program, RegisterId};
use super::bytecode::Instruction::*;


struct StackFrame<'a> {
    parameters: &'a mut [isize],
    registers: Vec<isize>,
    next_instruction: usize,
    routine: &'a Routine,
}

impl<'a> StackFrame<'a> {
    #[cfg(range_check)]
    fn store_parameter(&mut self,
                       parameter_name: RegisterId,
                       value: isize)
                       -> Result<(), RuntimeError> {
        if parameter_name >= self.parameters.len() {
            return Err(ParameterOutOfRange(parameter_name, self.parameters.len()));
        } else {
            self.parameters[parameter_name] = value;
            debug!("STOR PARAM{:2u} <- {}", parameter_name, value);
            return Ok(());
        }
    }

    #[cfg(range_check)]
    fn load_parameter(&mut self, parameter_name: RegisterId) -> Result<isize, RuntimeError> {
        if parameter_name >= self.parameters.len() {
            return Err(ParameterOutOfRange(parameter_name, self.parameters.len()));
        } else {
            let value = self.parameters[parameter_name];
            debug!("LOAD PARAM{:2u} = {}", parameter_name, value);
            return Ok(value);
        }
    }

    #[cfg(range_check)]
    fn store_register(&mut self,
                      register_name: RegisterId,
                      value: isize)
                      -> Result<(), RuntimeError> {
        if register_name >= self.registers.len() {
            return Err(RegisterOutOfRange(register_name, self.registers.len()));
        } else {
            self.registers[register_name] = value;
            debug!("STOR REG {:3u} <- {}", register_name, value);
            return Ok(());
        }
    }

    #[cfg(range_check)]
    fn load_register(&mut self, register_name: RegisterId) -> Result<isize, RuntimeError> {
        if register_name >= self.registers.len() {
            return Err(RegisterOutOfRange(register_name, self.registers.len()));
        } else {
            let value = self.registers[register_name];
            debug!("LOAD REG {:3u} = {}", register_name, value);
            return Ok(value);
        }
    }

    #[cfg(not(range_check))]
    #[inline]
    fn store_parameter(&mut self,
                       parameter_name: RegisterId,
                       value: isize)
                       -> Result<(), RuntimeError> {
        self.parameters[parameter_name] = value;
        Ok(())
    }

    #[cfg(not(range_check))]
    #[inline]
    fn load_parameter(&mut self, parameter_name: RegisterId) -> Result<isize, RuntimeError> {
        Ok(self.parameters[parameter_name])
    }

    #[cfg(not(range_check))]
    #[inline]
    fn store_register(&mut self,
                      register_name: RegisterId,
                      value: isize)
                      -> Result<(), RuntimeError> {
        self.registers[register_name] = value;
        Ok(())
    }

    #[cfg(not(range_check))]
    #[inline]
    fn load_register(&mut self, register_name: RegisterId) -> Result<isize, RuntimeError> {
        Ok(self.registers[register_name])
    }
}

#[cfg(range_check)]
macro_rules! try_check{
   ($e:expr) => (try!($e))
}

#[cfg(not(range_check))]
macro_rules! try_check{
  ($e:expr) => (e)
}

#[derive(Debug,PartialEq,Eq,Clone)]
#[allow(unused_attributes)]
pub enum RuntimeError {
    StackUnderflow,
    NotEnoughParameters(usize, usize), // actual, expected
    ParameterOutOfRange(usize, usize), // actual, maximum
    RegisterOutOfRange(usize, usize), // actual, maximum
    DivisionByZero,
    UnsupportedInstruction(Instruction),
    UnknownError(&'static str),
    RoutineSlotOutOfRange(usize, usize), // actual, maximum
}
use self::RuntimeError::*;
impl Error for RuntimeError {
    fn description(&self) -> &str {
        "Runtime error"
    }
}
impl Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &StackUnderflow => write!(f, "Stack underflow"),
            &NotEnoughParameters(actual, expected) => write!(f,
                "Not enough arguments to call routine. Expected {} Actual {}", expected, actual),
            &ParameterOutOfRange(actual, maximum) => write!(f,
                "Parameter index {} out of range. Maximum parameter index {}", actual, maximum),
            &RegisterOutOfRange(actual, maximum) => write!(f,
                "Register index {} out of range. Maximum register index {}", actual, maximum),
            &DivisionByZero => write!(f, "Division by zero"),
            &UnsupportedInstruction(ref ins) => write!(f, "Unsupported instruction: {:?}", ins),
            &UnknownError(txt) => write!(f, "Unknown error: {}", txt),
            &RoutineSlotOutOfRange(actual, maximum) => write!(f,
                "Routine index {} out of range. Maximum routine index {}", actual, maximum)
        }
    }
}

pub fn execute<'a>(program: &Program,
                   routine_slot: usize,
                   parameters: &'a mut [isize])
                   -> Result<(), RuntimeError> {
    if routine_slot >= program.routines.len() {
        return Err(RoutineSlotOutOfRange(routine_slot, program.routines.len()));
    }
    let routine = &program.routines[routine_slot];

    if parameters.len() < routine.num_parameters {
        return Err(NotEnoughParameters(parameters.len(), routine.num_parameters));
    }

    debug!("CALL        {}:{}({:?})+{:2} (instruction count: {})",
           &routine.name[..],
           routine_slot,
           parameters,
           routine.num_registers,
           routine.instructions.len());
    let mut frame = StackFrame {
        parameters: parameters,
        registers: FromIterator::from_iter(repeat(0).take(routine.num_registers)),
        next_instruction: 0,
        routine: routine,
    };

    #[inline]
    fn binary_op<F>(frame: &mut StackFrame,
                    lhs: RegisterId,
                    rhs: RegisterId,
                    op: F)
                    -> Result<(), RuntimeError>
        where F: Fn(isize, isize) -> Result<isize, RuntimeError>
    {
        let lhs_val = try!(frame.load_register(lhs));
        let rhs_val = try!(frame.load_register(rhs));
        let val = try!(op(lhs_val, rhs_val));
        try!(frame.store_register(lhs, val));
        return Ok(());
    }

    #[inline]
    fn unary_op<F>(frame: &mut StackFrame, operand: RegisterId, op: F) -> Result<(), RuntimeError>
        where F: Fn(isize) -> Result<isize, RuntimeError>
    {
        let value = try!(frame.load_register(operand));
        let value = try!(op(value));
        try!(frame.store_register(operand, value));
        Ok(())
    }

    loop {
        if frame.next_instruction >= frame.routine.instructions.len() {
            debug!("    END  of {}:{} reached.",
                   &frame.routine.name[..],
                   routine_slot);
            break;
        }

        let ins = &frame.routine.instructions[frame.next_instruction];
        debug!("{:12} XINS({:3}) {:?}",
               &(&frame.routine.name[..])[0..(min((&frame.routine.name[..])
                                                      .char_indices()
                                                      .count(),
                                                  12))],
               frame.next_instruction,
               ins);
        let mut control_transfer = false;
        match *ins {
            Add(lhs, rhs) => try!(binary_op(&mut frame, lhs, rhs, |lhs, rhs| Ok(lhs + rhs))),
            Sub(lhs, rhs) => try!(binary_op(&mut frame, lhs, rhs, |lhs, rhs| Ok(lhs - rhs))),
            Mul(lhs, rhs) => try!(binary_op(&mut frame, lhs, rhs, |lhs, rhs| Ok(lhs * rhs))),
            Div(lhs, rhs) => {
                try!(binary_op(&mut frame, lhs, rhs, |lhs, rhs| if rhs == 0 {
                    Err(DivisionByZero)
                } else {
                    Ok(lhs / rhs)
                }))
            }
            Rem(lhs, rhs) => {
                try!(binary_op(&mut frame, lhs, rhs, |lhs, rhs| if rhs == 0 {
                    Err(DivisionByZero)
                } else {
                    Ok(lhs % rhs)
                }))
            }
            Cmp(lhs, rhs) => try!(binary_op(&mut frame, lhs, rhs, |lhs, rhs| Ok(lhs - rhs))), 
            Neg(op) => try!(unary_op(&mut frame, op, |op| Ok(-op))),
            Not(op) => try!(unary_op(&mut frame, op, |op| Ok(if op == 0 { 1 } else { 0 }))),
            Lt(lhs, rhs) => {
                try!(binary_op(&mut frame,
                               lhs,
                               rhs,
                               |lhs, rhs| Ok(if lhs < rhs { 1 } else { 0 })))
            }
            Le(lhs, rhs) => {
                try!(binary_op(&mut frame,
                               lhs,
                               rhs,
                               |lhs, rhs| Ok(if lhs <= rhs { 1 } else { 0 })))
            }
            Eq(lhs, rhs) => {
                try!(binary_op(&mut frame,
                               lhs,
                               rhs,
                               |lhs, rhs| Ok(if lhs == rhs { 1 } else { 0 })))
            }
            Ne(lhs, rhs) => {
                try!(binary_op(&mut frame,
                               lhs,
                               rhs,
                               |lhs, rhs| Ok(if lhs != rhs { 1 } else { 0 })))
            }
            Gt(lhs, rhs) => {
                try!(binary_op(&mut frame,
                               lhs,
                               rhs,
                               |lhs, rhs| Ok(if lhs > rhs { 1 } else { 0 })))
            }
            Ge(lhs, rhs) => {
                try!(binary_op(&mut frame,
                               lhs,
                               rhs,
                               |lhs, rhs| Ok(if lhs >= rhs { 1 } else { 0 })))
            }
            BitNot(op) => try!(unary_op(&mut frame, op, |op| Ok(!op))),
            BitXor(lhs, rhs) => try!(binary_op(&mut frame, lhs, rhs, |lhs, rhs| Ok(lhs ^ rhs))),
            BitAnd(lhs, rhs) => try!(binary_op(&mut frame, lhs, rhs, |lhs, rhs| Ok(lhs & rhs))),
            BitOr(lhs, rhs) => try!(binary_op(&mut frame, lhs, rhs, |lhs, rhs| Ok(lhs | rhs))),
            Lit(reg, v) => try!(frame.store_register(reg, v)),
            StParam(param, reg) => {
                let value = try!(frame.load_register(reg));
                try!(frame.store_parameter(param, value));
            }
            LdParam(reg, param) => {
                let value = try!(frame.load_parameter(param));
                try!(frame.store_register(reg, value))
            }
            Mov(dest, src) => {
                let value = try!(frame.load_register(src));
                try!(frame.store_register(dest, value));
            }
            JumpZero(target, cond) => {
                let value = try!(frame.load_register(cond));
                if value == 0 {
                    frame.next_instruction = target;
                    control_transfer = true;
                }
            }
            Jump(target) => {
                frame.next_instruction = target;
                control_transfer = true;
            }
            Call(slot, param_base_reg) => {
                try!(execute(program, slot, &mut frame.registers[param_base_reg..]));
            }
        }
        if !control_transfer {
            frame.next_instruction = frame.next_instruction + 1;
        }
    }

    return Ok(());
}
