
pub type RegisterId = uint;

#[deriving(Show,PartialEq,Eq,Clone)]
pub enum Instruction {
  Add,
  Sub,
  Mul,
  Div,
  Rem,
  Cmp,
  Lit(int),
  StParam(RegisterId),
  LdParam(RegisterId),
  StReg(RegisterId),
  LdReg(RegisterId)
}

#[deriving(Show,Clone)]
pub struct Routine {
  pub num_parameters: uint,
  pub num_registers: uint,
  pub instructions: Vec<Instruction>  
}

impl Routine {
  pub fn new(num_parameters: uint, num_registers: uint, instructions: Vec<Instruction>) -> Routine {
    return Routine {
      num_parameters: num_parameters,
      num_registers: num_registers,
      instructions: instructions
    };
  }
}