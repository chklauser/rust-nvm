
pub type RegisterId = uint;
pub type ParameterId = uint;

// Two-Address-Code (for those operations that require two operands)
//  * both operands must be registers
//  * first/left operand is also destination register
#[deriving(Show,PartialEq,Eq,Clone)]
pub enum Instruction {
  Add(RegisterId,RegisterId),
  Sub(RegisterId,RegisterId),
  Mul(RegisterId,RegisterId),
  Div(RegisterId,RegisterId),
  Rem(RegisterId,RegisterId),
  Cmp(RegisterId,RegisterId),
  Neg(RegisterId),
  Not(RegisterId),
  Lt(RegisterId,RegisterId),
  Le(RegisterId,RegisterId),
  Gt(RegisterId,RegisterId),
  Ge(RegisterId,RegisterId),
  Eq(RegisterId,RegisterId),
  Ne(RegisterId,RegisterId),
  BitNot(RegisterId),
  BitXor(RegisterId,RegisterId),
  BitAnd(RegisterId,RegisterId),
  BitOr(RegisterId,RegisterId),
  Lit(RegisterId, int),
  StParam(ParameterId, RegisterId),
  LdParam(RegisterId,ParameterId),
  Mov(RegisterId,RegisterId),
  JumpZero(uint, RegisterId),
  Jump(uint),
  Call(RegisterId, uint) // Name of first register that is part of arguments; routine ID
}

#[deriving(Show,Clone)]
pub struct Routine {
  pub name: String,
  pub num_parameters: uint,
  pub num_registers: uint,
  pub instructions: Vec<Instruction>  
}

impl Routine {
  pub fn new(name: String, num_parameters: uint, num_registers: uint, instructions: Vec<Instruction>) -> Routine {
    return Routine {
      num_parameters: num_parameters,
      num_registers: num_registers,
      instructions: instructions,
      name: name
    };
  }
}

#[deriving(Show,Clone)]
pub struct Program {
  pub routines: Vec<Routine>
}

impl Program {
  pub fn new() -> Program {
    Program {
      routines: Vec::new()
    }
  }
}