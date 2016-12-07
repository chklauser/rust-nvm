
pub type RegisterId = usize;
pub type ParameterId = usize;


// Two-Address-Code (for those operations that require two operands)
//  * both operands must be registers
//  * first/left operand is also destination register
#[derive(Debug,PartialEq,Eq,Clone)]
#[allow(unused_attributes)]
pub enum Instruction {
    Add(RegisterId, RegisterId),
    Sub(RegisterId, RegisterId),
    Mul(RegisterId, RegisterId),
    Div(RegisterId, RegisterId),
    Rem(RegisterId, RegisterId),
    Cmp(RegisterId, RegisterId),
    Neg(RegisterId),
    Not(RegisterId),
    Lt(RegisterId, RegisterId),
    Le(RegisterId, RegisterId),
    Gt(RegisterId, RegisterId),
    Ge(RegisterId, RegisterId),
    Eq(RegisterId, RegisterId),
    Ne(RegisterId, RegisterId),
    BitNot(RegisterId),
    BitXor(RegisterId, RegisterId),
    BitAnd(RegisterId, RegisterId),
    BitOr(RegisterId, RegisterId),
    Lit(RegisterId, isize),
    StParam(ParameterId, RegisterId),
    LdParam(RegisterId, ParameterId),
    Mov(RegisterId, RegisterId),
    JumpZero(usize, RegisterId),
    Jump(usize),
    Call(RegisterId, usize), // Name of first register that is part of arguments; routine ID
}

#[derive(Debug,Clone)]
#[allow(unused_attributes)]
pub struct Routine {
    pub name: String,
    pub num_parameters: usize,
    pub num_registers: usize,
    pub instructions: Vec<Instruction>,
}

impl Routine {
    pub fn new(name: String,
               num_parameters: usize,
               num_registers: usize,
               instructions: Vec<Instruction>)
               -> Routine {
        return Routine {
            num_parameters: num_parameters,
            num_registers: num_registers,
            instructions: instructions,
            name: name,
        };
    }
}

#[derive(Debug,Clone)]
#[allow(unused_attributes)]
pub struct Program {
    pub routines: Vec<Routine>,
}

impl Program {
    pub fn new() -> Program {
        Program { routines: Vec::new() }
    }
}
