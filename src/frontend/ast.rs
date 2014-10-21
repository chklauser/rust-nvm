
#[deriving(Clone,Show,Eq,PartialEq)]
pub enum Expr {
  Addition(Box<Expr>,Box<Expr>),
  Subtraction(Box<Expr>,Box<Expr>),
  Multiplication(Box<Expr>,Box<Expr>),
  Division(Box<Expr>,Box<Expr>),
  Remainder(Box<Expr>,Box<Expr>),
  BinaryNot(Box<Expr>),
  Not(Box<Expr>),
  Variable(String),
  Constant(int),
  Function(String, Vec<Expr>) // one return register baked in
}

#[deriving(Show,PartialEq,Eq,Clone)]
pub enum Stmt {
  Assign(String, Expr),
  While(Expr,Vec<Stmt>),
  Condition(Expr, Vec<Stmt>, Vec<Stmt>)
}

#[deriving(Show,PartialEq,Eq,Clone)]
pub enum Decl {
  // Routine name, parameters, registers, body
  Routine(String, Vec<String>, Vec<String>, Vec<Stmt>)
}

#[deriving(Show,PartialEq,Eq,Clone)]
pub struct Program(Vec<Decl>);