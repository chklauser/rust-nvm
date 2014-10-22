
#[deriving(Clone,Show,Eq,PartialEq)]
pub enum Expr {
  Addition(Box<Expr>,Box<Expr>),
  Subtraction(Box<Expr>,Box<Expr>),
  Multiplication(Box<Expr>,Box<Expr>),
  Division(Box<Expr>,Box<Expr>),
  Remainder(Box<Expr>,Box<Expr>),
  BinaryNot(Box<Expr>),
  LessThan(Box<Expr>,Box<Expr>),
  LessThanOrEqualTo(Box<Expr>,Box<Expr>),
  EqualTo(Box<Expr>,Box<Expr>),
  NotEqualTo(Box<Expr>,Box<Expr>),
  GreaterThan(Box<Expr>,Box<Expr>),
  GreaterThanOrEqualTo(Box<Expr>,Box<Expr>),
  Not(Box<Expr>),
  Variable(String),
  Constant(int),
  Function(String, Vec<Arg>) // one return register baked in
}

#[deriving(Clone,Show,Eq,PartialEq)]
pub enum Arg {
  ByVal(Expr),
  ByRef(String)
}

#[deriving(Show,PartialEq,Eq,Clone)]
pub enum Stmt {
  Assign(String, Expr),
  While(Expr,Vec<Stmt>),
  Condition(Expr, Vec<Stmt>, Vec<Stmt>),
  RoutineCall(String, Vec<Arg>)
}

#[deriving(Show,PartialEq,Eq,Clone)]
pub enum Decl {
  // Routine name, parameters, body
  Routine(String, Vec<String>, Vec<Stmt>)
}

#[deriving(Show,PartialEq,Eq,Clone)]
pub struct Program(pub Vec<Decl>);