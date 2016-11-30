
use super::ast::{Stmt,Program};

use super::FrontendError;
use std::error::Error;
use std::fmt::{self, Display};

#[derive(Clone,Debug,PartialEq,Eq)]
pub struct ParseError;
impl Error for ParseError {
  fn description(&self) -> &str {
    "ParseError"
  }
}
impl Display for ParseError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.description())
  }
}

impl From<ParseError> for FrontendError {
  fn from(s: ParseError) -> FrontendError {
    FrontendError::FeParserError(s)
  }
}

pub fn parse_routine(routine_body_text: &str) -> Result<Vec<Stmt>,ParseError> {
  return panic!("routine not implemented");
}

pub fn parse_program(program_text: &str) -> Result<Program,ParseError> {
  return panic!("program not implemented");
}

/*

use super::super::ast::*;
use super::super::ast::Decl::*;
use super::super::ast::Expr::*;
use super::super::ast::Stmt::*;
use super::super::ast::Arg::*;

use std::result::Result;

#[pub]
program -> Program
  = __ ds:(__ d:decl __ { d })* { Program(ds) };

decl -> Decl
  = "routine" __ id:identifier __ "(" __ args:(identifier ** (__ "," __)) __")" __ "{" 
    ss:body
    "}" { Routine(id, args, ss) };

#[pub]
body -> Vec<Stmt>
  = __ ss:(statement ** (__ ";" __)) __ {ss} ;

statement -> Stmt
  = "while" __ cond:expr "{" body:body "}" { While(cond, body) }
  / if_condition
  / lhs:identifier __ "<-" __ rhs:expr  { Assign(lhs, rhs) }
  / "call" __ id:identifier __ "(" __ argv:(arg ** (__ "," __)) __ ")" { RoutineCall(id, argv) };

if_condition -> Stmt
  = "if" __ cond:expr "{" ifbranch:body "}" __
     elsebranch_opt:( "else" __ ( "{" branch:body "}" { branch } 
                                / inner:if_condition  { vec![inner] }) )? { Condition(cond, ifbranch, elsebranch_opt.unwrap_or_else(|| Vec::new())) }

arg -> Arg
  = "ref" __ id:identifier { ByRef(id) }
  / e:expr { ByVal(e) }

expr -> Expr
  = e:logicalUnaryExpr { e }

logicalUnaryExpr -> Expr
  = "not" __ e:logicalUnaryExpr __ { Not(box e) }
  / e:cmpExpr { e }

cmpExpr -> Expr
  = lhs:sumExpr __ right:( "<"  __ rhs:sumExpr __ { (1u8,rhs) }
                        /  "<=" __ rhs:sumExpr __ { (2u8,rhs) }
                        /  "==" __ rhs:sumExpr __ { (3u8,rhs) }
                        /  "!=" __ rhs:sumExpr __ { (4u8,rhs) }
                        /  ">"  __ rhs:sumExpr __ { (5u8,rhs) }
                        /  ">=" __ rhs:sumExpr __ { (6u8,rhs) }
                        /  "" { (7u8,Constant(0)) }
                        ) { match right {
                              (1,rhs) => LessThan(box lhs, box rhs),
                              (2,rhs) => LessThanOrEqualTo(box lhs, box rhs),
                              (3,rhs) => EqualTo(box lhs, box rhs),
                              (4,rhs) => NotEqualTo(box lhs, box rhs),
                              (5,rhs) => GreaterThan(box lhs, box rhs),
                              (6,rhs) => GreaterThanOrEqualTo(box lhs, box rhs),
                              _ => lhs,
                            } 
                          };

sumExpr -> Expr
  = lhs:productExpr __ "+" __ rhs:sumExpr __ { Addition(box lhs, box rhs) }
  / lhs:productExpr __ "-" __ rhs:sumExpr __ { Subtraction(box lhs, box rhs) }
  / e:productExpr __ { e };

productExpr -> Expr
  = lhs:binBinExpr __ "*" __ rhs:productExpr __ { Multiplication(box lhs, box rhs) }
  / lhs:binBinExpr __ "/" __ rhs:productExpr __ { Division(box lhs, box rhs) }
  / lhs:binBinExpr __ "%" __ rhs:productExpr __ { Remainder(box lhs, box rhs) }
  / e:binBinExpr __ { e }

binBinExpr -> Expr
  = e:unBinExpr __ { e }

unBinExpr -> Expr
  = "!" __ e:unBinExpr __ { BinaryNot(box e) }
  / e:atomExpr __ { e }

atomExpr -> Expr
  = n:number __ { Constant(n) }
  / id:identifier __ "(" __ argv:(arg ** (__ "," __)) __ ")" __ { Function(id,argv) } 
  / id:identifier __ { Variable(id) }
  / "(" __ e:expr ")" __ { e }

number -> isize
  = [0-9_]+ { isize::from_str_radix(match_str, 10).unwrap() }

identifier -> String
  = ([a-zA-Z_][a-zA-Z0-9_]*)!("while"/"if"/"else"/"not"/"routine"/"call"/"ref") { match_str.to_string() }

// Taken from https://github.com/kevinmehall/rust-peg/blob/master/src/grammar.rustpeg
__ = (whitespace / eol / comment)*

comment
  = singleLineComment
  / multiLineComment

singleLineComment
  = "//" (!eolChar .)*

multiLineComment
  = "/*" (!"*/" .)* "*slash"

/* Modeled after ECMA-262, 5th ed., 7.3. */
eol
  = "\n"
  / "\r\n"
  / "\r"
  / "\u2028"
  / "\u2029"

eolChar
  = [\n\r\u2028\u2029]

/* Modeled after ECMA-262, 5th ed., 7.2. */
whitespace
  = [ \t\u00A0\uFEFF\u1680\u180E\u2000-\u200A\u202F\u205F\u3000] // \v\f removed

"#);

*/


