
extern crate combine;
extern crate combine_language;

use self::combine_language::{LanguageEnv, LanguageDef, Identifier, expression_parser, Assoc, Fixity, Lex, Reserved};
use self::combine::{satisfy, Parser, parser, ParseResult, optional};
use self::combine::primitives::{Stream, StreamOnce};
use self::combine::combinator::{sep_by, many};
use self::combine::char::{string,letter, alpha_num, char};
use super::ast::{Stmt, Program, Expr, Arg};

use super::FrontendError;
use std::fmt;
use std::error::Error;

impl <I> From<combine::ParseError<I>> for FrontendError where
        I : StreamOnce, combine::ParseError<I> : fmt::Display{
    fn from(e: combine::ParseError<I>) -> Self {
        FrontendError::FeParserError(format!("{}", &e))
    }
}
fn arg_list<I>(toy: &LanguageEnv<I>, input: I) -> ParseResult<Vec<Arg>, I> where I : Stream<Item=char> {
    let expr = parser(|i| expr(toy, i));
    let arg = (toy.reserved("ref").with(toy.identifier()).map(|id| Arg::ByRef(id))).or(expr.map(Arg::ByVal));
    let mut arg_list = toy.parens(sep_by(arg, toy.lex(char(','))));
    arg_list.parse_stream(input)
}
fn body<I>(toy: &LanguageEnv<I>, input: I) -> ParseResult<Vec<Stmt>, I> where I : Stream<Item=char> {
    // WHILE
    let while_loop = toy.reserved("while").with(parser(|i| expr(toy, i)))
        .and(toy.braces(parser(|i| body(toy, i))))
        .map(|(cond,stmts)| Stmt::While(cond, stmts));

    // CONDITION
    let if_condition = toy.reserved("if").with(parser(|i| expr(toy, i)))
        .and(toy.braces(parser(|i| body(toy, i)))).and(
        optional(toy.reserved("else").with(toy.braces(parser(|i| body(toy, i))))))
        .map(|((cond,ifBlock),optElseBlock)|
            Stmt::Condition(cond, ifBlock, optElseBlock.unwrap_or_else(||Vec::new())) );

    // ASSIGNMENT
    let assignment = toy.identifier().and(toy.reserved_op("<-").with(parser(|i| expr(toy, i))))
        .map(|(lhs, rhs)| Stmt::Assign(lhs, rhs));

    // CALL
    let call = toy.reserved("call").with(toy.identifier()).and(parser(|i| arg_list(toy, i)))
        .map(|(id, args)| Stmt::RoutineCall(id, args));

    // STATEMENT
    let statement = while_loop.or(if_condition).or(assignment).or(call);

    // STATEMENT LIST
    sep_by(statement, toy.lex(char(';'))).parse_stream(input)
}
fn expr<I>(toy: &LanguageEnv<I>, input: I) -> ParseResult<Expr, I> where I : Stream<Item=char> {
    // ATOM
    let atom = toy.identifier().and(optional(parser(|i| arg_list(toy, i))))
        .map(|(id, optArgs)| {
            let id2 = id.clone();
            optArgs.map(|args| Expr::Function(id, args)).unwrap_or(Expr::Variable(id2))
        })
        .or(toy.parens(parser(|i| expr(toy, i))))
        .or(toy.integer().map(|x| Expr::Constant(x as isize)));

    // HIGH PRECEDENCE UNARY OPERATORS
    let unbin_not = many(toy.reserved_op("!")).and(atom)
        .map(|(ps,a): (Vec<&'static str>, Expr)| ->  Expr {
            let ex : Expr = ps.into_iter().fold(a, |e: Expr, _:&'static str| -> Expr { Expr::BinaryNot(Box::new(e)) } );
            ex
        } );

    // BINARY OPERATORS
    let bin_op = toy.reserved_op("+")
        .or(toy.reserved_op("-"))
        .or(toy.reserved_op("*"))
        .or(toy.reserved_op("/"))
        .or(toy.reserved_op("%"))
        .or(toy.reserved_op("<"))
        .or(toy.reserved_op(">"))
        .or(toy.reserved_op("<="))
        .or(toy.reserved_op(">="))
        .or(toy.reserved_op("=="))
        .or(toy.reserved_op("!="))
        .map(|op| {
            let prec = match op {
                "<" | ">" | "<=" | ">=" | "==" | "!=" => 4,
                "+" | "-" => 6,
                "*" | "/" | "%" => 7,
                x => panic!("Internal parser error. Unknown operator/missing operator definition {}", x)
            };
            (op, Assoc { precedence: prec, fixity: Fixity::Left })
        });
    let bin_expr = expression_parser(unbin_not, bin_op, |lhs, op, rhs| {
        let mk_op = match op {
            "<" => Expr::LessThan,
            ">" => Expr::GreaterThan,
            ">=" => Expr::GreaterThanOrEqualTo,
            "<=" => Expr::LessThanOrEqualTo,
            "==" => Expr::EqualTo,
            "!=" => Expr::NotEqualTo,
            "+" => Expr::Addition,
            "-" => Expr::Subtraction,
            "*" => Expr::Multiplication,
            "/" => Expr::Division,
            "%" => Expr::Remainder,
            x => panic!("Internal parser error. Unknown operator/missing operator definition {}", x)
        };
        mk_op(Box::new(lhs), Box::new(rhs))
    });

    let mut log_not_expr = many(toy.reserved("not")).and(bin_expr)
        .map(|(ps,a) : (Vec<&'static str>, Expr)| ps.into_iter().fold(a, |e, _| Expr::Not(Box::new(e))));

    log_not_expr.parse_stream(input)
}

pub fn parse_routine(routine_body_text: &str) -> Result<Vec<Stmt>, combine::ParseError<&str>> {
    let toy = new_toy_env();

    parser(|i| body(&toy, i)).parse(routine_body_text).map(|(stmts, _)| stmts)
}

pub fn parse_program(program_text: &str) -> Result<Program, combine::ParseError<&str>> {
    return panic!("program not implemented");
}

// ================================================================================================

fn new_toy_env<'a, I>() -> LanguageEnv<'a, I> where I : 'a+Stream<Item=char> {
    LanguageEnv::new(LanguageDef {
        ident: Identifier {
            start: letter(),
            rest: alpha_num(),
            reserved: ["if", "else", "while", "call"].iter().map(|x| (*x).into()).collect()
        },
        op: Identifier {
            start: satisfy(|c| "+-*/<>=%".chars().any(|x| x == c)),
            rest: satisfy(|c| "+-*/<>=%".chars().any(|x| x == c)),
            reserved: ["+","-","*","/","%", "<-"].iter().map(|x| (*x).into()).collect()
        },
        comment_start: string("/*").map(|_|()),
        comment_end: string("*/").map(|_|()),
        comment_line: string("//").map(|_|())
    })
}

// use super::super::ast::*;
// use super::super::ast::Decl::*;
// use super::super::ast::Expr::*;
// use super::super::ast::Stmt::*;
// use super::super::ast::Arg::*;
//
// use std::result::Result;
//
// #[pub]
// program -> Program
// = __ ds:(__ d:decl __ { d })* { Program(ds) };
//
// decl -> Decl
// = "routine" __ id:identifier __ "(" __ args:(identifier ** (__ "," __)) __")" __ "{"
// ss:body
// "}" { Routine(id, args, ss) };
//
// #[pub]
// body -> Vec<Stmt>
// = __ ss:(statement ** (__ ";" __)) __ {ss} ;
//
// statement -> Stmt
// = "while" __ cond:expr "{" body:body "}" { While(cond, body) }
// / if_condition
// / lhs:identifier __ "<-" __ rhs:expr  { Assign(lhs, rhs) }
// / "call" __ id:identifier __ "(" __ argv:(arg ** (__ "," __)) __ ")" { RoutineCall(id, argv) };
//
// if_condition -> Stmt
// = "if" __ cond:expr "{" ifbranch:body "}" __
// elsebranch_opt:( "else" __ ( "{" branch:body "}" { branch }
// / inner:if_condition  { vec![inner] }) )? { Condition(cond, ifbranch, elsebranch_opt.unwrap_or_else(|| Vec::new())) }
//
// arg -> Arg
// = "ref" __ id:identifier { ByRef(id) }
// / e:expr { ByVal(e) }
//
// expr -> Expr
// = e:logicalUnaryExpr { e }
//
// logicalUnaryExpr -> Expr
// = "not" __ e:logicalUnaryExpr __ { Not(box e) }
// / e:cmpExpr { e }
//
// cmpExpr -> Expr
// = lhs:sumExpr __ right:( "<"  __ rhs:sumExpr __ { (1u8,rhs) }
// /  "<=" __ rhs:sumExpr __ { (2u8,rhs) }
// /  "==" __ rhs:sumExpr __ { (3u8,rhs) }
// /  "!=" __ rhs:sumExpr __ { (4u8,rhs) }
// /  ">"  __ rhs:sumExpr __ { (5u8,rhs) }
// /  ">=" __ rhs:sumExpr __ { (6u8,rhs) }
// /  "" { (7u8,Constant(0)) }
// ) { match right {
// (1,rhs) => LessThan(box lhs, box rhs),
// (2,rhs) => LessThanOrEqualTo(box lhs, box rhs),
// (3,rhs) => EqualTo(box lhs, box rhs),
// (4,rhs) => NotEqualTo(box lhs, box rhs),
// (5,rhs) => GreaterThan(box lhs, box rhs),
// (6,rhs) => GreaterThanOrEqualTo(box lhs, box rhs),
// _ => lhs,
// }
// };
//
// sumExpr -> Expr
// = lhs:productExpr __ "+" __ rhs:sumExpr __ { Addition(box lhs, box rhs) }
// / lhs:productExpr __ "-" __ rhs:sumExpr __ { Subtraction(box lhs, box rhs) }
// / e:productExpr __ { e };
//
// productExpr -> Expr
// = lhs:binBinExpr __ "*" __ rhs:productExpr __ { Multiplication(box lhs, box rhs) }
// / lhs:binBinExpr __ "/" __ rhs:productExpr __ { Division(box lhs, box rhs) }
// / lhs:binBinExpr __ "%" __ rhs:productExpr __ { Remainder(box lhs, box rhs) }
// / e:binBinExpr __ { e }
//
// binBinExpr -> Expr
// = e:unBinExpr __ { e }
//
// unBinExpr -> Expr
// = "!" __ e:unBinExpr __ { BinaryNot(box e) }
// / e:atomExpr __ { e }
//
// atomExpr -> Expr
// = n:number __ { Constant(n) }
// / id:identifier __ "(" __ argv:(arg ** (__ "," __)) __ ")" __ { Function(id,argv) }
// / id:identifier __ { Variable(id) }
// / "(" __ e:expr ")" __ { e }
//
// number -> isize
// = [0-9_]+ { isize::from_str_radix(match_str, 10).unwrap() }
//
// identifier -> String
// = ([a-zA-Z_][a-zA-Z0-9_]*)!("while"/"if"/"else"/"not"/"routine"/"call"/"ref") { match_str.to_string() }
//
// Taken from https://github.com/kevinmehall/rust-peg/blob/master/src/grammar.rustpeg
// __ = (whitespace / eol / comment)*
//
// comment
// = singleLineComment
// / multiLineComment
//
// singleLineComment
// = "//" (!eolChar .)*
//
// multiLineComment
// = "/*" (!"*/" .)* "*slash"
//
// Modeled after ECMA-262, 5th ed., 7.3. */
// eol
// = "\n"
// / "\r\n"
// / "\r"
// / "\u2028"
// / "\u2029"
//
// eolChar
// = [\n\r\u2028\u2029]
//
// Modeled after ECMA-262, 5th ed., 7.2. */
// whitespace
// = [ \t\u00A0\uFEFF\u1680\u180E\u2000-\u200A\u202F\u205F\u3000] // \v\f removed
//
// "#);
//
//
