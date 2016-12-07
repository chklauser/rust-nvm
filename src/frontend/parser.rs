
extern crate combine;
extern crate combine_language;

use self::combine_language::{LanguageEnv, LanguageDef, Identifier, expression_parser, Assoc,
                             Fixity};
use self::combine::{satisfy, Parser, parser, ParseResult, optional, many, eof, State, try};
use self::combine::primitives::{Stream, StreamOnce};
use self::combine::combinator::sep_by;
use self::combine::char::{string, letter, alpha_num, char};
use super::ast::{Stmt, Program, Expr, Arg, Decl};

use super::FrontendError;
use std::fmt;

impl<I> From<combine::ParseError<I>> for FrontendError
    where I: StreamOnce,
          combine::ParseError<I>: fmt::Display
{
    fn from(e: combine::ParseError<I>) -> Self {
        FrontendError::FeParserError(format!("{}", &e))
    }
}
fn arg_list<I>(toy: &LanguageEnv<I>, input: I) -> ParseResult<Vec<Arg>, I>
    where I: Stream<Item = char>
{
    let expr = parser(|i| expr(toy, i));
    let arg = (toy.reserved("ref").with(toy.identifier()).map(|id| Arg::ByRef(id)))
        .or(expr.map(Arg::ByVal));
    let mut arg_list = toy.parens(sep_by(arg, toy.lex(char(','))));
    arg_list.parse_stream(input)
}
fn if_condition<I>(toy: &LanguageEnv<I>, input: I) -> ParseResult<Stmt, I>
    where I: Stream<Item = char>
{
    // CONDITION
    let mut if_condition = toy.reserved("if")
        .with(parser(|i| expr(toy, i)))
        .and(toy.braces(parser(|i| body(toy, i))))
        .and(optional(toy.reserved("else").with(toy.braces(parser(|i| body(toy, i)))
            .or(parser(|i| if_condition(toy, i)).map(|s| vec![s])))))
        .map(|((cond, if_block), opt_else_block)| {
            Stmt::Condition(cond, if_block, opt_else_block.unwrap_or_else(|| Vec::new()))
        });

    if_condition.parse_stream(input)
}
fn body<I>(toy: &LanguageEnv<I>, input: I) -> ParseResult<Vec<Stmt>, I>
    where I: Stream<Item = char>
{
    // WHILE
    let while_loop = toy.reserved("while")
        .with(parser(|i| expr(toy, i)))
        .and(toy.braces(parser(|i| body(toy, i))))
        .map(|(cond, stmts)| Stmt::While(cond, stmts));

    // CONDITION
    let if_condition = parser(|i| if_condition(toy, i));

    // ASSIGNMENT
    let assignment = try(toy.identifier()
            .skip(toy.reserved_op("<-"))
            .and(parser(|i| expr(toy, i))))
        .map(|(lhs, rhs)| Stmt::Assign(lhs, rhs));

    // CALL
    let call = toy.reserved("call")
        .with(toy.identifier())
        .and(parser(|i| arg_list(toy, i)))
        .map(|(id, args)| Stmt::RoutineCall(id, args));

    // STATEMENT
    let statement = while_loop.or(if_condition).or(assignment).or(call);

    // STATEMENT LIST
    sep_by(statement, toy.lex(char(';'))).parse_stream(input)
}
fn expr<I>(toy: &LanguageEnv<I>, input: I) -> ParseResult<Expr, I>
    where I: Stream<Item = char>
{
    // ATOM
    let atom = toy.identifier()
        .and(optional(parser(|i| arg_list(toy, i))))
        .map(|(id, opt_args)| {
            let id2 = id.clone();
            opt_args.map(|args| Expr::Function(id, args)).unwrap_or(Expr::Variable(id2))
        })
        .or(toy.parens(parser(|i| expr(toy, i))))
        .or(toy.integer().map(|x| Expr::Constant(x as isize)));

    // HIGH PRECEDENCE UNARY OPERATORS
    let unbin_not = many(toy.reserved_op("!"))
        .and(atom)
        .map(|(ps, a): (Vec<&'static str>, Expr)| -> Expr {
            let ex: Expr = ps.into_iter().fold(a, |e: Expr, _: &'static str| -> Expr {
                Expr::BinaryNot(Box::new(e))
            });
            ex
        });

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
                x => {
                    panic!("Internal parser error. Unknown operator/missing operator definition {}",
                           x)
                }
            };
            (op,
             Assoc {
                 precedence: prec,
                 fixity: Fixity::Left,
             })
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
            x => {
                panic!("Internal parser error. Unknown operator/missing operator definition {}",
                       x)
            }
        };
        mk_op(Box::new(lhs), Box::new(rhs))
    });

    let mut log_not_expr = many(toy.reserved("not"))
        .and(bin_expr)
        .map(|(ps, a): (Vec<&'static str>, Expr)| {
            ps.into_iter().fold(a, |e, _| Expr::Not(Box::new(e)))
        });

    log_not_expr.parse_stream(input)
}

pub fn parse_routine(routine_body_text: &str)
                     -> Result<Vec<Stmt>, combine::ParseError<State<&str>>> {
    let toy = new_toy_env();

    let mut body = toy.white_space().with(parser(|i| body(&toy, i))).skip(eof());
    body.parse(State::new(routine_body_text)).map(|(stmts, _)| stmts)
}

pub fn parse_program(program_text: &str) -> Result<Program, combine::ParseError<State<&str>>> {
    let toy = new_toy_env();
    let param_list = toy.parens(sep_by(toy.identifier(), toy.lex(char(','))));
    let body = parser(|i| body(&toy, i));
    let routine_decl = toy.reserved("routine")
        .with(toy.identifier())
        .and(param_list)
        .and(toy.braces(body))
        .map(|((id, args), stmts)| {
            let r = Decl::Routine(id, args, stmts);
            r
        });
    let mut program = toy.white_space().with(many(routine_decl)).skip(eof()).map(|ds| {
        let p = Program(ds);
        p
    });

    program.parse(State::new(program_text)).map(|(r, _)| r)
}

// ================================================================================================

fn new_toy_env<'a, I>() -> LanguageEnv<'a, I>
    where I: 'a + Stream<Item = char>
{
    LanguageEnv::new(LanguageDef {
        ident: Identifier {
            start: letter(),
            rest: alpha_num(),
            reserved: ["if", "else", "while", "call", "ref", "routine"]
                .iter()
                .map(|x| (*x).into())
                .collect(),
        },
        op: Identifier {
            start: satisfy(|c| "+-*/<>=%".chars().any(|x| x == c)),
            rest: satisfy(|c| "+-*/<>=%".chars().any(|x| x == c)),
            reserved: ["+", "-", "*", "/", "%", "<-"].iter().map(|x| (*x).into()).collect(),
        },
        comment_start: string("/*").map(|_| ()),
        comment_end: string("*/").map(|_| ()),
        comment_line: string("//").map(|_| ()),
    })
}
