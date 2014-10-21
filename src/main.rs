#![feature(phase)]
#![feature(globs)]
#![feature(macro_rules)]

#[phase(plugin, link)] 
extern crate log;
#[phase(plugin)]
extern crate peg_syntax_ext;

#[cfg(test)]
extern crate test;

use vm::bytecode;
use std::iter::range_inclusive;

#[macro_escape]
mod util;

mod vm;
mod frontend;

#[cfg(test)]
mod tests;



// generates a routine that computes n iterations of fibonacci over 2 initial parameters
#[cfg(not(test))]
fn gen_fib_routine(n:uint) -> frontend::FrontendResult<vm::bytecode::Routine> {
  debug!("Generating fibonacci function for N={:u}",n);
  return match n {
    0 => Ok(bytecode::Routine::new(1,0,vec![
      bytecode::Lit(0), 
      bytecode::StParam(0)])),
    1 => Ok(bytecode::Routine::new(2,0,vec![
      bytecode::LdParam(1), 
      bytecode::StParam(0)])),
    2 => Ok(bytecode::Routine::new(3,0,vec![
      bytecode::LdParam(1), 
      bytecode::LdParam(2), 
      bytecode::StParam(0)])),
    n => {
      // F_1 <- P_1
      // F_2 <- P_2
      // F_i <- F_{i-1} + F_{i-2} for 2 < i < n
      // P_0 <- F_{n-1} + F_{n-2}
      let mut body = r#""#.to_string();
      let write_var = |body: &mut String, i: uint| {
        match i {
          0 => fail!("F0 does not exist"),
          1 => body.push('a'),
          2 => body.push('b'),
          k if k == n => body.push_str("return"),
          i => {
            body.push('F');
            body.push_str(i.to_string().as_slice())
          }
        }
      };
      for i in range_inclusive(3,n) {
        if i > 3 {
          body.push_str(";\n");
        }
        write_var(&mut body, i);
        body.push_str(" <- ");
        write_var(&mut body, i-1);
        body.push_str(" + ");
        write_var(&mut body, i-2);
      }
      body.push_str("\n");

      debug!("routine fib{}(return, a, b) [ {} ]", n, body);

      let name = format!("fib{:u}",n);
      return frontend::compile_function(name, vec!["return","a","b"].as_slice(), body.as_slice());
    }
  }
}

#[cfg(not(test))]
fn control_fib(n: uint, a: int, b: int) -> int{
  return match n {
    0 => 0,
    1 => a,
    2 => a+b,
    n => {
      let mut fim1 = b; //F_{i-1}
      let mut fim2 = a; //F_{i-2}
      for i in std::iter::range_inclusive(3,n){
        let next = fim1 + fim2;
        debug!("control_fib(n={}, a={}, b={}) F{} + F{} = F{} | {} + {} = {}",n,a,b,i-1,i-2,i,fim1,fim2,next);
        fim2 = fim1;
        fim1 = next;
      };
      fim1
    }
  }
}

#[cfg(not(test))]
fn main() {
  
  let n = 100;
  let a = 1;
  let b = 1;

  let main = gen_fib_routine(n).unwrap();

  info!("Executing function with F1={}, F2={}", a, b);  
  let mut params = vec![0,a,b];
  vm::machine::execute(&main, params.as_mut_slice()).unwrap();

  info!("Evaluating results");
  assert_eq!(control_fib(8,1,1),21);
  assert_eq!(params[1],a); // should remain unchanged
  assert_eq!(params[2],b); // should remain unchanged
  assert_eq!(params[0],control_fib(n,a,b));  
  println!("fib{}(F1={}, F2={}) = {}",n,a,b,params[0]);
  println!("   implemented using {} instructions", main.instructions.len());
}
