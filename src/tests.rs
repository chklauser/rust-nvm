
use super::frontend;
use super::vm;

use test::Bencher;
use test;

#[test]
fn sample() {
  let main = frontend::compile_function("main".to_string(), vec!["return", "x"].as_slice(), 
    r#"
    y <- 2*x+5;
    z <- y/x;
    return <- y + z
    "#).unwrap();

  info!("Executing function");
  let mut params = vec![0,4];
  vm::machine::execute(&main, params.as_mut_slice()).unwrap();

  info!("Evaluating");
  assert_eq!(params[1],4);
  assert_eq!(params[0],(2*4+5) + (2*4+5)/4);
}

#[test]
fn compile_fib_iter() {
  let result = fib::compile_iter_fib();
  assert!(result.is_ok());
  debug!("fib_iter compiled to {} instructions.", result.unwrap().instructions.len());
}

#[test]
fn run_gen_fib8() {
  let n = 8;
  let a = 1;
  let b = 1;
  let routine = fib::gen_fib_routine(n).unwrap();
  let mut params = [0, a, b];
  assert!(vm::machine::execute(&routine, params).is_ok());
  assert_eq!(params[1],a);
  assert_eq!(params[2],b);
  assert_eq!(params[0],fib::control_fib(n,a,b));
}

#[test]
fn run_fib_iter8() {
  let routine = fib::compile_iter_fib().unwrap();
  let n = 8;
  let a = 1;
  let b = 1;
  let mut params = [0, n, a, b];
  vm::machine::execute(&routine, params).unwrap();
  assert_eq!(params[1],n);
  assert_eq!(params[2],a);
  assert_eq!(params[3],b);
  assert_eq!(params[0],fib::control_fib(n as uint,a,b));
}

mod fib {
  use super::super::frontend;
  use super::super::vm;
  use super::super::vm::bytecode;
  use std::iter::range_inclusive;

  // generates a routine that computes n iterations of fibonacci over 2 initial parameters
  pub fn gen_fib_routine(n:uint) -> frontend::FrontendResult<vm::bytecode::Routine> {
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

        debug!("routine fib{}(return, a, b) [ instruction count: {} ]", n, body.len());

        let name = format!("fib{:u}",n);
        return frontend::compile_function(name, ["return","a","b"], body.as_slice());
      }
    }
  }

  pub fn control_fib(n: uint, a: int, b: int) -> int{
    return match n {
      0 => 0,
      1 => a,
      2 => a+b,
      n => {
        let mut fim1 = b; //F_{i-1}
        let mut fim2 = a; //F_{i-2}
        for _ in range_inclusive(3,n){
          let next = fim1 + fim2;
          fim2 = fim1;
          fim1 = next;
        };
        fim1
      }
    }
  }

  pub fn compile_iter_fib() -> frontend::FrontendResult<bytecode::Routine> {
    let code = r#"
      // parameters: return, n, a, b
      if not n {
        return <- 0
      } else {
        if not (n - 1) {
          return <- a
        } else {
          if not (n - 2) {
            return <- a + b
          } else {
            fim1 <- b;
            fim2 <- a;
            i <- 3;
            while (n-i)+1 {
              next <- fim1 + fim2;
              fim2 <- fim1;
              fim1 <- next;
              i <- i + 1
            };
            return <- fim1
          }
        }
      }
    "#;
    return frontend::compile_function("fib_iter".to_string(),["return","n","a","b"], code);
  }
}

#[bench]
fn bench_run_control_fib8(b: &mut Bencher) {
  b.iter(|| test::black_box(fib::control_fib(8,1,1)));
}

#[bench]
fn bench_run_gen_memo_fib8(b: &mut Bencher) {
  let routine = fib::gen_fib_routine(8).unwrap();
  let control = fib::control_fib(8,1,1);
  b.iter(|| {
    let mut params = [0,1,1];
    vm::machine::execute(&routine, &mut params).unwrap();
    assert_eq!(params[0],control);
  })
}

#[bench]
fn bench_run_gen_memo_fib8_dystack(b: &mut Bencher) {
  let mut routine = fib::gen_fib_routine(8).unwrap();
  let control = fib::control_fib(8,1,1);
  routine.max_stack_size = None;
  b.iter(|| {
    let mut params = [0,1,1];
    vm::machine::execute(&routine, &mut params).unwrap();
    assert_eq!(params[0],control);
  })
}

#[bench]
fn bench_run_control_fib100(b: &mut Bencher) {
  b.iter(|| test::black_box(fib::control_fib(100,1,1)));
}

#[bench]
fn bench_run_gen_memo_fib100(b: &mut Bencher) {
  let routine = fib::gen_fib_routine(100).unwrap();
  let control = fib::control_fib(100,1,1);
  b.iter(|| {
    let mut params = [0,1,1];
    vm::machine::execute(&routine, &mut params).unwrap();
    assert_eq!(params[0],control);
  })
}

#[bench]
fn bench_iter_fib8(b: &mut Bencher) {
  let routine = fib::compile_iter_fib().unwrap();
  let n = 8;
  let mut params = [0, n, 1, 1];  
  b.iter(|| vm::machine::execute(&routine, params).unwrap());
}

#[bench]
fn bench_iter_fib100(b: &mut Bencher) {
  let routine = fib::compile_iter_fib().unwrap();
  let n = 100;
  let mut params = [0, n, 1, 1];  
  b.iter(|| vm::machine::execute(&routine, params).unwrap());
}