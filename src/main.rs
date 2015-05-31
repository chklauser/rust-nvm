#![feature(custom_derive,box_syntax,collections)]
#![feature(plugin)]
#![plugin(peg_syntax_ext)]

#[macro_use]
extern crate log;

#[cfg(test)]
extern crate test;

mod vm;
mod frontend;

#[cfg(test)]
mod tests;

#[cfg(not(test))]
fn main() {
  println!("Compiling program");
  let program = frontend::compile_program(r#"
    routine main(x) {
      x <- 15
    }
    "#).unwrap();

  println!("Executing program");
  let mut param = [0];
  vm::machine::execute(&program, 0,&mut param).unwrap();
  println!("Result: {}",param[0]);
}
