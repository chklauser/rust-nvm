#![feature(phase)]
#![feature(globs)]
#![feature(macro_rules)]

#[phase(plugin, link)] 
extern crate log;
#[phase(plugin)]
extern crate peg_syntax_ext;

#[cfg(test)]
extern crate test;

#[macro_escape]
mod util;

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
