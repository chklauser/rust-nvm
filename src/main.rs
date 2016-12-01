#[macro_use]
extern crate log;

extern crate env_logger;

#[cfg(all(test, bench))]
extern crate test;

mod vm;
mod frontend;

#[cfg(test)]
mod tests;

#[cfg(not(test))]
fn main() {
    env_logger::init().unwrap();
    println!("Compiling program");
    let program = frontend::compile_program(r#"
    routine main(x) {
      x <- 15
    }
    "#)
        .unwrap();

    println!("Executing program");
    let mut param = [0];
    vm::machine::execute(&program, 0, &mut param).unwrap();
    println!("Result: {}", param[0]);
}
