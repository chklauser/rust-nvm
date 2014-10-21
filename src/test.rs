
use super::frontend;
use super::vm;

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