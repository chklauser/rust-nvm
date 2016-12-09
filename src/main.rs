#![allow(dead_code)]

#[macro_use]
extern crate log;

extern crate env_logger;

#[macro_use]
extern crate clap;

#[cfg(all(test, bench))]
extern crate test;

mod vm;
mod frontend;

#[cfg(test)]
mod tests;

#[cfg(not(test))]
fn main() {
    use clap::{App, Arg};
    use std::io::{stderr, Write, Read};
    use std::process::exit;
    use std::fs::{File};
    use std::path::Path;
    use std::num::ParseIntError;

    let app = App::new(env!("CARGO_PKG_NAME"))
        .version(crate_version!())
        .about(env!("CARGO_PKG_DESCRIPTION"))
        .arg(Arg::with_name("INPUT")
            .help("Toy script to run")
            .required(false)
            .default_value("-")
            .index(1))
        .arg(Arg::with_name("ARG")
            .help("integer to passed to the program")
            .required(false)
            .index(2)
            .multiple(true));

    let matches = app.get_matches();
    let input_path = matches.value_of("INPUT").unwrap_or("-");
    let args_r : Vec<Result<isize, ParseIntError>> =
        matches.values_of_lossy("ARG").unwrap_or_else(Vec::new).into_iter()
        .map(|x| isize::from_str_radix(&x[..], 10))
        .collect();

    // Parse all of the arguments into a Vec<isize>
    let mut arg_n = 1;
    let mut has_error = false;
    for arg_r in &args_r {
        if let &Err(ref e) = arg_r {
            write!(stderr(), "Failed to parse argument {}: {}", arg_n, e)
                .expect("Cannot write to stderr");
            has_error = true;
        }

        arg_n += 1;
    }
    if has_error {
        exit(2);
    }
    let mut args : Vec<isize> = args_r.into_iter().map(|u| u.unwrap()).collect();

    // Read input file into in-memory buffer
    let input_fs_path = Path::new(input_path);
    let input_file_r = File::open(&input_fs_path);
    let mut input_file = match input_file_r {
        Err(e) => {
            write!(stderr(), "Failed to open script file {}: {}",
                   input_fs_path.to_string_lossy(), e).expect("Cannot write to stderr");
            exit(3)
        },
        Ok(v) => v
    };

    let mut input_buf = String::new();
    if let Err(e) = input_file.read_to_string(&mut input_buf) {
        write!(stderr(), "Failed to read script file {}: {}",
            input_fs_path.to_string_lossy(), e).expect("Cannot write to stderr");
    }

    // Compile program
    let program = match frontend::compile_program(&input_buf[..]) {
        Err(e) => {
            write!(stderr(), "Failed to compile program {}: {}",
                   input_fs_path.to_string_lossy(), e).expect("Cannot write to stderr");
            exit(4)
        },
        Ok(v) => v
    };

    // Execute program
    if let Err(e) = vm::machine::execute(&program, 0, &mut args[..]) {
        write!(stderr(), "Runtime error: {}", e).expect("Cannot write to stderr");
        exit(5);
    }

    // Write arguments back out
    let rendered_args : Vec<String> = args.into_iter().map(|a| format!("{}", a)).collect();
    println!("{}", rendered_args.join(" "));
    exit(0);
}
