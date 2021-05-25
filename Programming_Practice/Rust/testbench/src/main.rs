//#[macro_use] extern crate text_io;
//extern crate text_io;

// Imports
//use std::io::{self, Read, Write};
use std::io::Write;

// Main
fn main() {
    // Variables can be type annotated.
    let logical: bool = true;
    
    let a_float: f64 = 1.0;  // Regular annotation
    let an_integer   = 5i32; // Suffix annotation
    
    // Or a default will be used.
    let default_float   = 3.0; // `f64`
    let default_integer = 7;   // `i32`
    
    // A type can also be inferred from context 
    let mut inferred_type = 12; // Type i64 is inferred from another line
    inferred_type = 4294967296i64;
    
    // A mutable variable's value can be changed.
    let mut mutable = 12; // Mutable `i32`
    mutable = 21;
    
    // Error! The type of a variable can't be changed.
    //mutable = true;
    
    // Variables can be overwritten with shadowing.
    let mutable = true;
    
    println!("{}", logical);
    println!("{}", a_float);
    println!("{}", an_integer);
    println!("{}", default_float);
    println!("{}", default_integer);
    println!("{}", inferred_type);
    println!("{}", mutable);
    
    let mut atest: i32 = 10;
    
    println!("test: {}", atest);
    atest += 1;
    println!("test: {}", atest);
    
    //test();
    test2();
}

fn test() {
    let mut i: i32 = 1337;
    let mut text = String::new();

    std::io::stdout().write_all("Enter bullshit: ".as_bytes()).unwrap();
    std::io::stdout().flush().unwrap();
    std::io::stdin().read_line(&mut text).unwrap();
    std::io::stdout().write_all(text.as_bytes()).unwrap();
}

fn test2() {
    let n = 5;

    if n < 0 {
        print!("{} is negative", n);
    } else if n > 0 {
        print!("{} is positive", n);
    } else {
        print!("{} is zero", n);
    }

    let big_n = if n < 10 && n > -10 {
        println!(", and is a small number, increase ten-fold");
        10 * n
    } else {
        println!(", and is a big number, halve the number");
        n / 2
    };

    println!("{} -> {}", n, big_n);
}

