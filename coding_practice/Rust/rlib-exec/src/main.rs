extern crate rlib;

//use rlib::cool; // Use public module

fn main() {
    println!("Hello world!");
    rlib::public_function_in_test_crate();
    rlib::cool::cool_function();
    println!("{}", rlib::add(5, 2));
    //cool::cool_function();
}
