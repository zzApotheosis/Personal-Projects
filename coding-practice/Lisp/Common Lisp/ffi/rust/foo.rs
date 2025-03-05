/*
 * Try modifying these functions to see how they affect the Lisp implementation :)
 */

#[no_mangle]
pub fn my_add(a: i32, b: i32) -> i32 {
    eprintln!("Adding numbers in Rust!");
    a + b
}

static mut DATA: i32 = 0i32;

#[no_mangle]
pub fn set_data(a: i32) {
    unsafe {
	DATA = a;
    }
}

#[no_mangle]
pub fn get_data() -> i32 {
    unsafe {DATA}
}

