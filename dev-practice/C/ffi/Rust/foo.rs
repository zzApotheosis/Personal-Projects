#[no_mangle]
pub extern "C" fn rust_func() -> () {
    eprintln!("Function in Rust!!!");
}
