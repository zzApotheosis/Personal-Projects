fn main() {
    println!("Hello, world!");
    unsafe {
        library_function(42);
    }
}

#[link(name = "foo")]
unsafe extern "C" {
    unsafe fn library_function(_: ::std::ffi::c_int) -> ::std::ffi::c_void;
}
