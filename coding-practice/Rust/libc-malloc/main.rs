use std::os::raw::{c_char, c_void};

// Must match the same defined size in the C code
//const MSG_SIZE: usize = 1024;

fn main() -> Result<(), i32> {
    c_malloc_example();

    /* Ye, boi */
    Ok(())
}

fn c_malloc_example() -> () {
    let size = 10 * std::mem::size_of::<i32>();
    let ptr = unsafe { malloc(size) };

    if ptr.is_null() {
	eprintln!("malloc failed");
	return;
    }

    let int_ptr = ptr as *mut i32;

    // Write some values to the allocated memory.
    unsafe {
        for i in 0..10 {
            *int_ptr.add(i) = i as i32;
        }
    }

    // Read the values back.
    unsafe {
        for i in 0..10 {
            println!("{}", *int_ptr.add(i));
        }
    }

    // Free the allocated memory using free.
    unsafe {
        free(ptr);
    }

    //Example using c_char and passing to a C function
    let string_size = 11; //size of "Hello World" + null terminator.
    let string_ptr = unsafe { malloc(string_size) } as *mut c_char;

    if string_ptr.is_null(){
        eprintln!("malloc failed!");
        return;
    }

    let rust_string = "Hello World\0"; //null terminated string
    unsafe {
        for (i, byte) in rust_string.as_bytes().iter().enumerate() {
            *string_ptr.add(i) = *byte as c_char;
        }
    }

    println!("String from C memory: {}", unsafe{std::ffi::CStr::from_ptr(string_ptr).to_string_lossy()});

    unsafe{free(string_ptr as *mut c_void)};
}

extern "C" {
    fn malloc(size: usize) -> *mut c_void;
    fn free(ptr: *mut c_void);
}

