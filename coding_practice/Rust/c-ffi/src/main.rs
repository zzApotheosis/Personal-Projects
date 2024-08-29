#[repr(C)]
struct CoolStruct {
    pub x: cty::c_int,
    pub y: cty::c_int,
}

extern "C" {
    fn cool_function(i: cty::c_int, c: cty::c_char, cs: *const CoolStruct);
    fn set_x(new_x: cty::c_int);
    fn get_x() -> cty::c_int;
}

fn main() -> Result<(), i32> {
    let s1 = CoolStruct { x: 69, y: 11 };
    let s2 = CoolStruct { x: 420, y: 0 };
    unsafe {
        cool_function(s2.x, 'z' as cty::c_char, &s1);
        cool_function(s1.x, 'S' as cty::c_char, &s2);
    }

    let mut x: cty::c_int = 24;
    println!("Rust x = {}", x);
    println!("Calling unsafe C functions!");
    unsafe {
        println!("From Rust, C static x = {}", get_x());
        set_x(123 as cty::c_int);
        println!("From Rust, C static x = {}", get_x());
        x = get_x();
    }
    println!("Rust x = {}", x);
    return Ok(());
}
