#[repr(C)]
pub struct CoolStruct {
    pub x: cty::c_int,
    pub y: cty::c_int,
}

extern "C" {
    pub fn cool_function(i: cty::c_int, c: cty::c_char, cs: *mut CoolStruct);
    pub fn get_x() -> i32;
}

fn c_cool_function(i: cty::c_int, c: cty::c_char, cs: *mut CoolStruct) {
    unsafe {
        cool_function(i, c, cs);
    }
}

fn main() -> Result<(), i32> {
    let mut s = CoolStruct { x: 69, y: 11 };
    c_cool_function(1, 0x23, &mut s);
    c_cool_function(10, 0x23, &mut s);
    let mut x = 100;
    println!("x = {}", x);
    unsafe {
        x = get_x();
    }
    println!("x = {}", x);
    return Ok(());
}
