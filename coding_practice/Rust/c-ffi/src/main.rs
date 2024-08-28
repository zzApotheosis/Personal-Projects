#[repr(C)]
pub struct CoolStruct {
    pub x: cty::c_int,
    pub y: cty::c_int,
}

extern "C" {
    pub fn cool_function(i: cty::c_int, c: cty::c_char, cs: *mut CoolStruct);
    pub fn set_x(new_x: cty::c_int);
    pub fn get_x() -> cty::c_int;
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
    let x: cty::c_int;
    println!("Calling unsafe C functions!");
    unsafe {
        set_x(123 as cty::c_int);
        x = get_x();
    }
    println!("x = {}", x);
    return Ok(());
}
