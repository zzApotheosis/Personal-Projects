#[repr(C)]
pub struct CoolStruct {
    pub x: cty::c_int,
    pub y: cty::c_int,
}

extern "C" {
    pub fn cool_function(
        i: cty::c_int,
        c: cty::c_char,
        cs: *mut CoolStruct
    );
}

fn main() -> Result<(), i32> {
    let mut s = CoolStruct{x: 7, y: 11};
    unsafe{
        cool_function(1, 0x34, &mut s);
    };
    Ok(())
}
