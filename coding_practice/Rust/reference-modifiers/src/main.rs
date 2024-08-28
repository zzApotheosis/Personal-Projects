fn main() {
    /* Standard mutable references */
    let mut a: u32 = 1336;
    let a_ref: &mut u32 = &mut a;
    (*a_ref) += 1;
    println!("a = {}", *a_ref);

    /* Unsafe raw pointers */
    let b: u32 = 420;
    let b_raw = &b as *const u32;
    unsafe {
        println!("b = {}", *b_raw);
    }

    /* Standard immutable references */
    let c: u32 = 69;
    let c_ref = &c;
    println!("c = {}", *c_ref);
    //(*c_ref) += 1; // This won't work!
}
