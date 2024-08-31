const fn is_target_little_endian() -> bool {
    u16::from_ne_bytes([1, 0]) == 1
}

fn main() {
    if is_target_little_endian() {
        println!("We are little-endian!");
    } else {
        println!("We are big-endian!");
    }
}
