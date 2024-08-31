const ARRAY_SIZE: usize = 32;
fn main() -> std::result::Result<(), i32> {
    let mut buf: [u8; ARRAY_SIZE] = [0; ARRAY_SIZE];
    for i in 0..buf.len() {
        buf[i] = i as u8;
    }
    println!("One cool fixed-size stack array:");
    println!("{:?}", buf);
    Ok(())
}
