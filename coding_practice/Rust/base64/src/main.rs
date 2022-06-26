use base64::{encode, decode};

fn main() {
    let my_string: &str = "HAHA SUPER LIGMA";
    let encoded = encode(my_string);
    println!("{}", encoded);
    let decoded = decode(encoded).unwrap();
    for i in decoded {
        print!("{}", i as char);
    }
    println!();
}
