use base64::{decode, encode};

fn main() {
    let my_string: &str = "HAHA SUPER LIGMA";
    let encoded = encode(my_string);
    println!("{}", encoded);
    let decoded = decode(encoded).unwrap();
    println!("{}", std::string::String::from_utf8(decoded).unwrap());
}
