static LOREM_IPSUM: &str = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod
tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non
proident, sunt in culpa qui officia deserunt mollit anim id est laborum.";

use std::fs::File;
use std::path::Path;
use std::io::{Write, Read};
use std::string::String;

fn main() {
    let mut file;
    let path = Path::new("lorem_ipsum.txt");
    
    // Write!
    let res = File::create(&path);
    if res.is_ok() {
        file = res.unwrap();
        let res = file.write_all(LOREM_IPSUM.as_bytes());
        if !res.is_ok() {
            panic!("OH SHIT 2");
        }
    } else {
        panic!("OH SHIT");
    }

    // Read!
    let res = File::open(&path);
    if res.is_ok() {
        file = res.unwrap();
        let mut buffer: String = String::new();
        let res = file.read_to_string(&mut buffer);
        if res.is_ok() {
            let res = std::io::stdout().write(format!("{}\n", buffer).as_bytes());
            if !res.is_ok() {
                panic!("OH SHIT 4");
            }
        } else {
            panic!("OH SHIT 3");
        }
    }
}
