use std::io::{self, Read, Write};

fn main() -> io::Result<()> {
    let mut i: i32 = 0;
    println!("{}", i);
    i += 1;
    println!("{}", i);
    
    let mut text = String::new();
    println!("Fuck you!");
    io::stdin().read_to_string(&mut text)?;
    println!("Received text: {}", text);
    
    // Test
    io::stdout().write_all()?;
    io::stdout().write_all(text.as_bytes())?;
    
    Ok(())
}

