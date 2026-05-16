use foo;

fn main() {
    println!("Hello, world!");
    let a = 6;
    let b = 7;
    eprintln!("{a} + {b} = {}", foo::add(a, b));
}
