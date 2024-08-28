fn func1(mut a: &mut u32) {
    func2(&mut a);
}

fn func2(b: &mut &mut u32) {
    (**b) += 1;
}

fn show(s: &str, x: u32) {
    println!("{} = {}", s, x);
}

fn main() {
    let mut a: u32 = 50;
    show("a", a);
    func1(&mut a);
    show("a", a);
}
