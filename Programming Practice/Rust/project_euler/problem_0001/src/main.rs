fn main() {
    // Define method variables
    let mut sum: u32 = 0;
    let limit: u32 = 1000;

    // Loop
    for i in 0..limit {
        if i % 3 == 0 || i % 5 == 0 {
            sum += i;
        }
    }

    // Done
    println!("{}", sum);
}

