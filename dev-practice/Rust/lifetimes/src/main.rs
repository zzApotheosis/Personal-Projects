/*
 * This is a simple function that compares two string references, determines which one is longer than the other, and returns that reference.
 */
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() {
        x
    } else {
        y
    }
}

/*
 * Main function.
 */
fn main() {
    let string1 = String::from("short");
    let result;
    {
        let string2 = String::from("longer");
        result = longest(&string1, &string2);
        println!("The longest string is: {}", result);
    }
    // println!("The longest string is: {}", result); // This is not valid because string2 does not live long enough
}
