pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}

pub fn public_function_in_test_crate() {
    println!("Hello, rlib!");
}

pub mod cool {
    pub fn cool_function() {
        println!("This is one cool function, if I do say so myself.");
    }
}
