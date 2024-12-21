/*
 * These unit test functions are considered "dead code" because they're only used
 * in unit tests. The Rust compiler by default assumes written Rust code is expected
 * to be used in real implementations.
 */
#[allow(dead_code)]
mod c {
    extern "C" {
        pub fn test_add(_: &mut i32);
        pub fn test_bad_add(_: &mut i32);
    }
}

fn main() {
    println!("Hello, world!");
    println!("Use `cargo test` to run the unit tests");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        let mut result: i32 = 0;
        unsafe {
            c::test_add(&mut result);
        }
        assert_eq!(result, 0);
    }

    /*
     * This is expected to fail
     */
    #[test]
    fn test_bad_add() {
        let mut result: i32 = 0;
        unsafe {
            c::test_bad_add(&mut result);
        }
        assert_eq!(result, 0);
    }
}
