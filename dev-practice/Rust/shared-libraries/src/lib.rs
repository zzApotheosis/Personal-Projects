pub fn add(left: u64, right: u64) -> u64 {
    left + right
}

pub fn div(top: u64, bottom: u64) -> u64 {
    top / bottom
}

enum Coin<T> {
    Penny { t: T, tt: T },
    Nickel,
    Dime,
    Quarter,
    Dollar,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }

    #[test]
    #[should_panic]
    fn divide_by_zero() {
        let _ = div(10, 0);
    }

    #[test]
    #[should_panic]
    #[ignore]
    fn ignored_test() {
        panic!("Oh dear");
    }

    #[test]
    #[should_panic]
    fn not_ignored_test() {
        panic!("Oh dear");
    }

    #[test]
    fn make_coin() {
        let c = Coin::<&str>::Penny {
            t: "wut",
            tt: "bruh",
        };
        match &c {
            Coin::Penny { t, tt } => {
                eprintln!(
                    "Wow! This penny has some weird stuff written on it... {} and {}",
                    t, tt
                );
            }
            _ => {}
        }
    }
}
