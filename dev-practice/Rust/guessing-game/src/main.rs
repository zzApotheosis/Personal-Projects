/*
 * https://doc.rust-lang.org/book/ch02-00-guessing-game-tutorial.html
 */

use std::cmp::Ordering;

const INTRO_MESSAGE: &str = "Guess the number! Type \"quit\" to stop playing.";
const BAD_STDIN_READ: &str = "Couldn't read from stdin... This should never happen!";
const ILLEGAL_INPUT: &str = "You fuckin idiot.";
const GUESS_TOO_LOW: &str = "Too low, fuckin idiot.";
const GUESS_TOO_HIGH: &str = "Too high, fuckin idiot.";
const GUESS_CORRECT: &str = "Good job. Maybe you're not a fuckin idiot after all.";

fn main() {
    eprintln!("{}", INTRO_MESSAGE);

    let num = rand::random_range(1..=100);

    // eprintln!("The secret number is: {num}");

    'main_loop: loop {
        eprint!("> ");
        let mut guess = String::new();
        let bytes_read = std::io::stdin()
            .read_line(&mut guess)
            .expect(BAD_STDIN_READ);

        if bytes_read == 0 {
            break 'main_loop;
        }

        match guess.trim() {
            "quit" => break 'main_loop,
            "" => continue 'main_loop,
            _ => {}
        }

        let guess: u32 = match guess.trim().parse() {
            Ok(val) => val,
            Err(e) => {
                eprintln!("{} {}", ILLEGAL_INPUT, e.to_string());
                continue 'main_loop;
            }
        };

        match guess.cmp(&num) {
            Ordering::Less => eprintln!("{}", GUESS_TOO_LOW),
            Ordering::Greater => eprintln!("{}", GUESS_TOO_HIGH),
            Ordering::Equal => {
                eprintln!("{}", GUESS_CORRECT);
                break 'main_loop;
            }
        }
    }
}
