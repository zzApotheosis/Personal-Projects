/*
 * Unlike C, Rust only allows placing labels on certain expressions. The loop
 * is one of the most commmon expressions that can be referenced through labels.
 *
 * See: https://doc.rust-lang.org/book/ch03-05-control-flow.html?highlight=label#repetition-with-loops
 */

fn main() {
    'main_loop: loop {
        let mut x = 0i32;
        'inner_loop: loop {
            if x >= 3 {
                break 'main_loop;
            }

            if x > 10 {
                /* Technically this would never run */
                break 'inner_loop;
            }

            eprintln!("Not ready...");
            x += 1;
        }
    }

    eprintln!("We made it!");
}
