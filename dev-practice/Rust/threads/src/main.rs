use ::std::thread::{self, JoinHandle};
use ::std::time::Duration;
use ::std::vec::Vec;

const NTHREADS: u64 = 16;

fn main() -> ::std::result::Result<(), i32> {
    let mut threads: Vec<JoinHandle<()>> = Vec::new();
    for _ in 0..NTHREADS {
        threads.push(thread::spawn(thread_main))
    }

    thread::sleep(Duration::from_millis(1_500));
    for thread in threads {
        thread.join().expect("Thread should be joined");
    }

    let mut threads: Vec<JoinHandle<()>> = Vec::new();
    for _ in 0..NTHREADS {
        threads.push(thread::spawn(implicit_detach));
    }
    thread::sleep(Duration::from_millis(1_000));

    Ok(())
}

fn thread_main() {
    thread::sleep(Duration::from_millis(1_000));
    eprintln!("Interesting");
}

fn implicit_detach() {
    eprintln!("Ohhhhhh you didn't join me");
}
