use std::{
    sync::mpsc::{self, Receiver, Sender},
    thread::{self, JoinHandle},
};

const NTHREADS: u32 = 4;

fn main() -> std::result::Result<(), i32> {
    let (tx, rx): (Sender<i32>, Receiver<i32>) = mpsc::channel();
    let mut children: Vec<JoinHandle<()>> = Vec::new();

    for id in 0..NTHREADS {
        let thread_tx = tx.clone();

        let child = thread::spawn(move || {
            thread_tx.send(id as i32).unwrap();
            println!("thread {} finished", id);
        });

        children.push(child);
    }

    let mut ids = Vec::with_capacity(NTHREADS as usize);
    for _ in 0..NTHREADS {
        ids.push(rx.recv());
    }

    for child in children {
        child.join().expect("child thread panicked");
    }

    println!("{:?}", ids);

    Ok(())
    // Err(1)
}
