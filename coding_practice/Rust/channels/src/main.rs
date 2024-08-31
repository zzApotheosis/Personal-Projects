use std::{
    io::Read,
    sync::mpsc::{self, Receiver, Sender},
    thread::{self, JoinHandle},
    time::Duration,
};

const NTHREADS: u32 = 4;

fn thread_main(id: u32, tx: Sender<u32>, rand_delay: u32) -> std::result::Result<(), i32> {
    // Ignore errors for now. Obviously this is bad practice, but this is just a simple example
    println!("Thread #{id}, sleeping for {rand_delay} nanoseconds");
    std::thread::sleep(Duration::from_nanos(rand_delay as u64));
    tx.send(id).ok();
    Ok(())
}

fn main() -> std::result::Result<(), i32> {
    let (tx, rx): (Sender<u32>, Receiver<u32>) = mpsc::channel();
    let mut children: Vec<JoinHandle<()>> = Vec::new();
    let mut buf: [u8; 4] = [0u8; 4];
    let mut rng_source = std::fs::File::open("/dev/urandom").ok().unwrap();

    for id in 0..NTHREADS {
        rng_source.read(&mut buf).expect("NOT GOOD");
        let rand_delay = u32::from_ne_bytes(buf) % 1_000_000_000u32;

        /*
         * NOTE: With Channels in Rust, there can be multiple transmitters, but there can only be
         * one receiver.
         */
        let thread_tx = tx.clone();

        // let child = thread::spawn(move || {
        //     thread_tx.send(id as i32).unwrap();
        //     println!("thread {} finished", id);
        // });
        // let child = thread::spawn(&thread_main); // This would be valid if thread_main took no arguments
        let child = thread::spawn(move || {
            thread_main(id, thread_tx, rand_delay).ok();
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
