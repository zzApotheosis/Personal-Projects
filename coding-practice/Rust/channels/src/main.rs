use std::{
    io::{Read, Write},
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

// fn dump(n: u32) {
//     println!("{:#034b}", n);
// }

fn u8_slice_to_u32(buf: &[u8; 4], target: &mut u32) {
    (*target) = 0u32;
    for i in 0..buf.len() {
        (*target) <<= 8;
        (*target) |= buf[i] as u32;
    }
}

fn main() -> std::result::Result<(), i32> {
    let (tx, rx): (Sender<u32>, Receiver<u32>) = mpsc::channel();
    let mut children: Vec<JoinHandle<()>> = Vec::new();
    let mut buf: [u8; 4] = [0u8; 4];
    let mut rng_source = std::fs::File::open("/dev/urandom").ok().unwrap();

    for id in 0..NTHREADS {
        rng_source.read(&mut buf).expect("NOT GOOD");
        let mut rand_delay: u32 = 0u32;
        u8_slice_to_u32(&buf, &mut rand_delay);

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

    /*
     * I tried to replicate the getc() function in C here,
     * but it doesn't behave the same way. It will properly
     * get a single character from stdin, but it only does so
     * when the stdin buffer is flushed with a newline. That
     * is, it will only accept the first character when the
     * user presses enter on the keyboard. Ctrl-D may also
     * work.
     *
     * In any case, this is a good snippet of code to learn
     * the different ways to implement things in Rust and to
     * understand what is considered good practice and what
     * isn't.
     */
    std::io::stdout()
        .write_all(b"Press any key to continue\n")
        .ok();
    std::io::stdin()
        .lock()
        .bytes()
        .next()
        .and_then(|result| result.ok())
        .map(|byte| byte as i32);

    Ok(())
    // Err(1)
}
