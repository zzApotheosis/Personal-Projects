/*
 * I had AI help me write this... So I will need to come back and
 * get a better understanding of how everything works here.
 */

use std::{
    sync::{Arc, Mutex, mpsc},
    thread::JoinHandle,
};

type Job = Box<dyn FnOnce() + Send + 'static>;

enum Message {
    NewJob(Job),
    Terminate,
}

struct ThreadPool {
    workers: Vec<Worker>,
    sender: mpsc::Sender<Message>,
}

impl ThreadPool {
    pub fn new(n: u32) -> Self {
        let (sender, receiver) = mpsc::channel();
        let receiver = Arc::new(Mutex::new(receiver));

        let mut workers = Vec::with_capacity(n as usize);
        for id in 0..n {
            workers.push(Worker::new(id, Arc::clone(&receiver)));
        }

        Self { workers, sender }
    }

    pub fn queue<F>(&self, f: F)
    where
        F: FnOnce() + Send + 'static,
    {
        let job = Box::new(f);
        self.sender
            .send(Message::NewJob(job))
            .expect("Thread pool disconnected");
    }

    pub fn join(&mut self) {
        // Send terminate signal to each worker
        for _ in &self.workers {
            self.sender
                .send(Message::Terminate)
                .expect("Send terminate failed");
        }

        // Now drain and join each thread (takes ownership)
        for worker in self.workers.drain(..) {
            if let Some(thread) = worker.thread {
                thread.join().expect("Can't join thread!");
            }
        }
    }
}

struct Worker {
    _id: u32,
    thread: Option<JoinHandle<()>>,
}

impl Worker {
    fn new(id: u32, receiver: Arc<Mutex<mpsc::Receiver<Message>>>) -> Self {
        let thread = std::thread::spawn(move || {
            loop {
                let message = receiver
                    .lock()
                    .expect("Mutex poisoned")
                    .recv()
                    .expect("Channel disconnected");

                match message {
                    Message::NewJob(job) => {
                        eprintln!("[{id}] Running job...");
                        job();
                    }
                    Message::Terminate => break,
                }
            }
        });

        Self {
            _id: id,
            thread: Some(thread),
        }
    }
}

fn main() {
    type T = u32;
    const N_JOBS: T = 32;
    const POOL_SIZE: T = 4;
    let mut pool = ThreadPool::new(POOL_SIZE);

    for i in 0..N_JOBS {
        pool.queue(move || {
            eprintln!("This is job {i}!");
            std::thread::sleep(std::time::Duration::from_millis(500));
            eprintln!("Job {i} finished");
        });
    }

    pool.join();
}
