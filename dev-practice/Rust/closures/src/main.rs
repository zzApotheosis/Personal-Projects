use std::{boxed::Box, collections::VecDeque};

type Job = Box<dyn FnOnce() + 'static>;
type T = u32;

fn main() {
    const N: T = 16;

    let mut queue = JobQueue::new();
    for i in 0..N {
        match queue.enqueue(Box::new(move || eprintln!("[{i}] Check this out!"))) {
            Ok(_) => {}
            Err(e) => eprintln!("{e}"),
        }
    }

    assert!(queue.size() > 0);

    while queue.size() > 0 {
        let job = queue.pop();
        if job.is_none() {
            continue;
        }

        let job = job.unwrap();
        job();
    }

    assert!(queue.size() == 0);
}

pub struct JobQueue {
    jobs: VecDeque<Job>,
}

impl JobQueue {
    pub fn new() -> Self {
        Self {
            jobs: VecDeque::new(),
        }
    }

    pub fn enqueue(&mut self, job: Job) -> Result<(), String> {
        self.jobs.push_back(job);
        Ok(())
    }

    pub fn pop(&mut self) -> Option<Job> {
        self.jobs.pop_front()
    }

    pub fn size(&self) -> usize {
        self.jobs.len()
    }

    pub fn clear(&mut self) {
        self.jobs.clear();
    }
}
