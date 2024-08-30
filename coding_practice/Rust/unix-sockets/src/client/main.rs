use core::panic;
use std::io::prelude::*;
use std::os::unix::net::UnixStream;
use std::time::Duration;
use std::vec::Vec;

const SOCKET: &str = "/tmp/rustsock";
const DELAY: Duration = Duration::new(0u64, 0_500_000_000u32);

fn main() -> std::io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        panic!("Pass some arguments to this program, freaking noob.");
    }

    let mut stream: UnixStream = UnixStream::connect(SOCKET)?;
    let mut msg: String = std::string::String::new();

    for i in 1..args.len() {
        stream.write_all(args[i].as_bytes())?;
        stream.write(b"\n")?;

        let mut reader = std::io::BufReader::new(&mut stream);
        msg.clear();
        reader.read_line(&mut msg)?;
        let msg = msg.trim();
        println!("Received response: {msg}");

        std::thread::sleep(DELAY);
    }

    /*
     * In this implementation, I am defining that the server should close the
     * connection when the server receives an empty message, delimited by
     * the newline character "\n".
     */
    stream.write_all(b"\n")?;
    Ok(())
}
