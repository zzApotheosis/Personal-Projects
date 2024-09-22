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

    let stream: UnixStream = UnixStream::connect(SOCKET)?;
    let mut stream_reader = std::io::BufReader::new(&stream);
    let mut stream_writer = std::io::BufWriter::new(&stream);
    let mut msg: String;
    let mut response: Vec<String> = Vec::new();

    println!("Sending messages...");
    for i in 1..args.len() {
        print!("\r{i}");
        std::io::stdout().flush().ok();
        msg = args[i].clone() + "\n";
        stream_writer.write_all(msg.as_bytes())?;
        stream_writer.flush()?;

        msg.clear();
        stream_reader.read_line(&mut msg)?;
        let msg = msg.trim();
        response.push(String::from(msg));

        std::thread::sleep(DELAY);
    }
    println!();

    /*
     * In this implementation, I am defining that the server should close the
     * connection when the server receives an empty message, delimited by
     * the newline character "\n".
     */
    stream_writer.write_all(b"\n")?;

    /*
     * Let's show all the responses we got from the server, in order.
     */
    print!("All responses received from server:");
    for msg in response {
        print!(" {msg}");
    }
    println!();
    Ok(())
}
