use std::io::{BufRead, Write};
use std::os::unix::net::{UnixListener, UnixStream};
use std::thread;

const SOCKET: &str = "/tmp/rustsock";

fn handle_client(stream: UnixStream) -> std::io::Result<()> {
    let mut reader = std::io::BufReader::new(&stream); // I don't trust this

    /*
     * NOTE: This implementation uses the newline "\n" character
     * as a delimiter in the communication. Otherwise, the stream
     * would wait forever until it received EOF to act on the data
     * received. As our favorite cringe lord once said in a certain
     * prequel movie, "this is where the fun begins".
     */
    loop {
        let mut msg: String = String::new();
        reader.read_line(&mut msg)?;
        let msg = msg.trim();
        if msg.len() == 0usize {
            break;
        }
        println!("Received message: {msg}");
        // writeln!(&stream, "{}", msg)?;
        (&stream).write_all(msg.as_bytes())?;
        (&stream).write_all(b"\n")?;
    }

    stream.shutdown(std::net::Shutdown::Both)?;
    Ok(())
}

fn main() -> std::io::Result<()> {
    let listener = UnixListener::bind(SOCKET)?;

    // accept connections and process them, spawning a new thread for each one
    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                /* connection succeeded */
                thread::spawn(|| {
                    handle_client(stream).expect("NOT GOOD");
                    println!("Client finished");
                });
            }

            Err(err) => {
                /* connection failed */
                eprintln!("{err}");
                break;
            }
        }
    }
    Ok(())
}
