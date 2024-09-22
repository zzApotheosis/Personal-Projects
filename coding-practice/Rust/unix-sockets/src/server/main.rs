use std::io::{BufRead, Write};
use std::os::unix::net::{UnixListener, UnixStream};
use std::thread;

const SOCKET: &str = "/tmp/rustsock";

fn handle_client(stream: UnixStream) -> std::io::Result<()> {
    let mut stream_reader = std::io::BufReader::new(&stream);
    let mut stream_writer = std::io::BufWriter::new(&stream);

    /*
     * NOTE: This implementation uses the newline "\n" character
     * as a delimiter in the communication. Otherwise, the stream
     * would wait forever until it received EOF to act on the data
     * received. As our favorite cringe lord once said in a certain
     * prequel movie, "this is where the fun begins".
     */
    loop {
        /*
         * Get a message from the client.
         */
        let mut msg: String = String::new();
        stream_reader.read_line(&mut msg)?;
        let msg = msg.trim();

        /*
         * Check for empty string (that's the signal to quit).
         */
        if msg.len() == 0usize {
            break;
        }
        println!("Received message: {msg}");

        /*
         * Echo the message back to the client.
         */
        let response = String::from(msg) + "\n";
        stream_writer.write_all(response.as_bytes())?;
        stream_writer.flush()?;
    }

    /*
     * Shutdown the connection.
     */
    stream.shutdown(std::net::Shutdown::Both)?;

    Ok(())
}

fn main() -> std::io::Result<()> {
    // Ignore errors here; it's okay if it doesn't currently exist
    std::fs::remove_file(SOCKET).ok();

    // Listen on the socket
    let listener = UnixListener::bind(SOCKET)?;

    // accept connections and process them, spawning a new thread for each one
    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                /* connection succeeded */
                thread::spawn(move || {
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

    // Remove the socket file
    std::fs::remove_file(SOCKET).expect("UNABLE TO REMOVE SOCKET");

    Ok(())
}
