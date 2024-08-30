use std::io::Write;
use std::os::unix::net::{UnixListener, UnixStream};
use std::thread;

fn handle_client(mut stream: UnixStream) {
    // ...
    // stream
    stream.write_all(b"what").expect("NOT GOOD");
    stream.shutdown(std::net::Shutdown::Both).expect("NOT GOOD");
}

fn main() -> std::io::Result<()> {
    let listener = UnixListener::bind("/tmp/rustsock")?;

    // accept connections and process them, spawning a new thread for each one
    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                /* connection succeeded */
                thread::spawn(|| handle_client(stream));
            }
            Err(err) => {
                /* connection failed */
                eprintln!("{}", err);
                break;
            }
        }
    }
    Ok(())
}
