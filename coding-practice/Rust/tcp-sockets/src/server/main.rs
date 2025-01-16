use std::io::Read;
use std::net::{TcpListener, TcpStream};

fn main() {
    serve().expect("Server error");
}

fn serve() -> ::std::io::Result<()> {
    // TODO
    let listener = TcpListener::bind("0.0.0.0:8080")?;
    for stream in listener.incoming() {
        // TODO: Fork and handle client
        let client_stream = stream?;
        ::std::thread::spawn(move || handle_client(client_stream));
    }
    Ok(())
}

fn handle_client(mut stream: TcpStream) {
    // TODO
    eprintln!("Got client!");
    let mut buffer = [0u8; 1024];

    stream.read(&mut buffer).expect("Unable to read stream");
    let s = ::std::str::from_utf8_mut(&mut buffer).expect("Conversion error");

    eprintln!("Received message: {}", s);

    // Done
    stream
        .shutdown(::std::net::Shutdown::Both)
        .expect("Unable to shutdown stream");
}
