use std::{io::Write, net::Shutdown};

fn main() {
    let mut connection =
        ::std::net::TcpStream::connect("127.0.0.1:8080").expect("Unable to connect to server");

    connection
        .write_fmt(format_args!("Hello from client!"))
        .expect("Unable to send message to server");

    connection.shutdown(Shutdown::Both).unwrap();
}
