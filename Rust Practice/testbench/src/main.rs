extern crate lettre;

use lettre::{SendableEmail, EmailAddress, Transport, Envelope, SmtpClient};

fn main() {
    let to_address = "zzApotheosis@gmail.com";
    let smtp_server = "smtp.googlemail.com";
    let smtp_username = "zzApotheosis@gmail.com";
    let smtp_password = "gruxigzwkpbdxljj";
    let smtp_port: u16 = 587;

    let email = EmailBuilder::new()
        .to(to_address)
        .from(smtp_username)
        .subject("Hi, Hello world")
        .body("Hello world.")
        //.attachment_from_file(Path::new("Cargo.toml"), None, &TEXT_PLAIN)
        .build()
        .unwrap();

    // Open a local connection on port 25
    let mut mailer = SmtpTransportBuilder::new((sntp_server, smtp_port)).unwrap
        .hello_name("localhost")
        .credentials(smtp_username, smtp_password)
        .security_level(SecurityLevel::AlwaysEncrypt)
        .smtp_utf8(true)
        .build();

    // Send the email
    let result = mailer.send(email.clone());

    if result.is_ok() {
        println!("Email sent");
    } else {
        println!("Could not send email: {:?}", result);
    }

    //assert!(result.is_ok());
}
