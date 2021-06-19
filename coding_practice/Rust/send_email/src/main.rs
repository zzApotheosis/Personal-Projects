extern crate lettre;
extern crate lettre_email;

use lettre::smtp::authentication::IntoCredentials;
use lettre::{SmtpClient, Transport};
use lettre_email::EmailBuilder;

fn main() {
    let to_address = "zzApotheosis@gmail.com";
    let user = "zzApotheosis@gmail.com";
    let password = "tgacfijbmtjaayip";
    let subject = "Hello Rust!";
    let body = "Hello Rust! Big ligma!";
    let smtp_address = "smtp.gmail.com";

    let email = EmailBuilder::new()
        .from(user)
        .to(to_address)
        .subject(subject)
        .text(body)
        .build()
        .unwrap()
        .into();

    let credentials = (user, password).into_credentials();

    let mut client = SmtpClient::new_simple(smtp_address)
        .unwrap()
        .credentials(credentials)
        .transport();

    let _result = client.send(email);
}


