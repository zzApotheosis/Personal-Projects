extern crate cc;

fn main() {
    cc::Build::new().file("src/cool.c").compile("libcool.a");
}
