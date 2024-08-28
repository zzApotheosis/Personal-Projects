extern crate cc;

fn main() {
    // Tell Cargo that if the given file changes, to rerun this build script.
    println!("cargo:rerun-if-changed=src/cool.c");
    println!("cargo:rerun-if-changed=src/cool.h");
    cc::Build::new().file("src/cool.c").compile("libcool.a");
}
