fn main() -> Result<(), i32> {
    // Tell Cargo that if the given file changes, to rerun this build script.
    println!("cargo:rerun-if-changed=src/c/foo.c");
    println!("cargo:rerun-if-changed=src/c/foo.h");
    println!("cargo:rerun-if-changed=src/c/test-foo.h");
    // println!("cargo:rustc-link-lib=gmp");
    // println!("cargo:rustc-link-arg=-lgmp");
    let mut libcool_builder = cc::Build::new();
    libcool_builder.file("src/c/foo.c");
    libcool_builder.compile("libfoo.a");
    Ok(())
}
