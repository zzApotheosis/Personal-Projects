fn main() -> Result<(), i32> {
    // Tell Cargo that if the given file changes, to rerun this build script.
    println!("cargo:rerun-if-changed=src/cool.c");
    println!("cargo:rerun-if-changed=src/cool.h");
    // println!("cargo:rustc-link-lib=gmp");
    println!("cargo:rustc-link-arg=-lgmp");
    let mut libcool_builder = cc::Build::new();
    libcool_builder.file("src/cool.c");
    libcool_builder.compile("libcool.a");
    Ok(())
}
