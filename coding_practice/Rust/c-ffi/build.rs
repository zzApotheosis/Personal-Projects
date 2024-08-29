fn main() -> Result<(), i32> {
    // Tell Cargo that if the given file changes, to rerun this build script.
    println!("cargo:rerun-if-changed=src/cool.c");
    println!("cargo:rerun-if-changed=src/cool.h");
    let mut c_compiler = cc::Build::new();
    c_compiler.file("src/cool.c");
    c_compiler.compile("libcool.a");
    Ok(())
}
