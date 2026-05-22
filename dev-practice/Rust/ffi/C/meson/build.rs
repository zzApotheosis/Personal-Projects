use std::env;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    // Get the output directory provided by Cargo
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());

    let status = Command::new("./src/c/build.sh")
        .status()
        .expect("Failed to invoke C build script");
    assert!(status.success());

    // Define paths for our object file and static library
    // let obj_path = out_dir.join("my_lib.o");
    // let lib_path = out_dir.join("libmy_lib.a");

    // Step 1: Compile the C file into an object file (.o)
    // let compile_status = Command::new("cc")
    //     .arg("-fPIE")
    //     .arg("-c") // Compile and assemble, but do not link
    //     .arg("src/c/my_lib.c") // The source file
    //     .arg("-o")
    //     .arg(&obj_path) // Output object file
    //     .status()
    //     .expect("Failed to execute C compiler (cc). Is it installed?");

    // assert!(compile_status.success(), "C compilation failed");

    // Step 2: Archive the object file into a static library (.a)
    // let ar_status = Command::new("ar")
    //     .arg("rcs") // r: insert, c: create, s: index
    //     .arg(&lib_path) // Output static library
    //     .arg(&obj_path) // Input object file
    //     .status()
    //     .expect("Failed to execute archiver (ar). Is it installed?");

    // assert!(ar_status.success(), "Archiving failed");

    // Step 3: Tell Cargo where to look for the library
    println!("cargo:rustc-link-search=native={}", out_dir.display());

    println!("cargo:rustc-link-arg=-Wl,-rpath=$ORIGIN:$ORIGIN/../lib64");

    // Step 4: Tell Cargo which library to link (strip the 'lib' prefix and '.a' suffix)
    // "my_lib" means look for libmy_lib.a on Unix or my_lib.lib on Windows
    println!("cargo:rustc-link-lib=static=foo");
    println!("cargo:rustc-link-lib=dylib=gcrypt");

    // Re-run build script if the C file changes
    println!("cargo:rerun-if-changed=src/c/src/meson.build");
    println!("cargo:rerun-if-changed=src/c/build.sh");
    println!("cargo:rerun-if-changed=src/c/src/foo.c");
    println!("cargo:rerun-if-changed=src/c/src/foo.h");
}
