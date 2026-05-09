fn main() {
    println!("cargo:rerun-if-changed=src/config.json");

    // let out_dir: String = std::env::var("OUT_DIR").unwrap();
    // let dest_path = Box::from(&out_dir);
    // let dest_path = std::path::Path::from(&out_dir).join("config.json");
}
