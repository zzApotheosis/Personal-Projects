use std::io::Read;

const JSON_FILE: &str = "src/config.json";

fn main() {
    let mut f = std::fs::File::open(JSON_FILE).expect("Interesting");
    let mut json_data: Vec<u8> = Vec::new();
    f.read_to_end(&mut json_data).expect("Interesting");
    let json_data = json::parse(
        String::from_utf8(json_data)
            .expect("Failed to parse UTF8 data")
            .trim(),
    )
    .expect("Failed to parse JSON data");

    eprintln!("{}", json::stringify_pretty(json_data, 2));
}
