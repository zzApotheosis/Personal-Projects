use proc_macro::TokenStream;

#[proc_macro_derive(Describe)]
pub fn describe_derive(input: TokenStream) -> TokenStream {
    // 1. Convert the input tokens into a standard string
    let input_str = input.to_string();

    // 2. Naive parsing: Find the word immediately following "struct"
    let struct_name =
        extract_struct_name(&input_str).expect("Describe macro could not find a valid struct name");

    // 3. Generate the output code as a raw string
    let generated_code = format!(
        "impl {} {{
            pub fn describe(&self) {{
                println!(\"This is a struct named '{}'\");
            }}
        }}",
        struct_name, struct_name
    );

    // 4. Parse the string back into a compiler TokenStream
    generated_code.parse().unwrap()
}

// A simple helper function to hunt down the struct name
fn extract_struct_name(input: &str) -> Option<&str> {
    let words: Vec<&str> = input.split_whitespace().collect();
    for i in 0..words.len() {
        if words[i] == "struct" && i + 1 < words.len() {
            // Get the word after 'struct', stripping any trailing braces or whitespace
            return Some(words[i + 1].trim_end_matches('{'));
        }
    }
    None
}
