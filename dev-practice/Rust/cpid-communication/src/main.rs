use std::io::{Read, Write};
use std::process::{Command, Stdio};

fn main() -> std::io::Result<()> {
    // 1. Configure and spawn the child process
    let mut child = Command::new("sort")
        .stdin(Stdio::piped()) // Tell the OS to set up a write pipe to child's stdin
        .stdout(Stdio::piped()) // Tell the OS to set up a read pipe from child's stdout
        .spawn()?; // Spawn the process

    // 2. Safely take ownership of the stdin and stdout streams.
    // We use .take() because these are wrapped in an Option.
    let mut stdin = child.stdin.take().expect("Failed to open stdin");
    let mut stdout = child.stdout.take().expect("Failed to open stdout");

    // Data to send to the child process
    let input_data = "banana\napple\ncherry\n";
    println!("[Parent] Sending data to child:\n{}", input_data);

    // 3. Write data to the child's stdin
    stdin.write_all(input_data.as_bytes())?;

    // 4. CRITICAL: Drop stdin to close the pipe.
    // This sends EOF to the child process, letting it know no more data is coming.
    drop(stdin);

    // 5. Read the output from the child's stdout
    let mut output_data = String::new();
    stdout.read_to_string(&mut output_data)?;

    println!("[Parent] Received sorted data from child:\n{}", output_data);

    // 6. Wait for the child process to finish executing
    let status = child.wait()?;
    println!("[Parent] Child process exited: {}", status);

    Ok(())
}
