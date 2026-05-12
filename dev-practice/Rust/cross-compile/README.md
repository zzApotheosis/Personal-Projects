# README

Cross compiling from Linux to Windows.

## Steps to Build

- `rustup target add x86_64-pc-windows-gnu`  
- `sudo dnf install mingw64-gcc mingw64-binutils`
- `cargo build --target x86_64-pc-windows-gnu`

## Steps to Run

Since we are using the `x86_64-pc-windows-gnu` target rather than `x86_64-pc-windows-msvc`,
the runtime environment needs to have a GNU environment available to use. It is expected that the
operating system contains an installation of [MinGW](https://www.mingw-w64.org/) or similar runtime
environment.

- Copy the executable file to a Windows machine that has a GNU runtime environment installed.
- Open a PowerShell or command prompt and execute the program!
