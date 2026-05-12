# README

Cross compiling from Linux to Windows.

## Steps to Build

- `rustup target add x86_64-pc-windows-gnu`  
- `sudo dnf install mingw64-gcc mingw64-binutils`
- `cargo build --target x86_64-pc-windows-gnu`

## Steps to Run

- Copy the executable file to a Windows machine that has a GNU runtime environment installed.  
- Create a batch script with the following contents

```
cross-compile.exe
pause
```

- Double click the batch script and watch it execute the program!
