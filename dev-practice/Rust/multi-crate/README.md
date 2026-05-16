# Multi-Crate Project

With Rust's package manager `cargo`, multiple projects can be organized and structured in a way that one project can include and depend on another project.
This is an example to use the `foo` project as a library crate and have the `bar` project use it as a dependency.

## Steps to Re-create

What exists in this project is an already-completed example, but it can be re-created.

- `mkdir -pv foo bar`
- `cd foo`
- `cargo init --lib`
- `cd ../bar`
- `cargo init --bin`

Modify the `Cargo.toml` file in the `bar` project to use the local `foo` project.

```toml
[dependencies]
foo = { path = "../foo" }
```

## Cargo Workspace

For additional structure, `cargo` can recognize when it is running in a virtual workspace environment. This example places a `Cargo.toml` file at the
root directory (the same directory as this `README` file) that specifies the following contents:

```toml
[workspace]
resolver = "3"
members = ["foo", "bar"]
```

When `cargo` is invoked from this directory, it understands that it needs to work with the member projects specified in `Cargo.toml`.

Invoking `cargo build` will build all member projects, which provides a clean and structured way to organize different but related projects.

