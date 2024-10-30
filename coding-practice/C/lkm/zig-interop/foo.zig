pub export fn zig_foo(n: u32) callconv(.C) u32 {
    return n + 1;
}
