const std = @import("std");

var rand: std.rand.DefaultPrng = std.rand.DefaultPrng.init(0);

pub export fn zig_increment(n: u32) callconv(.C) u32 {
    const new_num = n + 1;
    return new_num;
}

pub export fn init_rando(seed: c_long) callconv(.C) void {
    rand = std.rand.DefaultPrng.init(@intCast(seed));
}

pub export fn gimme_rando() callconv(.C) c_uint {
    return rand.random().int(c_uint);
}
