const std = @import("std");

pub export fn zig_foo(x: u32) callconv(.C) void {
    const stdout = std.io.getStdOut();
    stdout.writer().print("{}? A really nice number :)\n", .{x}) catch |e| {
        std.log.err("{}", .{e});
    };
}
