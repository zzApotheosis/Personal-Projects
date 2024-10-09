const std = @import("std");

pub export fn zig_foo(x: u32) callconv(.C) void {
    const stdout = std.io.getStdOut();
    stdout.writer().print("Hello from Zig!\n", .{}) catch |e| {
        std.log.err("{}", .{e});
    };
    var buffer: [64:0]u8 = undefined;
    var msg: [:0]u8 = undefined;
    if (x < 10) {
        msg = std.fmt.bufPrintZ(&buffer, "A pretty small number :)", .{}) catch |e| {
            std.log.err("{}", .{e});
            return;
        };
    } else if (x < 100) {
        msg = std.fmt.bufPrintZ(&buffer, "A nice-sized number!", .{}) catch |e| {
            std.log.err("{}", .{e});
            return;
        };
    } else {
        msg = std.fmt.bufPrintZ(&buffer, "You really had to prove something...", .{}) catch |e| {
            std.log.err("{}", .{e});
            return;
        };
    }
    stdout.writer().print("{}? {s}\n", .{ x, msg }) catch |e| {
        std.log.err("{}", .{e});
    };
}
