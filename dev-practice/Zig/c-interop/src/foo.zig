const std = @import("std");

pub export fn zig_foo(x: u32) callconv(.C) void {
    const stdout = std.io.getStdOut();
    stdout.writer().print("Hello from Zig!\n", .{}) catch |e| {
        std.log.err("{}", .{e});
    };
    var buffer: [64:0]u8 = undefined;
    var msg: [:0]u8 = undefined;
    if (x < 60) {
        msg = std.fmt.bufPrintZ(&buffer, "No offense, but you suck.", .{}) catch |e| {
            std.log.err("{}", .{e});
            return;
        };
    } else if (x < 70) {
        msg = std.fmt.bufPrintZ(&buffer, "Do better next time.", .{}) catch |e| {
            std.log.err("{}", .{e});
            return;
        };
    } else if (x < 80) {
        msg = std.fmt.bufPrintZ(&buffer, "Not bad, but could be better.", .{}) catch |e| {
            std.log.err("{}", .{e});
            return;
        };
    } else if (x < 90) {
        msg = std.fmt.bufPrintZ(&buffer, "Good job!", .{}) catch |e| {
            std.log.err("{}", .{e});
            return;
        };
    } else if (x < 100) {
        msg = std.fmt.bufPrintZ(&buffer, "Outstanding!", .{}) catch |e| {
            std.log.err("{}", .{e});
            return;
        };
    } else if (x < 110) {
        msg = std.fmt.bufPrintZ(&buffer, "Okay, now you're just showing off.", .{}) catch |e| {
            std.log.err("{}", .{e});
            return;
        };
    } else if (x == 110) {
        msg = std.fmt.bufPrintZ(&buffer, "You really had to prove something, didn't you... freaking nerd.", .{}) catch |e| {
            std.log.err("{}", .{e});
            return;
        };
    }
    stdout.writer().print("{}? {s}\n", .{ x, msg }) catch |e| {
        std.log.err("{}", .{e});
    };
}
