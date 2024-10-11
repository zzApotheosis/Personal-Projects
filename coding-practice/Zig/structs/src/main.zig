const std = @import("std");
const lib = @import("root.zig");

pub fn main() !void {
    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("Run `zig build test` to run the tests.\n", .{});

    try bw.flush(); // don't forget to flush!

    use_lib() catch |e| {
        std.log.err("{}", .{e});
    };
}

fn use_lib() !void {
    const stdout = std.io.getStdOut();
    const a = @as(i32, 60);
    const b = @as(i32, 9);
    try stdout.writer().print("{} + {} = {}\n", .{ a, b, lib.add(a, b) });

    var my_struct = lib.MyStruct.new();
    my_struct.increment();
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
