const std = @import("std");
const foo = @import("foo.zig");

pub fn main() !void {
    try std.io.getStdOut().writer().print("Run zig test src/main.zig\n", .{});
}

fn foo1(comptime T: type, a: T, b: T) T {
    return a + b;
}

fn foo2(comptime T: type, a: T, b: T) T {
    return a - b;
}

test "function pointers but not really lmao" {
    const NUM_TYPE: type = i32;
    var n: NUM_TYPE = undefined;
    const a: NUM_TYPE = 10;
    const b: NUM_TYPE = 5;
    foo.bar(NUM_TYPE, foo1, a, b, &n);
    try std.testing.expectEqual(n, @as(NUM_TYPE, 15));
    foo.bar(NUM_TYPE, foo2, a, b, &n);
    try std.testing.expectEqual(n, @as(NUM_TYPE, 5));
}
