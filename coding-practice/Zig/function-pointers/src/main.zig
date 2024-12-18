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

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

test "function pointers but not really lmao" {
    var n: i32 = undefined;
    const a: i32 = 10;
    const b: i32 = 5;
    foo.bar(i32, foo1, a, b, &n);
    try std.testing.expect(n == @as(i32, 15));
    foo.bar(i32, foo2, a, b, &n);
    try std.testing.expect(n == @as(i32, 5));
}
