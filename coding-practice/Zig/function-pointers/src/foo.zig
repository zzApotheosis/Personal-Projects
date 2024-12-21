const std = @import("std");
const testing = std.testing;

export fn add(a: i32, b: i32) i32 {
    return a + b;
}

pub fn bar(T: type, f: fn (T: type, a: T, b: T) T, a: T, b: T, r: ?*T) void {
    const c = f(T, a, b);
    r.?.* = c;
}

test "basic add functionality" {
    try testing.expect(add(3, 7) == 10);
}
