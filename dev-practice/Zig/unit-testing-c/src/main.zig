const std = @import("std");
const c = @cImport({
    @cInclude("project.h");
});

pub fn main() !void {
    try std.io.getStdOut().writer().print("Run `zig build test` to run the unit tests!\n", .{});
}

test "basic addition" {
    var result: i32 = undefined;
    c.test_foo_test_add(&result);
    if (result != 0) return error.TestFailed;
}

test "expected failure" {
    var result: i32 = undefined;
    c.test_foo_test_bad_add(&result);
    if (result != 0) return error.TestFailed;
}
