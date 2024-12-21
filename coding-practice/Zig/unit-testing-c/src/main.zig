const std = @import("std");
const c = @cImport({
    @cInclude("c/test-foo.h");
});
const test_add = c.test_add;
const test_bad_add = c.test_bad_add;

pub fn main() !void {
    try std.io.getStdOut().writer().print("Fuck\n", .{});
}

test "basic addition" {
    var result: i32 = undefined;
    test_add(&result);
    if (result != 0) return error.TestFailed;
}

test "expected failure" {
    var result: i32 = undefined;
    test_bad_add(&result);
    if (result != 0) return error.TestFailed;
}
