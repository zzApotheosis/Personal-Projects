const std = @import("std");
const common = struct {
    usingnamespace @import("common.zig");
};

const allocator = std.heap.c_allocator;
const limit: u32 = 8192;

pub fn main() void {
    var vector: std.ArrayList(u8) = std.ArrayList(u8).init(allocator);
    defer vector.deinit();

    common.fill(&vector, limit) catch |e| {
        std.log.err("{}", .{e});
        return;
    };

    common.dump(&vector) catch |e| {
        std.log.err("{}", .{e});
        return;
    };
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
