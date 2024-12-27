const std = @import("std");
const BST = @import("bst.zig").BST;

pub fn main() !void {
    const allocator = std.heap.c_allocator;
    const K: type = i32;
    const V: type = i32;

    const key_of_interest: K = 495;
    const limit: usize = 512;

    var bst = BST(K, V).init(allocator);
    defer bst.deinit();
    for (0..limit) |i| {
        const n: K = @intCast(i);
        try bst.add(n, @as(V, @intCast(limit)) - n);
    }
    std.debug.print("bst size: {}\n", .{bst.size()});
    std.debug.print("bst max depth: {}\n", .{bst.maxDepth()});
    std.debug.print("bst node <{}> depth: {}\n", .{
        key_of_interest,
        bst.depth(key_of_interest),
    });
    // try bst.dump(std.io.getStdErr());

    try bst.balance();
    std.debug.print("After binary tree rebalance...\n", .{});
    std.debug.print("bst size: {}\n", .{bst.size()});
    std.debug.print("bst max depth: {}\n", .{bst.maxDepth()});
    std.debug.print("bst node <{}> depth: {}\n", .{
        key_of_interest,
        bst.depth(key_of_interest),
    });
    // try bst.dump(std.io.getStdErr());
}
