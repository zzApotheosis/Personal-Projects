const std = @import("std");
const BST = @import("bst.zig").BST;

pub fn main() !void {
    const allocator = std.heap.c_allocator;

    // var node: Node(i32) = Node(i32).new(allocator);
    // defer node.deinit();
    // node.setData(5);
    // try node.setLeft(7);
    // try node.setLeft(8);
    // try node.setRight(5);
    // std.debug.print("data = {}\n", .{node.getData()});

    // const ligma = struct {};
    // std.debug.print("{}\n", .{@TypeOf(ligma)});

    const key_of_interest: i32 = 15;
    const limit: usize = 15;

    var bst: BST(i32, i32) = BST(i32, i32).init(allocator);
    defer bst.deinit();
    for (0..limit) |i| {
        const n: i32 = @intCast(i);
        try bst.add(n, @as(i32, @intCast(limit)) - n);
    }
    std.debug.print("bst size: {}\n", .{bst.size()});
    std.debug.print("bst max depth: {}\n", .{bst.maxDepth()});
    std.debug.print("bst node <{}> depth: {}\n", .{
        key_of_interest,
        bst.depth(key_of_interest),
    });
    try bst.dump(std.io.getStdErr());

    try bst.balance();
    std.debug.print("After binary tree rebalance...\n", .{});
    std.debug.print("bst size: {}\n", .{bst.size()});
    std.debug.print("bst max depth: {}\n", .{bst.maxDepth()});
    std.debug.print("bst node <{}> depth: {}\n", .{
        key_of_interest,
        bst.depth(key_of_interest),
    });
    try bst.dump(std.io.getStdErr());
}
