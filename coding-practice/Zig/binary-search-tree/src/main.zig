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

    const value_of_interest: i32 = 420;

    var bst: BST(i32) = BST(i32).new(allocator);
    defer bst.deinit();
    for (0..512) |i| {
        try bst.add(@as(i32, @intCast(i)));
    }
    std.debug.print("bst size: {}\n", .{bst.size()});
    std.debug.print("bst max depth: {}\n", .{bst.maxDepth()});
    std.debug.print("bst node <{}> depth: {}\n", .{
        value_of_interest,
        bst.depth(value_of_interest),
    });
    // try bst.dump(std.io.getStdErr());

    try bst.balance();
    std.debug.print("After binary tree rebalance...\n", .{});
    std.debug.print("bst size: {}\n", .{bst.size()});
    std.debug.print("bst max depth: {}\n", .{bst.maxDepth()});
    std.debug.print("bst node <{}> depth: {}\n", .{
        value_of_interest,
        bst.depth(value_of_interest),
    });
    // try bst.dump(std.io.getStdErr());
}
