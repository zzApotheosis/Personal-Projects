const std = @import("std");
const bst = @import("bst.zig");
const BST = bst.BST;

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

    var bst1: BST(i32) = BST(i32).new(allocator);
    defer bst1.deinit();
    try bst1.add(10);
    try bst1.add(5);
    try bst1.add(15);
    try bst1.add(3);
    try bst1.add(7);
    try bst1.add(13);
    try bst1.add(17);
    try bst1.add(20);
    try bst1.add(10);
    try bst1.add(10);
    try bst1.add(11);
    try bst1.add(10);
    std.debug.print("bst size: {}\n", .{bst1.size()});
    std.debug.print("bst max depth: {}\n", .{bst1.maxDepth()});
    std.debug.print("bst node <{}> depth: {}\n", .{
        13,
        bst1.depth(13),
    });
    try bst1.dump(std.io.getStdErr());

    const array_list: std.ArrayList(i32) = try bst1.toArrayList();
    defer array_list.deinit();
    for (array_list.items) |e| {
        std.debug.print("<{}>\n", .{e});
    }

    var bst2: BST(i32) = BST(i32).new(allocator);
    defer bst2.deinit();
    try bst2.add(0);
    std.debug.print("bst2 size: {}\n", .{bst2.size()});
    std.debug.print("bst2 max depth: {}\n", .{bst2.maxDepth()});

    var bst3: BST(i32) = BST(i32).new(allocator);
    defer bst3.deinit();
    std.debug.print("bst3 size: {}\n", .{bst3.size()});
    std.debug.print("bst3 max depth: {}\n", .{bst3.maxDepth()});
}
